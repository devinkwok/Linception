# functions for cleaning and moving data
# also building dataset for meta-model

# traceback errors for debugging
options(error=function()traceback(2))

# need car library for vif function
# TODO: need R version 3.05 or higher to get most recent version of car
# library("car", lib.loc="lib")

DATA_PATH = "datasets"
MAX_DATASETS = 1  # to limit computation time while debugging

MIN_COLUMNS = 2
MAX_COLUMNS = 15
MIN_ROWS = 100
MAX_ROWS = 10000
NUM_FOLDS = 1  # for k fold cross validation, set to 5 later

# checks if dataframe is valid to include
is_valid_data = function(dataframe) {
    if(!inherits(dataframe, "data.frame")) {
        print("ERROR: object is not a dataframe:")
        return(FALSE)
    }

    # check for sufficient predictors
    columns = colnames(dataframe)
    if (length(columns) < MIN_COLUMNS || length(columns) > MAX_COLUMNS) {
        print("ERROR: invalid number of predictors")
        return(FALSE)
    }

    # check for sufficient samples
    if (nrow(dataframe) < MIN_ROWS ||  nrow(dataframe) > MAX_ROWS) {
        print("ERROR: invalid number of samples")
        return(FALSE)
    }

    # check for na values
    for (column in columns) {
        number_na = sum(is.na(dataframe$column))
        if (number_na > 0) {
            print("ERROR: NA values found in dataframe")
            return(FALSE)
        }
    }

    #TODO: check for categorical variables

    #TODO: check for min/max number of predictors

    #TODO: check for any other issues? (e.x. zeros for logistic regression)

    return(TRUE)
}

# for some reason R doesn't want to work with getdata in the for loop (maybe due to scope?)
# this extra function seems to fix that problem
get_builtin_data = function(name) {
    return(get(name))
}

# regex for removing first space and any trailing characters from string
strip_whitespace_trailing = function(string) {
    return(sub(" .*", "", string))
}

# utility function to enable use of R's built in datasets
# later we will use files with data from outside sources
save_builtin_datasets_to_file = function(max_datasets, path) {

    # beware: data() returns a dataframe
    # with results in data()$results but this is a vector!
    # the relevant dataset names are in the 3rd column [,3]
    names = data(package = "datasets")$results[,3]

    # need to get rid of any characters after a space in the names
    # for R to be able to find the dataset
    names = lapply(names, strip_whitespace_trailing)
    added_names = vector("list", max_datasets)

    # use a separate increment operator because append concatenates lists together
    # and also using array indexing breaks get(name) in get_builtin_data... wtf
    i = 1
    for (name in names) {
        dataset = get_builtin_data(name)
        dataset = data.frame(dataset)
        if (is_valid_data(dataset)) {
            filename = file.path(path, paste(name, ".csv", sep=""))
            write.table(dataset, file=filename)
            added_names[i] = filename
            print(paste("Saved dataset", filename))
            i = i + 1
        }
        else {
            print(paste("Failed to save dataset", name))
        }
        # truncate to max_datasets
        if (i > max_datasets) {
            break
        }
    }
    print(paste("Saved ", i - 1, " datasets"))
    return(added_names)
}

load_dataframe = function(path, filename) {
    return(read.table(file.path(path, filename)))
}

# creates a {0, 1} dataframe of [size] with every combination of 0 and 1 in rows
combination_matrix = function(size) {
    return(expand.grid(rep(list(c(0:1)), size)))
}

# refactor this out of fit_linear_models
build_formula_string = function(inclusion_matrix_row, columns) {
    response_var = columns[1]
    predictors = columns[-1] # this removes column 1 from the list
    num_included = sum(inclusion_matrix_row)
    
    included_predictors = vector("list", num_included)
    k = 1  # because R needs index variables
    
    # iterate through columns for each predictor
    for (j in 1:length(predictors)) {
        if (inclusion_matrix_row[j] == 1) {
            included_predictors[k] = predictors[j]
            k = k + 1
        }
    }

    # if there are no predictors, add a constant term
    formula_string = "1"
    if (length(included_predictors) > 0) {
        formula_string = paste(included_predictors, collapse="+")
    }
    formula_string = paste(response_var, "~", formula_string)
    return(formula_string)
}

# TODO: implement k-fold and validate each
k_fold_cv_train_lm = function(inclusion_matrix_row, dataframe, num_folds) {
    # get dataset columns
    columns = colnames(dataframe)
    models = list()
    test_results = list()

    formula_string = build_formula_string(inclusion_matrix_row, columns)

    for (i in 1:num_folds) {
        train_data = dataframe
        # TODO: create proper test subset
        test_data = dataframe
        model = lm(formula=formula_string, data=train_data)
        models[[1]] = model
        test_results[[1]] = test_lm(model, test_data)
    }
    return(c(models, test_results))
}

# fits linear models for every combination of parameters to a dataset
# ASSUMPTION: first column of dataframe is used as response variable
fit_linear_models = function(dataframe, num_folds) {

    # go through all combinations of predictors
    # TODO: if there are too many predictors it is better to stochastically sample this space
    
    # use a matrix of 0 and 1 to indication inclusion/exclusion of predictor
    # rows are for different linear models
    # columns are the inclusion/exclusion of that column in the dataframe
    # number of predictors is number of columns - 1
    inclusion_matrix = combination_matrix(length(dataframe) - 1)

    model_data = vector("list", nrow(inclusion_matrix))

    # iterate through rows for each model
    for (i in 1:length(inclusion_matrix[,1])) {
        # train k-fold number of models
        model_data[[i]] = k_fold_cv_train_lm(inclusion_matrix[i,], dataframe, num_folds)
    }
    return(model_data)
}

# finds MSE for linear_model on test_data, assuming first column is response
# TODO: test
test_lm = function(linear_model, test_data) {
    predict_response = predict(linear_model, test_data)
    true_response = test_data[,1]
    num_samples = length(true_response)
    mse = (predict_response - true_response)^2 / num_samples
    return(mse)
}

# returns statistics for a single linear model
# TODO finish and test
get_individual_statistics = function(linear_model) {
    summary_obj = summary(linear_model)
    statistics = list(
        # could use summary_obj$df instead
        "num_training_samples"=(linear_model$df.residual + linear_model$rank),
        "num_coefficients"=linear_model$rank,
        "r_squared"=summary_obj$r.squared,
        "r_squared_adjusted"=summary_obj$adj.r.squared,

        # VIF needs package
        )
    return(statistics)
}

# TODO incomplete, requires graph of adjacent parameter subsets
get_paired_statistics = function(lm_subset, lm_subset_statistics, lm_superset, lm_superset_statistics) {
    # significance of added parameter: needs to know
    # which parameter is added
    # use summary_obj$coefficients

    # ANOVA significance

    # AIC and BIC relative
    
    # relative values of individual statistics
}

# tests loading data, then generating linear models as data to input into meta model
generate_lms = function() {
    filenames = list.files(path=DATA_PATH)
    filenames = filenames[1:MAX_DATASETS]
    linear_models = list()

    for (filename in filenames) {
        dataset = load_dataframe(DATA_PATH, filename)

        models_and_data = fit_linear_models(dataset, NUM_FOLDS)
        linear_models = append(linear_models, models_and_data)
    }
    return(linear_models)
}

# TODO: call this after linear models generated
# need graph of adjacent models
generate_lm_statistics = function(linear_model_table) {

}

# only need to run the following once to get data into files
# filenames = save_builtin_datasets_to_file(MAX_DATASETS, DATA_PATH)

# where everything starts
lms = generate_lms()

generate_lm_statistics(lms)