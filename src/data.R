# functions for cleaning and moving data
# also building dataset for meta-model

source(file.path("src", "util.R"))
# work in parent folder to access folders like datasets and lib

# traceback errors for debugging
options(error=function()traceback(2))

# need car library for vif function
# TODO: need R version 3.05 or higher to get most recent version of car
# library("car", lib.loc="lib")

DATA_PATH = file.path("datasets", "set1")
# DATA_PATH = file.path("datasets", "kevin")
OUTPUT_PATH = "outputs"
MAX_DATASETS = 5  # to limit computation time while debugging
NUM_FOLDS = 5
RANDOM_SEED = 001 # seed for rng generator

# internal data format: data frames don't work with lm objects
# instead, use a named list of lists
# initialize variable as NULL to let the functions filling the
# lists determine the columns

# creates a {0, 1} dataframe of [size] with every combination of 0 and 1 in rows
combination_matrix = function(size) {
    return(expand.grid(rep(list(c(0:1)), size)))
}

# ASSUMPTION: the first named column is the response variable
get_response_colname = function(dataframe) {
    columns = colnames(dataframe)
    return(columns[1])
}

get_predictors_colnames = function(dataframe) {
    columns = colnames(dataframe)
    return(columns[-1]) # this removes column 1 from the list
}

# refactor this out of fit_linear_models
build_formula_string = function(inclusion_matrix_row, dataframe) {
    response_var = get_response_colname(dataframe)
    predictors = get_predictors_colnames(dataframe)
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

k_fold_split = function(dataframe, num_folds) {
    train_sets = vector("list", num_folds)
    test_sets = vector("list", num_folds)

    rows = nrow(dataframe)
    min_rows_per_fold = floor(rows / num_folds)
    fold_id = rep(1:num_folds, min_rows_per_fold)

    # add in remainder to match with number of rows ONLY if remainder nonzero
    remainder = rows %% num_folds
    if (remainder > 0) {
        fold_id = c(fold_id, 1:remainder)
    }

    # randomize selection of folds
    set.seed(RANDOM_SEED) # make output consistent
    fold_id = sample(fold_id) # this permutes the ids

    for (i in 1:num_folds) {
        # train: select from dataframe where fold != i
        train_sets[[i]] = dataframe[fold_id != i,]
        # test: select from dataframe where fold == i
        test_sets[[i]] = dataframe[fold_id == i,]
    }

    k_folds = list(
        "train"=train_sets,
        "test"=test_sets
        )
    
    return(k_folds)
}

# fits linear models for every combination of parameters to a dataset
# ASSUMPTION: first column of dataframe is used as response variable
fit_linear_models = function(data_index, dataframe, num_folds) {
    # go through all combinations of predictors
    # TODO: if there are too many predictors it is better to stochastically sample this space
    
    # use a matrix of 0 and 1 to indication inclusion/exclusion of predictor
    # rows are for different linear models
    # columns are the inclusion/exclusion of that column in the dataframe
    # number of predictors is number of columns - 1
    num_cols = length(get_predictors_colnames(dataframe))
    inclusion_matrix = combination_matrix(num_cols)

    # assign data in data frame to k-folds
    # adding a new column
    data_folds = k_fold_split(dataframe, num_folds)

    model_data = NULL

    # iterate through rows and k-folds for each model
    for (i in 1:length(inclusion_matrix[,1])) {

        included_predictors = inclusion_matrix[i,]
        formula_string = build_formula_string(included_predictors, dataframe)

        for (j in 1:num_folds) {
            
            train_data = data_folds[["train"]][[j]]
            response_var = train_data[[get_response_colname(dataframe)]]
            model = lm(formula=formula_string, data=train_data)

            model_data_row = list(
                "dataset"=data_index,
                "lm"=model,
                "response_mean"=mean(response_var),
                "response_sd"=sd(response_var),
                "predictors"=paste(included_predictors, collapse=""),
                "k_fold"=j
                )

            test_data = data_folds[["test"]][[j]]
            test_results = test_lm(model, test_data)

            statistics = get_individual_statistics(model)

            model_data_row = append(model_data_row, test_results)
            model_data_row = append(model_data_row, statistics)
            model_data = append_list_of_lists_rows(model_data, model_data_row)
        }
    }
    return(model_data)
}

# finds MSE for linear_model on test_data, assuming first column is response
test_lm = function(linear_model, test_data) {
    predict_response = predict(linear_model, newdata=test_data)
    true_response = test_data[[get_response_colname(test_data)]]
    num_samples = length(true_response)
    sse = sum((predict_response - true_response)^2)
    mse = sse / num_samples
    sd_response = sd(true_response)

    test_results = list(
        "sse"=sse,
        "test_mean"=mean(true_response),
        "test_sd"=sd_response,
        "mse"=mse,
        "mse_sqrt"=sqrt(mse),
        "standardized_mse"=mse / sd_response,
        "standardized_mse_sqrt"=sqrt(mse / sd_response)
        ) # can add more columns here as needed
    
    return(test_results)
}

# returns statistics for a single linear model
get_individual_statistics = function(linear_model) {
    summary_obj = summary(linear_model)
    statistics = list(
        # could use summary_obj$df instead
        "num_training_samples"=(linear_model$df.residual + linear_model$rank),
        "num_coefficients"=linear_model$rank,
        "r_squared"=summary_obj$r.squared,
        "r_squared_adjusted"=summary_obj$adj.r.squared
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
generate_lms = function(data_directory, max_datasets, num_folds) {
    linear_models = NULL

    filenames = list_data_filenames(data_directory)
    filenames = filenames[1:max_datasets]  # truncate
    
    for (i in 1:length(filenames)) {
        name = filenames[i]
        dataset = load_dataframe(data_directory, name)
        if (is.null(dataset)) {
            logging_print("WARNING: dataset not loaded", name)
        }
        else {
            logging_print("Generating linear models for dataset:", name)
            models_and_data = fit_linear_models(i, dataset, num_folds)
            linear_models = append_list_of_lists_rows(linear_models, models_and_data)
        }
    }
    return(linear_models)
}

generate_data = function() {
    lms_and_data = generate_lms(DATA_PATH, MAX_DATASETS, NUM_FOLDS)
    # remove column with linear models from output since they are objects
    excluded_columns = list("lm"=TRUE)
    dataframe = make_data_frame(lms_and_data, excluded_columns)
    metafile_name = timestamp("lm-data-individual")
    logging_print("Generated lm data with number of rows, head:",
                    nrow(dataframe), head(dataframe))
    save_dataframe(dataframe, metafile_name, OUTPUT_PATH)
}

# only need to run the following once to get data into files
# filenames = save_builtin_datasets_to_file(MAX_DATASETS, file.path("datasets", "builtin"))

# to use: run this
generate_data()