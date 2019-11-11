# functions for cleaning and moving data
# also building dataset for meta-model

# work in parent folder to access folders like datasets and lib

# traceback errors for debugging
options(error=function()traceback(2))

# need car library for vif function
# TODO: need R version 3.05 or higher to get most recent version of car
# library("car", lib.loc="lib")

DATA_PATH = "datasets"
OUTPUT_PATH = "outputs"
EXTENSION = "csv"
MAX_DATASETS = 5  # to limit computation time while debugging

MIN_COLUMNS = 2
MAX_COLUMNS = 15
MIN_ROWS = 100
MAX_ROWS = 10000
NUM_FOLDS = 5 # for k fold cross validation, set to 5 later
RANDOM_SEED = 001 # seed for rng generator

# internal data format: data frames don't work with lm objects
# instead, use a named list of lists
# initialize variable as NULL to let the functions filling the
# lists determine the columns

# utility function for printing error messages
logging_print = function(message, ...) {
    dots = list(...)
    print(message)
    for (obj in dots) {
        if(typeof(obj) == "list") {
            print(head(obj))
        }
        else {
            print(obj)
        }
    }
}

# utility function to simplify appending one or more rows of
# lists of lists together
#
# easier than using data frame, because linear model objects
# are considered lists, so they don't fit into data frames nicely
#
# to append a column, use the regular append function directly

append_list_of_lists_rows = function(list_of_lists_1, list_of_lists_2) {
    
    # if either list is NULL, return the non-null list
    if (is.null(list_of_lists_1)) {
        if(is.null(list_of_lists_2)) {
            logging_print("FATAL: both list of lists are null")
            return(NULL)
        }
        # swap so that list_1 is not NULL, so that everything
        # after this uses the same code path
        return(append_list_of_lists_rows(list_of_lists_2,list_of_lists_1))
    }

    # sanity check, lists should have same number of columns
    num_columns = length(list_of_lists_1)
    if (num_columns < 1) {
        logging_print("FATAL: list of lists must contain at least one column",
            list_of_lists_1, list_of_lists_2)
        return(NULL)
    }
    if (is.null(list_of_lists_2)) {
        return(list_of_lists_1)
    }

    # sanity checks if both lists not null
    if (num_columns != length(list_of_lists_2)) {
        logging_print("FATAL: list of lists do not match in number of columns",
            list_of_lists_1, list_of_lists_2)
        return(NULL)
    }
    if (!identical(names(list_of_lists_1), names(list_of_lists_2))) {
        logging_print("FATAL: list of lists do not match in number of columns",
            list_of_lists_1, list_of_lists_2)
        return(NULL)
    }

    appended_list = vector("list", num_columns)
    for (i in 1:length(list_of_lists_1)) {
        appended_list[[i]] = append(list_of_lists_1[[i]], list_of_lists_2[[i]])
    }
    # weird syntax for assigning names
    names(appended_list) = names(list_of_lists_1)
    return(appended_list)
}

# checks if dataframe is valid to include
is_valid_data = function(dataframe) {
    if(!inherits(dataframe, "data.frame")) {
        logging_print("ERROR: object is not a dataframe:", dataframe)
        return(FALSE)
    }

    # check for sufficient predictors
    num_columns = length(colnames(dataframe))
    if (num_columns < MIN_COLUMNS || num_columns > MAX_COLUMNS) {
        logging_print("ERROR: invalid number of predictors", num_columns)
        return(FALSE)
    }

    # check for sufficient samples
    num_samples = nrow(dataframe)
    if (num_samples < MIN_ROWS ||  num_samples > MAX_ROWS) {
        logging_print("ERROR: invalid number of samples", num_samples)
        return(FALSE)
    }

    # check for na values
    for (column in columns) {
        number_na = sum(is.na(dataframe$column))
        if (number_na > 0) {
            logging_print("ERROR: NA values found in dataframe", number_na)
            return(FALSE)
        }
    }

    #TODO: check for categorical variables

    #TODO: check for min/max number of predictors

    #TODO: check for any other issues? (e.x. zeros for logistic regression)

    return(TRUE)
}

save_table = function(dataframe, name, path) {
    filename = file.path(path, paste(name, EXTENSION, sep="."))
    write.table(dataframe, file=filename)
    logging_print("Saved dataset", filename)
    return(filename)
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
            added_names[i] = save_table(dataset, name, path)
            i = i + 1
        }
        else {
            logging_print("Failed to save dataset", name)
        }
        # truncate to max_datasets
        if (i > max_datasets) {
            break
        }
    }
    logging_print("Saved total num of datasets:", i-1)
    return(added_names)
}

load_dataframe = function(path, filename) {
    return(read.table(file.path(path, filename)))
}

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
        # print(train_sets[[i]])
        # print(test_sets[[i]])
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
# TODO: test
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
# TODO finish and test
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

list_data_filenames = function(path) {
    filenames = list.files(path=path)

    valid_indexes = vector()
    for (i in 1:length(filenames)) {
        extension = strsplit(filenames[i], "\\.")[[1]]
        extension = extension[[length(extension)]]
        if (extension == EXTENSION) {
            valid_indexes = append(valid_indexes, i)
        }
    }
    
    if (length(valid_indexes) > 0) {
        return(filenames[valid_indexes])
    }
    else {
        return(NULL)
    }
}

# tests loading data, then generating linear models as data to input into meta model
generate_lms = function(data_directory, max_datasets, num_folds) {
    linear_models = NULL

    filenames = list_data_filenames(data_directory)
    filenames = filenames[1:max_datasets]  # truncate
    
    for (i in 1:length(filenames)) {
        name = filenames[i]
        dataset = read.table(file.path(data_directory, name))
        logging_print("Generating linear models for dataset:", name)
        models_and_data = fit_linear_models(i, dataset, num_folds)
        linear_models = append_list_of_lists_rows(linear_models, models_and_data)
    }
    return(linear_models)
}

# excluded_columns is a named list, if the item is not null the column is excluded
make_data_frame = function(list_of_lists, excluded_columns) {
    col_names = names(list_of_lists)
    num_cols = length(col_names)
    dataframe = NULL # otherwise dataframe has 0 rows and won't append

    for (name in col_names) {
        # if excluded_columns$NAME returns non-null,
        # list_of_lists$NAME is not added to the data frame
        if (is.null(excluded_columns[[name]])) {
            column = unlist(list_of_lists[[name]], use.names="FALSE")

            if (is.null(dataframe)) {
                dataframe = data.frame(column)
                colnames(dataframe) = c(name)
            }
            else {  # append column
                dataframe[[name]] = column
            }
        }
    }
    return(dataframe)
}

timestamp = function(prefix) {
    time = gsub("[ :]", "-", Sys.time())
    return(paste(prefix, time, sep="_"))
}

generate_data = function() {
    lms_and_data = generate_lms(DATA_PATH, MAX_DATASETS, NUM_FOLDS)
    # remove column with linear models since they are objects
    excluded_columns = list("lm"=TRUE)
    dataframe = make_data_frame(lms_and_data, excluded_columns)
    metafile_name = timestamp("lm-data-individual")
    logging_print("Generated lm data with head():", head(dataframe))
    save_table(dataframe, metafile_name, OUTPUT_PATH)
}

# only need to run the following once to get data into files
# filenames = save_builtin_datasets_to_file(MAX_DATASETS, DATA_PATH)

# to use: run this
generate_data()