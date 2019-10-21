# functions for cleaning and moving data

# checks if dataframe is valid to include
is_valid_data = function(dataframe) {
    # check for na values
    columns = colnames(dataframe)
    for (column in columns) {
        number_na = sum(is.na(dataframe$column))
        if (number_na > 0) {
            return(FALSE)
        }
    }

    #TODO: check for categorical variables

    #TODO: check for min/max number of predictors

    #TODO: check for any other issues? (e.x. zeros for logistic regression)

    return(TRUE)
}

# load and check data, return a list of datasets
load_data = function(max_datasets=5) {
    # TODO: use R's built in datasets for now
    # beware: data() returns a dataframe
    # with results in data()$results but this is a vector!
    # the relevant dataset names are in the 3rd column [,3]
    builtin_names = data(package = "datasets")$results[,3]
    datasets = data(list=builtin_names)
    for (dataset in datasets) {
        if (!is_valid_data(dataset)) {
            print(paste("FATAL: NA values found in dataset", dataset))
            return(FALSE)
        }
    }
    datasets = datasets[1:max_datasets]
    print(paste("Loaded ", length(datasets), " datasets"))
    return(datasets)
}

# creates a {0, 1} dataframe of [size] with every combination of 0 and 1 in rows
combination_matrix = function(size) {
    return(expand.grid(rep(list(c(0:1)), size)))
}

# fits linear models for every combination of parameters to a dataset
# ASSUMPTION: first column of dataframe is used as response variable
# FIXME: incomplete
fit_linear_models = function(dataframe) {
    models = list()
    #TODO: k-fold cross validation
    # get dataset columns
    columns = names(dataframe)
    num_predictors = length(columns) - 1
    response_var = columns[1]
    predictors = columns[-1] # this removes column 1 from the list

    # go through all combinations of predictors
    # TODO: if there are too many predictors it is better to stochastically sample this space
    
    # use a matrix of 0 and 1 to indication inclusion/exclusion of predictor
    # rows are for different linear models
    # columns are the inclusion/exclusion of that column in the dataframe
    inclusion_matrix = combination_matrix()
    for (i in range(length(inclusion_matrix[,1]))) {
        included_predictors = list()
        for (j in range(num_predictors)) {
            if (inclusion_matrix[i, j] == 1) {
                append(included_predictors, predictors[j])
            }
        }
        predictor_string = paste(included_predictors, collapse="+")
        model = lm(formula=paste(response_var, "~", predictor_string), data=dataframe)
        append(models, model)
    }
    return(models)
}

# tests loading data, then generating linear models as data to input into meta model
generate_lms = function() {
    datasets = load_data()
    linear_models = list()
    for (dataset in datasets) {
        # FIXME: not working yet
        # append(linear_models, fit_linear_models(dataset))
    }
    return(linear_models)
}

lms = generate_lms()
print(lms) #debugging