 # functions for cleaning and moving data

# traceback errors for debugging
options(error=function()traceback(2))

DATA_PATH = "datasets"
MIN_COLUMNS = 2
MAX_COLUMNS = 15
MIN_ROWS = 100
MAX_ROWS = 10000

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

# fits linear models for every combination of parameters to a dataset
# ASSUMPTION: first column of dataframe is used as response variable
# FIXME: incomplete
fit_linear_models = function(dataframe) {
    models = list()
    #TODO: k-fold cross validation
    # get dataset columns
    columns = colnames(dataframe)
    num_predictors = length(columns) - 1
    
    response_var = columns[1]
    predictors = columns[-1] # this removes column 1 from the list

    # go through all combinations of predictors
    # TODO: if there are too many predictors it is better to stochastically sample this space
    
    # use a matrix of 0 and 1 to indication inclusion/exclusion of predictor
    # rows are for different linear models
    # columns are the inclusion/exclusion of that column in the dataframe
    inclusion_matrix = combination_matrix(num_predictors)
    for (i in range(length(inclusion_matrix[,1]))) {
        included_predictors = list()
        for (j in range(num_predictors)) {
            if (inclusion_matrix[i, j] == 1) {
                included_predictors = append(included_predictors, predictors[j])
            }
        }
        predictor_string = paste(included_predictors, sep="+")
        print(predictor_string)
        model = lm(formula=paste(response_var, "~", predictor_string), data=dataframe)
        models = append(models, c(inclusion_matrix[,j], model))
    }
    return(models)
}

# tests loading data, then generating linear models as data to input into meta model
generate_lms = function() {
    filenames = list.files(path=DATA_PATH)
    linear_models = list()
    print(filenames)
    for (filename in filenames) {
        dataset = load_dataframe(DATA_PATH, filename)
        print(head(dataset))
        models = fit_linear_models(dataset)
        linear_models = append(linear_models, models)
    }
    return(linear_models)
}

# only need to run the following once to get data into files
filenames = save_builtin_datasets_to_file(5, DATA_PATH)

# where everything starts
lms = generate_lms()