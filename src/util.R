EXTENSION = "csv"

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

# regex for removing first space and any trailing characters from string
strip_whitespace_trailing = function(string) {
    return(sub(" .*", "", string))
}

# for some reason R doesn't want to work with getdata in the for loop (maybe due to scope?)
# this extra function seems to fix that problem
get_builtin_data = function(name) {
    return(get(name))
}

load_dataframe = function(path, filename) {
    return(read.table(file.path(path, filename)))
}

save_dataframe = function(dataframe, name, path) {
    filename = file.path(path, paste(name, EXTENSION, sep="."))
    write.table(dataframe, file=filename)
    logging_print("Saved dataset", filename)
    return(filename)
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

list_data_filenames = function(path) {
    print(path)
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
            added_names[i] = save_dataframe(dataset, name, path)
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