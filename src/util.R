# utility functions for loading and saving dataframes
# and manipulating list of lists

# conditions for dataset to satisfy
DEFAULT_REQUIRES = list(
    "TYPEOF" = "data.frame",
    "MIN_RESPONSE" = 1,
    "MAX_RESPONSE" = 1,
    "MIN_COLS" = 3,
    "MAX_COLS" = 15,
    "MIN_ROWS" = 100,
    "MAX_ROWS" = 10000,

    # escape all R and regex reserved characters in prefixes
    "RESPONSE_VAR_PREFIX" = "RESPONSE_VARIABLEx",
    "NULL_PREDICTOR_PREFIX" = "NULL_PREDICTORx"
)

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

    # go through every column and append them together
    appended_list = vector("list", num_columns)
    for (i in 1:num_columns) {
        appended_list[[i]] = append(list_of_lists_1[[i]], list_of_lists_2[[i]])
    }
    # weird syntax for assigning names
    names(appended_list) = names(list_of_lists_1)
    return(appended_list)
}

# find first index of obj_to_match in column
get_list_of_lists_index = function(list_of_lists, column, obj_to_match) {
    return(match(obj_to_match, list_of_lists[[column]]))
}

# returns a horizontal slice of a list of lists at index
get_list_of_lists_row = function(list_of_lists, index) {
    num_columns = length(list_of_lists)
    # go through every column and append them together
    row = vector("list", num_columns)
    for (i in 1:num_columns) {
        row[[i]] = list_of_lists[[i]][[index]]
    }
    names(row) = names(list_of_lists)
    return(row)
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

find_prefixed_var_index = function(columns, prefix) {
    return(grep(paste(prefix, ".*", sep=""), columns))
}

# removes columns named as empty string ""
remove_null = function(dataframe, null_prefix) {
    df = dataframe
    columns = colnames(df)
    indexes = find_prefixed_var_index(columns, null_prefix)
    df[indexes] = NULL
    return(df)
}

replace_and_shuffle_to_front = function(vector, index, replacement) {
    return(c(replacement, vector[-index]))
}

# checks if dataframe is valid to include
is_valid_data = function(dataframe, requires=DEFAULT_REQUIRES) {
    if(!inherits(dataframe, requires$TYPEOF)) {
        logging_print("ERROR: object is not a dataframe:", dataframe)
        return(FALSE)
    }

    columns = colnames(remove_null(dataframe, requires$NULL_PREDICTOR_PREFIX))

    #check for one response variable
    index = find_prefixed_var_index(columns, requires$RESPONSE_VAR_PREFIX)
    if (length(index) < requires$MIN_RESPONSE
            || length(index) > requires$MAX_RESPONSE) {
        logging_print("ERROR: invalid number of response variables", index)
        return(FALSE)
    }

    # check for sufficient predictors
    num_columns = length(columns) - length(index)
    if (num_columns < requires$MIN_COLS
            || num_columns > requires$MAX_COLS) {
        logging_print("ERROR: invalid number of predictors", num_columns)
        return(FALSE)
    }

    # check for sufficient samples
    num_rows = nrow(dataframe)
    if (num_rows < requires$MIN_ROWS
            ||  num_rows > requires$MAX_ROWS) {
        logging_print("ERROR: invalid number of samples", num_rows)
        return(FALSE)
    }

    # check for na values
    for (column in columns) {
        number_na = sum(is.na(dataframe[column]))
        if (number_na > 0) {
            logging_print("ERROR: NA values found in dataframe", number_na)
            return(FALSE)
        }
    }

    #TODO: check for categorical variables
    #TODO: check for any other issues? (e.x. zeros for logistic regression)
    return(TRUE)
}

load_dataframe = function(filename, requires=DEFAULT_REQUIRES) {
    dataframe = read.table(filename)
    if (is_valid_data(dataframe, requires)) {
        # remove null columns
        dataframe = remove_null(dataframe, requires$NULL_PREDICTOR_PREFIX)
        # reorder so that response variable comes first
        # FIXME: this only reorders the names, need to reorder dataframe
        # other_indexes = c(1:length(columns))
        # dataframe = dataframe[,c(index, )]
        columns = colnames(dataframe)
        index = find_prefixed_var_index(columns, requires$RESPONSE_VAR_PREFIX)
        columns = replace_and_shuffle_to_front(columns, index, columns[index])
        return(dataframe)
    }
    return(NULL)
}

save_dataframe = function(dataframe, name, path) {
    filename = file.path(path, name)
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

timestamp = function(prefix, suffix) {
    time = gsub("[ :]", "-", Sys.time())
    output_string = paste(prefix, time, sep="_")
    output_string = paste(output_string, suffix, sep=".")
    return(output_string)
}

list_data_filenames = function(path, target_extension) {
    filenames = list.files(path=path)

    valid_indexes = vector()
    for (i in 1:length(filenames)) {
        extension = strsplit(filenames[i], "\\.")[[1]]
        extension = extension[[length(extension)]]
        if (extension == target_extension) {
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
save_builtin_datasets_to_file = function(max_datasets, path, ext, requires=DEFAULT_REQUIRES) {

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
        # if the dataset can't be made into a dataframe, ignore it and move on
        tryCatch(
            {
                dataset = data.frame(dataset)
                # assume first column is response variable, tag with prefix
                columns = colnames(dataset)
                response_name = paste(requires$RESPONSE_VAR_PREFIX, columns[1], sep="")
                columns = replace_and_shuffle_to_front(columns, 1, response_name)
                colnames(dataset) = columns
                
                if (!is_valid_data(dataset, requires)) {
                    stop("conditions not met", call = NULL)
                }
                added_names[i] = save_dataframe(dataset, paste(name, ext, sep="."), path)
                i = i + 1
            },
            error=function(cond)
            {
                logging_print("Failed to save dataset", name, cond$message)
            }
        )

        # truncate to max_datasets
        if (i > max_datasets) {
            break
        }
    }
    logging_print("Saved total num of datasets:", i-1)
    return(added_names)
}
