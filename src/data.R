# functions for building dataset for meta-model
# work in parent folder to access folders like datasets and lib

# library("car", lib.loc="lib")

source(file.path("src", "util.R"))
source(file.path("src", "graph.R"))

# seed rng generator to get consistent results
RANDOM_SEED = 001

# traceback errors for debugging
options(error=function()traceback(2))


# internal data format: data frames don't work with lm objects
# instead, use a named list of lists
# initialize variable as NULL to let the functions filling the
# lists determine the columns

# ASSUMPTION: the first named column is the response variable
get_response_colname = function(dataframe) {
    columns = colnames(dataframe)
    return(columns[1])
}

get_predictors_colnames = function(dataframe) {
    columns = colnames(dataframe)
    return(columns[-1]) # this removes column 1 from the list
}

k_fold_split = function(dataframe, num_folds) {
    train_sets = vector("list", num_folds)
    test_sets = vector("list", num_folds)
    fold_num = vector("integer", num_folds)

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
        fold_num[[i]] = i
    }

    k_folds = list(
        "train"=train_sets,
        "test"=test_sets,
        "fold_num"=fold_num
        )
    
    return(k_folds)
}

# fits linear models for every combination of parameters to a dataset
fit_linear_models = function(data_index, dataframe, num_folds) {
    response_name = get_response_colname(dataframe)
    predictors = get_predictors_colnames(dataframe)

    #create a graph of the predictors and get a list of vertices
    graph = create_pgraph(length(predictors))
    vertices = list_pgraph_vertices(graph)

    # assign data in data frame to k-folds
    # TODO: split data into smaller numbers of observations
    data_folds = k_fold_split(dataframe, num_folds)

    individual_data = NULL
    pairwise_data = NULL

    # iterate through vertices of graph and k-folds for each model
    # iterate through folds first so that the inner loop fills the
    # graph, then builds the edges
    for (i in 1:num_folds) {

        train_data = data_folds[["train"]][[i]]
        test_data = data_folds[["test"]][[i]]
        response_var = train_data[[response_name]]
        per_fold_outputs = NULL
        # need separate list because R can't handle objects in lists!
        model_bank = list()

        for (j in 1:length(vertices)) {
            vertex = vertices[j]
            predictor_matrix = get_vertex_predictors(vertex)
            formula_string = build_formula_string(predictor_matrix, response_name, predictors)
            model = lm(formula=formula_string, data=train_data)
            model_bank[[j]] = model

            model_data = list(
                "dataset"=data_index,
                "response_mean"=mean(response_var),
                "response_sd"=sd(response_var),
                "predictors"=predictor_str(vertex),
                "k_fold"=data_folds[["fold_num"]][[i]]
                )

            test_results = test_lm(model, test_data)
            statistics = get_individual_stats(model)

            model_data = append(model_data, test_results)
            model_data = append(model_data, statistics)
            per_fold_outputs = append_list_of_lists_rows(per_fold_outputs, model_data)
        }

        # iterate through edges of graph and get pair statistics
        edges = list_pgraph_edges(graph)
        for (k in 1:num_edges(graph)) {
            edge = edges[k,]
            subset = predictor_str(get_edge_subset(graph, edge))
            superset = predictor_str(get_edge_superset(graph, edge))
            added_predictor_index = get_edge_added_predictor_index(graph, edge)
            added_predictor = predictors[[added_predictor_index]]

            sub_index = get_list_of_lists_index(per_fold_outputs, "predictors", subset)
            super_index = get_list_of_lists_index(per_fold_outputs, "predictors", superset)
            subset_lm = model_bank[[sub_index]]
            superset_lm = model_bank[[super_index]]
            subset_data = get_list_of_lists_row(per_fold_outputs, sub_index)
            superset_data = get_list_of_lists_row(per_fold_outputs, super_index)

            paired_stats = get_paired_stats(subset_lm, superset_lm,
                    subset_data, superset_data, added_predictor)
            pairwise_data = append_list_of_lists_rows(pairwise_data, paired_stats)
        }
        individual_data = append_list_of_lists_rows(individual_data, per_fold_outputs)
    }

    return(list("individual"=individual_data, "paired"=pairwise_data))
}

# finds MSE for linear_model on test_data
test_lm = function(linear_model, test_data) {
    predict_response = predict(linear_model, newdata=test_data)
    true_response = test_data[[get_response_colname(test_data)]]
    num_samples = length(true_response)
    sse = sum((predict_response - true_response)^2)
    mse = sse / num_samples
    sd_response = sd(true_response)

    test_results = list(
        "test_mean"=mean(true_response),
        "test_sd"=sd_response,
        "mse"=mse,
        "std_mse"=mse / sd_response,
        "std_mse_sqrt"=sqrt(mse / sd_response)
        ) # can add more columns here as needed
    
    return(test_results)
}

# returns statistics for a single linear model
get_individual_stats = function(linear_model) {
    summary_obj = summary(linear_model)

    statistics = list(
        # could use summary_obj$df instead
        "num_training_samples"=(linear_model$df.residual + linear_model$rank),
        "num_coefficients"=linear_model$rank,
        "r_sq"=summary_obj$r.squared,
        "r_sq_adj"=summary_obj$adj.r.squared
        # VIF needs package
        )

    return(statistics)
}

get_paired_stats = function(subset_lm, superset_lm, subset_stats, superset_stats, added_predictor) {
    superset_summary = summary(superset_lm)$coefficients
    added_predictor_index = match(added_predictor, rownames(superset_summary))
    # t-test for coefficient, column 1 gives beta and 4 gives p-value
    predictor_coefficient = superset_summary[added_predictor_index, 1]
    predictor_pvalue = superset_summary[added_predictor_index, 4]

    # ANOVA row 2 gives superset model, p-value is in column 4
    anova_pvalue = anova(subset_lm, superset_lm)[2,4]

    statistics = list(
        "dataset"=superset_stats[["dataset"]],
        "k_fold"=superset_stats[["k_fold"]],
        "response_mean"=superset_stats[["response_mean"]],
        "response_sd"=superset_stats[["response_sd"]],
        "num_training_samples"=superset_stats[["num_training_samples"]],
        "superset_num_coefficients"=superset_stats[["num_coefficients"]],
        "subset_predictors"=subset_stats[["predictors"]],
        "superset_predictors"=superset_stats[["predictors"]],
        "added_predictor"=added_predictor,
        "predictor_coefficient"=predictor_coefficient,
        "predictor_pvalue"=predictor_pvalue,
        "anova_pvalue"=anova_pvalue,
        "superset_mse"= superset_stats[["mse"]],
        "superset_test_sd"= superset_stats[["test_sd"]],
        "mse_diff"=subset_stats[["mse"]] - superset_stats[["mse"]],
        "mse_ratio"=subset_stats[["mse"]] / superset_stats[["mse"]],
        "std_mse_diff"=subset_stats[["std_mse"]] - superset_stats[["std_mse"]],
        "std_mse_ratio"=subset_stats[["std_mse"]] / superset_stats[["std_mse"]],
        "superset_r_sq"=superset_stats[["r_sq"]],
        "superset_r_sq_adj"=superset_stats[["r_sq_adj"]],
        "r_sq_diff"=superset_stats[["r_sq"]] - subset_stats[["r_sq"]],
        "r_sq_ratio"=superset_stats[["r_sq"]] / subset_stats[["r_sq"]],
        "r_sq_adj_diff"=superset_stats[["r_sq_adj"]] - subset_stats[["r_sq_adj"]],
        "r_sq_adj_ratio"=superset_stats[["r_sq_adj"]] / subset_stats[["r_sq_adj"]]

        # TODO: AIC, BIC, power transform, VIF (needs "car" package)
        )

    return(statistics)
}

# loads each file and generates linear models as data to input into meta model
generate_meta_data = function(num_folds, max_datasets, data_path, output_path, ext) {
    # get files
    filenames = list_data_filenames(data_path, ext)
    filenames = filenames[1:max_datasets]  # truncate

    individual_data = NULL
    paired_data = NULL
    
    # generate data
    for (i in 1:length(filenames)) {
        name = filenames[i]
        dataset = load_dataframe(file.path(data_path, name))
        if (is.null(dataset)) {
            logging_print("WARNING: dataset not loaded", name)
        }
        else {
            logging_print("Generating linear models for dataset:", name)
            models_and_data = fit_linear_models(i, dataset, num_folds)
            individual_data = append_list_of_lists_rows(individual_data, models_and_data[["individual"]])
            paired_data = append_list_of_lists_rows(paired_data, models_and_data[["paired"]])
        }
    }

    # remove column with linear models from output since they are objects
    excluded_columns = list("lm"=TRUE)
    individual_df = make_data_frame(individual_data, excluded_columns)
    paired_df = make_data_frame(paired_data, excluded_columns)

    # save results to file
    logging_print("Generated paired and individual lm data with number of rows, head:",
                    nrow(paired_df), head(paired_df),
                    nrow(individual_df), head(individual_df))
    save_dataframe(individual_df, timestamp("lm-data-individual", ext), output_path)
    save_dataframe(paired_df, timestamp("lm-data-paired", ext), output_path)
}