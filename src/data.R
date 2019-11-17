# functions for building dataset for meta-model
# work in parent folder to access folders like datasets and lib

# need car library for VIF and power transform functions
library("car")

# include other files in the project
source(file.path("src", "util.R"))
source(file.path("src", "graph.R"))

# seed rng generator to get consistent results
RANDOM_SEED = 001
# lower bound for how many observations to fit lm
# NOTE: powerTransform breaks when sample size is too small for some datasets
MIN_TRAINING_SIZE = 100
# lower bound for how much smaller a subset can be
MIN_SUBSET_PROPORTION = 0.5

# traceback errors for debugging
options(error=function()traceback(2))


# internal data format: data frames don't work with lm objects
# instead, use a named list of lists
# initialize variable as NULL to let the functions filling the
# lists determine the columns

# ASSUMPTION: first column is response
get_response_colname = function(dataframe) {
    columns = colnames(dataframe)
    return(columns[1])
}

# ASSUMPTION: first column is response
get_predictor_colnames = function(dataframe) {
    columns = colnames(dataframe)
    return(columns[-1]) # this removes column 1 from the list
}

# returns a vector of sizes of subsets
get_subset_split = function(rows_per_train_set, num_subsets) {
    if (rows_per_train_set < MIN_TRAINING_SIZE + num_subsets) {
        logging_print("ERROR: not enough training observations to meet
                minimum plus num_subsets", MIN_TRAINING_SIZE, num_subsets)
        return(NULL)
    }

    # get subset_proportion as even geometric division from rows_per_train_set to MIN_TRAINING_SIZE
    subset_range = rows_per_train_set / MIN_TRAINING_SIZE
    subset_proportion =  1 / subset_range^(1 / num_subsets)
    if (subset_proportion < MIN_SUBSET_PROPORTION) {
        subset_proportion = MIN_SUBSET_PROPORTION
    }
    subsets = list()
    subsets[[1]] = rows_per_train_set
    if (num_subsets > 1) {
        for (i in 2:num_subsets) {
            superset_obs = subsets[[i-1]]
            num_obs = round(superset_obs * subset_proportion)
            if (num_obs >= superset_obs) {
                num_obs = superset_obs - 1
            }
            subsets[[i]] = num_obs
        }
    }
    return(subsets)
}

# splits into k folds but also into subsets with less observations
# by repeated multiplication by subset_proportion:
# for example, if subset_proportion = 0.5 and there are 1000 training 
# observations, then it creates subsets of 1000, 500, 250, 125, etc.
# TODO: 
k_fold_split = function(dataframe, num_folds, num_subsets) {
    train_sets = vector("list", num_folds)
    test_sets = vector("list", num_folds)
    fold_num = vector("integer", num_folds)

    num_rows = nrow(dataframe)
    # if num_folds == 1, then the whole dataset will be used for training
    if (num_folds == 1) {
        train_sets[[1]] = dataframe
        # test set is meaningless - only put a single sample so test code doesn't break
        test_sets[[1]] = dataframe[1,]
        fold_num[[1]] = 1
    }
    else {
        rows_per_fold = floor(num_rows / num_folds)
        rows_per_train_set = rows_per_fold * (num_folds - 1)
        # otherwise split into subsets
        subset_splits = get_subset_split(rows_per_train_set, num_subsets)

        fold_id = rep(1:num_folds, rows_per_fold)

        # TODO; put remainder in test set using index remainder_id
        # for now just remove them lol
        # that way every training set is the same size
        remainder = num_rows %% num_folds
        # add in remainder to match with number of rows ONLY if remainder nonzero
        # remainder_id = num_folds + 1
        # if (remainder > 0) {
        #     fold_id = c(fold_id, rep(remainder_id, remainder))
        # }

        # randomize selection of folds
        set.seed(RANDOM_SEED) # make output consistent
        fold_id = sample(fold_id) # this permutes the ids

        j = 1 # index variable
        for (i in 1:num_folds) {

            # train: select from dataframe where fold != i
            train_superset = dataframe[fold_id != i,]
            # test: select from dataframe where fold == i
            test_set = dataframe[fold_id == i,]

            for (num_observations in subset_splits) {
                # split data into smaller subsets of observations
                train_sets[[j]] = train_superset[1:num_observations,]
                test_sets[[j]] = test_set
                fold_num[[j]] = i
                j = j + 1
            }
        }
    }

    k_folds = list(
        "train"=train_sets,
        "test"=test_sets,
        "fold_num"=fold_num
        )

    return(k_folds)
}

# fits linear models for every combination of parameters to a dataset
fit_linear_models = function(data_index, dataframe, num_folds, num_subsets) {
    response_name = get_response_colname(dataframe)
    predictors = get_predictor_colnames(dataframe)

    #create a graph of the predictors and get a list of vertices
    graph = create_pgraph(length(predictors))
    vertices = V(graph)

    # assign data in data frame to k-folds
    data_folds = k_fold_split(dataframe, num_folds, num_subsets)

    individual_data = NULL
    pairwise_data = NULL

    # iterate through vertices of graph and k-folds for each model
    # iterate through folds first so that the inner loop fills the
    # graph, then builds the edges
    for (i in 1:length(data_folds[["train"]])) {

        train_data = data_folds[["train"]][[i]]
        test_data = data_folds[["test"]][[i]]
        response_var = train_data[[response_name]]
        per_fold_outputs = NULL
        # need separate lists because R can't handle objects in lists!
        model_bank = list()
        ptransform_bank = list()

        for (j in 1:length(vertices)) {
            vertex = vertices[j]
            predictor_matrix = get_vertex_predictors(vertex)
            formula_string = build_formula_string(predictor_matrix, response_name, predictors)

            model = lm(formula=formula_string, data=train_data)
            model_bank[[j]] = model

            ptransform = get_power_transform(train_data, predictor_matrix)
            ptransform_bank[[j]] = ptransform

            model_data = list(
                "dataset"=data_index,
                "response_mean"=mean(response_var),
                "response_sd"=sd(response_var),
                "predictors"=vertex_to_str(vertex),
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
            subset = vertex_to_str(get_edge_subset(graph, edge))
            superset = vertex_to_str(get_edge_superset(graph, edge))
            added_predictor_index = get_edge_added_predictor_index(graph, edge)
            added_predictor = predictors[[added_predictor_index]]

            # pull objects out of the various lists that temporariliy store them
            # because R is a mess
            sub_index = get_list_of_lists_index(per_fold_outputs, "predictors", subset)
            super_index = get_list_of_lists_index(per_fold_outputs, "predictors", superset)
            sub_lm = model_bank[[sub_index]]
            super_lm = model_bank[[super_index]]
            sub_ptransform = ptransform_bank[[sub_index]]
            super_ptransform = ptransform_bank[[super_index]]
            sub_data = get_list_of_lists_row(per_fold_outputs, sub_index)
            super_data = get_list_of_lists_row(per_fold_outputs, super_index)

            # get stats for the model pair along this edge
            paired_stats = get_paired_stats(sub_lm, super_lm, sub_ptransform,
                    super_ptransform, sub_data, super_data, added_predictor)
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
        )
    
    return(test_results)
}

# returns statistics for a single linear model
get_individual_stats = function(linear_model) {
    summary_obj = summary(linear_model)

    statistics = list(
        "num_training_samples"=(linear_model$df.residual + linear_model$rank),
        "num_coefficients"=linear_model$rank,
        "r_sq"=summary_obj$r.squared,
        "r_sq_adj"=summary_obj$adj.r.squared
        )

    return(statistics)
}

get_power_transform = function(dataset, predictor_matrix) {
    response = get_response_colname(dataset)
    predictors = get_predictor_colnames(dataset)
    matrix = cbind(dataset[response])
    for (i in 1:length(predictors)) {
        if (predictor_matrix[i] == 1) {
            name = predictors[[i]]
            column = dataset[name]
            matrix = cbind(matrix, column)
        }
    }
    return(powerTransform(matrix))
}

aic_correction = function(aic, n, p) {
    return(aic + 2*(p + 2)*(p + 3) / (n - p - 1))
}

# all stats are of the form superset - subset or superset/subset
get_paired_stats = function(subset_lm, superset_lm, subset_ptransform,
            superset_ptransform, subset_stats, superset_stats, added_predictor) {

    superset_summary = summary(superset_lm)$coefficients
    added_predictor_index = match(added_predictor, rownames(superset_summary))
    # t-test for coefficient, column 1 gives beta and 4 gives p-value
    predictor_coefficient = superset_summary[added_predictor_index, 1]
    predictor_pvalue = superset_summary[added_predictor_index, 4]

    # ANOVA row 2 gives superset model, p-value is in column 4
    anova_pvalue = anova(subset_lm, superset_lm)[2,4]

    # AIC and BIC difference
    n = superset_stats[["num_training_samples"]]
    sub_p = subset_stats[["num_coefficients"]]
    super_p = superset_stats[["num_coefficients"]]
    sub_AIC = extractAIC(subset_lm, k=2)[[2]]
    super_AIC = extractAIC(superset_lm, k=2)[[2]]
    sub_AIC_cor = aic_correction(sub_AIC, n, sub_p)
    super_AIC_cor = aic_correction(super_AIC, n, super_p)
    sub_BIC = extractAIC(subset_lm, k=log(n))[[2]]
    super_BIC = extractAIC(superset_lm, k=log(n))[[2]]

    # shapiro-wilk normality test on residuals
    sub_normality_pvalue = shapiro.test(subset_lm$residuals)[[2]]
    super_normality_pvalue = shapiro.test(superset_lm$residuals)[[2]]

    # variance inflation factor
    # set to 0 if there is only 1 predictor (2 coefficients including intercept)
    vifs = c(0)
    if (super_p > 2) {
        vifs = vif(superset_lm)
    }
    # need to reduce index by 1 because intercept isn't included
    added_predictor_vif = vifs[[added_predictor_index - 1]]

    # power transform (just the distance is fine for now)
    # estimated power, row is the predictor and column 1 is estimated power
    ptransform_summary = summary(superset_ptransform)
    response_est_ptransform = ptransform_summary$result[1,1]
    added_predictor_est_ptransform = ptransform_summary$result[added_predictor_index,1]
    # pvalue for all untransformed, row 2 for untransformed and column 3 for pvalue
    null_ptransform_pvalue = ptransform_summary$tests[2,3]

    statistics = list(
        "dataset"=superset_stats[["dataset"]],
        "k_fold"=superset_stats[["k_fold"]],
        "response_mean"=superset_stats[["response_mean"]],
        "response_sd"=superset_stats[["response_sd"]],
        "num_training_samples"=n,
        "superset_num_coefficients"=super_p,
        "superset_predictor_matrix"=superset_stats[["predictors"]],
        "added_predictor"=added_predictor,
        "added_predictor_coefficient"=predictor_coefficient,
        "added_predictor_pvalue"=predictor_pvalue,
        "superset_anova_pvalue"=anova_pvalue,
        "superset_mse"= superset_stats[["mse"]],
        "superset_test_sd"= superset_stats[["test_sd"]],
        "mse_diff"=subset_stats[["mse"]] - superset_stats[["mse"]],
        "mse_ratio"=subset_stats[["mse"]] / superset_stats[["mse"]],
        "std_mse_diff"=subset_stats[["std_mse"]] - superset_stats[["std_mse"]],
        "std_mse_ratio"=subset_stats[["std_mse"]] / superset_stats[["std_mse"]],
        "superset_r_sq"=superset_stats[["r_sq"]],
        "superset_r_sq_adj"=superset_stats[["r_sq_adj"]],
        "r_sq_diff"=subset_stats[["r_sq"]] - superset_stats[["r_sq"]],
        "r_sq_ratio"=subset_stats[["r_sq"]] / superset_stats[["r_sq"]],
        "r_sq_adj_diff"=subset_stats[["r_sq_adj"]] - superset_stats[["r_sq_adj"]],
        "r_sq_adj_ratio"=subset_stats[["r_sq_adj"]] / superset_stats[["r_sq_adj"]],
        "aic_diff"=sub_AIC - super_AIC,
        "aic_ratio"=sub_AIC / super_AIC,
        "aic_corrected_diff"=sub_AIC_cor - super_AIC_cor,
        "aic_corrected_ratio"=sub_AIC_cor / super_AIC_cor,
        "bic_diff"=sub_BIC - super_BIC,
        "bic_ratio"=sub_BIC / super_BIC,
        "superset_normality_pvalue"=super_normality_pvalue,
        "normality_pvalue_diff"=sub_normality_pvalue - super_normality_pvalue,
        "normality_pvalue_ratio"=sub_normality_pvalue / super_normality_pvalue,
        "added_predictor_vif"=added_predictor_vif,
        "mean_vif"=mean(vifs),
        "max_vif"=max(vifs),
        "null_ptransform_pvalue"=null_ptransform_pvalue,
        "response_est_ptransform"=response_est_ptransform,
        "added_predictor_est_ptransform"=added_predictor_est_ptransform
        )

    return(statistics)
}

# loads each file and generates linear models as data to input into meta model
generate_meta_data = function(num_folds, num_subsets,
            max_datasets, data_path, output_path, ext) {

    # get files
    filenames = list_data_filenames(data_path, ext)
    filenames = filenames[1:max_datasets]  # truncate

    individual_data = NULL
    paired_data = NULL
    
    # generate data
    for (i in 1:length(filenames)) {
        name = filenames[i]
        # need to normalize because powerTransform can't take negative or zero values
        dataset = load_dataframe(file.path(data_path, name), normalize=TRUE)
        if (is.null(dataset)) {
            logging_print("WARNING: dataset not loaded", name)
        }
        else {
            logging_print("Generating linear models for dataset:", name)
            models_and_data = fit_linear_models(i, dataset, num_folds, num_subsets)
            individual_data = append_list_of_lists_rows(individual_data, models_and_data[["individual"]])
            paired_data = append_list_of_lists_rows(paired_data, models_and_data[["paired"]])
        }
    }

    individual_df = make_data_frame(individual_data)
    paired_df = make_data_frame(paired_data)

    # save results to file
    logging_print("Generated paired and individual lm data with number of rows, head:",
                    nrow(paired_df), head(paired_df),
                    nrow(individual_df), head(individual_df))
    save_dataframe(individual_df, timestamp("lm-data-individual", ext), output_path)
    save_dataframe(paired_df, timestamp("lm-data-paired", ext), output_path)
}