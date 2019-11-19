source(file.path("src", "util.R"))
source(file.path("src", "graph.R"))
source(file.path("src", "data.R"))

PREDICTED_MSE_KEY = "meta_model_predicted_mse"

fit_model_to_predictors = function(dataframe, meta_model, print_intermediate_steps=FALSE) {
    predictors = get_predictor_colnames(dataframe)
    num_predictors = length(predictors)
    data_folds = k_fold_split(dataframe, 1, 1)
    train_data = data_folds[["train"]][[1]]
    test_data = data_folds[["test"]][[1]]

    # forward step method: add one predictor at a time from 0 predictors
    graph = create_incremental_pgraph(num_predictors)
    current_vertex = get_null_model_vertex(graph)
    best_model_index = NA
    best_matrix = ""

    # max number of steps is the number of predictors
    for (step in 1:num_predictors) {

        # expand graph one predictor at a time otherwise it gets too big for memory
        graph = grow_incremental_pgraph(graph, current_vertex)
        # growth technically replaces previous graph obj, so need to get the reference to the new obj
        current_vertex = V(graph)[as_ids(current_vertex)]
        # choose neighbours that are in forward direction (increasing parameters)
        next_vertices = forward_step_vertices(graph, current_vertex)
        # make edgelist from current_vertex to next_vertices
        edges = vertices_to_edges(current_vertex, next_vertices)

        # get pair statistics for next_vertices
        lms_and_data = vertices_to_lm(c(current_vertex, next_vertices), train_data, test_data, 1, 1)
        paired_dataframe = edges_to_pairs(graph, edges, predictors, lms_and_data)
        # predict mse for these pairs
        predicted_mse_diff = predict(meta_model, newdata=paired_dataframe)

        # stop when the models are getting worse, which means mse_diff < 0
        best_mse_diff = 0

        # for each number of predictors, find model with best predicted_mse
        for (i in 1:length(next_vertices)) {
            predictor_matrix = vertex_to_str(next_vertices[i])
            model_index = paired_dataframe$superset_predictor_matrix == predictor_matrix
            mse_diff = predicted_mse_diff[model_index][[1]]
            if (print_intermediate_steps) {
                logging_print("... checking model...", predictor_matrix, mse_diff)
            }
            # select this model
            if (mse_diff > best_mse_diff) {
                current_vertex = next_vertices[i]
                best_model_index = model_index
                best_mse_diff = mse_diff
                best_matrix = predictor_matrix
            }
        }
        current_best_lm = get_model(dataframe, paired_dataframe, best_model_index)
        if (print_intermediate_steps) {
            logging_print("Current best at step, mse_diff", step, best_mse_diff, model_summary(current_best_lm))
        }
        if (best_mse_diff <= 0 || step >= num_predictors) {
            if (print_intermediate_steps) {
                logging_print("Best reached", step)
            }
            else {
                logging_print("Best at step, mse_diff", step, best_mse_diff, model_summary(current_best_lm))
            }
            return(best_matrix)
        }
    }
    # this point shouldn't be reachable by code
}

# convenience function for printing
model_summary = function(linear_model) {
    summary_obj = summary(linear_model)
    predictors = attr(summary(linear_model)$terms, "variables")
    return(list("predictors"=predictors, "r_squared"=summary_obj$r.squared))
}

get_model = function(dataframe, paired_dataframe, index) {
    response_name = get_response_colname(dataframe)
    predictor_names = get_predictor_colnames(dataframe)
    model_string = paired_dataframe$superset_predictor_matrix[index]
    matrix = predictor_str_to_matrix(model_string)
    formula = build_formula_string(matrix, response_name, predictor_names)
    linear_model = lm(formula, data=dataframe)
    return(linear_model)
}

bootstrap_meta_model = function(meta_dataframe, max_iterations) {

    model_history = list()

    # choose an arbitrary non empty predictor such as r squared
    meta_model = lm("RESPONSE_VARIABLExstd_mse_diff~r_sq_adj_diff", data=meta_dataset)

    # loop until model is stable or for max of n times
    for (i in 1:max_iterations) {
        logging_print("Bootstrapping meta-model iteration", i)

        # fit model to meta_dataframe, use as new meta_model
        meta_model = fit_model_to_predictors(meta_dataframe, meta_model)
        model_history[[i]] = meta_model

        # if the new model is the same as any of the previous ones, stop
        if (i > 1) {
            for (j in 1:(i-1)) {
                old_predictors = model_summary(model_history[[j]])$predictors
                new_predictors = model_summary(meta_model)$predictors
                if (length(old_predictors) == length(new_predictors)) {
                    if (old_predictors == new_predictors) {
                        logging_print("Boostrap done - found repeated model from step", j)
                        return(model_history)
                    }
                }
            }
        }
    }
    logging_print("Boostrapping max steps reached")
    return(model_history)
}
