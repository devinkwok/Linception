source(file.path("src", "util.R"))
source(file.path("src", "graph.R"))
source(file.path("src", "data.R"))

PREDICTED_MSE_KEY = "meta_model_predicted_mse"

fit_model_to_predictors = function(dataframe, meta_model) {
    num_predictors = ncol(dataframe) - 1
    # generate every model
    models_and_data = fit_linear_models(1, dataframe, 1, 1)
    paired_dataframe = make_data_frame(models_and_data[["paired"]])
    # for each pair, predict the MSE
    predicted_mse_diff = predict(meta_model, newdata=paired_dataframe)

    # generate a separate graph object because it's easier to reuse code paths
    graph = create_pgraph(num_predictors)

    # forward step method: add one predictor at a time from 0 predictors
    current_vertex = get_null_model_vertex(graph)
    best_model_index = NA

    # max number of steps is the number of predictors
    for (step in 1:num_predictors) {
        # must be better than equal
        best_mse_diff = 0
        # choose neighbours that are in forward direction (increasing parameters)
        next_vertices = forward_step_vertices(graph, current_vertex)

        # for each number of predictors, find model with best predicted_mse
        for (i in 1:length(next_vertices)) {
            predictor_matrix = vertex_to_str(next_vertices[i])
            model_index = paired_dataframe["superset_predictor_matrix"] == predictor_matrix
            mse_diff = predicted_mse_diff[model_index][[1]]
            logging_print("... checking model...", predictor_matrix, mse_diff)
            # select this model
            if (mse_diff > best_mse_diff) {
                current_vertex = next_vertices[i]
                best_model_index = model_index
                best_mse_diff = mse_diff
            }
        }
        current_best_lm = get_model(dataframe, paired_dataframe, best_model_index)
        logging_print("Current best at step, mse_diff", step, best_mse_diff, model_summary(current_best_lm))
        if (best_mse_diff <= 0 || step >= num_predictors) {
            logging_print("Best reached", step)
            return(current_best_lm)
        }
    }
    # this point shouldn't be reachable by code
}

model_summary = function(linear_model) {
    summary_obj = summary(linear_model)
    predictors = attr(summary(linear_model)$terms, "variables")
    return(c(predictors, summary_obj$r.squared))
}

get_model = function(dataframe, paired_dataframe, index) {
    response_name = get_response_colname(dataframe)
    predictor_names = get_predictor_colnames(dataframe)
    model_string = paired_dataframe["superset_predictor_matrix"][index]
    formula = build_formula_string(predictor_str_to_matrix(model_string), response_name, predictor_names)
    linear_model = lm(formula, data=dataframe)
    return(linear_model)
}

bootstrap_meta_model = function(meta_dataframe) {
    # choose an arbitrary non empty predictor such as r squared
    # meta_model =

    # loop until model is stable or for max of n times
    # fit model to meta_dataframe, use as new meta_model
    # meta_model = fit_model_to_predictors(meta_dataframe, meta_model)
}