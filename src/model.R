source(file.path("src", "util.R"))
source(file.path("src", "graph.R"))
source(file.path("src", "data.R"))

fit_model_to_predictors = function(dataframe, meta_model) {
    # generate graph

    # start from 0
    # generate every model from this point

    # get statistics for each model
    
    # evaluate with meta model

    # choose best model, unless all are worse, then stop
}

bootstrap_meta_model = function(meta_dataframe) {
    # choose an arbitrary non empty predictor such as r squared
    # meta_model =

    # loop until model is stable or for max of n times
    # fit model to meta_dataframe, use as new meta_model
    # meta_model = fit_model_to_predictors(meta_dataframe, meta_model)
}