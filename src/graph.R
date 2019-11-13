library("igraph")

# create predictor graph from response_columns
#
# each node describes a certain subset of predictors
# using an inclusion matrix where 0 is out and 1 is in
#
# each edge connects adjacent subsets, ones which
# differ only by the inclusion/removal of one predictor
create_pgraph = function(response_columns) {
    make_graph()
    return(graph)
}

# randomly remove edges and nodes to reduce size of graph
prune_pgraph = function(graph, max_nodes, max_edges) {
    # TODO: currently does no pruning, returns original
    return(graph)
}

# returns a list of all nodes in the graph
list_pgraph_nodes = function(graph) {

}

# returns a list of all edges in the graph
list_pgraph_edges = function(graph) {

}

# creates a formula string to put into lm() from inclusion matrix
build_formula_string = function(inclusion_matrix_row, response_name, predictor_names) {
    num_included = sum(inclusion_matrix_row)
    
    included_predictors = vector("list", num_included)
    j = 1  # because R needs index variables
    
    # iterate through columns for each predictor
    for (i in 1:length(predictor_names)) {
        if (inclusion_matrix_row[i] == 1) {
            included_predictors[j] = predictor_names[i]
            j = j + 1
        }
    }

    # if there are no predictors, add a constant term
    formula_string = "1"
    if (length(included_predictors) > 0) {
        formula_string = paste(included_predictors, collapse="+")
    }
    formula_string = paste(response_name, "~", formula_string)
    return(formula_string)
}

# TODO: delete
# creates a {0, 1} dataframe of [size] with every combination of 0 and 1 in rows
combination_matrix = function(size) {
    return(expand.grid(rep(list(c(0:1)), size)))
}