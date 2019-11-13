library("igraph")

# create predictor graph from predictor_names
#
# each vertex describes a certain subset of predictors
# using an inclusion matrix where 0 is out and 1 is in
#
# each edge connects adjacent subsets, ones which
# differ only by the inclusion/removal of one predictor
#
# ASSUMPTION: by convention edges are added from subset to superset
# that is, smaller model pointing to larger model, but the graph
# itself is bidirectional
create_pgraph = function(num_predictors) {
    graph = make_empty_graph(n=0, directed=FALSE)
    # use a matrix of 0 and 1 to indication inclusion/exclusion of predictor
    # rows are for different linear models
    # columns are the inclusion/exclusion of that column in the dataframe
    predictor_matrix = combination_matrix(num_predictors)

    # add vertex for every combination of predictors
    for (i in 1:length(predictor_matrix[,1])) {
        predictors = unlist(predictor_matrix[i,])
        # need to wrap the vector in an object (list) otherwise it breaks
        # because R data type coercion is ridiculous
        graph = add_vertices(graph, 1, "predictors"=list(predictors), "lm_data"=list(0))
    }

    # add edges by checking every other vertex with one extra predictor
    vertices = list_pgraph_vertices(graph)
    num_vertices = length(vertices)

    for (i in 1:num_vertices) {
        v1_predictors = get_vertex_predictors(vertices[i])
        
        num_predictors = sum(v1_predictors)
        
        for (j in 1:num_vertices) {
            v2_predictors = get_vertex_predictors(vertices[j])

            # check that vertices[j] has exactly one extra predictor
            # so edges are only added once per pair from one direction
            if (sum(v2_predictors) == num_predictors + 1) {
                # check that vertices[j] differs only by one predictor
                differences = sum(abs(v1_predictors - v2_predictors))
                if (differences == 1) {
                    graph = add_edges(graph, c(i,j))
                }
            }
        }
    }
    return(prune_pgraph(graph))
}

# if there are too many predictors it is better to stochastically
# sample this space
# randomly remove edges and vertices to reduce size of graph
prune_pgraph = function(graph, max_vertices=NULL, max_edges=NULL) {
    # TODO: currently does no pruning, returns original
    return(graph)
}

# returns a list of all vertices in the graph (wrapper)
list_pgraph_vertices = function(graph) {
    return(V(graph))
}

num_edges = function(graph) {
    return(length(E(graph)))
}

# returns a list of all edges in the graph (wrapper)
list_pgraph_edges = function(graph) {
    return(as_edgelist(graph))
}

# gets the predictors as a vector of 0 and 1 (1 means included)
get_vertex_predictors = function(vertex) {
    # predictors are stored as a vector in a list,
    # need to unwrap to use it

    # TODO: there is a bizare bug where removing this print statement
    # will break sometimes, but not all the time
    print(vertex)
    return(vertex$predictors[[1]])
}

get_vertex_start = function(graph, edge) {
    return(V(graph)[edge[1]])
}

get_vertex_end = function(graph, edge) {
    return(V(graph)[edge[2]])
}

# helper function to return vertex_end - vertex_start
# sum is positive if vertex_end is bigger
predictor_diff = function(graph, edge) {
    vertex_start = get_vertex_predictors(get_vertex_start(graph, edge))
    vertex_end = get_vertex_predictors(get_vertex_end(graph, edge))
    return(vertex_end - vertex_start)
}

# get the smaller vertex
get_edge_subset = function(graph, edge) {
    if(sum(predictor_diff(graph, edge)) > 0) {
        return(get_vertex_start(graph, edge))
    }
    return(get_vertex_end(graph, edge))
}

# get the bigger vertex
get_edge_superset = function(graph, edge) {
    if(sum(predictor_diff(graph, edge)) > 0) {
        return(get_vertex_end(graph, edge))
    }
    return(get_vertex_start(graph, edge))
}

# get index of the predictor which is added/removed between the 2 vertices
get_edge_added_predictor_index = function(graph, edge) {
    added_predictor_index = match(1, abs(predictor_diff(graph, edge)))
    return(added_predictor_index)
}

# creates a formula string to put into lm() from inclusion matrix
build_formula_string = function(predictor_matrix, response_name, predictor_names) {
    num_included = sum(predictor_matrix)
    
    included_predictors = vector("list", num_included)
    j = 1  # because R needs index variables
    
    # iterate through columns for each predictor
    for (i in 1:length(predictor_names)) {
        if (predictor_matrix[i] == 1) {
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

# creates a {0, 1} dataframe of [size] with every combination of 0 and 1 in rows
combination_matrix = function(size) {
    return(expand.grid(rep(list(c(0:1)), size)))
}

predictor_str = function(vertex) {
    return(paste(get_vertex_predictors(vertex), collapse=""))
}