library("igraph")

source(file.path("src", "util.R"))

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
#
# the first vertex of the graph is the null lm with no predictors
create_pgraph = function(num_predictors) {
    graph = make_empty_graph(n=0, directed=TRUE)
    # use a matrix of 0 and 1 to indication inclusion/exclusion of predictor
    # rows are for different linear models
    # columns are the inclusion/exclusion of that column in the dataframe
    predictor_matrix = combination_matrix(num_predictors)

    # add vertex for every combination of predictors
    num_vertices = length(predictor_matrix[,1])
    graph = add_vertices(graph, num_vertices)
    for (i in 1:num_vertices) {
        graph = set_vertex_predictors(graph, i, predictor_matrix[i,])
    }

    # add edges by checking every other vertex with one extra predictor
    for (i in 1:num_vertices) {
        v1_predictors = get_vertex_predictors(V(graph)[i])
        
        num_predictors = sum(v1_predictors)
        
        for (j in 1:num_vertices) {
            v2_predictors = get_vertex_predictors(V(graph)[j])

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
    return(graph)
}

create_incremental_pgraph = function(num_predictors) {
    graph = make_empty_graph(n=0, directed=TRUE)
    graph = add_vertices(graph, 1)
    graph = set_vertex_predictors(graph, 1, rep(0, num_predictors))
    return(graph)
}

grow_incremental_pgraph = function(graph, vertex) {
    # add all predictor combinations that are one more than current vertex
    predictors = get_vertex_predictors(vertex)
    start_index = as_ids(vertex)
    num_predictors = length(predictors)
    graph = add_vertices(graph, num_predictors - sum(predictors))

    for (i in 1:num_predictors) {
        # make an added predictor if not already present
        if (predictors[[i]] == 0) {
            current_index = start_index + i
            new_predictors = predictors
            new_predictors[[i]] = 1
            graph = set_vertex_predictors(graph, current_index, new_predictors)
            graph = add_edges(graph, c(start_index, current_index))
        }
    }
    return(graph)
}

get_null_model_vertex = function(graph) {
    return(V(graph)[1])
}

get_full_model_vertex = function(graph) {
    return(tail(V(graph), n=1))
}

forward_step_vertices = function(graph, vertex) {
    return(neighbors(graph, as_ids(vertex), mode="out"))
}

vertices_to_edges = function(vertex_from, vertices_to) {
    from_id = as_ids(vertex_from)
    to_ids = as_ids(vertices_to)
    edges = cbind(rep(from_id, length(to_ids)), unlist(to_ids))
    return(edges)
}

backward_step_vertices = function(graph, vertex) {
    return(neighbors(graph, as_ids(vertex), mode="in"))
}

num_edges = function(edgelist) {
    return(length(edgelist[,1]))
}

# returns a list of all edges in the graph (wrapper)
list_pgraph_edges = function(graph) {
    return(as_edgelist(graph))
}

set_vertex_predictors = function(graph, index, predictors) {
    predictors = unlist(predictors)
    # need to wrap the vector in an object (list) otherwise it breaks
    # because R data type coercion is ridiculous
    return(set_vertex_attr(graph, "predictors", index, list(predictors)))
}

# gets the predictors as a vector of 0 and 1 (1 means included)
get_vertex_predictors = function(vertex) {
    # predictors are stored as a vector in a list,
    # need to unwrap to use it
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

vertex_to_str = function(vertex) {
    string = paste(get_vertex_predictors(vertex), collapse="")
    string = gsub("0", "F", string)
    string = gsub("1", "T", string)
    return(string)
}

predictor_str_to_matrix = function(predictor_str) {
    string = gsub("F", "0", predictor_str)
    string = gsub("T", "1", string)
    return(as.numeric(strsplit(string, "")[[1]]))
}