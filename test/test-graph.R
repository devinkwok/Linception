source(file.path("test", "include.R"))
source(file.path("src", "graph.R"))


test_create_pgraph = function() {
    predictors = c("a", "b", "c")
    num_predictors = length(predictors)
    graph = create_pgraph(num_predictors)
    assert_equal(2^num_predictors, length(V(graph)))
    num_edges = 0
    for (i in 0:num_predictors) {
        # the number of edges added at each step is the number of models
        # with i predictors (n choose i) times the number of predictors left
        num_edges = num_edges + choose(num_predictors, i)*(num_predictors - i)
    }
    assert_equal(num_edges, length(E(graph)))

    # manually check to see the edges are in the right place
    print(combination_matrix(num_predictors))
    print(graph)
}
test_create_pgraph()