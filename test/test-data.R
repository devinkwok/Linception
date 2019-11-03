source(file.path("src", "data.R"))

assert_equal = function(obj_1, obj_2) {
    if (identical(obj_1, obj_2)) {
        print("test passed")
    }
    else {
        print("TEST FAILED: objects differ")
        print(obj_1)
        print(obj_2)
    }
}

test_append_list_of_lists = function() {
    list_1 = NULL
    list_2 = NULL
    assert_equal(TRUE, is.null(append_list_of_lists(list_1, list_2)))

    list_1 = list("a"=c(1,2,3), "b"=c("A","B","C"), "c"=c(0.1,0.2,0.3))
    assert_equal(list_1, append_list_of_lists(list_1, list_2))

    list_2 = list("a"=c(4,5), "b"=c("D","E"), "c"=c(0.4,0.5))
    assert_equal(list("a"=c(1,2,3,4,5), "b"=c("A","B","C","D","E"), "c"=c(0.1,0.2,0.3,0.4,0.5))
        , append_list_of_lists(list_1, list_2))
}
test_append_list_of_lists()