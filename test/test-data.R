source(file.path("src", "data.R"))

EPSILON = 0.00000001

assert_equal = function(obj_1, obj_2) {
    if (is.numeric(obj_1) && is.numeric(obj_2)) {
        if (obj_1 - obj_2 < EPSILON) {
            return(TRUE)
        }
    }
    if (identical(obj_1, obj_2)) {
        print("test passed")
        return(TRUE)
    }
    print("TEST FAILED: objects differ")
    print(paste(obj_1, typeof(obj_1)))
    print(paste(obj_2, typeof(obj_2)))
    return(FALSE)
}

test_append_list_of_lists_rows = function() {
    list_1 = NULL
    list_2 = NULL
    assert_equal(TRUE, is.null(append_list_of_lists_rows(list_1, list_2)))

    list_1 = list("a"=c(1,2,3), "b"=c("A","B","C"), "c"=c(0.1,0.2,0.3))
    assert_equal(list_1, append_list_of_lists_rows(list_1, list_2))

    list_2 = list("a"=c(4,5), "b"=c("D","E"), "c"=c(0.4,0.5))
    assert_equal(list("a"=c(1,2,3,4,5), "b"=c("A","B","C","D","E"), "c"=c(0.1,0.2,0.3,0.4,0.5))
        , append_list_of_lists_rows(list_1, list_2))
}
test_append_list_of_lists_rows()

test_k_fold_split = function() {
    num = as.integer(10)
    dataframe = data.frame(c(1:num), c(1:num))
    splits = 3
    min_per_split = as.integer(floor(num/splits))
    data_folds = k_fold_split(dataframe, splits)
    for (i in 1:splits) {
        train = data_folds[["train"]][[i]]
        test = data_folds[["test"]][[i]]
        assert_equal(TRUE, min_per_split <= nrow(test))
        assert_equal(num, nrow(train) + nrow(test))
    }

}
test_k_fold_split()

test_test_lm = function() {
    num = as.integer(10)
    train = data.frame(c(1:num))
    train$predict = c(1:num)
    colnames(train) = c("response", "predict")
    test = data.frame(c(1:num))
    test$predict = train$predict
    colnames(test) = c("response", "predict")

    lm_1 = lm("response~predict", data=train)
    test_1 = test_lm(lm_1, test)
    assert_equal(0, test_1[["mse"]])

    test$response = data.frame(c(2:(num+1)))
    test_2 = test_lm(lm_1, test)
    assert_equal(1, test_2[["mse"]])
}
test_test_lm()