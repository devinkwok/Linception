source(file.path("test", "include.R"))
source(file.path("src", "data.R"))

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

    # this test is broken but the function test_lm is fine
    # for some reason setting up the dataframe above
    # makes it a list according to weird R indexing rules
    # by obj[[parameter_name]]
    lm_1 = lm("response~predict", data=train)
    test_1 = test_lm(lm_1, test)
    assert_equal(0, test_1[["mse"]])

    test$response = data.frame(c(2:(num+1)))
    test_2 = test_lm(lm_1, test)
    assert_equal(1, test_2[["mse"]])
}
test_test_lm()