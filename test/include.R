EPSILON = 0.00000001

assert_equal = function(obj_1, obj_2) {
    if (is.numeric(obj_1) && is.numeric(obj_2)) {
        if (abs(obj_1 - obj_2) < EPSILON) {
            notify_test_passed(obj_1, obj_2)
            return(TRUE)
        }
    }
    if (identical(obj_1, obj_2)) {
        notify_test_passed(obj_1, obj_2)
        return(TRUE)
    }
    notify_test_failed(obj_1, obj_2)
    return(FALSE)
}

notify_test_passed = function(obj_1, obj_2) {
    print("test passed")
}

notify_test_failed = function(obj_1, obj_2) {
    print("TEST FAILED: objects differ")
    print(paste(obj_1, typeof(obj_1)))
    print(paste(obj_2, typeof(obj_2)))
}