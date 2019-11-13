source(file.path("test", "include.R"))
source(file.path("src", "util.R"))

INPUT_PATH = file.path("datasets", "set1")
EXTENSION = "csv"

test_load_dataframe = function() {
    filenames = list_data_filenames(INPUT_PATH, EXTENSION)
    for (i in 1:length(filenames)) {
        name = filenames[i]
        dataset = load_dataframe(file.path(INPUT_PATH, name))
        print(head(dataset))
    }
}
test_load_dataframe()

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