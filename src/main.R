source(file.path("src", "util.R"))
source(file.path("src", "data.R"))
source(file.path("src", "model.R"))

# constants
META_DATA_FILENAME = file.path("datasets", "meta_data", "lm-data-paired-ordering-fixed.csv")
BUILTIN_DATA_PATH = file.path("datasets", "builtin")
INPUT_PATH = file.path("datasets", "builtin_set")
OUTPUT_PATH = file.path("datasets", "meta_data")
VERIFICATION_INPUTS = file.path("datasets", "builtin_set")
EXTENSION = "csv"
# reduce below numbers to limit computation time while debugging
MAX_DATASETS = 5
NUM_FOLDS = 5 # num_folds must be greater than 1
NUM_SUBSETS = 5

meta_data_filename = META_DATA_FILENAME
meta_model = NULL # insert hand trained meta model here

## to get builtin data into files: run this
# filenames = save_builtin_datasets_to_file(MAX_DATASETS, BUILTIN_DATA_PATH, EXTENSION)

## to generate meta_data: run this
meta_data_filename = generate_meta_data(NUM_FOLDS, NUM_SUBSETS, MAX_DATASETS, INPUT_PATH, OUTPUT_PATH, EXTENSION)

## to bootstrap the meta model: run this
# meta_dataset = load_dataframe(meta_data_filename)
# meta_model = bootstrap_meta_model(meta_dataset)

## to select parameters using meta model: run this
# generated_lms = list()
# filenames = list_data_filenames(VERIFICATION_INPUTS, EXTENSION)
# for (i in 1:length(filenames)) {
#     dataframe = load_dataframe(file.path(VERIFICATION_INPUTS, filenames[i]))
#     model = fit_model_to_predictors(dataframe, meta_model)
#     print(summary(model))
#     generated_lms[[i]] = model
# }
# return(generated_lms)