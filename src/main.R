source(file.path("src", "data.R"))

# constants
BUILTIN_DATA_PATH = file.path("datasets", "builtin")
INPUT_PATH = file.path("datasets", "set1")
OUTPUT_PATH = "outputs"
EXTENSION = "csv"
# reduce below numbers to limit computation time while debugging
MAX_DATASETS = 5
NUM_FOLDS = 5

# only need to run the following once to get data into files
# filenames = save_builtin_datasets_to_file(MAX_DATASETS, BUILTIN_DATA_PATH, EXTENSION)

# to use: run this
meta_data_filename = generate_meta_data(NUM_FOLDS, MAX_DATASETS, INPUT_PATH, OUTPUT_PATH, EXTENSION)