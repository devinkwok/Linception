source(file.path("src", "util.R"))
source(file.path("src", "data.R"))
source(file.path("src", "model.R"))

# constants
META_DATA_FILENAME = file.path("datasets", "meta_data", "lm-data-paired-test.csv")
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

## to get builtin data into files: run this
# filenames = save_builtin_datasets_to_file(MAX_DATASETS, BUILTIN_DATA_PATH, EXTENSION)

## to generate meta_data: run this
# meta_data_filename = generate_meta_data(NUM_FOLDS, NUM_SUBSETS, MAX_DATASETS, INPUT_PATH, OUTPUT_PATH, EXTENSION)

meta_dataset = load_dataframe(meta_data_filename, requires=NO_LIMIT_REQUIRES)
# insert hand trained meta model here
meta_model = lm("RESPONSE_VARIABLExstd_mse_diff~r_sq_adj_diff", data=meta_dataset)
# meta_model = lm("RESPONSE_VARIABLExstd_mse_diff~num_training_samples+superset_num_coefficients+added_predictor_coefficient+added_predictor_pvalue+superset_anova_pvalue+r_sq_adj_diff+aic_corrected_ratio+superset_normality_pvalue+normality_pvalue_diff+mean_vif+null_ptransform_pvalue+response_est_ptransform+added_predictor_est_ptransform", data=meta_dataset)

## to bootstrap the meta model: run this
# meta_model = bootstrap_meta_model(meta_dataset)

## to select parameters using meta model: run this
generated_lms = list()
filenames = list_data_filenames(VERIFICATION_INPUTS, EXTENSION)
for (i in 1:length(filenames)) {
    # need to normalize because powerTransform can't take negative or zero values
    dataframe = load_dataframe(file.path(VERIFICATION_INPUTS, filenames[i]), normalize=TRUE)
    model = fit_model_to_predictors(dataframe, meta_model)
    generated_lms[[i]] = model
}
return(generated_lms)