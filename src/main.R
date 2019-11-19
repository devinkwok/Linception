source(file.path("src", "util.R"))
source(file.path("src", "data.R"))
source(file.path("src", "model.R"))

# constants
META_DATA_FILENAME = file.path("datasets", "meta_data", "lm-data-paired.csv")
# META_DATA_FILENAME = file.path("datasets", "meta_data", "lm-data-paired-train-set.csv")
BUILTIN_DATA_PATH = file.path("datasets", "builtin")
INPUT_PATH = file.path("datasets", "builtin_set")
# INPUT_PATH = file.path("datasets", "train_set")
OUTPUT_PATH = file.path("datasets", "meta_data")
# VERIFICATION_INPUTS = file.path("datasets", "builtin_set")
VERIFICATION_INPUTS = file.path("datasets", "train_set")
EXTENSION = "csv"
# reduce below numbers to limit computation time while debugging
MAX_DATASETS = 5
NUM_FOLDS = 5 # num_folds must be greater than 1
NUM_SUBSETS = 1
NUM_BOOSTRAP_ITERATIONS = 20

meta_data_filename = META_DATA_FILENAME

## to get builtin data into files: run this
# filenames = save_builtin_datasets_to_file(MAX_DATASETS, BUILTIN_DATA_PATH, EXTENSION)

## to generate meta_data: run this
# meta_data_filename = generate_meta_data(NUM_FOLDS, NUM_SUBSETS, MAX_DATASETS, INPUT_PATH, OUTPUT_PATH, EXTENSION)

meta_dataset = load_dataframe(meta_data_filename, requires=NO_LIMIT_REQUIRES)
print(summary(meta_dataset))
# insert hand trained meta model here
# meta_model = lm("RESPONSE_VARIABLExstd_mse_diff~aic_ratio + aic_corrected_ratio", data=meta_dataset)

## to bootstrap the meta model: run this - WARNING this takes a looooong time!
# meta_models = bootstrap_meta_model(meta_dataset, NUM_BOOSTRAP_ITERATIONS)
# meta_model = meta_models[[length(meta_models)]]

## this is the hand fit model
meta_model = lm("RESPONSE_VARIABLExstd_mse_diff ~ num_training_samples+
    superset_anova_pvalue+r_sq_adj_diff+aic_diff+bic_diff+mean_vif+
    null_ptransform_pvalue+response_est_ptransform", data=meta_dataset)
# revised hand fit model
# meta_model = lm("RESPONSE_VARIABLExstd_mse_diff ~ num_training_samples+
#     superset_anova_pvalue+r_sq_adj_diff+aic_diff+bic_diff+mean_vif", data=meta_dataset)

## these are two models found using boostrap method previously
# meta_model = lm("RESPONSE_VARIABLExstd_mse_diff ~ added_predictor_coefficient+
#     added_predictor_pvalue+superset_anova_pvalue+superset_r_sq_adj+
#     r_sq_diff+r_sq_adj_diff+aic_ratio+aic_corrected_diff", data=meta_dataset)

# meta_model = lm("RESPONSE_VARIABLExstd_mse_diff ~ superset_anova_pvalue+superset_r_sq+
#     r_sq_diff", data=meta_dataset)
# meta_model = lm("RESPONSE_VARIABLExstd_mse_diff ~ r_sq_adj_diff", data=meta_dataset)

print(summary(meta_model))

## to select parameters using meta model: run this
generated_lms = list()
filenames = list_data_filenames(VERIFICATION_INPUTS, EXTENSION)
for (i in 1:length(filenames)) {
    filename = file.path(VERIFICATION_INPUTS, filenames[i])
    dataframe = load_dataframe(filename, normalize=TRUE)
    model_str = fit_model_to_predictors(dataframe, meta_model, print_intermediate_steps=TRUE)
    generated_lms[[i]] = model_str
}

# look at rank of actual test MSE for each selected model
meta_dataset = read.table("datasets/meta_data/lm-data-individual-train-set.csv", header=TRUE, sep=",")
# take average of each column across k_folds for identical models
for (i in 1:length(filenames)) {
    filename = file.path(VERIFICATION_INPUTS, filenames[i])
    dataframe = load_dataframe(filename, normalize=TRUE)
    model_str = generated_lms[[i]]
    subset_by_dataset = meta_dataset[meta_dataset$dataset == i,]
    test_mse = data.frame(subset_by_dataset$std_mse)
    colnames(test_mse) = c("mse")
    test_mse$tagged = subset_by_dataset$predictors == model_str
    test_mse = test_mse[order(test_mse$mse),]
    total_ranks = nrow(test_mse)
    test_mse$rank = c(1:total_ranks)
    ranks = test_mse[test_mse$tagged,]$rank
    average_rank = mean(ranks)
    print(paste("Linception selected model predictors:", model_str))
    print("Standardized MSE for selected model:")
    summary(test_mse[test_mse$tagged,]$mse)
    print("Standardized MSE for all subsets:")
    print(summary(subset_by_dataset$std_mse))
    print(paste("Average model rank", average_rank, "out of", total_ranks, "for quantile", average_rank / total_ranks))
}