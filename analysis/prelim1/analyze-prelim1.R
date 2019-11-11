# read table
df = read.table("outputs/lm-data-individual_2019-11-10-18-19-09.csv")

# "sse" "mse" "mse_sqrt" "standardized_mse" and "standardized_mse_sqrt"
# are the response variables, only choose ONE
# standardized_mse has been divided by the standard deviation of the
# test sample, this should work better for comparisons across
# different datasets
# DO NOT use as predictors since they are calculated from each other

# "response_mean" and "response_sd" are statistics on the response variable
# these are diagnostic values that will be used to decide whether other
# statistics need to be scaled, for example large mse may be more correlated
# to large deviance in response variable than any choice of predictors
# DO NOT use as predictors

# similarly, "test_mean" and "test_sd" are also used to scale other inputs
# DO NOT use as predictors

# "dataset" "predictors" and "k_fold" are index variables referencing
# the dataset, selection of predictors, and which data fold was used to
# train this lm. Use to check how consistent the test result mse is, as well as
# how consistent the meta model is on different folds or different datasets.
# DO NOT use as predictors

#"num_training_samples" "num_coefficients" "r_squared" "r_squared_adjusted"
# are all individual statistics that can be used to predict mse

df_cleaned = df
df_cleaned$response_mean = NULL
df_cleaned$response_sd = NULL
df_cleaned$test_mean = NULL
df_cleaned$test_sd = NULL
df_cleaned$sse = NULL
df_cleaned$mse = NULL
df_cleaned$mse_sqrt = NULL
df_cleaned$standardized_mse = NULL
df_cleaned$predictors = NULL
df_cleaned$dataset = as.factor(df$dataset)
df_cleaned$k_fold = as.factor(df$k_fold)
attach(df_cleaned)

# look for trends in these pairwise plots
pdf("analysis/prelim1/pairwise-plots.pdf")
plot(df_cleaned)
dev.off()

# these box plots should show be consistent between
# k folds and datasets
pdf("analysis/prelim1/boxplots.pdf")
par(mfrow=c(2,1))
boxplot(standardized_mse_sqrt~k_fold)
boxplot(standardized_mse_sqrt~dataset)
dev.off()

# first naive meta lm:
lm1 = lm("standardized_mse_sqrt~num_training_samples+num_coefficients+r_squared+r_squared_adjusted")
summary(lm1)
# write to file
sink("analysis/prelim1/lm1.txt")
print(summary(lm1))
sink()