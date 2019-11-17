# if you load the file lm-data-paired-ordering-fixed.csv it should have no infinite values caused by division by 0
df = read.table("analysis/lm-data-paired-test.csv")

summary(df)

# this is the full meta-model
lm1 = lm("RESPONSE_VARIABLExstd_mse_diff~num_training_samples+superset_num_coefficients+added_predictor_coefficient+added_predictor_pvalue+superset_anova_pvalue+superset_r_sq+superset_r_sq_adj+r_sq_diff+r_sq_ratio+r_sq_adj_diff+r_sq_adj_ratio+aic_diff+aic_ratio+aic_corrected_diff+aic_corrected_ratio+bic_diff+bic_ratio+superset_normality_pvalue+normality_pvalue_diff+normality_pvalue_ratio+added_predictor_vif+mean_vif+max_vif+null_ptransform_pvalue+response_est_ptransform+added_predictor_est_ptransform", data=df)

summary(lm1)

library("car")
# the inflation factors are through the roof
vif(lm1)

shapiro.test(lm1$residuals)

# I pruned this meta-model a bit, didn't look carefully at whether each removed predictor has the highest r squared
# you should use the forward/backward step method to select predictors
lm2 = lm("RESPONSE_VARIABLExstd_mse_diff~num_training_samples+superset_num_coefficients+added_predictor_coefficient+added_predictor_pvalue+superset_anova_pvalue+r_sq_adj_diff+aic_corrected_ratio+superset_normality_pvalue+normality_pvalue_diff+mean_vif+null_ptransform_pvalue+response_est_ptransform+added_predictor_est_ptransform", data=df)

summary(lm2)

# most of these are under control
vif(lm2)

shapiro.test(lm2$residuals)


# these are the n best regular models
n = 10
for (i in 1:max(df$NULL_PREDICTORxdataset)) {
    subset_by_dataset = df[df$NULL_PREDICTORxdataset == i,]
    indexes = order(subset_by_dataset$NULL_PREDICTORxsuperset_mse)
    print(df[indexes[1:n],"NULL_PREDICTORxsuperset_predictor_matrix"])
}

min(df[df$NULL_PREDICTORxdataset == 5,]$NULL_PREDICTORxsuperset_mse)