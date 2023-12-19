library(e1071) # SVM package

# SVM --------------------------------------------------------------------------
#### All variables ####
##### With a linear kernel #####
# svm_linear_all <- svm(Label ~ ., data = higgs_training, kernel = "linear")
# saveRDS(svm_linear_all, here("output/svm_linear_all.RDS")) # save object since it takes a long time to make
svm_linear_all <- readRDS(here("output/svm_linear_all.RDS"))
summary(svm_linear_all)
# fitted_svm_linear_all <- predict(svm_linear_all, higgs_testing)
# saveRDS(fitted_svm_linear_all, here("output/fitted_svm_linear_all.RDS")) # save object since it takes a long time to make
fitted_svm_linear_all <- readRDS("output/fitted_svm_linear_all.RDS")

# Calculate AMS
approx_mean_sig(predictions = fitted_svm_linear_all,
                labels = higgs_testing$Label,
                weights = testing_weights)
# 2.009864

##### With a radial kernel #####
svm_radial_all <- svm(Label ~ ., data = higgs_training, kernel = "radial")
summary(svm_radial_all)
fitted_svm_radial_all <- predict(svm_radial_all, higgs_testing)
approx_mean_sig(predictions = fitted_svm_radial_all,
                labels = higgs_testing$Label,
                weights = testing_weights)
# 2.743058


