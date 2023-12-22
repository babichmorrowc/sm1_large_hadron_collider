library(e1071) # SVM package

# SVM --------------------------------------------------------------------------
#### All variables ####
##### With a linear kernel #####
tictoc::tic()
svm_linear_all <- svm(Label ~ ., data = higgs_training, kernel = "linear") # takes ~90 minutes to run on 20% training set
tictoc::toc()
# saveRDS(svm_linear_all, here("output/svm_linear_all_20.RDS")) # save object since it takes a long time to make
# svm_linear_all <- readRDS(here("output/svm_linear_all.RDS"))
summary(svm_linear_all)
tictoc::tic()
fitted_svm_linear_all <- predict(svm_linear_all, higgs_testing) # takes ~35 minutes to run on 80% testing
tictoc::toc()
# saveRDS(fitted_svm_linear_all, here("output/fitted_svm_linear_all_20.RDS")) # save object since it takes a long time to make
# fitted_svm_linear_all <- readRDS("output/fitted_svm_linear_all.RDS")

# Calculate AMS
approx_median_sig(predictions = fitted_svm_linear_all,
                labels = higgs_testing$Label,
                weights = testing_weights)
# 2.010107

##### With a radial kernel #####
tictoc::tic()
svm_radial_all <- svm(Label ~ ., data = higgs_training, kernel = "radial") # takes ~6.5 hours to run on 20% training set
tictoc::toc()
saveRDS(svm_radial_all, here("output/svm_radial_all_20.RDS")) # save object since it takes a long time to make
summary(svm_radial_all)
tictoc::tic()
fitted_svm_radial_all <- predict(svm_radial_all, higgs_testing) # takes ~28 minutes onn 20% training set
tictoc::toc()
saveRDS(fitted_svm_radial_all, here("output/fitted_svm_radial_all_20.RDS")) # save object since it takes a long time to make
approx_median_sig(predictions = fitted_svm_radial_all,
                labels = higgs_testing$Label,
                weights = testing_weights)
# 2.813594


