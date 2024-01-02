library(e1071) # SVM package

# Testing kernels --------------------------------------------------------------
# All variables
#### With a linear kernel ####
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

#### With a radial kernel ####
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

# Tuning SVMs ------------------------------------------------------------------
#### All variables ####
# Radial kernel
tictoc::tic()
svm_radial_tune <- ams_tune_svm_parallel(svm,
                                         Label ~ .,
                                         data = higgs_training_20,
                                         kernel = "radial",
                                         cross = 5,
                                         training_data_weights = training_weights_20,
                                         ranges = list(
                                           cost = c(0.5, 1, 2),
                                           gamma = c(0.01, 0.1, 0.5)
                                         )
)
tictoc::toc() # takes 3.8 hours for just gamma, 7.5 hours for gamma and cost
# saveRDS(svm_radial_tune, here("output/svm_radial_tune_20_gc.RDS")) # save object since it takes a long time to make
# svm_radial_tune <- readRDS(here("output/svm_radial_tune_20.RDS"))
svm_radial_tune$best.parameters # gamma = 0.1, cost = 1
tictoc::tic()
fitted_tuned_svm_radial_all <- predict(svm_radial_tune$best.model, higgs_testing_20)
tictoc::toc() # 25 minutes
# saveRDS(fitted_tuned_svm_radial_all, here("output/fitted_tuned_svm_radial_all_20.RDS")) # save object since it takes a long time to make
# fitted_svm_linear_all <- readRDS("output/fitted_svm_linear_all.RDS")

# Calculate AMS
approx_median_sig(predictions = fitted_tuned_svm_radial_all,
                  labels = higgs_testing_20$Label,
                  weights = testing_weights_20)
# 2.832 with gamma = 0.1, cost = 1

#### Dropping algebraic co-dependencies and uniform variables ####
tictoc::tic()
svm_radial_tune_drop_codep_unif <- ams_tune_svm_parallel(svm,
                                                         Label ~ .,
                                                         data = higgs_training_20_drop_codep_unif,
                                                         kernel = "radial",
                                                         cross = 5,
                                                         training_data_weights = training_weights_20,
                                                         ranges = list(
                                                           # cost = c(0.5, 1, 2),
                                                           gamma = c(0.01, 0.1, 0.5)
                                                         )
)
tictoc::toc() # 14 hours for some reason
# saveRDS(svm_radial_tune_drop_codep_unif, here("output/svm_radial_tune_drop_codep_unif_20.RDS")) # save object since it takes a long time to make
# svm_radial_tune_drop_codep_unif <- readRDS(here("output/svm_radial_tune_drop_codep_unif_20.RDS"))
svm_radial_tune_drop_codep_unif$best.parameters # gamma = 0.1
tictoc::tic()
fitted_tuned_svm_radial_drop_codep_unif <- predict(svm_radial_tune_drop_codep_unif$best.model, higgs_testing_20_drop_codep_unif)
tictoc::toc() # 18 minutes
# saveRDS(fitted_tuned_svm_radial_drop_codep_unif, here("output/fitted_tuned_svm_radial_drop_codep_unif_20.RDS")) # save object since it takes a long time to make
# fitted_svm_linear_all <- readRDS("output/fitted_svm_linear_all.RDS")

# Calculate AMS
approx_median_sig(predictions = fitted_tuned_svm_radial_drop_codep_unif,
                  labels = higgs_testing_20$Label,
                  weights = testing_weights_20)
# 2.851

#### Top 10 mutual information ####