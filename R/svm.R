library(e1071) # SVM package

# SVM --------------------------------------------------------------------------
#### All variables ####
# With a linear kernel
svm_linear_all <- svm(Label ~ ., data = higgs_training, kernel = "linear")
summary(svm_linear_all)
fitted_svm_linear_all <- predict(svm_linear_all, higgs_testing, type = "response")
fitted_svm_linear_all <- unlist(ifelse(fitted_svm_linear_all > 0.5, 1, 0))
fitted_svm_linear_all_s_b <- ifelse(fitted_all == 1, "s", "b")

# Calculate AMS
approx_mean_sig(predictions = fitted_all_s_b,
                labels = ifelse(higgs_testing$Label == 1, "s", "b"),
                weights = testing_weights)