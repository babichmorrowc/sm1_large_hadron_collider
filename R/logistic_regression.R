# Logistic regression ----------------------------------------------------------
#### All variables ####
glm_all <- glm(Label ~ ., data = higgs_training, family = binomial)
summary(glm_all)
fitted_all <- predict(glm_all, higgs_testing, type = "response")
fitted_all <- unlist(ifelse(fitted_all > 0.5, 1, 0))
fitted_all_s_b <- ifelse(fitted_all == 1, "s", "b")

# Calculate AMS
approx_mean_sig(predictions = fitted_all_s_b,
                labels = ifelse(higgs_testing$Label == 1, "s", "b"),
                weights = testing_weights)

#### Without angle variables and collinearities ####
glm_drop_combo <- glm(Label ~ ., data = higgs_training_drop_combo, family = binomial)
summary(glm_drop_combo)
fitted_drop_combo <- predict(glm_drop_combo, higgs_testing_drop_combo, type = "response")
fitted_drop_combo <- unlist(ifelse(fitted_drop_combo > 0.5, 1, 0))
fitted_drop_combo_s_b <- ifelse(fitted_drop_combo == 1, "s", "b")

# Calculate AMS
approx_mean_sig(predictions = fitted_drop_combo_s_b,
                labels = ifelse(higgs_testing$Label == 1, "s", "b"),
                weights = testing_weights)
