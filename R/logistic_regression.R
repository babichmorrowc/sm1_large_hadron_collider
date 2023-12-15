library(caret) # ML package

# Training vs test split -------------------------------------------------------
set.seed(999)
index <- createDataPartition(higgs_vars$Label, p = 0.8, list = FALSE)
# Training data
higgs_training <- higgs_vars[index,] %>% 
  # mutate label to 0s and 1s
  mutate(Label = ifelse(Label == "s", 1, 0))
# Dropping uniform and collinear
higgs_training_drop_combo <- higgs_training %>% 
  dplyr::select(-all_of(unif_vars),
                -c(PRI_jet_leading_pt,
                   PRI_jet_subleading_pt,
                   PRI_lep_pt,
                   PRI_tau_pt))

# Testing data
higgs_testing <- higgs_vars[-index,] %>% 
  # mutate label to 0s and 1s
  mutate(Label = ifelse(Label == "s", 1, 0))
# Dropping uniform and collinear
higgs_testing_drop_combo <- higgs_testing %>% 
  dplyr::select(-all_of(unif_vars),
                -c(PRI_jet_leading_pt,
                   PRI_jet_subleading_pt,
                   PRI_lep_pt,
                   PRI_tau_pt))
# Calculate adjusted weights for testing data
testing_weights <- adjust_weights(complete_data = higgs_data,
                                  subset_data = higgs_data[-index,],
                                  unadjusted_weight_col = "Weight",
                                  label_col = "Label")

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
