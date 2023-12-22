library(caret) # ML package

# Training vs test split -------------------------------------------------------
set.seed(999)
index <- createDataPartition(higgs_vars$Label, p = 0.20, list = FALSE)
# Training data
higgs_training <- higgs_vars[index,] %>% 
  mutate(Label = as.factor(Label))
  # mutate label to 0s and 1s
  # mutate(Label = ifelse(Label == "s", 1, 0))
# Dropping uniform and collinear
higgs_training_drop_combo <- higgs_training %>% 
  dplyr::select(-all_of(unif_vars),
                -c(PRI_jet_leading_pt,
                   PRI_jet_subleading_pt,
                   PRI_lep_pt,
                   PRI_tau_pt))

# Testing data
higgs_testing <- higgs_vars[-index,] %>% 
  mutate(Label = as.factor(Label))
  # mutate label to 0s and 1s
  # mutate(Label = ifelse(Label == "s", 1, 0))
# Dropping uniform and collinear
higgs_testing_drop_combo <- higgs_testing %>% 
  dplyr::select(-all_of(unif_vars),
                -c(PRI_jet_leading_pt,
                   PRI_jet_subleading_pt,
                   PRI_lep_pt,
                   PRI_tau_pt))
# Calculate adjusted weights for testing data
testing_weights <- adjust_weights(complete_data = higgs_data_orig,
                                  subset_data = higgs_data_orig[-index,],
                                  unadjusted_weight_col = "Weight",
                                  label_col = "Label")
