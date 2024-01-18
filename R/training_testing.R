library(caret) # ML package

# Training vs test split -------------------------------------------------------
#### 0.1% training for test purposes #####
# set.seed(999)
# index <- createDataPartition(higgs_vars$Label, p = 0.001, list = FALSE)
# # Training data
# higgs_training <- higgs_vars[index,] %>% 
#   mutate(Label = as.factor(Label))
#   # mutate label to 0s and 1s
#   # mutate(Label = ifelse(Label == "s", 1, 0))
# # Dropping uniform and collinear
# higgs_training_drop_combo <- higgs_training %>% 
#   dplyr::select(-all_of(unif_vars),
#                 -c(PRI_jet_leading_pt,
#                    PRI_jet_subleading_pt,
#                    PRI_lep_pt,
#                    PRI_tau_pt))
# 
# # Calculate adjusted weights for training data
# training_weights <- adjust_weights(complete_data = higgs_data_orig,
#                                    subset_data = higgs_data_orig[index,],
#                                    unadjusted_weight_col = "Weight",
#                                    label_col = "Label")
# 
# 
# # Testing data
# higgs_testing <- higgs_vars[-index,] %>% 
#   mutate(Label = as.factor(Label))
#   # mutate label to 0s and 1s
#   # mutate(Label = ifelse(Label == "s", 1, 0))
# # Dropping uniform and collinear
# higgs_testing_drop_combo <- higgs_testing %>% 
#   dplyr::select(-all_of(unif_vars),
#                 -c(PRI_jet_leading_pt,
#                    PRI_jet_subleading_pt,
#                    PRI_lep_pt,
#                    PRI_tau_pt))
# # Calculate adjusted weights for testing data
# testing_weights <- adjust_weights(complete_data = higgs_data_orig,
#                                   subset_data = higgs_data_orig[-index,],
#                                   unadjusted_weight_col = "Weight",
#                                   label_col = "Label")

#### 20% training ####
set.seed(999)
index_20 <- createDataPartition(higgs_vars$Label, p = 0.2, list = FALSE)
# Training data
higgs_training_20 <- higgs_vars[index_20,] %>% 
  mutate(Label = as.factor(Label))
# Drop codependencies & uniform variables
higgs_training_20_drop_codep_unif <- higgs_training_20 %>% 
  dplyr::select(-all_of(unif_vars),
                -c(PRI_jet_leading_pt,
                   PRI_jet_subleading_pt,
                   PRI_lep_pt,
                   PRI_tau_pt))
# Use top 10 mutual information only
mut_info_vars <-
  c(
    'DER_mass_MMC',
    'DER_mass_transverse_met_lep',
    'DER_mass_vis',
    'PRI_tau_pt',
    'DER_met_phi_centrality',
    'DER_pt_ratio_lep_tau',
    'DER_sum_pt',
    'DER_pt_h',
    'DER_deltaeta_jet_jet',
    'DER_mass_jet_jet'
  )
higgs_training_20_mut_info <- higgs_training_20 %>% 
  dplyr::select(mut_info_vars,
                Label)
# Drop bottom 10 mutual information
lowest_mut_info_vars <-
  c(
    'PRI_jet_leading_phi',
    'PRI_lep_eta',
    'PRI_jet_subleading_pt',
    'PRI_lep_pt',
    'PRI_jet_subleading_phi',
    'DER_pt_tot',
    'PRI_tau_eta',
    'PRI_tau_phi',
    'PRI_lep_phi',
    'PRI_met_phi'
  )
higgs_training_20_drop_mut_info <- higgs_training_20 %>% 
  dplyr::select(-all_of(lowest_mut_info_vars))

# Calculate adjusted weights for training data
training_weights_20 <- adjust_weights(complete_data = higgs_data_orig,
                                   subset_data = higgs_data_orig[index_20,],
                                   unadjusted_weight_col = "Weight",
                                   label_col = "Label")


# Testing data
higgs_testing_20 <- higgs_vars[-index_20,] %>% 
  mutate(Label = as.factor(Label))
# Drop codependencies & uniform variables
higgs_testing_20_drop_codep_unif <- higgs_testing_20 %>% 
  dplyr::select(-all_of(unif_vars),
                -c(PRI_jet_leading_pt,
                   PRI_jet_subleading_pt,
                   PRI_lep_pt,
                   PRI_tau_pt))
# Use top 10 mutual information only
higgs_testing_20_mut_info <- higgs_testing_20 %>% 
  dplyr::select(mut_info_vars,
                Label)
# Drop bottom 10 mutual information
higgs_testing_20_drop_mut_info <- higgs_testing_20 %>% 
  dplyr::select(-all_of(lowest_mut_info_vars))

# Calculate adjusted weights for testing data
testing_weights_20 <- adjust_weights(complete_data = higgs_data_orig,
                                     subset_data = higgs_data_orig[-index_20,],
                                     unadjusted_weight_col = "Weight",
                                     label_col = "Label")

