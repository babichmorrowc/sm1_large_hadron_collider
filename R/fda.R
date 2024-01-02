# Perform Fisher Discriminant Analysis on the data

# Data cleaning ----------------------------------------------------------------
#### Drop algebraic co-dependencies ####
# Drop the individual components that sum to PRI_jet_all_pt
# and those that make up the DER_pt_ratio_lep_tau ratio
higgs_vars_drop_codep <- higgs_vars %>% 
  select(-c(PRI_jet_leading_pt,
            PRI_jet_subleading_pt,
            PRI_lep_pt,
            PRI_tau_pt))

# Make matrix of predictor variables
higgs_vars_drop_codep_matrix <- higgs_vars_drop_codep %>% 
  select(-Label) %>% 
  as.matrix()

#### Drop uniform variables ####
# In addition to removing the uniformly distributed variables
# Remove the variables that are uniformly distributed between signal & background
higgs_vars_drop_codep_unif <- higgs_vars_drop_codep %>% 
  dplyr::select(-all_of(unif_vars))

# Make matrix of predictor variables
higgs_vars_drop_codep_unif_matrix <- higgs_vars_drop_codep_unif %>% 
  select(-Label) %>% 
  as.matrix()

#### Highest mutual information ####
# Finally, use the variables with the top 10 highest mutual information
mut_info_vars <- c('PRI_met_phi', 'DER_pt_tot', 'PRI_tau_pt', 'PRI_lep_pt', 'PRI_met', 'DER_mass_vis', 'DER_mass_transverse_met_lep', 'DER_pt_h', 'DER_sum_pt', 'PRI_met_sumet')
higgs_vars_mut_info <- higgs_vars %>% 
  select(all_of(mut_info_vars),
         Label)

# Make matrix of predictor variables
higgs_vars_mut_info_matrix <- higgs_vars_mut_info %>% 
  select(-Label) %>% 
  as.matrix()

# Use fisher_discrim function --------------------------------------------------
#### With all variables ####
fisher_discrim_all <- fisher_discrim(higgs_vars, class_pos = "s", class_neg = "b")
fisher_discrim_all_pred <- apply(as.matrix(select(higgs_vars, -Label)), 1, function(x) sum(x*fisher_discrim_all))
scatter_ratio_all <- scatter_ratio(higgs_vars, class_pos = "s", class_neg = "b", w = fisher_discrim_all)

# Create scaled dataframe for ggplot
fisher_discrim_higgs_vars_all <- cbind(
  fisher_discrim_all_pred,
  dplyr::select(higgs_vars, Label)
)

# ggplot() +
#   geom_histogram(aes(x = fisher_discrim_all_pred, y = stat(density), fill = Label),
#                  data = fisher_discrim_higgs_vars_all)

#### Drop algebraic co-dependencies ####
fisher_discrim_drop_codep <- fisher_discrim(higgs_vars_drop_codep, class_pos = "s", class_neg = "b")
fisher_discrim_drop_codep_pred <- apply(higgs_vars_drop_codep_matrix, 1, function(x) sum(x*fisher_discrim_drop_codep))
scatter_ratio_drop_codep <- scatter_ratio(higgs_vars_drop_codep, class_pos = "s", class_neg = "b", w = fisher_discrim_drop_codep)

# Create scaled dataframe for ggplot
fisher_discrim_higgs_vars_drop_codep <- cbind(
  fisher_discrim_drop_codep_pred,
  dplyr::select(higgs_vars, Label)
)

# ggplot() +
#   geom_histogram(aes(x = fisher_discrim_drop_codep_pred, y = stat(density), fill = Label),
#                  data = fisher_discrim_higgs_vars_drop_codep)


#### Drop uniform variables ####
fisher_discrim_drop_codep_unif <- fisher_discrim(higgs_vars_drop_codep_unif, class_pos = "s", class_neg = "b")
fisher_discrim_drop_codep_unif_pred <- apply(higgs_vars_drop_codep_unif_matrix, 1, function(x) sum(x*fisher_discrim_drop_codep_unif))
scatter_ratio_drop_codep_unif <- scatter_ratio(higgs_vars_drop_codep_unif, class_pos = "s", class_neg = "b", w = fisher_discrim_drop_codep_unif)

# Create scaled dataframe for ggplot
fisher_discrim_higgs_vars_drop_codep_unif <- cbind(
  fisher_discrim_drop_codep_unif_pred,
  dplyr::select(higgs_vars, Label)
)

# ggplot() +
#   geom_histogram(aes(x = fisher_discrim_drop_codep_unif_pred, y = stat(density), fill = Label),
#                  data = fisher_discrim_higgs_vars_drop_codep_unif)

#### Highest mutual information ####
fisher_discrim_mut_info <- fisher_discrim(higgs_vars_mut_info, class_pos = "s", class_neg = "b")
fisher_discrim_mut_info_pred <- apply(higgs_vars_mut_info_matrix, 1, function(x) sum(x*fisher_discrim_mut_info))
scatter_ratio_mut_info <- scatter_ratio(higgs_vars_mut_info, class_pos = "s", class_neg = "b", w = fisher_discrim_mut_info)

# Create scaled dataframe for ggplot
fisher_discrim_higgs_vars_mut_info <- cbind(
  fisher_discrim_mut_info_pred,
  dplyr::select(higgs_vars, Label)
)
