# Perform Fisher Discriminant Analysis on the data
source(here("R/fisher_discrim.R"))
# library(MASS) # package for lda
# library(lfda) # package for lfda

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

#### Drop lowest mutual information ####
# Finally, drop the variables with the top 10 lowest mutual information

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

#### Drop lowest mutual information ####

# Use lda function -------------------------------------------------------------
#### All variables ####
# using the original data with -999 values and all variables
# fda_all <- MASS::lda(Label ~ ., higgs_vars)
# # Getting:
# # Warning message:
# #   In lda.default(x, grouping, ...) : variables are collinear
# # fda_all
# scatter_ratio(higgs_vars, class_pos = "s", class_neg = "b", w = fda_all$scaling)
# fda_all_pred <- predict(fda_all, higgs_vars)
# # ldahist(data = fda_all_pred$x[,1], g = higgs_vars$Label) # not a lot of separation
# 
# # Create scaled dataframe for ggplot
# higgs_vars_all_pred_scaled <- cbind(
#   fda_all_pred$x,
#   dplyr::select(higgs_vars, Label)
# )
# 
# #### Drop uniform variables ####
# fda_drop_unif <- MASS::lda(Label ~ ., higgs_vars_drop_unif)
# # Getting:
# # Warning message:
# #   In lda.default(x, grouping, ...) : variables are collinear
# # fda_drop_unif
# 
# fda_drop_unif_pred <- predict(fda_drop_unif, higgs_vars_drop_unif)
# # ldahist(data = fda_drop_unif_pred$x[,1], g = higgs_vars_drop_unif$Label) # basically the same
# 
# # Create scaled dataframe for ggplot
# higgs_vars_drop_unif_pred_scaled <- cbind(
#   fda_drop_unif_pred$x,
#   dplyr::select(higgs_vars, Label)
# )
# 
# #### Drop uniform + combo variables ####
# fda_drop_combo <- MASS::lda(Label ~ ., higgs_vars_drop_codep_unif)
# # No more warning message!
# # fda_drop_combo
# scatter_ratio(higgs_vars_drop_codep_unif, class_pos = "s", class_neg = "b", w = fda_drop_combo$scaling)
# fda_drop_combo_pred <- predict(fda_drop_combo, higgs_vars_drop_combo)
# # ldahist(data = fda_drop_combo_pred$x[,1], g = higgs_vars_drop_combo$Label) # basically the same
# 
# # Create scaled dataframe for ggplot
# higgs_vars_drop_combo_pred_scaled <- cbind(
#   fda_drop_combo_pred$x,
#   dplyr::select(higgs_vars, Label)
# )

# Use lfda function ------------------------------------------------------------
# Won't run:
# lfda_drop_codep <- lfda(x = higgs_vars_drop_codep[,-27], y = higgs_vars_drop_codep$Label, r = 2)
# Error: cannot allocate vector of size 582.3 Gb

# kern_mat <- kmatrixGauss(higgs_training_drop_codep[, -27])

