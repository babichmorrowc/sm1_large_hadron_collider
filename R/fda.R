# Perform Fisher Discriminant Analysis on the data
# source(here("R/clean_data.R"))

library(MASS) # package for lda
library(lfda) # package for lfda
library(caret) # ML package

# Data cleaning ----------------------------------------------------------------
#### Drop uniform variables ####
# Remove the variables that are uniformly distributed between signal & background
higgs_vars_drop_unif <- higgs_vars %>% 
  dplyr::select(-all_of(unif_vars))

#### Drop uniform + combo variables ####
# Also drop the individual components that sum to PRI_jet_all_pt
# and those that make up the DER_pt_ratio_lep_tau ratio
higgs_vars_drop_combo <- higgs_vars_drop_unif %>% 
  dplyr::select(-c(PRI_jet_leading_pt,
                   PRI_jet_subleading_pt,
                   PRI_lep_pt,
                   PRI_tau_pt))

# Use lda function -------------------------------------------------------------
#### All variables ####
# using the original data with -999 values and all variables
fda_all <- lda(Label ~ ., higgs_vars)
# Getting:
# Warning message:
#   In lda.default(x, grouping, ...) : variables are collinear
# fda_all

fda_all_pred <- predict(fda_all, higgs_vars)
# ldahist(data = fda_all_pred$x[,1], g = higgs_vars$Label) # not a lot of separation

# Create scaled dataframe for ggplot
higgs_vars_all_pred_scaled <- cbind(
  fda_all_pred$x,
  dplyr::select(higgs_vars, Label)
)

#### Drop uniform variables ####
fda_drop_unif <- lda(Label ~ ., higgs_vars_drop_unif)
# Getting:
# Warning message:
#   In lda.default(x, grouping, ...) : variables are collinear
# fda_drop_unif

fda_drop_unif_pred <- predict(fda_drop_unif, higgs_vars_drop_unif)
# ldahist(data = fda_drop_unif_pred$x[,1], g = higgs_vars_drop_unif$Label) # basically the same

# Create scaled dataframe for ggplot
higgs_vars_drop_unif_pred_scaled <- cbind(
  fda_drop_unif_pred$x,
  dplyr::select(higgs_vars, Label)
)

#### Drop uniform + combo variables ####
fda_drop_combo <- lda(Label ~ ., higgs_vars_drop_combo)
# No more warning message!
# fda_drop_combo

fda_drop_combo_pred <- predict(fda_drop_combo, higgs_vars_drop_combo)
# ldahist(data = fda_drop_combo_pred$x[,1], g = higgs_vars_drop_combo$Label) # basically the same

# Create scaled dataframe for ggplot
higgs_vars_drop_combo_pred_scaled <- cbind(
  fda_drop_combo_pred$x,
  dplyr::select(higgs_vars, Label)
)

# Use lfda function ------------------------------------------------------------
# Won't run:
# lfda_drop_combo <- lfda(x = higgs_training_drop_combo, y = higgs_training_drop_combo$Label, r = 2) 

kern_mat <- kmatrixGauss(higgs_training_drop_combo[, -23])

# Use fisher_discrim function --------------------------------------------------
#### Drop uniform + combo variables ####
fisher_discrim_drop_combo <- fisher_discrim(higgs_vars_drop_combo, class_pos = "s", class_neg = "b")

higgs_vars_drop_combo_matrix <- higgs_vars_drop_combo %>% 
  select(-Label) %>% 
  as.matrix()
fisher_discrim_drop_combo_pred <- apply(higgs_vars_drop_combo_matrix, 1, function(x) sum(x*fisher_discrim_drop_combo))

# Create scaled dataframe for ggplot
fisher_discrim_higgs_vars_drop_combo <- cbind(
  fisher_discrim_drop_combo_pred,
  dplyr::select(higgs_vars, Label)
)

ggplot() +
  geom_histogram(aes(x = fisher_discrim_drop_combo_pred, y = stat(density), fill = Label), data = fisher_discrim_higgs_vars_drop_combo)
