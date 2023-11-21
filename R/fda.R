# Perform Fisher Discriminant Analysis on the data
source(here("R/clean_data.R"))

library(MASS) # package for lda
library(caret) # ML package

# Run FDA ----------------------------------------------------------------------
#### All variables ####
# using the original data with -999 values and all variables
fda_all <- lda(Label ~ ., higgs_vars)
# Getting:
# Warning message:
#   In lda.default(x, grouping, ...) : variables are collinear
fda_all

fda_all_pred <- predict(fda_all, higgs_vars)
ldahist(data = fda_all_pred$x[,1], g = higgs_vars$Label) # not a lot of separation

#### Drop uniform variables ####
# Remove the variables that are uniformly distributed between signal & background
higgs_vars_drop_unif <- higgs_vars %>% 
  dplyr::select(-all_of(unif_vars))

fda_drop_unif <- lda(Label ~ ., higgs_vars_drop_unif)
# Getting:
# Warning message:
#   In lda.default(x, grouping, ...) : variables are collinear
fda_drop_unif

fda_drop_unif_pred <- predict(fda_drop_unif, higgs_vars_drop_unif)
ldahist(data = fda_drop_unif_pred$x[,1], g = higgs_vars_drop_unif$Label) # basically the same

#### Drop uniform + combo variables ####
# Also drop the individual components that sum to PRI_jet_all_pt
# and those that make up the DER_pt_ratio_lep_tau ratio
higgs_vars_drop_combo <- higgs_vars_drop_unif %>% 
  dplyr::select(-c(PRI_jet_leading_pt,
                   PRI_jet_subleading_pt,
                   PRI_lep_pt,
                   PRI_tau_pt))

fda_drop_combo <- lda(Label ~ ., higgs_vars_drop_combo)
# No more warning message!
fda_drop_combo

fda_drop_combo_pred <- predict(fda_drop_combo, higgs_vars_drop_combo)
ldahist(data = fda_drop_combo_pred$x[,1], g = higgs_vars_drop_combo$Label) # basically the same
