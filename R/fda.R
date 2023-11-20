# Perform Fisher Discriminant Analysis on the data
source(here("R/clean_data.R"))

library(MASS) # package for lda
library(caret) # ML package

# Training / test split --------------------------------------------------------
# Take 80% of the data for training, 20% for testing
set.seed(999)
index <- createDataPartition(higgs_vars$Label, p = 0.8, list = FALSE)
fda_training <- higgs_vars[index,]
fda_testing <- higgs_vars[-index,]

# create dataframes without the missing variables
fda_training_nomissing <- fda_training %>% 
  dplyr::select(-all_of(missing_vars))
fda_testing_nomissing <- fda_testing %>% 
  dplyr::select(-all_of(missing_vars))

# Run FDA ----------------------------------------------------------------------
# using the original data with -999 values
fda <- lda(Label ~ ., fda_training)
# Getting:
# Warning message:
#   In lda.default(x, grouping, ...) : variables are collinear
fda

fda_pred <- predict(fda, fda_training)
ldahist(data = fda_pred$x[,1], g = fda_training$Label) # not a lot of separation

# using only variables without missing values
fda_nomissing <- lda(Label ~ ., fda_training_nomissing)
# Still getting:
# Warning message:
#   In lda.default(x, grouping, ...) : variables are collinear
fda_nomissing

fda_nomissing_pred <- predict(fda_nomissing, fda_training_nomissing)
ldahist(data = fda_nomissing_pred$x[,1], g = fda_training_nomissing$Label) # still not a lot of separation



