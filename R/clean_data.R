# Load packages
library(tidyverse)
library(skimr)

# Import data ------------------------------------------------------------------
# Download the data from http://opendata.cern.ch/record/328
# And save it in the /data folder
higgs_data_orig <- read_csv("./data/atlas-higgs-challenge-2014-v2.csv")
skim(higgs_data_orig) # quick data summary

# Clean data -------------------------------------------------------------------
# Dataframe with just the predictors and label variables
higgs_vars <- higgs_data_orig %>% 
  dplyr::select(starts_with(c("DER", "PRI", "Label")))

# Dataframe replacing the -999 with NA
higgs_data_na <- higgs_data_orig %>% 
  # Replace all of the -999 values with NA
  mutate(across(where(is.numeric), ~na_if(.x, -999)))
# Skim of data with NA values
# complete_rate shows variables with lots of missing data
skim(higgs_data_na)

# Get the names of all columns with missing data
missing_vars <- higgs_data_na %>% 
  dplyr::select(where(~any(is.na(.)))) %>% 
  names()

# EDA --------------------------------------------------------------------------
# Example histogram of a variable split by signal vs. background
ggplot(higgs_data_na, aes(x = DER_lep_eta_centrality, fill = Label)) +
  geom_histogram()
