# Load packages
library(tidyverse)
library(skimr)
library(here)
library(higgiesmalls)

# Import data ------------------------------------------------------------------
# Download the data from http://opendata.cern.ch/record/328
# And save it in the /data folder
higgs_data_orig <- read_csv(here("data/atlas-higgs-challenge-2014-v2.csv"))
# skim(higgs_data_orig) # quick data summary

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
# skim(higgs_data_na)

# Get the names of all columns with missing data
missing_vars <- higgs_data_na %>% 
  dplyr::select(where(~any(is.na(.)))) %>% 
  names()

# Names of columns that are uniformly distributed
unif_vars <- c("PRI_jet_leading_phi", "PRI_met_phi", "PRI_lep_phi", "PRI_tau_phi")

# EDA --------------------------------------------------------------------------
# Example histogram of a variable split by signal vs. background
# ggplot(higgs_data_na, aes(x = DER_lep_eta_centrality, fill = Label)) +
#   geom_histogram()
