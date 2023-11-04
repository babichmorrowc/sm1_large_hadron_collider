# Load packages
library(tidyverse)
library(skimr)

# Import data ------------------------------------------------------------------
# Download the data from http://opendata.cern.ch/record/328
# And save it in the /data folder
higgs_data_orig <- read_csv("./data/atlas-higgs-challenge-2014-v2.csv")
skim(higgs_data_orig) # quick data summary

# Clean data -------------------------------------------------------------------
higgs_data <- higgs_data_orig %>% 
  # Replace all of the -999 values with NA
  mutate(across(where(is.numeric), ~na_if(.x, -999)))
