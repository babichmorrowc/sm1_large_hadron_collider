library(here)
library(ggplot2)
library(infotheo)

source(here("R/clean_data.R"))
source(here("R/variable_plots.R"))

KL_divergence <- function(X,Y){
  
}

Mutual_Information <- function(parameter){
  XYdata <- gg_build(plots(parameter))$data[[1]]
  print(XYdata)
}

V <- higgs_vars
Mutual