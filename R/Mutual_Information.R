library(here)
library(ggplot2)
library(infotheo)

source(here("R/clean_data.R"))
source(here("R/variable_plots.R"))

KL_divergence <- function(X,Y){
  
}

Mutual_Information <- function(parameter){
  XYdata <- ggplot_build(plots(parameter))$data[[1]]
  Para_GivenB <- filter(XYdata, fill =="#F8766D")$y
  Para_GivenS <- filter(XYdata,fill != "#F8766D")$y
  ParaB <- discretize(Para_GivenB, nbins = 200)
  ParaS <- discretize(Para_GivenS,nbins = 200)
  return(mutinformation(ParaS,ParaB))
}

names(higgs_vars)
MIData <- data.frame(names = sapply(names(higgs_vars),Mutual_Information))


