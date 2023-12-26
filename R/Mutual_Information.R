library(here)
library(ggplot2)
library(infotheo)

source(here("R/clean_data.R"))
source(here("R/variable_plots.R"))

KL_divergence <- function(X,Y){
  
}
#|-----------------------------------------------|
#|
Mutual_Information <- function(parameter){
  XYdata <- ggplot_build(plots(parameter))$data[[1]]
  #obtains the data for the variable given b and s from the variable_plots file
  Para_GivenB <- filter(XYdata, fill =="#F8766D")$y
  Para_GivenS <- filter(XYdata,fill != "#F8766D")$y
  #use the colour of each plot to filter the densities into the b and s cases
  #take the y value out, so we have a set of outputs for random variables
  ParaB <- discretize(Para_GivenB, nbins = 200)
  ParaS <- discretize(Para_GivenS,nbins = 200)
  #discretize the data to make it compatible with the mutinfo function
  return(mutinformation(ParaS,ParaB))
}
#|-----------------------------------------------|

Mutual_Information2 <- function(parameter){
  PGivenB <- filter(higgs_data_orig,Label == "b")[parameter]
  PGivenS <- filter(higgs_data_orig,Label == "s")[parameter]
  PB <- discretize(PGivenB, nbins = 200)
  print(PB)
  PS <- discretize(PGivenS,nbins = 200)
  print(PS)
  return(mutinformation(PB,PS))
}


header <- names(higgs_vars)
MIData <- data.frame(names = header, data = as.vector(unlist(sapply(header,Mutual_Information))))
Mutual_Information2(header[1])
sapply(header,Mutual_Information2)
MIData2 <- data.frame(names = header, data = as.vector(unlist(sapply(header,Mutual_Information2))))
MI_Data_Ordered <- MIData[order(MIData$data),]
MI_Data2_Ordered <- MIData2[order(MIData2$data),]
head(MI_Data_Ordered,n= 10)
MI_Data2_Ordered

