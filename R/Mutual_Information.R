library(here)
library(ggplot2)
library(infotheo)

source(here("R/clean_data.R"))

#|-----------------------------------------------|




Mutual_Information<- function(parameter){
  Variable = higgs_data_orig[parameter]
  Labels = higgs_data_orig["Label"]
  Labels[Labels == "s"] <- "1"
  Labels[Labels == "b"] <- "0"
  Labels_Val <- apply(Labels,2,as.numeric)
  ParaB <- discretize(Variable, nbins = 20000)
  ParaS <- discretize(Labels_Val,nbins = 20000)
  return(mutinformation(ParaB,ParaS,method = "emp"))
}

#COMPUTE MUTUAL INFO BETWEEN VAR and Y
MIData <- data.frame(names = header, data = as.vector(unlist(sapply(header,Mutual_Information))))
MI_Data_Ordered <- MIData[order(MIData$data),]


