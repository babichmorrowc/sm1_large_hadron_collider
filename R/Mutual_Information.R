library(here)#file organiser
library(infotheo)#information theory package

source(here("R/clean_data.R"))#take higgs data

#|Mutual Info-----------------------------------------|

Mutual_Information<- function(parameter){
  Variable = higgs_data_orig[parameter]
  Labels = higgs_data_orig["Label"]
  #take variable data form higgs orig
  Labels[Labels == "s"] <- "1"
  Labels[Labels == "b"] <- "0"
  Labels_Val <- apply(Labels,2,as.numeric)
  #replace with numeric for mutinfo
  ParaB <- discretize(Variable, nbins = 20000)
  ParaS <- discretize(Labels_Val,nbins = 20000)
  #mutinformation requires binned data
  return(mutinformation(ParaB,ParaS,method = "emp"))
}
#|----------------------------------------------------|

#|Variables to use-----------------------------------|
header = names(higgs_data_orig[2:30])
#|----------------------------------------------------|

#Compute MI-------------------------------------------|
#The code below finds MIdata, takes ~5 minutes 
#MIData <- data.frame(names = header, data = as.vector(unlist(sapply(header,Mutual_Information))))
#print(MIData)
#MI_Data_Ordered <- MIData[order(MIData$data),]
#|----------------------------------------------------|

#Local Copy of dataframe-------------------------------------------|
MI_Data_STORED <- data.frame(names = header,data=c(0.157847348,0.099242021,0.090955700,0.034024850,0.033387741,0.032628663,0.032134847,0.018698667,0.010926408,0.039696267,0.043557790,0.048218121,0.025173021,0.064883381,0.009364819,0.004573067,0.013445495,0.013914809,0.003812932,0.027523670,0.003771850,0.031078017,0.019823370,0.026665620,0.025529809,0.015581755,0.013785587,0.020635994,0.013326433))
MI_Data_STORED_Ordered <- MI_Data_STORED[order(MI_Data_STORED$data,decreasing = TRUE),]
