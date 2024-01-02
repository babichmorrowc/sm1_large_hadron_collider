library(here)
library(ggplot2)
library(infotheo)

source(here("R/clean_data.R"))

#|-----------------------------------------------|

Mutual_Information<- function(parameter){
  PGivenB <- filter(higgs_data_orig,Label == "b")[parameter]
  PGivenS <- filter(higgs_data_orig,Label == "s")[parameter]
  PGivenB_Est <- sample(as.vector(unlist(PGivenB)),size = 20000,replace = FALSE)
  PGivenS_Est <- sample(as.vector(unlist(PGivenS)),size = 20000,replace = FALSE)
  ParaB <- discretize(PGivenB_Est, nbins = 20000)
  ParaS <- discretize(PGivenS_Est,nbins = 20000)
  return(mutinformation(ParaB,ParaS,method = "emp"))
}



header <- names(higgs_vars[0:30])
MIData2 <- data.frame(names = header, data = as.vector(unlist(sapply(header,Mutual_Information))))
MI_Data2_Ordered <- MIData2[order(MIData2$data),]
head(MI_Data2_Ordered,n= 10) #these are the lowest mutual information scoring variables
#MI_Data2_Ordered this is the whole set, i use this in the rmd file

