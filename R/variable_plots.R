library(tidyverse)
library(here)
library(skimr)
library(ggplot2)
library(purrr)
library(ggpubr) #for ggarrange
library(DT) #datatables

source(here("R/clean_data.R"))

#|-------------------------------------------------|
#function which produces a density plot for each variable, with two different densities
# which are the the variable given that it is labelled background and labelled as a signal respectively

plots <- function(Parameter){
  #function takes each variable as input
  pl <- ggplot(higgs_data_na, aes(x= get(Parameter), fill = Label)) +
    #create ggplot with higgs_data_na splitting the data into two categories by using the Label (={b,s}) variable
    geom_density(alpha = 0.5) +
    #choose opacity of plots
    labs(x = Parameter)
  return(pl)
}

#|-------------------------------------------------|
#function which produces the joint distribution of the var|b and var|s 
plots_pairwise <- function(P1){
  #takes in variable as an input
  mydiff <- function(data, diff){return(c(data, rep(NA, diff)))}
  P1_b <- as.vector(unlist(filter(higgs_data_na,Label == "b")[P1]))
  P1_s <- as.vector(unlist(filter(higgs_data_na, Label == "s")[P1]))
  #filter the variable into its data points given that they are labelled signal and background
  difference = abs(length(P1_b)-length(P1_s))
  smaller_P1 <- min(length(P1_b),length(P1_s))
  if (smaller_P1 == length(P1_b)){
    P1_b <- mydiff(P1_b,difference)
  } else{
    P1_s <- mydiff(P1_s,difference)}
  #the above fills in the shorter vector between P1_b and p1_s with na values
  #this is because stat_bin_2d won't acceot different length vectors as input
  pl <- ggplot() +
    stat_bin_2d(bins = 70,aes(x=P1_b,y = P1_s))+
    theme_bw()
  return(pl)
}

#|-------------------------------------------------|
# function which produces the product distribution of var|b and var|s
plots_product <- function(P1){
  b_data <- filter(ggplot_build(plots(P1))$data[[1]],fill == "#F8766D")
  s_data <- filter(ggplot_build(plots(P1))$data[[1]],fill != "#F8766D")
  print(head(b_data))
  print(head(s_data))
}

#|-------------------------------------------------|
#define which families of variables to plot
cols_ToPlot <- c(colnames(higgs_data_na)[8:9],colnames(higgs_data_na)[29:30]) #random sample of histograms
cols_ToPlot2 <- c(colnames(higgs_data_na)[1:30]) #all variables but with na values put in
cols_ToPlot3 <- c(colnames(higgs_data_orig)[1:30]) #all variables but with -999 values put in
#LIST OF RELEVANT PLOTS FOR THE RMD FILE
#|----------------------------------------------------------|
#SAMPLE PLOTS for variable analysis section
#|--------------------------------------------|
#|----------------------------|
#sample 1D plots
#density_plots <- lapply(cols_ToPlot,plots)
#plot_sample_densities <- ggarrange(plotlist = density_plots,ncol = 2, nrow = 2, align = "v", common.legend = TRUE)
#plot_sample_densities
#|----------------------------|
#sample 2d histograms
#joint_density_plots <- lapply(cols_ToPlot,plots_pairwise)
#plot_sample_joint_densities <- ggarrange(plotlist = joint_density_plots,ncol = 2, nrow = 2, align = "v", common.legend = TRUE)
#plot_sample_joint_densities
#|--------------------------------------------|
#full family of plots for the appendix of rmd file
#|----------------------------|
#full 1d plots
#density_plots <- lapply(cols_ToPlot2,plots)
#plot_sample_densities <- ggarrange(plotlist = density_plots,ncol = 2, nrow = 2, align = "v", common.legend = TRUE)
#plot_sample_densities
#|----------------------------|
# full 2d plots
#joint_density_plots <- lapply(cols_ToPlot2,plots_pairwise)
#plot_sample_joint_densities <- ggarrange(plotlist = joint_density_plots,ncol = 2, nrow = 2, align = "v", common.legend = TRUE)
#plot_sample_joint_densities
#plots_product("PRI_jet_subleading_phi")

