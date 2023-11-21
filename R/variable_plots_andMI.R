library(tidyverse)
library(here)
library(skimr)
library(ggplot2)
library(purrr)
library(ggpubr) #for ggarrange
library(DT) #datatables
library(tfprobability)

source(here("R/clean_data.R"))

plots <- function(Parameter){#function which produces a geom_density plot for some random parameter
  pl <- ggplot(higgs_data_orig, aes(x= get(Parameter), fill = Label)) +
    geom_density(alpha = 0.5) +
    labs(x = Parameter)
  return(pl)
}

KL_divergence<- function(Parameter){
  density_func <- (ggplot_build(plots(Parameter))$data)[[1]]
  density_func_b <- filter(density_func, fill == "#F8766D")
  density_func_s <- filter(density_func, fill != "#F8766D")
  density_func_joint <- density_func
  density_func_s$y <- (density_func_s$y)/(sum(density_func_s$y))
  density_func_b$y <-(density_func_b$y)/(sum(density_func_b$y))
  density_func_joint$y <- (density_func_joint$y)/(sum(density_func_joint$y))
  KL_sb <- sum((density_func_s$y)*log(density_func_s$y/density_func_b$y))
  return(KL_sb)
}

#cols_ToPlot <- c(colnames(higgs_data)[2:3],colnames(higgs_data)[29:30]) #plot some subset of the variables
#plot_list <- lapply(cols_ToPlot,plots)#apply function to every one
#ggarrange(plotlist = plot_list,ncol = 2, nrow = 2, align = "v", common.legend = TRUE)#arrange in a grid


KL_divergence("PRI_tau_phi")
KL_divergence("PRI_lep_eta")
KL_divergence("PRI_tau_eta")
KL_divergence("DER_mass_transverse_met_lep")
KL_divergence("DER_deltar_tau_lep")
KL_divergence("DER_met_phi_centrality")
KL_divergence("PRI_met_phi")
KL_divergence("PRI_jet_num")
