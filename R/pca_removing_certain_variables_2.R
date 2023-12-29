# References:
#   http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/#compute-pca-in-r-using-prcomp

# Perform Principle Component Analysis on the data
source("R/clean_data.R")
library(factoextra)
library(ggplot2)
library(caret)

# Training / test split --------------------------------------------------------
# Take 80% of the data for training, 20% for testing and removing uninteresting variables
set.seed(999)
higgs_vars_subset <- select(higgs_vars,- c(all_of(missing_vars),c("PRI_jet_leading_phi","PRI_met_phi","PRI_lep_phi","PRI_tau_phi","PRI_jet_all_pt","DER_pt_ratio_lep_tau")))
index <- createDataPartition(higgs_vars_subset$Label, p = 0.0004, list = FALSE)
pca_training <- higgs_vars_subset[index,]
pca_testing <- higgs_vars_subset[-index,]

# Run PCA ----------------------------------------------------------------------
# using the original data with -999 values, result of pca = res.pca
res.pca <- prcomp(dplyr::select(pca_training,where(is.numeric)), scale = TRUE)

# shows scree plot, showing percentages of var explained by each pc, number of pcs should be at elbow (~3 or 4)
fviz_eig(res.pca)

# plotting pc1 against pc2
dtp <- data.frame('Label' = pca_training$Label, res.pca$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) +
  geom_point(aes(x = PC1, y = PC2, col = Label)) +
  theme_minimal() +
  ggtitle("Data as Represented by the First Two Principle Components") +
  xlab("PC1") +
  ylab("PC2")

# plotting pc1 vs pc2 vs pc3 as suggested by elbow plot
library(plotly)
dtp3d <- data.frame('Label' = pca_training$Label, res.pca$x[,1:3]) # the first two components are selected (NB: you can also select 3 for 3D plottings or 3+)
dtp3d$custom_colors <- c("#00AFBB",  "#FC4E07")
fig<- plot_ly(data = dtp3d, x = ~PC1, y = ~PC2, z = ~PC3,
              type = 'scatter3d',
              color = ~Label,
              colors = ~custom_colors,
              mode = "markers",
              marker = list(size = 3))
fig %>%
  layout(title ="Data as Represented by the First Three Principle Components",
         xaxis = list(title = "PC1"),
         yaxis = list(title = "PC2"),
         zaxis = list(title = "PC3"))

# creating a dataframe of results of pca
res.pca.df <-data.frame(res.pca$rotation) # gives contribution of each variable for each PC

# plotting bar chart of the contribution of each variable on pc1
ggplot(data=res.pca.df, aes(x=attributes(res.pca.df)$row.names, y=res.pca.df[,1])) +
  geom_bar(stat="identity", fill="#00AFBB") +
  geom_text(aes(label=round(PC1,2)),vjust=1.5, size=3.5) +
  theme_minimal() +
  ggtitle("Principle Component 1 Eigenvector") +
  xlab("Variables") +
  ylab("Contribution to PC1")

# plotting bar chart of the contribution of each variable on pc2
ggplot(data=res.pca.df, aes(x=attributes(res.pca.df)$row.names, y=res.pca.df[,2])) +
  geom_bar(stat="identity", fill="#00AFBB")+
  geom_text(aes(label=round(PC2,2)),vjust=1.5, size=3.5) +
  theme_minimal() +
  ggtitle("Principle Component 2 Eigenvector") +
  xlab("Variables") +
  ylab("Contribution to PC2")

# plotting bar chart of the contribution of each variable on pc1,2,&3 same plot
ggplot(data=res.pca.df, aes(x=attributes(res.pca.df)$row.names, y=res.pca.df[,3])) +
  geom_bar(stat="identity", fill="#00AFBB")+
  geom_text(aes(label=round(PC3,2)),vjust=1.5, size=3.5) +
  theme_minimal() +
  ggtitle("Principle Component 3 Eigenvector") +
  xlab("Variables") +
  ylab("Contribution to PC3")

