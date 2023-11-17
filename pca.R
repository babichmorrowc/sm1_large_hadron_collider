# References:
#   http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/#compute-pca-in-r-using-prcomp

# Perform Principle Component Analysis on the data
source("R/clean_data.R")
library(factoextra)
library(ggplot2)

# Training / test split --------------------------------------------------------
# Take 80% of the data for training, 20% for testing
set.seed(999)
index <- createDataPartition(higgs_vars$Label, p = 0.0004, list = FALSE)
pca_training <- higgs_vars[index,]
pca_testing <- higgs_vars[-index,]

# create dataframes without the missing variables
pca_training_nomissing <- pca_training %>%
  dplyr::select(-all_of(missing_vars))

pca_testing_nomissing <- pca_testing %>%
  dplyr::select(-all_of(missing_vars))


# Run PCA ----------------------------------------------------------------------
# using the original data with -999 values, result of pca = res.pca
res.pca <- prcomp(dplyr::select(pca_training_nomissing,where(is.numeric)), scale = TRUE)

# shows scree plot, showing percentages of var explained by each pc, number of pcs should be at elbow (~3 or 4)
fviz_eig(res.pca)

# plotting pc1 against pc2
dtp <- data.frame('Label' = pca_training_nomissing$Label, res.pca$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) +
  geom_point(aes(x = PC1, y = PC2, col = Label)) +
  theme_minimal()

# plotting pc1 vs pc2 vs pc3 as suggested by elbow plot
library(plotly)
dtp3d <- data.frame('Label' = pca_training_nomissing$Label, res.pca$x[,1:3]) # the first two components are selected (NB: you can also select 3 for 3D plottings or 3+)
dtp3d$custom_colors <- c("#00AFBB",  "#FC4E07")
fig<- plot_ly(data = dtp3d, x = ~PC1, y = ~PC2, z = ~PC3,
              type = 'scatter3d',
              color = ~Label,
              colors = ~custom_colors,
              mode = "markers",
              marker = list(size = 3))
fig

