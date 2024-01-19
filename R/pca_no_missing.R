# References:
#   http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/#compute-pca-in-r-using-prcomp

# Perform Principle Component Analysis on the data
library(here)
source(here("R/clean_data.R"))
library(factoextra)
library(ggplot2)
library(caret)

# Training / test split --------------------------------------------------------
# Takes a random selection of data points to use in the plots since ~80k
set.seed(999)
higgs_vars_subset <- select(higgs_vars,- c(all_of(missing_vars)))
index <- createDataPartition(higgs_vars_subset$Label, p = 0.001, list = FALSE)

# Run PCA ----------------------------------------------------------------------
# using the original data with no variables with missing data
res.pca <- prcomp(dplyr::select(higgs_vars_subset,where(is.numeric)), scale = TRUE)

# shows scree plot, showing percentages of var explained by each pc, number of pcs should be at elbow (~3 or 4)
scree_plot <- fviz_eig(res.pca)

# plotting pc1 against pc2
dtp12 <- data.frame('Label' = higgs_vars_subset$Label[index], res.pca$x[index,1:2]) # the first two components are selected 
PC1_PC2_2D <-ggplot(data = dtp12) +
  geom_point(aes(x = PC1, y = PC2, col = Label), size=0.8) +
  theme_minimal() +
  ggtitle("PC1 vs PC2") +
  xlab("PC1") +
  ylab("PC2") + guides(color = FALSE)

# plotting pc2 against pc3
dtp23 <- data.frame('Label' = higgs_vars_subset$Label[index], res.pca$x[index,2:3]) # the first two components are selected
PC2_PC3_2D <- ggplot(data = dtp23) +
  geom_point(aes(x = PC2, y = PC3, col = Label), size=0.8) +
  theme_minimal() +
  ggtitle("PC2 vs PC3") +
  xlab("PC2") +
  ylab("PC3") + guides(color = FALSE)


#plotting pc1 against pc3
dtp13 <- data.frame('Label' = higgs_vars_subset$Label[index], cbind(PC1 =res.pca$x[index, 1], PC3 =res.pca$x[index, 3])) # the first two components are selected 
PC1_PC3_2D <- ggplot(data = dtp13) +
  geom_point(aes(x = PC1, y = PC3, col = Label), size=0.8) +
  theme_minimal() +
  ggtitle("PC1 vs PC3") +
  xlab("PC1") +
  ylab("PC3") + guides(color = FALSE)



# plotting pc1 vs pc2 vs pc3 as suggested by elbow plot
library(plotly)
dtp3d <- data.frame('Label' = higgs_vars_subset$Label[index], res.pca$x[index,1:3]) # the first two components are selected 
custom_colors <- c("#00AFBB",  "#FC4E07")
PC1_PC2_PC3_3D <- plot_ly(data = dtp3d[1:500,], x = ~PC1, y = ~PC2, z = ~PC3,
              type = 'scatter3d',
              color = ~Label,
              colors = custom_colors,
              mode = "markers",
              marker = list(size = 3)) %>%
              layout(title ="Data as Represented by the First Three Principle Components")

PC1_PC2_PC3_3D

# creating a dataframe of results of pca
res.pca.df <-data.frame(res.pca$rotation) # gives contribution of each variable for each PC

# plotting bar chart of the contribution of each variable on PC1 to PC4
PC1_var_contri <- ggplot(data=res.pca.df, aes(x=attributes(res.pca.df)$row.names, y=res.pca.df[,1])) +
  geom_bar(stat="identity", fill="#00AFBB") +
  geom_text(aes(label=round(PC1,2)),vjust=1.5, size=3.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Principle Component 1 Eigenvector") +
  xlab("Variables") +
  ylab("Contribution to PC1")


PC2_var_contri <- ggplot(data=res.pca.df, aes(x=attributes(res.pca.df)$row.names, y=res.pca.df[,2])) +
  geom_bar(stat="identity", fill="#00AFBB")+
  geom_text(aes(label=round(PC2,2)),vjust=1.5, size=3.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Principle Component 2 Eigenvector") +
  xlab("Variables") +
  ylab("Contribution to PC2")


PC3_var_contri <- ggplot(data=res.pca.df, aes(x=attributes(res.pca.df)$row.names, y=res.pca.df[,3])) +
  geom_bar(stat="identity", fill="#00AFBB")+
  geom_text(aes(label=round(PC3,2)),vjust=1.5, size=3.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Principle Component 3 Eigenvector") +
  xlab("Variables") +
  ylab("Contribution to PC3")


PC4_var_contri <- ggplot(data=res.pca.df, aes(x=attributes(res.pca.df)$row.names, y=res.pca.df[,4])) +
  geom_bar(stat="identity", fill="#00AFBB")+
  geom_text(aes(label=round(PC3,2)),vjust=1.5, size=3.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Principle Component 4 Eigenvector") +
  xlab("Variables") +
  ylab("Contribution to PC4")

