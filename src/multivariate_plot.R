

library(ggfortify)
df <- matrix_full_eco_elev_clim
df <- na.omit(df)
df_continous <- df[,colnames(df) %in% c("temp","elevation","precip")]
df_discrete <- df[,!(colnames(df) %in% c("temp","elevation","precip"))]



df_continous <- apply(df_continous,2,as.numeric)

pca_res <- prcomp(df_continous, scale. = TRUE)

autoplot(pca_res)

##########################################################################################
##########################################################################################

autoplot(pca_res, data = df_discrete, colour = 'species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

##########################################################################################
##########################################################################################

pca_plot <- autoplot(pca_res, data = df_discrete, colour = 'species',
                     loadings = TRUE, loadings.colour = 'black',
                     loadings.label = TRUE, loadings.label.size = 3)

pca_plot + theme_classic()

##########################################################################################
##########################################################################################


autoplot(pca_res, data = df_discrete, colour = 'species',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE) + theme_classic()


##########################################################################################
##########################################################################################

autoplot(pca_res, data = df_discrete, colour = 'species',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, frame.type = 'norm') + theme_classic()

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################
##########################################


row.names(df_continous) <- c(1:nrow(df_continous))
library(vegan)

dist_matt <- vegdist(df_continous, method = "euclidian")  #
D3_data_dist <- cmdscale(dist_matt, k = 3)
D3_data_dist <- data.frame(D3_data_dist)


cols <- df_discrete$species

PCOA <- ggplot(D3_data_dist, aes(x = X1, y = X2, color = cols)) +
  geom_point() + ggtitle("my project") +
  theme_classic()
PCOA


##########################################################################################
##########################################################################################
library(plotly)
##interactive 

intercative_pcao <- ggplotly(PCOA)
intercative_pcao

##########################################################################################
##########################################################################################
#### 3D

# Convert dataframe columns to numeric
df_continous <- apply(df_continous, 2, as.numeric)

# Perform PCA

# PCA
pca <- princomp(df_continous, scores=T, cor=T)

# Scores
scores <- pca$scores
x <- scores[,1]
y <- scores[,2]
z <- scores[,3]

# Loadings
loads <- pca$loadings

# Loadings names
load_names <- rownames(loads)

# Scale factor for loadings
scale.loads <- 5

# 3D plot
library(plotly)
p <- plot_ly() %>%
  add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="markers", color = df_discrete$species)

for (k in 1:nrow(loads)) {
   x <- c(0, loads[k,1])*scale.loads
   y <- c(0, loads[k,2])*scale.loads
   z <- c(0, loads[k,3])*scale.loads
   p <- p %>% add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="lines",
            line = list(width=8),
            opacity = 1,
            name = load_names[k])  # Adding names to the loadings
}
print(p)
