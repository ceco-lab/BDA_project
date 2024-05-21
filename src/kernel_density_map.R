
################################################################################
################################################################################

library(sf)
library(elevatr)
library(raster)
library(tidyverse)
library(RColorBrewer)
library(rayshader)
library(eks)


matrix_full_eco_elev_clim_sp1 <- matrix_full_eco_elev_clim[matrix_full_eco_elev_clim$species =="Cardamine hirsuta",]
matrix_full_eco_elev_clim_sp2 <- matrix_full_eco_elev_clim[matrix_full_eco_elev_clim$species =="Cardamine bellidifolia",]
sf_use_s2(FALSE)

#################
############# kernel sp1 
 sf_points1 <- data.frame(
    lat = matrix_full_eco_elev_clim_sp1$latitude,
    lon = matrix_full_eco_elev_clim_sp1$longitude
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)


skde1_sp1 <- st_kde(sf_points1, gridsize = c(100, 100))
plot(skde1_sp1)

data_sp1 = st_get_contour(skde1_sp1, cont = c(seq(50, 99, 5)), disjoint = FALSE)
# Create a function to generate the color palette
color_palette_sp1 <- colorRampPalette(c("white","darkred"))

# Define the number of colors in the palette
num_colors <- length(data_sp1$contlabel)  # Adjust as needed
# Generate the color palette
palette1 <- color_palette_sp1(num_colors)

#################
############# kernel sp2


 sf_points2 <- data.frame(
    lat = matrix_full_eco_elev_clim_sp2$latitude,
    lon = matrix_full_eco_elev_clim_sp2$longitude
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)


skde1_sp2 <- st_kde(sf_points2, gridsize = c(100, 100))
plot(skde1_sp2)

data_sp2 = st_get_contour(skde1_sp2, cont = c(seq(50, 99, 5)), disjoint = FALSE)
# Create a function to generate the color palette
color_palette_sp2 <- colorRampPalette(c("white","darkgreen"))

# Define the number of colors in the palette
num_colors <- length(data_sp2$contlabel)  # Adjust as needed
# Generate the color palette
palette2 <- color_palette_sp2(num_colors)

###################
################# plot 3d 

elmat <- raster_to_matrix(elevation_switzerland)

elmat %>%
 sphere_shade(texture = "bw") %>%
 add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.9)  %>%
 add_overlay(generate_polygon_overlay(data_sp1, 
                        palette = palette1, linewidth=0,
                        extent = extent(elevation_switzerland), heightmap = elmat),
                        alphalayer=0.7)  %>%
 add_overlay(generate_polygon_overlay(data_sp2, 
                        palette = palette2, linewidth=0,
                        extent = extent(elevation_switzerland), heightmap = elmat),
                        alphalayer=0.7)  %>%
plot_3d(elmat, zscale = 150, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))



##############
###############
########### plot(2d)


elmat <- raster_to_matrix(elevation_switzerland)

elmat %>%
 sphere_shade(texture = "desert") %>%
 add_overlay(generate_polygon_overlay(data_sp1, 
                        palette = palette1, linewidth=0,
                        extent = extent(elevation_switzerland), heightmap = elmat),
                        alphalayer=0.7)  %>%
 add_overlay(generate_polygon_overlay(data_sp2, 
                        palette = palette2, linewidth=0,
                        extent = extent(elevation_switzerland), heightmap = elmat),
                        alphalayer=0.7)  %>%

plot_map()







