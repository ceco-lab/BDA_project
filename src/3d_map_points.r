
################################################################################
################################################################################

library(sf)
library(elevatr)
library(raster)

sf_use_s2(FALSE)

Switzerland <- ne_countries(scale = "medium", returnclass = "sf",country ="Switzerland" )
elevation_switzerland <- get_elev_raster(Switzerland, z = 8)
plot(elevation_switzerland)



## crop and mask
r2 <- crop(elevation_switzerland, extent(Switzerland))
elevation_switzerland <- mask(r2, Switzerland)
plot(elevation_switzerland)

### sub part 


library(rayshader)
###### 3d plot v1 

elmat <- raster_to_matrix(elevation_switzerland)

attr(elmat, "extent") <- extent(elevation_switzerland)

elmat %>% 
  sphere_shade(texture = "bw") %>%
  plot_map()

############################################
###########################################

################## add texture  
library(png)

elevation.texture.map <- readPNG("./data/Switzerland2.png")
#elevation.texture.map.crop <- crop.image(elevation.texture.map,xleft=146,ybottom=7,xright=203,ytop=256)

##############################
############################### add points 


latitude <- matrix_full_eco_elev_clim$latitude
longitude <- matrix_full_eco_elev_clim$longitude

# Create a data frame for GBIF data
sp_points <- data.frame(longitude,latitude)

# Extract elevation values at Marmota marmota occurrences
ll_prj <- "EPSG:4326" 
points <- sp::SpatialPoints(sp_points, 
                            proj4string = sp::CRS(SRS_string = ll_prj))
elevation_points <- extract(elevation_switzerland, points, method='bilinear')
elevation_df <- data.frame(elevation = elevation_points)


  ###### 3d version 
elmat %>% 
  sphere_shade(texture = "bw") %>%
  add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.7)  %>%
   add_shadow(cloud_shade(elmat, zscale = 100, start_altitude = 500, end_altitude = 2000,), 0) %>%
   add_water(detect_water(elmat), color = "lightblue") %>%
plot_3d(elmat, zscale = 100, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))



# Create a factor vector with species
species_factor <- as.factor(matrix_full_eco_elev_clim$species)

# Create a vector of colors based on species names
color_vector <- ifelse(species_factor == "Cardamine hirsuta", "green",
                       ifelse(species_factor == "Cardamine bellidifolia", "red", "black"))

# Display the head of the color vector
head(color_vector)


# Render points on the 3D elevation map
render_points(
  extent = extent(Switzerland), size = 10,
  lat = matrix_full_eco_elev_clim$latitude, long = matrix_full_eco_elev_clim$longitude,
  altitude = elevation_points + 100, zscale = 150,
  color = color_vector
)


