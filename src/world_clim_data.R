

library(geodata)
###################################################################
###################################################################


# Assuming matrix_full is your data frame with latitude and longitude columns
spatial_points <- SpatialPoints(coords = matrix_full_eco_elev[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))


###################################################################
###################################################################
# Retrieve temperature data for Switzerland
sw_clim <- worldclim_country("switzerland", var = "tmin", path = tempdir())
sw_clim_br <- brick(sw_clim)


january_raster <- sw_clim_br$CHE_wc2.1_30s_tmin_1

# Extract temperature values at Marmota marmota occurrences
temp<- extract(january_raster, spatial_points, method = 'bilinear')
temp <- data.frame(temp)

# Plot density of temperature data for Marmota marmota occurrences
ggplot(temp, aes(x = temp)) +
  geom_density(color = "darkblue", fill = "lightblue", adjust = 3) +
  theme_bw()


###################################################################
###################################################################
###################################################################
###################################################################
# Retrieve precipitation data for Switzerland
sw_clim_pec <- worldclim_country("switzerland", var = "prec", path = tempdir())
sw_clim_pec <- brick(sw_clim_pec)
january_raster <- sw_clim_pec$CHE_wc2.1_30s_prec_1

# Extract precipitation values at Marmota marmota occurrences
precip <- extract(january_raster, spatial_points, method = 'bilinear')
precip <- data.frame(precip)

# Plot density of precipitation data for Marmota marmota occurrences
ggplot(precip, aes(x = precip)) +
  geom_density(color = "black", fill = "darkgreen", adjust = 2) +
  theme_bw()



#################
matrix_full_eco_elev_clim <- data.frame(matrix_full_eco_elev,precip,temp)


# Load required library
# Load required library
library(ggplot2)

# Create the ggplot
p4 <- ggplot(matrix_full_eco_elev_clim, aes(x = precip,y=temp, color = species)) +
  geom_point() +
  theme_minimal()


print(p4)