###
# Function to generate a kernal density map of the occurence data for CWR NA. For not this is just a visualization 
# tool for understanding the potential spatial biasing in the points. I want to be able to high light areas of high sampling
# density and areas of low density. A possible extension of this it attempting to smooth the varability acroos the area, 
# by adjusting the number of points in a region, though that will be a complicated process. 
# 20200125
# dan.carver@carverd.com
###

kernalDensity <- function(species){
  # so both cleanPoints and thrshold are both global objects so I should need to call them 
  k2 <- spatialEco::sp.kde(x = cleanPoints, newdata = thrshold, standardize = TRUE)
  raster::writeRaster(x = k2, filename = paste0(sp_dir, "/modeling/kernalDensity.tif"),overwrite=TRUE)

  } 

