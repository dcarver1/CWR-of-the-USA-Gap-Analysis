###
# Function to generate a kernal density map of the occurence data for CWR NA.
# This is a visualization tool that will help in understanding the potential
# spatial biasing in the occurrence dataset. I want to be able to highlight
# areas of high sampling density and areas of low density. A possible extension
# of this it attempting to smooth the varability acroos the area,
# by adjusting the number of points in a region, though that will be a
# complicated process.
# 20200414
# dan.carver@carverd.com
###

kernalDensity <- function(species){
  # so both cleanPoints and thrshold are both global objects so I should need to call them
  k2 <- spatialEco::sp.kde(x = cleanPoints, newdata = thrshold, standardize = TRUE)
  raster::writeRaster(x = k2, filename = paste0(sp_dir, "/modeling/kernalDensity.tif"),overwrite=TRUE)

  # potential work flow for test idea
  # extract kernal values back to cleanPoints
  # filter out a portion of the points with high kernal density
  # re run KDE
  # repeat until a range the distribution of the kernal density values reachs
  # a specific level of normal? or some other metric(range of KDE values)
  # rerun the modeling methodology and compare outputs 
  }
