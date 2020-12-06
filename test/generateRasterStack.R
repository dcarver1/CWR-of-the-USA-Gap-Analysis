###
# Creating a new raster stack file from individual bands 
# 20200721
# dan.carver@carverd.com
###
library(raster)
# list all files 
rasters <- list.files(path = "F:/nrelD/sorghumSDM/parameters/bioLayer_2.5/raster", full.names = TRUE)

rasters
rList <- c()
for(i in 1:length(rasters)){
  r1 <- raster::raster(rasters[i])  
  rList  <- append(x = rList, r1)
    
}
rList
rStack <- raster::stack(rList)

