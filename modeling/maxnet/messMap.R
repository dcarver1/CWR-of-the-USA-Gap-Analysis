###
# MESS Map : filtered model extent by range of top predictor
# 20200414
# dan.carver@carverd.com
###

messMap <- function(species) {
  # Load in top predictors 
  topPre <- read.csv(file = paste0(sp_dir,"/modeling/maxent/predictorImportance.csv" ))

  # load in bioValuesFor Predictors
  bioValue2 <- read.csv(file = paste0(sp_dir,"/modeling/maxent/bioValuesForPresencePoints.csv" ))%>%
    dplyr::filter(presence == 1)

  # use top predictor to select raster layer and clip that to native area
  top1 <- as.character(topPre$varNames[1])

  # determine range +/- 1% of top predictor
  vals1 <- dplyr::select(bioValue2, top1)
  r1 <- range(vals1)
  #diff <- (r1[2] - r1[1])*0.01 # 20191031, dropping the buffer section for now.
  h1 <- r1[2] #+ diff
  b1 <- r1[1] #- diff

  # load in raster for top predictor
  topRast <- rastersToModel%>%
    raster::subset(top1)


  # write a clause statement that test identifies all areas within that range of native range masked
  topRast[topRast > h1] <- 0
  topRast[topRast < b1] <- 0
  topRast[topRast > 0] <- 1
  # use the newly created raster to mask out the the threshold model. Output will be presented in the htmls
  thrshold <<- raster::raster(paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))
  thrshold[thrshold == 0] <- NA
  messMap <- thrshold * topRast
  joinMEss <- thrshold + messMap
  raster::writeRaster(x = messMap, filename = paste0(sp_dir, "/modeling/messMapThres.tif"),overwrite=TRUE)
}
