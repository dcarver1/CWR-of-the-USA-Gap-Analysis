###
# generate the native area mask based on intersection between points, ecoregions, and countries
# dan.carver@carverd.com
# 20200414
###


nat_area_shp <- function(species) {
  # clause for seeing if the product already exist
  if (file.exists(paste0(sp_dir, "/modeling/nativeArea/narea.shp"))){
    nativeArea <<-readOGR(paste0(sp_dir, "/modeling/nativeArea/narea.shp"),verbose = FALSE)}
  else{
    # define CRS to be equal between points and ecoRegions
    crs(cleanPoints) <- crs(ecoReg)
    # test to see which ecoregions have points within them
    ecoVal <- data.frame(over(x = cleanPoints, y = ecoReg))%>%
      dplyr::select(ECO_ID_U )%>%
      distinct()%>%
      drop_na()
    #Probably don't need this cause, as all occurrence should be land points,
    # but it's an easy check
    if(length(ecoVal$ECO_ID_U) == 0 ){
      print(paste0("No ecoregions intersected with the occurence data. Species can not be modeled."))
      }else{
        # subset ecoRegions that have points within them
        ecoAreas <- subset(ecoReg, ECO_ID_U %in% ecoVal$ECO_ID_U)
        # clip ecoregions to countries with points present
        clipArea <-rgeos::gIntersection(ecoAreas, naSHP)
        nativeArea <<- SpatialPolygonsDataFrame(clipArea, data.frame(ID=1:length(clipArea)))

        # write out spatail feature
        # I was having issues with writeOGR and providing the full file path, This
        # should be cleaned up as setwd could cause issues down the line
        setwd(paste0(sp_dir, "/modeling/nativeArea"))
        writeOGR(obj=nativeArea, dsn="narea.shp", layer="narea", driver="ESRI Shapefile") # this is in geographical projection
    }
  }
}
