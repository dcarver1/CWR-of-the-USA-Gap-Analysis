###
# generate the native area mask based on intersection between points, ecoregions, and countries
# 20190830
# carver.dan1@gmail.com
###

#species <- species1

nat_area_shp <- function(species) {
  if (file.exists(paste0(sp_dir, "/modeling/nativeArea/narea.shp"))){
    nativeArea<<-readOGR(paste0(sp_dir, "/modeling/nativeArea/narea.shp"),verbose = FALSE)}
  else{
  # define CRS to be equal between points and ecoRegions
  crs(cleanPoints) <- crs(ecoReg)
  
  # test to see which ecoregions have points within them 
  ecoVal <- data.frame(over(x = cleanPoints, y = ecoReg))%>%
    dplyr::select(ECO_ID_U )%>%
    distinct()%>%
    drop_na()
  if(length(ecoVal$ECO_ID_U) == 0 ){
    print(paste0("No ecoregions intersected with the occurence data. Species can not be modeled."))
  return("No Native Area")
    }else{
    # subset ecoRegions that have points within them 
    ecoAreas <- subset(ecoReg, ECO_ID_U %in% ecoVal$ECO_ID_U) ### DC added dissolve 
    # clip ecoregions to countries with points present
    clipArea <-rgeos::gIntersection(ecoAreas, naSHP)
    nativeArea <<- SpatialPolygonsDataFrame(clipArea, data.frame(ID=1:length(clipArea)))
    
    #write out feature
    setwd(paste0(sp_dir, "/modeling/nativeArea"))
    writeOGR(obj=nativeArea, dsn="narea.shp", layer="narea", driver="ESRI Shapefile") # this is in geographical projection
    return(nativeArea)
  }
  }
}

