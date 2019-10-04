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
  # Get unique list of countries 
  countries <- as.character(unique(cleanPoints@data$iso3_check))
  #data <- countries[complete.cases(countries),]
  # select those countries 
  countryAreas <- subset(countrySHP, ISO_A3 %in% countries) ### DC added dissolve 
  
  # define CRS to be equal between points and ecoRegions
  crs(xyData) <- crs(ecoReg)
  
  # test to see which ecoregions have points within them 
  ecoVal <- data.frame(over(x = xyData, y = ecoReg))%>%
    dplyr::select(ECO_ID )%>%
    distinct() %>%
    drop_na() %>%
    filter(ECO_ID != -9998) # -9998 are lakes
  
  # subset ecoRegions that have points within them 
  ecoAreas <- subset(ecoReg, ECO_ID %in% ecoVal$ECO_ID) ### DC added dissolve 
  # clip ecoregions to countries with points present
  clipArea <-rgeos::gIntersection(ecoAreas, countryAreas)
  nativeArea <<- SpatialPolygonsDataFrame(clipArea, data.frame(ID=1:length(clipArea)))
  
  #write out feature
  setwd(paste0(sp_dir, "/modeling/nativeArea"))
  writeOGR(obj=nativeArea, dsn="narea.shp", layer="narea", driver="ESRI Shapefile") # this is in geographical projection
  }
}