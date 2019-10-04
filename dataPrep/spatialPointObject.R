###
# call in the raw data and generate a spatial points dataframe
# 20199827
# carver.dan1@gmail.com
### 

#species <- species1

spPoints <- function(species){

  
  latLong <- dataThin %>%
    filter(hasLatLong == TRUE)
  latLong$latitude <- as.numeric(as.character(latLong$latitude))
  latLong$longitude <- as.numeric(as.character(latLong$longitude))
  latLong <- latLong[complete.cases(latLong[ , 6:7]),]
  
  if(nrow(latLong)>0){
    #https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
    coord <- dplyr::select(latLong, longitude,latitude)
    
    spPoint <- SpatialPointsDataFrame(coord,
                                 data = latLong)
                                 #proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    
    # mask to North America 
    crs(spPoint) <- crs(naSHP)
    intersect1 <- intersect(spPoint, naSHP)
    intersect1@data <- intersect1@data %>% dplyr::select(names(latLong))
    spPoint <<- intersect1
    write.csv(spPoint,file = paste0(sp_dir,"/occurrences/rawDataForNA.csv"),row.names = FALSE)
    
    }else{
    print("there are no coodinate pairs for this species")
    spPoint <<- "no data available"
  }
  
}
