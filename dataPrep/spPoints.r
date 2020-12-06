###
# call in the raw data and generate a spatial points dataframe
# dan.carver@carverd.com
# 20200414
###

spPoints <- function(species){
    # select all  rows with valid lat long
    latLong <- dataThin %>%
      dplyr::filter(hasLatLong == TRUE)
    latLong$latitude <- as.numeric(as.character(latLong$latitude))
    latLong$longitude <- as.numeric(as.character(latLong$longitude))
    latLong <- latLong[complete.cases(latLong[ , 6:7]),]

    if(nrow(latLong)>0){
      #https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
      coord <- latLong %>% dplyr::select(longitude,latitude)

      spPoint <<- sp::SpatialPointsDataFrame(coord,
                                        data = latLong)
      # clause for no lat long data
      if( nrow(spPoint@data) == 0){
        print("there are no coodinate pairs for this species")
        spPoint <<- "no data available"
      }
      # mask to North America
      crs(spPoint) <- crs(naSHP)
      intersect1 <- intersect(spPoint, naSHP)
      if(nrow(intersect1)==0){
        spPoint <<- "no data available"
      }else{
        spPoint <<- intersect1
        write.csv(spPoint@data,file = paste0(sp_dir,"/occurrences/rawDataForNA.csv"),row.names = FALSE)
      }
    }else{
      print("there are no coodinate pairs for this species")
      spPoint <<- "no data available"
    }
}
