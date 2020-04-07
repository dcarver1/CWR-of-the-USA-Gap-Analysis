###
# subset all point locations that do not fall within a country 
# 20190827
# carver.dan1@gmail.com
###

countryCheck <- function(species){
  ###
  # need to join between spatial points and spatial polygon dataframe to get country information
  #https://www.rdocumentation.org/packages/sp/versions/1.3-1/topics/over-methods
  
  # create spatial point object and set CRS 
  # set crs to the same 
  xyData <<- sp::SpatialPoints(spPoint@coords)
  crs(xyData) <- crs(countrySHP)

  # test to see which feautres fall within a country 
  countryVal <- data.frame(over(x = xyData, y = countrySHP)) %>%
    dplyr::select(ISO_A3)
  
  
  
  # add data back to spPoints and drop all columns that have no ISO3
  spPoint@data$iso3_check <- countryVal$ISO_A3
  onLand<- complete.cases(spPoint@data$iso3_check) #https://stackoverflow.com/questions/21567028/extracting-points-with-polygon-in-r
  spPoint <- spPoint[onLand,]

  # pull in states by taxon data and filter it to genus, species 
  sData <- statesData %>%
    dplyr::filter(name == "two")%>%
    dplyr::distinct(State) %>%
    dplyr::filter(State != "")
  if(nrow(sData) == 0){
    cleanPoints <<- spPoint
  }else{
      #seperate out points that fall inside Mex, Can, and Usa 
      spPoint$StateTest <- spPoint@data$iso3_check %in% c("MEX", "USA", "CAN")
      
      mucPoints <- spPoint[spPoint$StateTest == TRUE,]
      nonMucPoints <- spPoint[spPoint$StateTest == FALSE,]
      
      # filter the sp admin 
      if(nrow(mucPoints) > 0){
        sSp <- statesSpObject[statesSpObject$NAME_1 %in% sData$State,]
        
        #overlay points onto filter states data and remove any points that do not fall within know states
        statePoints <- as.data.frame(over(x = mucPoints, y = sSp))%>%
          dplyr::select(NAME_1)
        
        
        t1 <-nrow(statePoints)
        # add data back to spPoints and drop all columns that have no ISO3
        mucPoints <- mucPoints[!is.na(statePoints$NAME_1),]
        t2 <- nrow(mucPoints)
      }
      if(t2 != 0){
        spPoint <- rbind(mucPoints,nonMucPoints )  
        
        # filter Duplicates 
        uniqueP <- distinct(spPoint@data)
        coords <- cbind(uniqueP$longitude, uniqueP$latitude)
        
        cleanPoints <<- sp::SpatialPointsDataFrame(coords = coords, data = uniqueP, proj4string = crs(spPoint))
        
        # Might want to include a step that limits the points to us, canada, and mexico but I need
        # to check in with Colin about that First 
      }else{
        cleanPoints <<- "there are no points present withn the states of interest"
      }
  }
}

