###
# subset all point locations that do not fall within a country of interest
# and generate the first version of clean data which is used in the modeling
# process
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

  # overlay with country shp
  countryVal <- data.frame(over(x = xyData, y = countrySHP)) %>%
    dplyr::select(ISO_A3)

  # add data back to spPoints and drop all columns that have no ISO3
  spPoint@data$iso3_check <- countryVal$ISO_A3
  onLand<- complete.cases(spPoint@data$iso3_check) #https://stackoverflow.com/questions/21567028/extracting-points-with-polygon-in-r
  spPoint <- spPoint[onLand,]

  # pull in states by taxon data and filter it to genus, species
  sData <- statesData %>%
    dplyr::filter(name == species)%>%
    dplyr::distinct(State) %>%
    dplyr::filter(State != "")
  # clause for species with no state specific data in GRIN
  if(nrow(sData) == 0){
    cleanPoints <<- spPoint
  }else{
      #seperate out points that fall inside Mex, Can, and Usa
      spPoint$StateTest <- spPoint@data$iso3_check %in% c("MEX", "USA", "CAN")
      # we only test states for mex,usa,can. So two groups are generated here.
      mucPoints <- spPoint[spPoint$StateTest == TRUE,]
      nonMucPoints <- spPoint[spPoint$StateTest == FALSE,]

      # filter the sp admin
      if(nrow(mucPoints) > 0){
        
        # States SP Object is a large spatial polygon feature loaded in run lineal
        sSp <- statesSpObject[statesSpObject$NAME_1 %in% sData$State,]
        if(nrow(sSp)==0){
          # if no occurrence data is found within the selected states this
          # this process defaults back to include all data.
          cleanPoints <<- spPoint
        }else{
          #overlay points onto filter states data and remove any points that do not fall within know states
          crs(mucPoints) <- crs(ecoReg)
          crs(sSp) <- crs(ecoReg)
          statePoints <- as.data.frame(over(x = mucPoints, y = sSp))%>%
            dplyr::select(NAME_1)
          
          # add data back to spPoints and drop all columns that have no ISO3
          mucPoints <- mucPoints[!is.na(statePoints$NAME_1),]
          t2 <- nrow(mucPoints)
          #clause for when states were found to contain occurrences
          if(t2 != 0){
            spPoint <- rbind(mucPoints,nonMucPoints )
            # filter Duplicates
            uniqueP <- distinct(spPoint@data)
            coords <- cbind(uniqueP$longitude, uniqueP$latitude)
            # base data used in the modeling process.
            cleanPoints <<- sp::SpatialPointsDataFrame(coords = coords, data = uniqueP, proj4string = crs(spPoint))
        }else{
          # if no occurrence data is found within the selected states this
          # this process defaults back to include all data.
          cleanPoints <<- spPoint
        }
      }
        }else{
        # if no occurrence are found in can,usa,mex the all points are kept 
      spPoint <- nonMucPoints
      cleanPoints <<- spPoint
    }
  }
}
