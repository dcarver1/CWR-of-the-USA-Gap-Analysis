###
# subset all point locations that do not fall within a country 
# 20190827
# carver.dan1@gmail.com
###

testLand <- function(species){
  ###
  # need to join between spatial points and spatial polygon dataframe to get country information
  #https://www.rdocumentation.org/packages/sp/versions/1.3-1/topics/over-methods
  
  # create spatial point object and set CRS 
  # set crs to the same 
  xyData <<- sp::SpatialPoints(spPoint@coords)
  crs(xyData) <- crs(countrySHP)

   # qtm(spPoint)
   # qtm(countrySHP)
   # tm_shape(countrySHP) + tm_polygons()+
   #   tm_shape(spPoint) + tm_dots()

  # test to see which feautres fall within a country 
  countryVal <- data.frame(over(x = xyData, y = countrySHP)) %>%
    dplyr::select(ISO_A3)
  
  # add data back to spPoints and drop all columns that have no ISO3
  spPoint@data$iso3_check <- countryVal$ISO_A3
  onLand<- complete.cases(spPoint@data$iso3_check) #https://stackoverflow.com/questions/21567028/extracting-points-with-polygon-in-r
  cleanPoints <<- spPoint[onLand,]

  # Might want to include a step that limits the points to us, canada, and mexico but I need
  # to check in with Colin about that First 

}

