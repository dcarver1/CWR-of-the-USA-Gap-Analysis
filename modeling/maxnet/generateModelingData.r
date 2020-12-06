###
# generate background points based on the size of the native area
# 20190904
# dan.carver@carverd.com
###

generateModelingData <- function(species){
  #function for checking area
  numberBackground <- function(area){
    n <- rgeos::gArea(area)*58
    if( n >= 5000){
      n <- 5000}else{
        n <- n
      }
    return(n)
  }
  n <- numberBackground(nativeArea)

  # produce background points based on native area area
  bck_data <- spsample(nativeArea, n = n, type = "random" )
  crs(bck_data) <- crs(nativeArea)
  print(1)
  # test so that background points do not overlap with presence points
  # create the point buffer
  # 1. buffer values from known presece locations by 0.000556
  presBuff <- rgeos::gBuffer(sp::SpatialPoints(cleanPoints@coords), width=0.000556) #width=0.000556
  crs(presBuff) <- crs(nativeArea)
  # convert to spatial dataframe
  print(2)
  # 2. run an intersect between buffer and background point
  intersect <- data.frame(over(bck_data, presBuff))

  if(length(unique(intersect$over.bck_data..presBuff.))>1){
    nbd <- as.data.frame(bck_data@coords)
    nbd$intesect <- intersect
    nbd <- nbd %>%
      filter(is.na(intersect))
    bck_data <- sp::SpatialPoints(coords = c(nbd[,1:2]))
    crs(bck_data) <- crs(nativeArea)
  }
  print(3)
  # 3. extract all values to background points
  rasterStack <- bioVars %>%
    raster::crop(nativeArea)
  bck_vals <- raster::extract(x = rasterStack,y = bck_data)
  bck_data_bio <-as.data.frame(cbind(bck_data@coords, bck_vals))%>%
    mutate(presence = 0)
  bck_data_bio$longitude <- bck_data_bio$x
  bck_data_bio$latitude <- bck_data_bio$y
  bck_data_bio <- bck_data_bio %>% dplyr::select(-c("x","y"))
  print(4)
  # extract values to presence points
  prs_vals <- raster::extract(x = rasterStack,y = sp::SpatialPoints(cleanPoints@coords))
  print(4.1)
  prs_data_bio <- as.data.frame(cbind(prs_vals,cleanPoints@data[,2:3])) %>%
    mutate(presence = 1)
  print(5)
  # merge presence and background sets
  bioValues <<- dplyr::bind_rows(bck_data_bio,prs_data_bio)

  # write out csv of background/presence data
  write.csv(x = bioValues, file = paste0(sp_dir, "/occurrences/presBackgroundWithBiovars.csv"))
}
