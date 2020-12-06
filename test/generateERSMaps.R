


ersMaps <- function(sp_dir){
  

# function for filtering list based on character values
include <- function (theList, toMatch){
  matches <- unique (grep(paste(toMatch,collapse="|"),
                          theList, value=TRUE))
  return(matches)
}



#function for generating counts data based on modeled native occurrence area 
counts <- function(dat){
  dataThin <<- dat %>%
    dplyr::select(c("taxon", "latitude", "longitude", "type")) %>%
    mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "") %>%
    mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "") %>%
    mutate(hasLatLong = hasLat & hasLong)
  
  
  colNames <- c("species","totalRecords",	"hasLat", "hasLong","totalUseful", 	"totalGRecords",
                "totalGUseful","totalHRecords",	"totalHUseful" )
  noNas <- dataThin[complete.cases(dataThin),]
  tbl <- noNas %>%
    dplyr::group_by(type, hasLatLong ) %>%
    dplyr::summarize(total = n())
  
  df <- data.frame(matrix(NA, nrow = 1, ncol = 10))
  colnames(df) <- colNames
  df$species <- unique(dataThin$taxon)
  df$totalRecords <- nrow(dataThin)
  df$totalUseful <- sum((subset(tbl, hasLatLong == TRUE))$total)
  df$totalGRecords <- sum((subset(tbl, type == "G"))$total)
  df$totalGUseful <- sum((subset(tbl, type == "G" & hasLatLong == TRUE))$total)
  df$totalHRecords <- sum((subset(tbl, type == "H"))$total)
  df$totalHUseful <- sum((subset(tbl, type == "H" & hasLatLong == TRUE))$total)
  df$hasLat <- sum(dataThin$hasLat)
  df$hasLong <- sum(dataThin$hasLong)
  return(df)
  
}

# Palette options 
hPColor <- "#1184D4"
gPColor <- "#6300F0"
# background ecoregions = "#FFFFFF"
# predicted presence = "#45B320"
binaryPal <- c( "#FFFFFF","#45B320") # white, green 
gBufPal <- c(  "#FFFFFF","#45B320","#7570b3") # white , green, purple 
gBinary <- c("#FFFFFF","#7570b3") # White, Purple 
ecoPal <- "RdYlBu" # https://colorbrewer2.org/#type=diverging&scheme=RdBu&n=3
proPal <- "#7570b3" # same color as val 2 in gBufPal 
srsinPal <- c("#FFFFFF", "#FA4101") # white to red 



# taxa is the

baseDir <- paste0(sp_dir)

csv <- list.files(baseDir, pattern = ".csv", recursive = TRUE , full.names = TRUE)




if(length(include(csv,"eval_metrics_rep.csv")) == 0){
  print("Models were not produced for this taxon due to insufficent occurrences")
}else{
  tif <- list.files(path = baseDir, pattern = '.tif', recursive = TRUE, full.names = TRUE)
  thrshold <- raster(include(tif, "spdist_thrsld_median"))
}



if(length(include(csv,"eval_metrics.csv")) == 0){
  print("Models were not produced for this taxon due to insufficent occurrences")
}else{
  # extract values from ecoregion to predicted presences points
  thrs1 <- thrshold
  thrs1[thrs1 == 0] <- NA
  if(!1 %in% values(thrs1)){
    print("There is no predicted area of presence for this species.")
  }else{
    
    predictedPresence <- sp::SpatialPoints(raster::rasterToPoints(thrs1))
    raster::crs(predictedPresence) <- raster::crs(ecoReg)
    ecoVals <- sp::over(x = predictedPresence, y = ecoReg)
    ecoVals <- data.frame(ECO_ID_U=(unique(ecoVals$ECO_ID_U)))
    ecoVals <- ecoVals[which(!is.na(ecoVals) & ecoVals>0),]
    
    OccDataG <- cleanPoints[which(cleanPoints$type=="G"),c("longitude","latitude")]
    
    # convert SDM from binary to 1-NA for mask and area
    SdmMask <- thrshold
    SdmMask[which(SdmMask[] == 0)] <- NA
    if(nrow(OccDataG)>0){
      OccDataG <- OccDataG[which(!is.na(OccDataG$latitude)),]
      sp::proj4string(OccDataG) <- sp::CRS("+proj=longlat +datum=WGS84")
      
      # buffer G points
      buffer <- GapAnalysis::Gbuffer(xy = OccDataG,
                                     dist_m = 50000,
                                     output = 'sf')
      # rasterizing and making it into a mask
      buffer_rs <- fasterize::fasterize(buffer, thrshold)
      buffer_rs[!is.na(buffer_rs[])] <- 1
      buffer_rs <- buffer_rs * SdmMask
      # test for overlap between bugger and predicted areas 
      if(1 %in% values(buffer_rs)){
        gPoints <- sp::SpatialPoints(raster::rasterToPoints(buffer_rs))
        # extract values from ecoregions to points
        raster::crs(gPoints) <- raster::crs(ecoReg)
        ecoValsG <- sp::over(x = gPoints, y = ecoReg)
        ecoValsG <- data.frame(ECO_ID_U=(unique(ecoValsG$ECO_ID_U)))
        ecoValsG <- ecoValsG[which(!is.na(ecoValsG) & ecoValsG>0),]
        ecoGap <- ecoVals[!ecoVals %in% ecoValsG]
        
        if(length(ecoGap)!=0){
          ### Standard Gap map  
          # pull selected ecoregions and mask to presence area of the model
          eco2 <- ecoReg[ecoReg$ECO_ID_U %in% ecoGap,]
          #convert to sf object for conversion using fasterize
          eco2a <- sf::st_as_sf(eco2, SdmMask)
          # generate a ecoregion raster keeping the unique id.
          eco3 <- fasterize::fasterize(eco2a, SdmMask, field = "ECO_ID_U")
          # mask so only locations within the predicted presence area is included.
          gap_map <- eco3 * SdmMask
          if(length(ecoGap) > 30){
            tm_shape(thrshold, name = "Native area and distribution model")+
              tm_raster(palette = binaryPal, style = "cat",title = paste0("ERSex of ",species))+
              tm_shape(gap_map, name = "Ecoregions not conserved ex situ") +
              tm_raster(palette = ecoPal, style ="pretty", title = "Ecoregions not conserved ex situ") +
              tm_shape(buffer_rs, name = "G buffer area")+
              tm_raster(palette = proPal, title = "G Buffer", labels = "")
          }else{
            tm_shape(thrshold, name = "Native area and distribution model")+
              tm_raster(palette = binaryPal, style = "cat",title = paste0("ERSex of ",species))+
              tm_shape(gap_map, name = "Ecoregions not conserved ex situ") +
              tm_raster(palette = ecoPal, style ="cat", title = "Ecoregions not conserved ex situ") +
              tm_shape(buffer_rs, name = "G buffer area")+
              tm_raster(palette = proPal, title = "G Buffer", labels = "")
          }
          
        }else{
          ### All regions conserved 
          # pull selected ecoregions and mask to presence area of the model
          eco2 <- ecoReg[ecoReg$ECO_ID_U %in% ecoVals,]
          #convert to sf object for conversion using fasterize
          eco2a <- sf::st_as_sf(eco2, SdmMask)
          # generate a ecoregion raster keeping the unique id.
          eco3 <- fasterize::fasterize(eco2a, SdmMask, field = "ECO_ID_U")
          # mask so only locations within the predicted presence area is included.
          gap_map <- eco3 * SdmMask
          
          tm_shape(thrshold, name = "Native area and distribution model")+
            tm_raster(palette = binaryPal, style = "cat",title = paste0("ERSex of ",species))+
            tm_shape(buffer_rs, name = "G buffer area")+
            tm_raster(palette = proPal, title = "G Buffer", labels = "")
        }
        
      }else{
        ### condition for when g occurrence is present but does not overlap with predicted presence 
        ecoValsG <- NA
        ecoGap <- ecoVals
        # pull selected ecoregions and mask to presence area of the model
        eco2 <- ecoReg[ecoReg$ECO_ID_U %in% ecoGap,]
        #convert to sf object for conversion using fasterize
        eco2a <- sf::st_as_sf(eco2, thrshold)
        eco3 <- fasterize::fasterize(eco2a, thrshold, field = "ECO_ID_U")
        gap_map <- eco3 * SdmMask
        
        # mask so only locations within the predicted presence area is included.
        if(length(unique(raster::values(gap_map)))< 30){
          tm_shape(thrshold, name = "Native area and distribution model") + 
            tm_raster(palette = binaryPal, style = "cat", title = paste0("ERSex for ",species))+
            tm_shape(gap_map)+
            tm_raster(palette = ecoPal, style = "cat", title = "Ecoregions not conserved ex situ")
        }else{
          tm_shape(thrshold, name = "Native area and distribution model") + 
            tm_raster(palette = binaryPal, style = "cat", title = paste0("ERSex for ",species))+
            tm_shape(gap_map)+
            tm_raster(style = "pretty", title = "30 plus Ecoregions not conserved ex situ")
        }
      }
    }else{
      ecoGap <- ecoVals
      # pull selected ecoregions and mask to presence area of the model
      eco2 <- ecoReg[ecoReg$ECO_ID_U %in% ecoGap,]
      #convert to sf object for conversion using fasterize
      eco2a <- sf::st_as_sf(eco2, thrshold)
      eco3 <- fasterize::fasterize(eco2a, thrshold, field = "ECO_ID_U")
      gap_map <- eco3 * SdmMask
      
      # mask so only locations within the predicted presence area is included.
      if(length(unique(raster::values(gap_map)))< 30){
        tm_shape(thrshold, name = "Native area and distribution model") + 
          tm_raster(palette = binaryPal, style = "cat", title = paste0("ERSex for ",species))+
          tm_shape(gap_map)+
          tm_raster(palette = ecoPal, style = "cat", title = "Ecoregions not conserved ex situ")
      }else{
        tm_shape(thrshold, name = "Native area and distribution model") + 
          tm_raster(palette = binaryPal, style = "cat", title = paste0("ERSex for ",species))+
          tm_shape(gap_map)+
          tm_raster(style = "pretty", title = "30 plus Ecoregions not conserved ex situ")
      }
    }
    
    try(raster::writeRaster(x = gap_map,
                            filename = paste0(sp_dir, "/gap_analysis/exsitu/",species,"_ersEx.tif"),overwrite = TRUE)) 
  }
}        


if(length(include(csv,"eval_metrics.csv")) == 0){
  print("Models were not produced for this taxon due to insufficent occurrences")
}else{
  if(!1 %in% values(thrshold)){
    print("There is no predicted area of presence for this species.")
  }else{
    #convert protect area to points
    t1<- thrshold
    t1[t1 ==0,] <-NA
    proArea2 <- proArea *t1
    if(FALSE %in% unique(is.na(values(proArea2)))){
      
      protectPoints <- sp::SpatialPoints(raster::rasterToPoints(proArea2))
      # extract the ecoReg values to the points
      raster::crs(protectPoints) <- raster::crs(ecoReg)
      ecoValsPro <- sp::over(x = protectPoints, y = ecoReg)
      ecoValsPro <- data.frame(ECO_ID_U=(unique(ecoValsPro$ECO_ID_U)))
      ecoValsPro <- ecoValsPro[which(!is.na(ecoValsPro) & ecoValsPro>0),]
      ecoInProt <- length(ecoValsPro)
      
      # extract ecoregions values present in predicted presence area
      predictedPresence <- sp::SpatialPoints(raster::rasterToPoints(t1))
      raster::crs(predictedPresence) <- raster::crs(ecoReg)
      ecoVal <- sp::over(x = predictedPresence, y = ecoReg)
      ecoVal <- data.frame(ECO_ID_U=(unique(ecoVal$ECO_ID_U)))
      ecoVal <- ecoVal[which(!is.na(ecoVal) & ecoVal>0),]
      
      ecoGap <- ecoVal[!ecoVal %in% ecoValsPro]
      
      # number of ecoRegions in modeling area
      SdmMask <-  thrshold
      SdmMask[which(SdmMask[]==0)] <- NA
      
      if(length(ecoGap) >0){
        ###Show ecoregions
        # pull selected ecoregions and mask to presence area of the model
        eco2 <- ecoReg[ecoReg$ECO_ID_U %in% ecoGap,]
        #convert to sf object for conversion using fasterize
        eco2a <- sf::st_as_sf(eco2, SdmMask)
        # generate a ecoregion raster keeping the unique id.
        eco3 <- fasterize::fasterize(eco2a, SdmMask, field = "ECO_ID_U")
        # mask so only locations within the predicted presence area is included.
        #eco3[is.na(eco3),] <- 0
        gap_map <- eco3 * SdmMask
        
        map<-tm_shape(thrshold, name = "Native area and distribution model") +
          tm_raster(palette = binaryPal, style = "cat",title = paste0("ERSin for ",species))+
          tm_shape(gap_map, name = "Ecoregions not conserved in protected areas")+
          tm_raster(palette = ecoPal, style = "cat", title = "Ecoregions not conserved in protected areas")+
          tm_shape(proArea2, name = "Protected Areas")+
          tm_raster(title = "Protected areas" , palette = proPal, labels = "")
        
      }else{
        
        map <- tm_shape(thrshold, name = "Native area and distribution model")+
          tm_raster(palette = binaryPal, style = "cat", title = paste0("ERSin of ", species))+
          tm_shape(proArea2, name = "Protected areas")+
          tm_raster(title = "Protected areas" , palette = proPal, labels = "")
      }
    }else{
      ### for species with no protected areas within the predicted presense range 
      # extract ecoregions values present in predicted presence area
      predictedPresence <- sp::SpatialPoints(raster::rasterToPoints(t1))
      raster::crs(predictedPresence) <- raster::crs(ecoReg)
      ecoVal <- sp::over(x = predictedPresence, y = ecoReg)
      ecoVal <- data.frame(ECO_ID_U=(unique(ecoVal$ECO_ID_U)))
      ecoVal <- ecoVal[which(!is.na(ecoVal) & ecoVal>0),]
      ecoGap <- ecoVal
      
      # number of ecoRegions in modeling area
      SdmMask <-  thrshold
      SdmMask[which(SdmMask[]==0)] <- NA
      
      ###Show ecoregions 
      # pull selected ecoregions and mask to presence area of the model
      eco2 <- ecoReg[ecoReg$ECO_ID_U %in% ecoVal,]
      #convert to sf object for conversion using fasterize
      eco2a <- sf::st_as_sf(eco2, SdmMask)
      # generate a ecoregion raster keeping the unique id.
      eco3 <- fasterize::fasterize(eco2a, SdmMask, field = "ECO_ID_U")
      # mask so only locations within the predicted presence area is included.
      #eco3[is.na(eco3),] <- 0
      gap_map <- eco3 * SdmMask
      map <- tm_shape(thrshold, name = "Native area and distribution model")+
        tm_raster(palette = binaryPal, style = "cat", title = paste0("ERSin of ", species))+
        tm_shape(gap_map, name = "Ecoregions not conserved in protected areas")+ 
        tm_raster(palette = ecoPal, style = "cat", title = "Ecoregions not conserved in protected areas")
    }
    
    # clause for H. Winteri and possible others. Basically proarea exists but is generate out in tmap
    issues <- c("Helianthus winteri", "Elymus macgregorii", "Juglans nigra")
    if(species %in% issues){
      print("There is a visualization error that has prevented this map from displaying in an interactive format.")
    }else{
      map
    }
  }
  try(raster::writeRaster(x = gap_map,
                          filename = paste0(sp_dir, "/gap_analysis/insitu/",species,"_ersIn.tif"), overwrite = TRUE))
}
}
