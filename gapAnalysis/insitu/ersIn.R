###
# calculate the ers insitu = ecoregions in protected areas / ecoregions in SDM *100
# 20190919
# carver.dan1@gmail.com
###
ers_exsitu <- function(species) {
  #run only for spp with occ file
  if (sp_counts$totalGUseful == 0) {
    ers <- 0
    gbuf_nclass <- 0
    pa_nclass <- NA
  }else{

    # load in protected area maps and convert to points 
    pPoints <- sp::SpatialPoints(raster::rasterToPoints(protectSDM))
    # extract values from ecoregions to points 
    crs(pPoints) <- crs(ecoReg)
    ecoValsP <- sp::over(x = pPoints, y = ecoReg) %>% 
      distinct(ECO_ID )%>%
      filter(ECO_ID > 0)
    
    ecoValsProLen <- length(ecoValsP[!is.na(ecoValsP$ECO_ID),])
    
 
    #calculate ERS
    ers <- min(c(100, (ecoValsProLen/ecoValsPLen)*100))
  }
  #create data.frame with output
    df <- data.frame(ID=species, SPP_N_ECO = ecoValsPLen, SPP_WITHIN_PA_N_ECO = ecoValsProLen, ERS = ers)
    write.csv(df,paste(sp_dir,"/gap_analysis/insitu/ers_result.csv",sep=""),row.names=F)
  
  #return object
  return(df)
}