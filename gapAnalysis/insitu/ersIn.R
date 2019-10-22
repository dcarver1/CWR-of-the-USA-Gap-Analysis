###
# calculate the ers insitu = ecoregions in protected areas / ecoregions in SDM *100
# 20190919
# carver.dan1@gmail.com
###
ers_insitu <- function(species) {
  if(!file.exists(paste0(sp_dir,"/gap_analysis/insitu/ers_result.csv"))){
    # mask protect area to native area 
    proNative <- raster::mask(x = proArea,mask = nativeArea)
    # load in protected area maps and convert to points 
    protectPoints <- sp::SpatialPoints(raster::rasterToPoints(proNative))
    # extract values from ecoregions to points 
    crs(protectPoints) <- crs(ecoReg)
    ecoValsProt <- sp::over(x = protectPoints, y = ecoAreas) %>%
      distinct(ECO_ID ) %>%
      filter(ECO_ID > 0)
    #number of ecoRegions in protected areas 
    ecoInProt <- nrow(ecoValsProt)
    
    # number of ecoregion in the SDM 
    ecoInSDM <- nrow(ecoVal)

    #calculate ERS
    ers <- min(c(100, (ecoInProt/ecoInSDM)*100))
    #create data.frame with output
    df <- data.frame(ID=species, SPP_N_ECO = ecoInSDM, SPP_WITHIN_PA_N_ECO = ecoInProt, ERS = ers)
    write.csv(df,paste0(sp_dir,"/gap_analysis/insitu/ers_result.csv"),row.names=F)
    #return object
    return(df)
  }else{
    print("file exist, moving on")
  }

}