###
# calculate the ers insitu = ecoregions in protected areas / ecoregions in SDM *100
# 20200414
# dan.carver@carverd.com
###

ers_insitu <- function(species) {
    # import threshold and use it to mask protected area so only the pro in the predicted area are
    pa_spp <<- raster(paste0(sp_dir,"/modeling/spdist_thrsld_median.tif"))
    # this area method accounts for 0 and 1, need to replace 0 with NA values before determining the area
    pa_spp[pa_spp==0] <- NA
    thrshold <- raster(x = paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))
    thrshold[which(thrshold[] == 0)] <- NA
    # clause for no predicted present in thrshold raster
    if(!1 %in% (unique(values(thrshold)))){
      ecoInSDM <-0
      ecoInProt <-
      ers <- 0
      }else{
      crs(pa_spp) <- crs(proArea)
      proNative <- raster::crop(x = proArea, y = pa_spp)
      proNative <- proNative * pa_spp

      if(length(unique(values(proNative)))==1){
        ers <- 0
        ecoInProt <- 0
        # number of ecoregion in the SDM
        thrsPoints <- sp::SpatialPoints(raster::rasterToPoints(pa_spp))
        crs(thrsPoints) <- crs(ecoReg)
        ecoVal <- data.frame(over(x = thrsPoints, y = ecoReg))%>%
          dplyr::select(ECO_ID_U )%>%
          distinct() %>%
          drop_na()

        ecoInSDM <- nrow(ecoVal)
          }else{
            # load in protected area maps and convert to points
            protectPoints <- sp::SpatialPoints(raster::rasterToPoints(proNative))
            # extract values from ecoregions to points
            crs(protectPoints) <- crs(ecoReg)
            ecoValsProt <- sp::over(x = protectPoints, y = ecoReg) %>%
              dplyr::select(ECO_ID_U )%>%
              distinct(ECO_ID_U ) %>%
              drop_na()
            #number of ecoRegions in protected areas
            ecoInProt <- nrow(ecoValsProt)

            # number of ecoregion in the SDM
            thrsPoints <- sp::SpatialPoints(raster::rasterToPoints(pa_spp))
            crs(thrsPoints) <- crs(ecoReg)
            ecoVal <- data.frame(over(x = thrsPoints, y = ecoReg))%>%
              dplyr::select(ECO_ID_U )%>%
              distinct() %>%
              drop_na()

            ecoInSDM <- nrow(ecoVal)

            #calculate ERS
            ers <- min(c(100, (ecoInProt/ecoInSDM)*100))
        }
      }
        #create data.frame with output
    df <- data.frame(ID=species, SPP_N_ECO = ecoInSDM, SPP_WITHIN_PA_N_ECO = ecoInProt, ERS = ers)
    #write.csv(df,paste0(sp_dir,"/gap_analysis/insitu/ers_result.csv"),row.names=F)
    write.csv(df,paste0(sp_dir,"/gap_analysis/insitu/ers_resultPAUD.csv"),row.names=F)
}
