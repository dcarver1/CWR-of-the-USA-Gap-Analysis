###
# calculates the total ecoregions within modeled area where G occurrences have
# been collected
# dan.carver@carverd.com
# 20200414
###

ers_exsitu <- function(species) {
  
  
  # number of ecoRegions present in all points
  pa_spp <- raster(paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))
  pa_spp[pa_spp==0] <- NA
  
  thrsPoints <- sp::SpatialPoints(raster::rasterToPoints(pa_spp))
  crs(thrsPoints) <- crs(ecoReg)
  ecoVal <- data.frame(over(x = thrsPoints, y = ecoReg))%>%
    dplyr::select(ECO_ID_U )%>%
    distinct() %>%
    drop_na()
  ecoValsAllPointsLen <<- nrow(ecoVal)
  
    # Clause for species with no g points, as no buffer object has been created.
    if (!file.exists(paste0(sp_dir,"/modeling/alternatives/ga50.tif"))) {
      ers <- 0
      ecoValsGLen <- NA
      ecoValsAllPointsLen <- nrow(ecoVal)
      out_df <- data.frame(ID=species, SPP_N_ECO=0, G_N_ECO=0, ERS=ers)
      write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/ers_result.csv",sep=""),row.names=F)
    }else{
      #load g buffer
      gBuffer <- raster::raster(paste0(sp_dir,"/modeling/alternatives/ga50.tif"))
      # load in threshold model
      pa_spp <<- raster(paste0(sp_dir,"/modeling/spdist_thrsld_median.tif"))
      # this area method accounts for 0 and 1, need to replace 0 with NA values before determining the area
      pa_spp[pa_spp==0] <- NA
      # mask to native area
      gBuffer <- gBuffer * pa_spp

      #clause to test if any buffered area is within predicted area
      if(1 %in% unique(values(gBuffer))){
        # not needed because the gBuffer object only has values of 1
        gPoints <- sp::SpatialPoints(raster::rasterToPoints(gBuffer))
        # extract values from ecoregions to points
        crs(gPoints) <- crs(ecoReg)
        ecoValsG <- sp::over(x = gPoints, y = ecoReg) %>%
          distinct(ECO_ID_U )

        ecoValsGLen <- length(ecoValsG[!is.na(ecoValsG$ECO_ID_U),])


        #calculate ERS
        ers <- min(c(100, (ecoValsGLen/ecoValsAllPointsLen)*100))
        #create data.frame with output
        out_df <- data.frame(ID=species, SPP_N_ECO=ecoValsAllPointsLen, G_N_ECO=ecoValsGLen, ERS=ers)
        write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/ers_result.csv",sep=""),row.names=F)

      }else{
        # clause for when no buffered area exists within distribution
        ers <- 0
        out_df <- data.frame(ID=species, SPP_N_ECO=0, G_N_ECO=0, ERS=ers)
        write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/ers_result.csv",sep=""),row.names=F)
      }
    }
}
