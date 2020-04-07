##########################################   Start Functions    ###############################################
# This function calculates the ERSex It loads occurrences if they exist, then
# loads the presence/absence surface, creates the G buffer (i.e. CA50) and finally
# outputs the ERS and # eco classes in a data.frame (which is written into a file).
# @param (string) species: species ID
# @param (logical) debug: whether to save or not the intermediate raster outputs
# @return (data.frame): This function returns a data frame with ERS, # eco classes 
#                       of G buffer (i.e. CA50) and of the presence/absence surface.

#species <- species1
ers_exsitu <- function(species) {
    #load counts
    sp_counts <- read.csv(paste0(sp_dir,"/counts.csv"))
    
    # #area of model clipped to native area
    # 
    # # area of g points clipped to native area
    # 
    # # number of ecoregions present in model 
    # 
    crs(cleanPoints) <- crs(ecoReg)
    ecoVal <- data.frame(over(x = cleanPoints , y = ecoReg))%>%
      dplyr::select(ECO_ID_U )%>%
      distinct() %>%
      drop_na()

    
    #run only for spp with occ file
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

      if(1 %in% unique(values(gBuffer))){
        # not needed because the gBuffer object only has values of 1 
        #gBuffer[which(gBuffer[] == 0)] <- NA
        gPoints <- sp::SpatialPoints(raster::rasterToPoints(gBuffer))
        # extract values from ecoregions to points 
        crs(gPoints) <- crs(ecoReg)
        ecoValsG <- sp::over(x = gPoints, y = ecoReg) %>%
          distinct(ECO_ID_U ) 
        
        ecoValsGLen <- length(ecoValsG[!is.na(ecoValsG$ECO_ID_U),])
        
        
        # number of ecoRegions present in all points 
        ecoValsAllPointsLen <<- nrow(ecoVal)
        
        #calculate ERS
        ers <- min(c(100, (ecoValsGLen/ecoValsAllPointsLen)*100))
        #create data.frame with output
        out_df <- data.frame(ID=species, SPP_N_ECO=ecoValsAllPointsLen, G_N_ECO=ecoValsGLen, ERS=ers)
        write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/ers_result.csv",sep=""),row.names=F)
        
      }else{
        ers <- 0
        out_df <- data.frame(ID=species, SPP_N_ECO=0, G_N_ECO=0, ERS=ers)
        write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/ers_result.csv",sep=""),row.names=F)
      }        

    }
}

