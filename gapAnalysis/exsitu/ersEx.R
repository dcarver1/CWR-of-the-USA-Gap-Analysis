##########################################   Start Functions    ###############################################
# This function calculates the ERSex It loads occurrences if they exist, then
# loads the presence/absence surface, creates the G buffer (i.e. CA50) and finally
# outputs the ERS and # eco classes in a data.frame (which is written into a file).
# @param (string) species: species ID
# @param (logical) debug: whether to save or not the intermediate raster outputs
# @return (data.frame): This function returns a data frame with ERS, # eco classes 
#                       of G buffer (i.e. CA50) and of the presence/absence surface.

#species <- species1
ers_insitu <- function(species) {
  
  #load counts
  sp_counts <- read.csv(paste0(sp_dir,"/counts.csv"))
  
  #area of model clipped to native area
  
  # area of g points clipped to native area
  
  # number of ecoregions present in model 
  
  
  #run only for spp with occ file
  if (sp_counts$totalGUseful == 0) {
    ers <- 0
    gbuf_nclass <- 0
    pa_nclass <- NA
  }else{
    
    #load g buffer 
    gBuffer <- raster::raster(paste0(sp_dir,"/modeling/alternatives/ga50.tif"))
    gBuffer[which(gBuffer[] == 0)] <- NA
    gPoints <- sp::SpatialPoints(raster::rasterToPoints(gBuffer))
    # extract values from ecoregions to points 
    crs(gPoints) <- crs(ecoReg)
    ecoValsG <- sp::over(x = gPoints, y = ecoReg) %>%
      distinct(ECO_ID ) %>%
      filter(ECO_ID > 0)
    
    ecoValsGLen <- length(ecoValsG[!is.na(ecoValsG$ECO_ID),])
    

    # extract values from ecoregions to points 
    crs(protectSDM) <- crs(ecoReg)
    ecoValsP <- sp::over(x = pPoints, y = ecoReg) %>% 
      distinct(ECO_ID )%>%
      filter(ECO_ID > 0)
    
    ecoValsPLen <<- length(ecoValsP[!is.na(ecoValsP$ECO_ID),])
    
    
    #calculate ERS
    ers <- min(c(100, (ecoValsGLen/ecoValsPLen)*100))
    }
    #create data.frame with output
    out_df <- data.frame(ID=species, SPP_N_ECO=ecoValsPLen, G_N_ECO=ecoValsGLen, ERS=ers)
    write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/ers_result.csv",sep=""),row.names=F)
    
    #return object
    return(out_df)
}


#testing the function
#base_dir <- "~/nfs"
#source("~/Repositories/aichi13/src/config.R")
#source("~/Repositories/aichi13/src/1_modeling/1_2_alternatives/create_buffers.R")
#ers_exsitu(species)

