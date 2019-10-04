##########################################   Start Functions    ###############################################
# This function calculates the GRSex. It loads occurrences if they exist, then
# loads the presence/absence surface, creates the G buffer (i.e. CA50) and finally
# outputs the GRS and areas in a data.frame (which is written into a file).
# @param (string) species: species ID
# @param (logical) debug: whether to save or not the intermediate raster outputs
# @return (data.frame): This function returns a data frame with GRS and areas of G buffer (i.e. CA50)
#                       and of the presence/absence surface.

#species <- species1
grs_exsitu <- function(species) {
  
  if(sp_counts$totalGUseful < 1){
    grs <- 0
    gBuffer_area <- 0
    pa_spp_area <- NA
  }else{
  
  # load in ga50 and model outputs 
  gBuffer <<- raster::raster(paste0(sp_dir,"/modeling/alternatives/ga50.tif"))
  cell_size<-area(gBuffer, na.rm=TRUE, weights=FALSE)
  cell_size<-cell_size[!is.na(cell_size)]
  gBuffer_area<-length(cell_size)*median(cell_size)

  pa_spp <<- raster(paste0(sp_dir,"/spdist_thrsld.tif"))
  cell_size<-area(pa_spp, na.rm=TRUE, weights=FALSE)
  cell_size<-cell_size[!is.na(cell_size)]
  pa_spp_area <<-length(cell_size)*median(cell_size)

  #calculate GRS
  grs <- min(c(100, gBuffer_area/pa_spp_area*100))
    } 
    
  #create data.frame with output
  out_df <- data.frame(ID=species, SPP_AREA_km2=pa_spp_area, G_AREA_km2=gBuffer_area, GRS=grs)
  write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/grs_result.csv",sep=""),row.names=F)
    
  #return object
  return(out_df)
}