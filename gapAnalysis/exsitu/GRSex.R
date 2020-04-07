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
  
  if(!file.exists(paste0(sp_dir,"/modeling/alternatives/ga50.tif"))){
    grs <- 0
    gBufferRas_area <- 0
    pa_spp_area <- NA
  }else{
  
  # load in thrsld raster 
  pa_spp <<- raster(paste0(sp_dir,"/modeling/spdist_thrsld_median.tif"))
  # this area method accounts for 0 and 1, need to replace 0 with NA values before determining the area
  pa_spp[pa_spp==0] <- NA 
  cell_size<- area(pa_spp, na.rm=TRUE, weights=FALSE)
  cell_size<- cell_size[!is.na(cell_size)]
  pa_spp_area <<-length(cell_size)*median(cell_size)
    
  
  # load in ga50 and model outputs 
  gBufferRas <- raster::raster(paste0(sp_dir,"/modeling/alternatives/ga50.tif"))
  # mask buffer raster to the GA50 area 
  gBufferRas1 <<- gBufferRas * pa_spp
  
  if(length(unique(values(gBufferRas1)))==1 ){
    grs <- 0
    gBufferRas_area <- 0
    pa_spp_area <- NA
  }else{
    cell_size<-area(gBufferRas1, na.rm=TRUE, weights=FALSE)
    cell_size<-cell_size[!is.na(cell_size)]
    gBufferRas_area<<-length(cell_size)*median(cell_size)
    
    
    
    #calculate GRS
    grs <- min(c(100, gBufferRas_area/pa_spp_area*100))
  }
  } 
    
  #create data.frame with output
  out_df <- data.frame(ID=species, SPP_AREA_km2=pa_spp_area, G_AREA_km2=gBufferRas_area, GRS=grs)
  write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/grs_result.csv",sep=""),row.names=F)
    
  #return object
  return(out_df)
}


