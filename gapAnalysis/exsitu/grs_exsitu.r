###
# determines the total buffered area of g points withing modeled area
# dan.carver@carverd.com
# 20200414
###

grs_exsitu <- function(species) {
  # clause to see if any g points exist
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

  # clause to determine if any of the buffered area falls within predicted area
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
}
