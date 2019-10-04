###
# Calculate the GRSin = area in protect areas / total area * 100
# 20190919
# carver.dan1@gmail.com
###

#species=species1

insitu_grs = function(species) {
  if(sp_counts$totalGUseful < 1){
    grs <- 0
    gBuffer_area <- 0
    pa_spp_area <- NA
  }else{
    #GRSin = area in protect areas / total area * 100
    
    pa_spp[which(pa_spp[] == 0)] <- NA
    paCrop <- raster::crop(x = proArea, y = pa_spp)
    
    protectSDM <<- paCrop*pa_spp
    cell_size<-area(protectSDM, na.rm=TRUE, weights=FALSE)
    cell_size<-cell_size[!is.na(cell_size)]
    protect_area <<-length(cell_size)*median(cell_size)
    
    
    #calculate GRS
    grs <- min(c(100, protect_area/pa_spp_area*100))
    } 
  #create data.frame with output
  df <- data.frame(ID = species, SPP_AREA_km2 = pa_spp_area, SPP_WITHIN_PA_AREA_km2 = protect_area, GRS = grs)
  write.csv(df,paste(sp_dir,"/gap_analysis/insitu/grs_result.csv",sep=""),row.names=F)
}
  