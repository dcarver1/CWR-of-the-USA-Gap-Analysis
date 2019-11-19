###
# Calculate the GRSin = area in protect areas / total area * 100
# 20190919
# carver.dan1@gmail.com
###

#species=species1

insitu_grs = function(species) {
      #GRSin = area in protect areas / total area * 100
      # mask protect area to native area 
      proNative <- raster::mask(x = proArea,mask = nativeArea)
      # add threshold raster to protect areas 
      protectSDM <- thrshold + proNative 
      writeRaster(x = protectSDM,filename = paste0(sp_dir,"/modeling/alternatives/grs_pa_PAs_narea_areakm2.tif" ), overwrite=TRUE)

      # exclude protected areas outside of model threshold 
      thrshold2 <- thrshold
      thrshold2[which(thrshold2[] == 0)] <- NA
      proNative <- proNative *thrshold
      #calculated the area of cells with in protect areas within the threshold area
      proNative[which(proNative[] == 0)] <- NA
      cell_size<-area(proNative, na.rm=TRUE, weights=FALSE)
      cell_size<-cell_size[!is.na(cell_size)]
      protect_area <-length(cell_size)*median(cell_size)
      # complete for threshold predicted area 
      cell_size<-area(thrshold, na.rm=TRUE, weights=FALSE)
      cell_size<-cell_size[!is.na(cell_size)]
      thrshold_area <-length(cell_size)*median(cell_size)
      
      #calculate GRS
      grs <- min(c(100, protect_area/thrshold_area*100))
      
    #create data.frame with output
    df <- data.frame(ID = species, SPP_AREA_km2 = thrshold_area, SPP_WITHIN_PA_AREA_km2 = protect_area, GRS = grs)
    write.csv(df,paste(sp_dir,"/gap_analysis/insitu/grs_result.csv",sep=""),row.names=F)
}
  