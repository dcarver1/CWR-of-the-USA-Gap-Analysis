###
# Calculate the GRSin = area in protect areas / total area * 100
# 20200414
# dan.carver@carverd.com
###

insitu_grs = function(species) {
      #GRSin = area in protect areas / total area * 100
      # read in threshold raster
      thrshold <- raster(x = paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))
      # replace 0 with NA
      thrshold[which(thrshold[] == 0)] <- NA
      #clause for no predicted suitable habitat
      if(!1 %in% unique(values(thrshold))){
        protect_area <- 0
        grs <- 0
        thrshold_area <- 0
      }else{
        # mask protect area to native area
        proNative <- raster::crop(x = proArea, y = thrshold)
        # add threshold raster to protect areas
        protectSDM <- thrshold * proNative
        #writeRaster(x = protectSDM,filename = paste0(sp_dir,"/modeling/alternatives/grs_pa_PAs_narea_areakm2.tif" ), overwrite=TRUE)

        ### clause for no protected area in threshold area
        if(length(unique(values(protectSDM))) == 1){
          protect_area <- 0
          grs <- 0
          # calculate total species area
          cell_size<-area(thrshold, na.rm=TRUE, weights=FALSE)
          cell_size<-cell_size[!is.na(cell_size)]
          thrshold_area <- length(cell_size)*median(cell_size)

        }else{
          #calculated the area of cells with in protect areas within the threshold area
          protectSDM[which(protectSDM[] == 0)] <- NA
          cell_size<-area(protectSDM, na.rm=TRUE, weights=FALSE)
          cell_size<-cell_size[!is.na(cell_size)]
          protect_area <-length(cell_size)*median(cell_size)
          
          # complete for threshold predicted area
          cell_size<-area(thrshold, na.rm=TRUE, weights=FALSE)
          cell_size<-cell_size[!is.na(cell_size)]
          thrshold_area <-length(cell_size)*median(cell_size)

          #calculate GRS
          grs <- min(c(100, protect_area/thrshold_area*100))

        }
      }
    #create data.frame with output
    df <- data.frame(ID = species, SPP_AREA_km2 = thrshold_area, SPP_WITHIN_PA_AREA_km2 = protect_area, GRS = grs)
    #write.csv(df,paste(sp_dir,"/gap_analysis/insitu/grs_result.csv",sep=""),row.names=F)
    write.csv(df,paste(sp_dir,"/gap_analysis/insitu/grs_resultPAUD.csv",sep=""),row.names=F)
    
}
