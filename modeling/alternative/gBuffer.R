###
# buffer all g points and clip to native area 

create_buffers <- function(species){
  filename <- paste0(sp_dir,"/modeling/alternatives/ga50.tif")
  if (!file.exists(filename)) {
    ## select all g points from point object 
    p1 <- subset(spPoint, type == "G") # select all g pointsd
    
    if(nrow(p1@data)== 0){
      print("there are no g points for this species")      
    }else{
    ##buffering
    buffer <- rgeos::gBuffer(p1, width=bufferDist)
    # set extent equal to native area 
    rasters1 <- bioVars$as.RasterStack()
    
    ##rasterizing and making it into a mask
    buffer_rs <- rasterize(buffer, rasters1)

    # mask buffer to native area 
    maskBuff <- raster::crop(x = buffer_rs, y = nativeArea)
    
    ##writing raster
    writeRaster(maskBuff, paste0(sp_dir,"/modeling/alternatives/ga50.tif	"))
    #rm(buffer, rasters1 ,buffer_rs)
    }
  }
}
