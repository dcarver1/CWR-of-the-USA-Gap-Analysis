###
# buffer all g points and clip to native area
# dan.carver@carverd.com
# 20200414
###

create_buffers <- function(species){
    ## select all g points from point object
    p1 <- subset(cleanPoints, type == "G")
    # ensure matching CRS
    raster::crs(p1) <- raster::crs(nativeArea)

    #clause to test for G occurrences
    if(nrow(p1@data)== 0){
      print("there are no g points for this species")
    }else{
    ##buffering
    buffer <- geobuffer::geobuffer_pts(xy = p1,
                              dist_m = bufferDist,
                              output = 'sf')
    # set extent equal to native area
    rasters1 <- bioVars[[1]] %>%
      raster::crop(nativeArea) %>%
      raster::mask(nativeArea)

    ##rasterizing and matching cells to predictor layers
    buffer_rs <- fasterize::fasterize(buffer, rasters1)

    # mask buffer to native area
    maskBuff <- raster::crop(x = buffer_rs, y = nativeArea) %>%
      raster::mask(nativeArea)

    ##writing raster
    writeRaster(maskBuff, paste0(sp_dir,"/modeling/alternatives/ga50.tif	"),overwrite=TRUE)
  }
}
