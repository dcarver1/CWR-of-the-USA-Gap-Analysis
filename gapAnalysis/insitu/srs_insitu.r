###
# Calculate the proportion of points that fall within a protected areas found within the predicted
# model extent. Insitu SRS
# 20200303
# dan.carver@carverd.com
###

srs_insitu <- function(species) {

  totalNum <- nrow(cleanPoints)
  # read in threshold raster
  thrshold <- raster(x = paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))
  thrshold[which(thrshold[] == 0)] <- NA
  # mask protect area to native area
  proNative <- raster::crop(x = proArea, y = thrshold)
  #proNative[is.na(proNative)]<- 0
  # add threshold raster to protect areas
  protectSDM <- thrshold * proNative

  # set coodinate systems equal
  crs(cleanPoints) = crs(protectSDM)
  # run a extract to values and select all data. Test for NA, then sum true values for total number of points in protected
  #areas
  protectPoints <- sum(!is.na(raster::extract(x = protectSDM,y = cleanPoints)))

  #define SRS
  if(protectPoints >= 0 ){
    srsInsitu <- 100 *(protectPoints/totalNum)
  }else{
    srsInsitu <- 0
  }


  #create data.frame with output
  out_df <- data.frame(ID=species,
                       NTOTAL=totalNum,
                       ProTotal = protectPoints,
                       SRS=srsInsitu)
  write.csv(out_df,paste0(sp_dir,"/gap_analysis/insitu/srs_result.csv"),row.names=F)
  #write.csv(out_df,paste0(sp_dir,"/gap_analysis/insitu/srs_resultPAUD.csv"),row.names=F)

  #return object
  return(out_df)
}
