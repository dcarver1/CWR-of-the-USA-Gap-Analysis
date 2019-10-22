###
# Calculate the proportion of points that fall within a protected areas. Insitu SRS 
# 20191002
# carver.dan1@gmail.com
###


#species <- species1
srs_insitu <- function(species) {

  totalNum <- nrow(cleanPoints)

  # set coodinate systems equal  
  crs(cleanPoints) = crs(proArea)
  # run a extract to values and select all data. Test for NA, then sum true values for total number of points in protected
  #areas 
  protectPoints <- sum(!is.na(raster::extract(x = proArea,y = cleanPoints)))
  
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
  
  
  #return object
  return(out_df)
}

