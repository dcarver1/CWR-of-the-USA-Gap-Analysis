###
# Generate a SRSin base on all points before the species is modeled. So that a metric can be provided that
# will then be replaced in the SRSin method if a species can be sucessfully modeled.
# 20200303
# dan.carver@carverd.com
###

srs_insitu_preModel <- function(species) {
  # clause for no points in north america
  if(class(spPoint) == "character"){
    #create data.frame with output
    out_df <- data.frame(ID=species,
                         NTOTAL=0,
                         ProTotal = 0,
                         SRS.SRS=0)
    write.csv(out_df,paste0(sp_dir,"/gap_analysis/insitu/srs_result.csv"),     row.names=F)
    #write.csv(out_df,paste0(sp_dir,"/gap_analysis/insitu/srs_resultPAUD.csv"),row.names=F)

  }else{
    totalNum <- nrow(spPoint)
    # set coodinate systems equal
    crs(spPoint) = crs(proArea)
    # run a extract to values and select all data. Test for NA,
    # then sum true values for total number of points in protected areas
    protectPoints <- sum(!is.na(raster::extract(x = proArea,y = spPoint)))
    # define SRS
    if(protectPoints >= 0 ){
      srsInsitu <- 100 *(protectPoints/totalNum)
    }else{
      srsInsitu <- 0
    }

    #create data.frame with output
    out_df <- data.frame(ID=species,
                         NTOTAL=totalNum,
                         ProTotal = protectPoints,
                         SRS.SRS=srsInsitu)
    write.csv(out_df,paste0(sp_dir,"/gap_analysis/insitu/srs_result.csv"),row.names=F)
    #write.csv(out_df,paste0(sp_dir,"/gap_analysis/insitu/srs_resultPAUD.csv"),row.names=F)
    
  }
}
