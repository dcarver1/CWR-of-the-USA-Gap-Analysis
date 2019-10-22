###
# calcualte the fcsIn = GRSin +ERSin / 2 


fcs_insitu <- function(species) {
  
  #load GRS, and ERS file
  sp_srs <- read.csv(paste0(sp_dir,"/gap_analysis/insitu/srs_result.csv"))
  sp_grs <- read.csv(paste0(sp_dir,"/gap_analysis/insitu/grs_result.csv"))
  sp_ers <- read.csv(paste0(sp_dir,"/gap_analysis/insitu/ers_result.csv"))
  
  sp_fcs <- mean(c(sp_srs$SRS,sp_grs$GRS,sp_ers$ERS), na.rm=T)
  
  #create data.frame with output
  out_df <- data.frame(ID=species,SRS=sp_srs, GRS=sp_grs$GRS, ERS=sp_ers$ERS, FCS=sp_fcs)
  write.csv(out_df,paste(sp_dir,"/gap_analysis/insitu/summary.csv",sep=""),row.names=F)
  
  #return object
  return(out_df)
}


