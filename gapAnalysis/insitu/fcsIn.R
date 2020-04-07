###
# calcualte the fcsIn = GRSin +ERSin / 2 


fcs_insitu <- function(species) {
  
  #load SRS, GRS, and ERS file
  if(file.exists(paste0(sp_dir,"/gap_analysis/insitu/srs_result.csv"))){
    sp_srs <- read.csv(paste0(sp_dir,"/gap_analysis/insitu/srs_result.csv"))
  }
  if(file.exists(paste0(sp_dir,"/gap_analysis/insitu/grs_result.csv"))){
    sp_grs <- read.csv(paste0(sp_dir,"/gap_analysis/insitu/grs_result.csv"))
  }
  if(file.exists(paste0(sp_dir,"/gap_analysis/insitu/ers_result.csv"))){
    sp_ers <- read.csv(paste0(sp_dir,"/gap_analysis/insitu/ers_result.csv"))
  }

  if(file.exists(paste0(sp_dir,"/gap_analysis/insitu/grs_result.csv"))&
     file.exists(paste0(sp_dir,"/gap_analysis/insitu/ers_result.csv"))){
    sp_fcs <- mean(c(sp_srs$SRS,sp_grs$GRS,sp_ers$ERS), na.rm=T)
    
    #assign classes (min)
    if (sp_fcs < 25) {
      score <- "HP"
    } else if (sp_fcs >= 25 & sp_fcs < 50) {
      score <- "MP"
    } else if (sp_fcs >= 50 & sp_fcs < 75) {
      score <- "LP"
    } else {
      score <- "SC"
    }

    outDf <- data.frame(ID=species,SRS.NTOTAL = sp_srs$NTOTAL, SRS.ProTotal = sp_srs$ProTotal,
                        SRS.SRS=sp_srs$SRS, GRS=sp_grs$GRS,
                        ERS=sp_ers$ERS, FCS=sp_fcs, FCS_Score = score)
    #create data.frame with output
    write.csv(outDf,paste(sp_dir,"/gap_analysis/insitu/summary.csv",sep=""),row.names=F)
    
  }else{
    sp_fcs <- sp_srs$SRS
    
    #assign classes (min)
    if (sp_fcs < 25) {
      score <- "HP"
    } else if (sp_fcs >= 25 & sp_fcs < 50) {
      score <- "MP"
    } else if (sp_fcs >= 50 & sp_fcs < 75) {
      score <- "LP"
    } else {
      score <- "SC"
    }
    outDf <- data.frame(ID=species,SRS.NTOTAL = sp_srs$NTOTAL,SRS.ProTotal = sp_srs$ProTotal,
                        SRS.SRS=sp_srs$SRS, GRS=NA,
                        ERS=NA, FCS=sp_fcs, FCS_Score = score)
    write.csv(outDf,paste(sp_dir,"/gap_analysis/insitu/summary.csv",sep=""),row.names=F)
  }
  
  #return object
  return(outDf)
}


