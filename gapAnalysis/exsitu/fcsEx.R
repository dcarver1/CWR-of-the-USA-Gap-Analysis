##########################################   Start Functions    ###############################################
# This function calculates the FCSex. It loads srs.csv, grs.csv, ers.csv and
# calculates FCS. It saves output in summary.csv
# @param (string) species: species ID
# @return (data.frame): This function returns a data frame with ID, SRS, GRS, ERS, FCS
#                       for a given species.


fcs_exsitu <- function(species) {

  #load SRS, GRS, and ERS file
  if(file.exists(paste0(sp_dir,"/gap_analysis/exsitu/srs_result.csv"))){
    sp_srs <- read.csv(paste0(sp_dir,"/gap_analysis/exsitu/srs_result.csv"))
  }
  if(file.exists(paste0(sp_dir,"/gap_analysis/exsitu/grs_result.csv"))){
    sp_grs <- read.csv(paste0(sp_dir,"/gap_analysis/exsitu/grs_result.csv"))
  }
  if(file.exists(paste0(sp_dir,"/gap_analysis/exsitu/ers_result.csv"))){
    sp_ers <- read.csv(paste0(sp_dir,"/gap_analysis/exsitu/ers_result.csv"))
  }
  
  
  if(file.exists(paste0(sp_dir,"/gap_analysis/exsitu/grs_result.csv")) &
     file.exists(paste0(sp_dir,"/gap_analysis/exsitu/ers_result.csv"))){
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
    out_df <- data.frame(ID=species, SRS=sp_srs$SRS, GRS=sp_grs$GRS,
                         ERS=sp_ers$ERS, FCS=sp_fcs, FCS_Score = score)
    #create data.frame with output
    write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/summary.csv",sep=""),row.names=F)
  }else{
    sp_fcs <- sp_srs$SRS
    
    #assign classes 
    if (sp_fcs < 25) {
      score <- "HP"
    } else if (sp_fcs >= 25 & sp_fcs < 50) {
      score <- "MP"
    } else if (sp_fcs >= 50 & sp_fcs < 75) {
      score <- "LP"
    } else {
      score <- "SC"
    }
    out_df <- data.frame(ID=species, SRS=sp_srs$SRS, GRS=NA, 
                         ERS=NA, FCS=sp_fcs, FCS_Score = score)
    #create data.frame with output
    write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/summary.csv",sep=""),row.names=F)
  }
  

  #return object
  return(out_df)
}


