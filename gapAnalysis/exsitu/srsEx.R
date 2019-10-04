
##########################################   Start Functions    ###############################################
# This function calculates the ex-situ SRS. It loads counts.csv and computes SRS
# @param (string) species: species ID
# @return (data.frame): This function returns a data frame with SRS, and numbers
#                       of G, H, and total samples, with and without coordinates.

#species <- species1
srs_exsitu <- function(species) {

  sp_counts <<- read.csv(paste0(sp_dir,"/counts.csv"))
  
  if(sp_counts$totalGRecords >= 1 & sp_counts$totalHRecords == 0){
    srs <-100
  }
  
  #### this works for full distributions
  if (sp_counts$totalGRecords == 0 & sp_counts$totalHRecords ==0) {
    srs <- 0
  } else {
    srs <- min(c(100,sp_counts$totalGRecords/sp_counts$totalHRecords*100))
  }
  
  
  #create data.frame with output
  out_df <- data.frame(ID=species, 
                       NTOTAL=sp_counts$totalRecords,
                       NTOTAL_COORDS=sp_counts$totalUseful,
                       NG= sp_counts$totalGRecords,
                       NG_COORDS=sp_counts$totalGUseful,
                       NH=sp_counts$totalHRecords,
                       NH_COORDS=sp_counts$totalHUseful,
                       SRS=srs)
  write.csv(out_df,paste0(sp_dir,"/gap_analysis/exsitu/srs_result.csv"),row.names=F)
  
  
  #return object
  return(out_df)
}

#testing the function
#base_dir <- "~/nfs"
#source("~/Repositories/aichi13/src/config.R")
#srs_exsitu("2686262")

