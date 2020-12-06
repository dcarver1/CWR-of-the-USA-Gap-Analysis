###
# compile insitu and exsitu summary scripts and assign priority levels 
# 20190919
# carver.dan1@gmail.com
###

fcs_combine <- function(species) {
  
  #in-situ and ex-situ summary files
  file_in <- paste0(sp_dir,"/gap_analysis/insitu/summary.csv")
  file_ex <- paste0(sp_dir,"/gap_analysis/exsitu/summary.csv")
  
  #read data from in-situ and ex-situ files
  data_in <- read.csv(file_in, sep=",", header=T)
  data_ex <- read.csv(file_ex, sep=",", header=T)
  
  #compute FCSc_min and FCSc_max
  data_comb <- data.frame(ID=species, FCSex=data_ex$FCS, FCSin=data_in$FCS)
  data_comb$FCSc_min <- min(c(data_ex$FCS,data_in$FCS),na.rm=T)
  data_comb$FCSc_max <- max(c(data_ex$FCS,data_in$FCS),na.rm=T)
  data_comb$FCSc_mean <- mean(c(data_ex$FCS,data_in$FCS),na.rm=T)
  
  #assign classes (min)
  if (data_comb$FCSc_min < 25) {
    data_comb$FCSc_min_class <- "UP"
  } else if (data_comb$FCSc_min >= 25 & data_comb$FCSc_min < 50) {
    data_comb$FCSc_min_class <- "HP"
  } else if (data_comb$FCSc_min >= 50 & data_comb$FCSc_min < 75) {
    data_comb$FCSc_min_class <- "MP"
  } else {
    data_comb$FCSc_min_class <- "LP"
  }
  
  #assign classes (max)
  if (data_comb$FCSc_max < 25) {
    data_comb$FCSc_max_class <- "UP"
  } else if (data_comb$FCSc_max >= 25 & data_comb$FCSc_max < 50) {
    data_comb$FCSc_max_class <- "HP"
  } else if (data_comb$FCSc_max >= 50 & data_comb$FCSc_max < 75) {
    data_comb$FCSc_max_class <- "MP"
  } else {
    data_comb$FCSc_max_class <- "LP"
  }
  
  #assign classes (mean)
  if (data_comb$FCSc_mean < 25) {
    data_comb$FCSc_mean_class <- "UP"
  } else if (data_comb$FCSc_mean >= 25 & data_comb$FCSc_mean < 50) {
    data_comb$FCSc_mean_class <- "HP"
  } else if (data_comb$FCSc_mean >= 50 & data_comb$FCSc_mean < 75) {
    data_comb$FCSc_mean_class <- "MP"
  } else {
    data_comb$FCSc_mean_class <- "LP"
  }
  
  #create output directory if it doesnt exist
  comb_dir <- paste0(sp_dir,"/gap_analysis/combined")
  if (!file.exists(comb_dir)) {dir.create(comb_dir)}
  
  #save output file and return
  write.csv(data_comb, paste(comb_dir,"/fcs_combined.csv",sep=""), row.names=F)
  return(data_comb)
}
