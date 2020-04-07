### 
# update the counts CSV to reflect the useable points that are included in
# the modeling method. 
# dan.carver@carverd.com 
# 20200129
###

updateCounts <- function(species){
  # read in existing counts csv 
  c1 <- read.csv( file = paste0(sp_dir,"/counts.csv"))
  
  # pull ddata from cleanPoints feature 
  d1 <- cleanPoints@data
  
  # generate a summary table based on lat long values 
  tbl <- d1 %>%
    dplyr::group_by(type, hasLatLong ) %>%
    dplyr::summarize(total = n())
  # replace all the values that have 
  c1$totalUseful <- sum((subset(tbl, hasLatLong == TRUE))$total)
  c1$totalGUseful <- sum((subset(tbl, type == "G" & hasLatLong == TRUE))$total)
  c1$totalHUseful <- sum((subset(tbl, type == "H" & hasLatLong == TRUE))$total)
  c1$hasLat <- sum(dataThin$hasLat)
  c1$hasLong <- sum(dataThin$hasLong)
  c1$numberOfUniqueSources <- n_distinct(c1$numberOfUniqueSources)

  write.csv(x = c1, file = paste0(sp_dir,"/counts.csv"))
}

