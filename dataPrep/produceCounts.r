####

# 10/15/2018

# The goal of this work is to generate a tab seperated files that contains counts of multiple paraments as

# defined by the aichi docs


genCounts <- function(species){
    dataThin <<- rawData %>%
      dplyr::select(c("taxon", "latitude", "longitude", "type","databaseSource")) %>%
      mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "") %>%
      mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "") %>%
      mutate(hasLatLong = hasLat & hasLong)
    
    
    colNames <- c("species","totalRecords",	"hasLat", "hasLong","totalUseful", 	"totalGRecords",
                  "totalGUseful","totalHRecords",	"totalHUseful","numberOfUniqueSources" )
    noNas <- dataThin[complete.cases(dataThin),]
    tbl <- noNas %>%
      dplyr::group_by(type, hasLatLong ) %>%
      dplyr::summarize(total = n())
    
    df <- data.frame(matrix(NA, nrow = 1, ncol = 10))
    colnames(df) <- colNames
    df$species <- unique(dataThin$taxon)
    df$totalRecords <- nrow(dataThin)
    df$totalUseful <- sum((subset(tbl, hasLatLong == TRUE))$total)
    df$totalGRecords <- sum((subset(tbl, type == "G"))$total)
    df$totalGUseful <- sum((subset(tbl, type == "G" & hasLatLong == TRUE))$total)
    df$totalHRecords <- sum((subset(tbl, type == "H"))$total)
    df$totalHUseful <- sum((subset(tbl, type == "H" & hasLatLong == TRUE))$total)
    df$hasLat <- sum(dataThin$hasLat)
    df$hasLong <- sum(dataThin$hasLong)
    df$numberOfUniqueSources <- n_distinct(rawData$databaseSource)
    
    
    write.csv(df, file = paste0(sp_dir,"/counts.csv"),row.names = FALSE)
}
