####

# 10/15/2018

# The goal of this work is to generate a tab seperated files that contains counts of multiple paraments as

# defined by the aichi docs


counts1 <- function(species){
    dataThin <<- rawData %>%
      dplyr::select(c("taxon", "latitude", "longitude", "type","databaseSource")) %>%
      mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "" & !is.null(latitude) & latitude != "NULL") %>%
      mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "" & !is.null(longitude)& longitude != "NULL") %>%
      mutate(hasLatLong = hasLat & hasLong)


    colNames <- c("species","totalRecords",	"hasLat", "hasLong","totalUseful", 	"totalGRecords",
                  "totalGUseful","totalHRecords",	"totalHUseful","numberOfUniqueSources", 
                  "NA_occurrences","NA_GUseful" ,"NA_HUseful")
    # this was removing all reconds with no lat long, make the total number and the total number of useful elements equal
    #noNas <- dataThin[complete.cases(dataThin),]
    tbl <- dataThin %>%
      dplyr::group_by(type, hasLatLong ) %>%
      dplyr::summarize(total = n())

    countsData <- data.frame(matrix(NA, nrow = 1, ncol = 13))
    colnames(countsData) <- colNames
    
    #colnames(countsData) <- colNames
    countsData$species <- unique(dataThin$taxon)
    countsData$totalRecords <- nrow(dataThin)
    countsData$totalUseful <- sum((subset(tbl, hasLatLong == TRUE))$total)
    countsData$totalGRecords <- sum((subset(tbl, type == "G"))$total)
    countsData$totalGUseful <- sum((subset(tbl, type == "G" & hasLatLong == TRUE))$total)
    countsData$totalHRecords <- sum((subset(tbl, type == "H"))$total)
    countsData$totalHUseful <- sum((subset(tbl, type == "H" & hasLatLong == TRUE))$total)
    countsData$hasLat <- sum(dataThin$hasLat)
    countsData$hasLong <- sum(dataThin$hasLong)
    countsData$numberOfUniqueSources <- n_distinct(rawData$databaseSource)
    countsData$NA_occurrences <- 0
    countsData$NA_GUseful <- 0 
    countsData$NA_HUseful <- 0
    
    write.csv(countsData, file = paste0(sp_dir,"/counts.csv"),row.names = FALSE)
}
