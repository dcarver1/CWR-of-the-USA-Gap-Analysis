### 
# a script that will clean all older model run from the computer 
# 20200108 
# dan.carver@carverd.com
### 

# background 


baseDir <- "D:/cwrUSA/gap_analysis"

oldFiles <- list.dirs(path = baseDir, full.names = TRUE, recursive = TRUE) 

occData <- data.table::fread("D:/cwrNA/occurrence_data2019_05_29/combinedOccurance2020-04-07.csv",
                             header = TRUE)
occData <- occData[,2:nrow(occData)]
speciesList <- unique(occData$taxon)

old2 <- oldFiles[grep(pattern = "test20191023$", x = oldFiles)]
#filter files by the species list 
old3 <- old2[speciesList %in% old2]

n <- list.files(old2[1], recursive = TRUE)

unlink(x = old2[6], recursive = TRUE)



allFiles <- list.files(path="D:/cwrNA/gap_analysis/Capsicum", full.names = TRUE, recursive = TRUE) 
oldFiles <- allFiles[grep(pattern = "2019-11-06"| "2019-11-05", x = allFiles)]
unlink(x=oldFiles) 


deleteALot <- function(directory,pattern){
  allFolders <- list.dirs(path = directory, full.names = TRUE, recursive = TRUE)
  oldFolders <- allFolders[grep(pattern = pattern, x = allFolders)]
  unlink(x = oldFolders, recursive = TRUE)
  print(paste0("All files and folders containing ", pattern, " are gone forever."))
}


listOfPatterns <- c("test20200131$")

for(i in listOfPatterns){
  deleteALot(directory = "D:/cwrUSA/gap_analysis", pattern = i)
}
