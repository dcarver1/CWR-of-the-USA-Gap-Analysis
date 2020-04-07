###
# check all the names in the species list against the information we have from GRIN about native area
# 20200203
# dan.carver@carverd.com
###

#grin <- read.csv("D:/cwrNA/parameters/statePerTaxon/CWRofUSA_nativeareas_2020_1_30.csv")
#tGrin <- unique(grin$name)
#taxonGrin <- as.data.frame(tGrin)
#colnames(taxonGrin) <- "taxon"


#occData <<- data.table::fread("D:/cwrNA/occurrence_data2019_05_29/combinedOccurance2020-01-11.csv",header = TRUE)
#tOcc <- unique(occData$taxon)
#taxonOcc <- as.data.frame(tOcc)
#colnames(taxonOcc) <- "taxon"


## species with no matched data in  data in grin
#notInGrin <- tOcc[!tOcc %in% tGrin]
#### need to manually add these species to the spread sheet
#notInGrin


## species with no occ data
#noOcc <- tGrin[!tGrin %in% tOcc]
#noOcc
# drop cucurbitas and capsicum
#noOcc1 <- noOcc[-c(2,3,4,5,6)]
#noOcc1
