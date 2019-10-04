###
# The goal of this script is to compile all the csv from individual sources into
# a single element. 
# Not sure if this is really the beset structure to work with but I'm going to try
# it and see what happens 
### 

library(tidyverse)
library(data.table)


base_dir <- "D:/cwrOfNA/occurrence_data2019_05_29"

# read in all csvs 
files <- list.files(path = base_dir, pattern = ".csv",full.names = TRUE,recursive = TRUE)
# select all those that have 'refined' in the name 
refined <- files[grepl(pattern = 'refined', x = files)]

# create an empty df with same structure 
df <- data.frame(taxon=character(),
                 genus=character(),
                 species=character(),
                 latitude=double(),
                 longitude=double(),
                 databaseSource=character(),
                 institutionCode=character(),
                 type=factor(),
                 uniqueID=factor(),
                 sampleCategory=character(),
                 country=character(),
                 iso3=character(),
                 localityInformation=character(),
                 stringsAsFactors=FALSE)

# create function 
# read in one csv, r bind it to df, drop csv from memory 
appendTable <- function(dataframe, pathToData){
  data <- data.table::fread(pathToData, header = TRUE,)%>%
    dplyr::select("taxon","genus","species","latitude","longitude",
                 "databaseSource","institutionCode","type","uniqueID",
                 "sampleCategory","country","iso3","localityInformation")
  dataframe <- rbind(dataframe, data)
  rm(data)
  return(dataframe)
}

# apply function to list of csvs 
for(i in 1:length(refined)){
  path <- refined[i]
  df <- appendTable(dataframe = df, pathToData = path)
  print(paste0(path, " has been added"))
  print(dim(df))
}



# write out final csv 
write.csv(x = df, file = paste0(base_dir,"/combinedOccurance", Sys.Date(), ".csv"))
