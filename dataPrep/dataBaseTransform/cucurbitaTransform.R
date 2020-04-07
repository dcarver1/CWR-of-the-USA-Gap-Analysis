###
# reworking of cucurbita data
# 20191120
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "D:/cwrNA/occurrence_data2019_05_29/cucurbita"

# Load in data 
csvPath <- paste0(base_dir,"/Khoury_Cucurbita_paperdata_USAspecies20191206.csv")
data <- data.table::fread(input = csvPath,header = TRUE)
# Select necessary columns from dataset 

dataThin <- data 
nr <- nrow(dataThin)

# define structure of the empty dataframe 
df <- data.frame(taxon=character(nr),
                 genus=character(nr),
                 species=character(nr),
                 latitude=double(nr),
                 longitude=double(nr),
                 databaseSource=character(nr),
                 institutionCode=character(nr),
                 type=factor(nr),
                 uniqueID=factor(nr),
                 sampleCategory=character(nr),
                 country=character(nr),
                 iso3=character(nr),
                 localityInformation=character(nr),
                 biologicalStatus = character(nr), 
                 collectionSource = character(nr),
                 finalOriginStat = character(nr),
                 stringsAsFactors=FALSE)

# assign columns to location in empty dataframe
df$taxon <- dataThin$taxon
df$genus <- "Cucurbita"
df$species <- NA
df$latitude <- dataThin$latitude
df$longitude <- dataThin$longitude
df$databaseSource <- "cucurbita" 
df$institutionCode <- dataThin$institute
df$type <- dataThin$type
df$uniqueID <- dataThin$sample_number
df$sampleCategory <- dataThin$status
df$country <- dataThin$country
df$iso3 <- NA
df$localityInformation <- dataThin$locality
df$biologicalStatus <- NA
df$collectionSource <- NA
df$finalOriginStat <- NA 

# split taxon into genus and species 
df$taxon <- gsub(pattern = "_", replacement = " ", x = df$taxon)

df$species <- gsub(pattern = "Cucurbita", replacement = "", x = df$taxon)

#write CSV 
write.csv(x = df, file = paste0(base_dir, "/refinedCucurbita.csv"))
