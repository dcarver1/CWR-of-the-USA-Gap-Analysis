###
# reworking of capsicum data
# 20191120
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)
library(MazamaSpatialUtils)

#set base dir
base_dir <- "D:/cwrNA/occurrence_data2019_05_29/capsicum"

# Load in data 
csvPath <- paste0(base_dir,"/Khoury_Capsicum_paperdata_USAspecies20191206.csv")
data <- data.table::fread(input = csvPath,header = TRUE)
# Select necessary columns from dataset 

dataThin <- data %>%
  dplyr::select("taxon","latitude","longitude", "db","institutioncode",
         "type", "record_identifier","sampstat","collsrc", "country", 
         "iso2", "adm1", "adm2", "adm3", "adm4", "locality")
nr <- nrow(dataThin)


#replace iso 2 with iso 3 
dataThin$iso2 <- gsub(pattern = "",replacement = NA,x = dataThin$iso2)
dataThin$iso3 <- MazamaSpatialUtils::iso2ToIso3(dataThin$iso2)

#define locality information 
dataThin$local2 <- paste(dataThin$adm1,dataThin$adm2,dataThin$adm3,
                         dataThin$adm4, dataThin$locality,
                         sep=" -- ")

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
df$genus <- "Capsicum"
df$species <- "annuum var. glabriusculum"
df$latitude <- dataThin$latitude
df$longitude <- dataThin$longitude
df$databaseSource <- "Capsicum" 
df$institutionCode <- dataThin$institutioncode
df$type <- dataThin$type
df$uniqueID <- dataThin$record_identifier
df$sampleCategory <- dataThin$sampstat
df$country <- NA
df$iso3 <- dataThin$iso3
df$localityInformation <- dataThin$local2 
df$biologicalStatus <- dataThin$collsrc
df$collectionSource <- NA
df$finalOriginStat <- NA 

# split taxon to Genus and species 
df$taxon <- gsub(pattern = "_",replacement = " ",x = df$taxon)


# write out CSV 
write.csv(x= df, paste0(base_dir, "/refinedCapsicum.csv") )

