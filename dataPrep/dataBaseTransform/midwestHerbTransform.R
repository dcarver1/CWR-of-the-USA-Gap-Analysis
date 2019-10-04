###
# reworking of idigbio data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "D:/cwrOfNA/occurrence_data2019_05_29/Midwest_herbaria/SymbOutput_2019-05-28_151547_DwC-A"

# Load in data 
csvPath <- paste0(base_dir,"/occurrences.csv")
data <- data.table::fread(csvPath, header = TRUE)
# Select necessary columns from dataset 
dataThin <- data %>%
  select("id","institutionCode", "basisOfRecord",
        "scientificName","genus","specificEpithet", "country",
        "locality", "decimalLatitude","decimalLongitude")
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
                 stringsAsFactors=FALSE)

# assign columns to location in empty dataframe
df$taxon <- dataThin$scientificName
df$genus <- dataThin$genus
df$species <- dataThin$specificEpithet
df$latitude <- dataThin$decimalLatitude
df$longitude <- dataThin$decimalLongitude
df$databaseSource <- "midwestHerbarium " 
df$institutionCode <- dataThin$institutionCode
df$type <- "H"
df$uniqueID <- dataThin$id
df$sampleCategory <- dataThin$basisOfRecord
df$country <- dataThin$country
df$iso3 <- NA
df$localityInformation <- dataThin$locality


# actual code 
testLatLong <<- df %>%
  dplyr::select(c("uniqueID","latitude", "longitude")) %>%
  mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "") %>%
  mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "") %>%
  mutate(hasLatLong = hasLat & hasLong)

summariseErrors <- testLatLong %>%
  filter(hasLat == TRUE & hasLong ==FALSE | hasLat == FALSE & hasLong ==TRUE)

print(paste0("there are ", nrow(summariseErrors)," miss matach lat long pairs."))
write.csv(x = summariseErrors, file = paste0(base_dir,"/mismatchLatLong.csv"))


# write out the new dataframe 
write.csv(x = df, file = paste0(base_dir,"/refinedMidwestHerbarium.csv"))
