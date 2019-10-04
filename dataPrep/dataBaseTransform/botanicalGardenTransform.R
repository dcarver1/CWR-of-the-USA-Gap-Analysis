###
# reworking of botanical garden data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "D:/cwrOfNA/occurrence_data2019_05_29/botanicalGarden"


# Load in data 
csvPath <- paste0(base_dir,"/P1ABC GRIN PlantSearch match ALL.csv")
data <- data.table::fread(csvPath, header = TRUE)
# Select necessary columns from dataset 
dataThin <- data %>%
  select("Institution", "country_name", "CollectionType", 
         "ParentFULLTaxon_GRIN Global_2019 final", "ParentGenus",
         "ParentSpecies")
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
df$taxon <- dataThin$`ParentFULLTaxon_GRIN Global_2019 final`
df$genus <- dataThin$ParentGenus
df$species <- dataThin$ParentSpecies
df$latitude <- NA
df$longitude <- NA
df$databaseSource <- "BotanicialGarden"
df$institutionCode <- dataThin$Institution
df$type <- "G"
df$uniqueID <- NA  
df$sampleCategory <- dataThin$CollectionType
df$country <- dataThin$country_name
df$iso3 <- 
df$localityInformation <- NA

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
write.csv(x = df, file = paste0(base_dir,"/refinedBotanicalGarden.csv"))
