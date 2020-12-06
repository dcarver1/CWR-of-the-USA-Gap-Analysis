###
# reworking of idigbio data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "F:/nrelD/cwrNA/occurrence_data2019_05_29/Midwest_herbaria/SymbOutput_2019-05-28_151547_DwC-A"

# Load in data 
csvPath <- paste0(base_dir,"/occurrences.csv")
data <- data.table::fread(csvPath, header = TRUE)

# # sumarries data with lat long by year 
# o1 <- data %>%
#   dplyr::filter(!is.na(decimalLatitude))%>%
#   dplyr::filter(year == 1900)
# 
# %>%
#   dplyr::group_by(year)%>%
#   dplyr::summarise(count = n())
# View(o1)
# write.csv(o1,file = paste0(base_dir, "/1900examples.csv"))
# 

# Select necessary columns from dataset 
dataThin <- data %>%
  select("id","institutionCode", "basisOfRecord","occurrenceID",
        "scientificName","genus","specificEpithet", "taxonRank" , "infraspecificEpithet", 
        "country","stateProvince","county", "municipality", "locality",
        "decimalLatitude","decimalLongitude")
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
df$taxon <- dataThin$scientificName
df$genus <- dataThin$genus
df$species <- NA
df$latitude <- dataThin$decimalLatitude
df$longitude <- dataThin$decimalLongitude
df$databaseSource <- "midwestHerbarium " 
df$institutionCode <- dataThin$institutionCode
df$type <- "H"
df$uniqueID <- dataThin$id
df$sampleCategory <- dataThin$basisOfRecord
df$country <- dataThin$country
df$iso3 <- NA
df$localityInformation <- NA
df$biologicalStatus <- NA
df$collectionSource <- NA
df$finalOriginStat <- NA 

# Locality in formation, cacatanate "country","stateProvince","county", "municipality", "locality",
d2 <- dataThin %>% tidyr::unite("local2" , country,stateProvince,county, municipality, locality, sep = " -- ")
df$localityInformation <- d2$local2

# Species - concatenate "genus","specificEpithet", "taxonRank" , "infraspecificEpithet"
d3 <- dataThin %>% tidyr::unite("fullSpecies", specificEpithet, taxonRank , infraspecificEpithet, sep = " ")
df$species <- d3$fullSpecies

# pull in checkSynomyn function and apply it 
source(file="D:/cwrNA/src/dataPrep/dataBaseTransform/checkSynonymsFunction.R")
df <- checkSynonym(df)


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
