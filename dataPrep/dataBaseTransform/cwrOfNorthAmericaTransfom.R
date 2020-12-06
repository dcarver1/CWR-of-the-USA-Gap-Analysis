###
# reworking of botanical garden data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "F:/nrelD/cwrNA/occurrence_data2019_05_29/cwrofnorthamericabook"

# Load in data 
csvPath <- paste0(base_dir,"/CWRofNAmerica_dataextras.csv")
data <- data.table::fread(csvPath, header = TRUE)
# Select necessary columns from dataset 
dataThin <- data %>%
  select("id", "Taxon","Type","Source",
         "lat", "lon", "final_cult_stat")
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
df$taxon <- dataThin$Taxon
df$genus <- NA
df$species <- NA
df$latitude <- dataThin$lat
df$longitude <- dataThin$lon
df$databaseSource <- "cwrofnorthamericabook"
df$institutionCode <- dataThin$Source
df$type <- dataThin$Type
df$uniqueID <- dataThin$id  
df$sampleCategory <- NA
df$country <- NA
df$iso3 <- NA
df$localityInformation <- NA
df$biologicalStatus <- dataThin$final_cult_stat
df$collectionSource <- NA
df$finalOriginStat <- NA 

# determine Genus and species from taxon 
test1 <- tidyr::separate(data = df,taxon,into=c("genus","species","sep1","var1"),sep="_")
#create new column to populate 
test1$fullSpecies <- NA 
for(i in 1:nrow(test1)){
  if(is.na(test1$var1[i])){
    test1$fullSpecies[i] <- test1$species[i]
  }
  if(!is.na(test1$var1[i])){
    test1$fullSpecies[i] <- paste(test1$species[i],test1$sep1[i],test1$var1[i], sep="_")
  }
}
#set genus and species in final DF 
df$genus <- test1$genus
df$species <- test1$fullSpecies

# replace _ with " " on taxon 
df$taxon <- gsub(pattern = "_", replacement = " ",x = df$taxon)


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
write.csv(x = df, file = paste0(base_dir,"/refinedcwrOfNABook.csv"))

