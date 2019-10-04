###
# reworking of genesys data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "D:/cwrOfNA/occurrence_data2019_05_29/Genesys"

# Load in data 
csvPath <- paste0(base_dir,"/genesys-accessions-v18461d86381494a49a0faaa913f74c3da.csv")
data <- data.table::fread(csvPath, header = TRUE)
# Select necessary columns from dataset 
dataThin <- data %>%
  select("INSTCODE", "ACCENUMB", "GENUS", 
         "SPECIES", "SUBTAXA",
         "ORIGCTY","COLLSITE","DECLATITUDE","DECLONGITUDE", "SAMPSTAT" )
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
df$taxon <- NA
df$genus <- dataThin$GENUS
df$species <- NA
df$latitude <- dataThin$DECLATITUDE
df$longitude <- dataThin$DECLONGITUDE
df$databaseSource <- NA
df$institutionCode <- dataThin$INSTCODE
df$type <- "G"
df$uniqueID <- dataThin$ACCENUMB
df$sampleCategory <- dataThin$SAMPSTAT
df$country <- NA
df$iso3 <- dataThin$ORIGCTY
df$localityInformation <- dataThin$COLLSITE

# define species by combining species and subspecies categories 
df2 <- dataThin[1:100, ]


for(i in 1:nrow(dataThin)){
  if(dataThin$SUBTAXA[i]== ""){
    df$species[i] <- dataThin$SPECIES[i]
  }
  if(dataThin$SUBTAXA[i] != ""){
    df$species[i] <- paste(dataThin$SPECIES[i],dataThin$SUBTAXA[i], sep="_")
  }
}
# generate taxon by combining genus and species  
df$taxon <- paste0(df$genus,"_", df$species)


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
write.csv(x = df, file = paste0(base_dir,"/refinedGenesys.csv"))
