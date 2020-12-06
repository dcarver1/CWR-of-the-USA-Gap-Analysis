###
# reworking of idigbio data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "F:/nrelD/cwrNA/occurrence_data2019_05_29/WIEWS"

# Load in data 
csvPath <- paste0(base_dir,"/COLIN_GENUS.csv")
data <- data.table::fread(csvPath, header = TRUE) # 1516974      21   fill = TRUE just ends rsesion
## issues with read table... sticking with fread for now. 
# data2 <- read.table(csvPath, sep = "\t",header = TRUE, fill=TRUE) # 753805     21 line 376281 did not have 21 elements

#View(data)
# Select necessary columns from dataset 
dataThin <- data %>%
  dplyr::select("Country of origin (ISO3)", "Country of origin", "Holding institute code",
        "Accession number","Taxon","Latitude of collecting site (decimal degrees format)",
        "Longitude of collecting site (decimal degrees format)", "Type of germplasm storage",
        "Source of information", "Collecting/acquisition source", "Biological status","Source of information" )

# select out FAO-WIEWS from column 21 due to over lap with other databases 
# Use biological Status to select "", "100) Wild" ,"200) Weedy"   
dataThin <- dataThin %>%
  filter(`Source of information` == "FAO-WIEWS" |`Source of information` == "") %>%
  filter(`Biological status` == "100) Wild" | `Biological status` == "200) Weedy" | `Biological status` == "")

# filter for value based on preservation status 
dataThin <- dataThin[dataThin$`Type of germplasm storage` %in% c("20) Field","13) Seed long-term", "","12) Seed medium-term"),]

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
df$latitude <- dataThin$`Latitude of collecting site (decimal degrees format)`
df$longitude <- dataThin$`Longitude of collecting site (decimal degrees format)`
df$databaseSource <- "wiews"
df$institutionCode <- dataThin$`Holding institute code`
df$type <- "G"
df$uniqueID <- dataThin$`Accession number`
df$sampleCategory <- dataThin$`Type of germplasm storage`
df$country <- NA
df$iso3 <- NA
df$localityInformation <- dataThin$`Collecting/acquisition source`
df$biologicalStatus <- dataThin$`Biological status`
df$collectionSource <- dataThin$`Collecting/acquisition source`
df$finalOriginStat <- NA 

# pull in checkSynomyn function and apply it 
source(file="D:/cwrNA/src/dataPrep/dataBaseTransform/checkSynonymsFunction.R")
df <- checkSynonym(df)

# Spilt name to get at genus and species 
#test <- df[1:100,]
df$name <- df$taxon
df <- tidyr::separate(data = df, "name",into =c('genus','spec','sub1','sub2','sub3', 'sub4'),sep=' ')
#View(df)


#Function to split full name into taxon/species              
setSpecies <- function(dataFrame){
  if(!is.na(dataFrame$sub1)){
    dataFrame$species <- paste(dataFrame$spec,dataFrame$sub1,
                               dataFrame$sub2, sep="_")
  }
  if(is.na(dataFrame$sub1)){
    dataFrame$species <- dataFrame$spec
  }
  return(dataFrame)
}
df4 <- setSpecies(df)

# remove all NA 
df6 <- str_remove_all(df4$species, 'NA') %>%
  str_remove_all("__")
df4$species <- df6
df4 <- subset(x = df4, select = -c(spec,sub1,sub2) )

# test for mis matched latlong values 
testLatLong <- df4 %>%
  dplyr::select(c("uniqueID","latitude", "longitude")) %>%
  mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "") %>%
  mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "") %>%
  mutate(hasLatLong = hasLat & hasLong)

summariseErrors <- testLatLong %>%
  filter(hasLat == TRUE & hasLong ==FALSE | hasLat == FALSE & hasLong ==TRUE)

print(paste0("there are ", nrow(summariseErrors)," miss matach lat long pairs."))
write.csv(x = summariseErrors, file = paste0(base_dir,"/mismatchLatLong.csv"))


# write out the new dataframe 
write.csv(x = df4, file = paste0(base_dir,"/refinedWiews.csv"))




