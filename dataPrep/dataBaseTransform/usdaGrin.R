###
# reworking of idigbio data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "F:/nrelD/cwrNA/occurrence_data2019_05_29/USDA_NPGS_GRINGlobal"

# Load in data 
csvPath <- paste0(base_dir,"/USDA_CWRofUSA.csv")
data <- data.table::fread(input = csvPath,header = TRUE)
# Select necessary columns from dataset 
dataThin <- data %>%
  select("Taxon","accession_number","status_code", "site_short_name","improvement_status_code",
         "country", "latitude","longitude","formatted_locality")
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
df$latitude <- dataThin$latitude
df$longitude <- dataThin$longitude
df$databaseSource <- "USDA_NPGS_GRINGlobal" 
df$institutionCode <- dataThin$site_short_name
df$type <- NA
df$uniqueID <- dataThin$accession_number
df$sampleCategory <- as.character(dataThin$status_code)
df$country <- dataThin$country
df$iso3 <- NA
df$localityInformation <- dataThin$formatted_locality
df$biologicalStatus <- dataThin$improvement_status_code
df$collectionSource <- NA
df$finalOriginStat <- NA 

# pull in checkSynomyn function and apply it 
source(file="D:/cwrNA/src/dataPrep/dataBaseTransform/checkSynonymsFunction.R")
df <- checkSynonym(df)

# applying G or H value based on status category 

for(i in 1:nrow(df)){
  if(df$sampleCategory[i] =="INACTIVE"){
    df$type[i] <- "H"
  }else{
    df$type[i] <- "G"
  }
}

# filter out for only wild and unknow locations 
df <- df[df$biologicalStatus %in% c("WILD","UNCERTAIN",""),]


# Spilt name to get at genus and species 
#test <- df[1:100,]
df$name <- df$taxon
df <- tidyr::separate(data = df, "name",into =c('genus','spec','sub1','sub2'),sep=' ')

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

# # Set type based on value in status code 
# setType <- function(dataFrame){
#   if(is.na(dataFrame$sampleCategory)){
#     dataFrame$type <- "G"
#   }else(
#     if(dataFrame$sampleCategory == "INACTIVE"){
#       dataFrame$type <- "H"
#     }else{
#       dataFrame$type <- "G"
#     }
#       
#   )
#   
#   return(dataFrame)
# }
# 
# df4 <- setType(df4)
df4 <- subset(x = df4, select = -c(spec,sub1,sub2) )

testLatLong <<- df4 %>%
  dplyr::select(c("latitude", "longitude")) %>%
  mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "") %>%
  mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "") %>%
  mutate(hasLatLong = hasLat & hasLong)

summariseErrors <- testLatLong %>%
  filter(hasLat == TRUE & hasLong ==FALSE | hasLat == FALSE & hasLong ==TRUE)

print(paste0("there are ", nrow(summariseErrors)," miss matach lat long pairs."))


# write out the new dataframe 
write.csv(x = df4, file = paste0(base_dir,"/refinedUSDAGrin.csv"))
