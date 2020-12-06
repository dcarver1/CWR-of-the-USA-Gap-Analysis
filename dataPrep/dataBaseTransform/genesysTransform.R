###
# reworking of genesys data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "F:/nrelD/cwrNA/occurrence_data2019_05_29/Genesys"

# Load in data 
csvPath <- paste0(base_dir,"/genesys-accessions-v18461d86381494a49a0faaa913f74c3da.csv")
data <- data.table::fread(csvPath, header = TRUE)#87940    43 
# Select necessary columns from dataset 
dataThin <- data %>%
  select("INSTCODE", "ACCENUMB", "GENUS", 
         "SPECIES", "SUBTAXA",
         "ORIGCTY","COLLSITE","DECLATITUDE","DECLONGITUDE", "SAMPSTAT", "COLLSRC",
         "HISTORIC")
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
df$taxon <- NA
df$genus <- dataThin$GENUS
df$species <- dataThin$SPECIES
df$latitude <- dataThin$DECLATITUDE
df$longitude <- dataThin$DECLONGITUDE
df$databaseSource <- "Genesys"
df$institutionCode <- dataThin$INSTCODE
df$type <- NA
df$uniqueID <- dataThin$ACCENUMB
df$sampleCategory <- dataThin$SAMPSTAT
df$country <- NA
df$iso3 <- dataThin$ORIGCTY
df$localityInformation <- dataThin$COLLSITE
df$biologicalStatus <- NA
df$collectionSource <- dataThin$COLLSRC
df$finalOriginStat <- NA
df$historicTemp <- dataThin$HISTORIC
dim(df)

# define species by combining species and subspecies categories 
for(i in 1:nrow(dataThin)){
  if(dataThin$SUBTAXA[i]!= ""){
    df$species[i] <- paste(dataThin$SPECIES[i],dataThin$SUBTAXA[i], sep=" ")
  }
}

# generate taxon by combining genus and species  
df$taxon <- paste0(df$genus," ", df$species)


# SAMPSTATRemove non wild- exclude 300 and above (but include 999)
df <- df[df$sampleCategory %in% c(999,100,110,120, 130, 200, 999, NA),]
dim(df)

# COLLSRC- exclude 30, 40, 50
df <- df[!df$collectionSource %in% c(30, 40, 50),]
dim(df)

# exclude all data from USDA collections 
USDAcodes <- c("USA003" ,"USA004", "USA005" ,"USA016" ,"USA020",
"USA022", "USA026", "USA028", "USA029", "USA042" ,"USA047", "USA049",
"USA074", "USA108", "USA133", "USA148", "USA151", "USA167", "USA176",
 "USA390", "USA955", "USA956", "USA970", "USA971", "USA995")

df <- df[!df$institutionCode %in% USDAcodes,]
dim(df)


# test for historic sample 
for(i in 1:nrow(df)){
  if(dataThin$HISTORIC[i] == TRUE){
    df$type[i] <- "H"
  }else{
    df$type[i] <- "G"
  }
}


# pull in checkSynomyn function and apply it 
source(file="D:/cwrNA/src/dataPrep/dataBaseTransform/checkSynonymsFunction.R")
df <- checkSynonym(df)
dim(df)


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


