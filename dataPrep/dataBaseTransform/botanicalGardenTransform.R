###
# reworking of botanical garden data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)
library(naniar)
library(reshape2)

#set base dir
base_dir <- "F:/nrelD/cwrNA/occurrence_data2019_05_29/botanicalGarden"

# Load in data 
csvPath <- paste0(base_dir,"/P1ABC GRIN PlantSearch match ALL.csv")
data <- data.table::fread(csvPath, header = TRUE)
# Select necessary columns from dataset 
dataThin <- data %>%
  select("Institution",  "CollectionType", 
         "ParentFULLTaxon_GRIN Global_2019 final", "ParentGenus",
         "ParentSpecies","ParentInfraRank", "ParentInfraEpi",
         "ParentInfraRank2", "ParentInfraEpi2")
nr <- nrow(dataThin)
# construct species from species plus 4 interspecific columns 
dataThin <- dataThin %>% naniar::replace_with_na(replace = list(ParentSpecies="",ParentInfraRank ="",
                                                                ParentInfraEpi="", ParentInfraRank2="",
                                                                ParentInfraEpi2="" ))


# function for testing for before concatinating species. 
f = function(x){
  if(!is.na(x[,7])){
    x[,5] <- paste(x[,5], " ", x[,6], " ", x[,7])}
  else{}
  if(!is.na(x[,9])){
    x[,5] <- paste(x[,5], " ", x[,6], " ", x[,7], " ", x[,8], " ", x[,9])
  }
  return(x)
}


t2 <- data.frame()
for(i in 1:nr){
  t3 <- f(dataThin[i])
  t2 <- rbind(t2,t3)
}






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
df$taxon <- t2$`ParentFULLTaxon_GRIN Global_2019 final`
df$genus <- t2$ParentGenus
df$species <- t2$ParentSpecies 
df$latitude <- NA
df$longitude <- NA
df$databaseSource <- "BotanicialGarden"
df$institutionCode <- t2$Institution
df$type <- "G"
df$uniqueID <- NA  
df$sampleCategory <- t2$CollectionType
df$country <- NA
df$iso3 <- NA
df$localityInformation <- NA
df$biologicalStatus <- NA
df$collectionSource <- NA
df$finalOriginStat <- NA 

# pull in checkSynomyn function and apply it 
source(file="D:/cwrNA/src/dataPrep/dataBaseTransform/checkSynonymsFunction.R")
df2 <- checkSynonym(df)

# generate a list of missmatch lat long values 
testLatLong <<- df2 %>%
  dplyr::select(c("uniqueID","latitude", "longitude")) %>%
  mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "") %>%
  mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "") %>%
  mutate(hasLatLong = hasLat & hasLong)

# pull missmatched lat long values 
summariseErrors <- testLatLong %>%
  filter(hasLat == TRUE & hasLong ==FALSE | hasLat == FALSE & hasLong ==TRUE)
# write out summary 
print(paste0("there are ", nrow(summariseErrors)," miss matach lat long pairs."))
write.csv(x = summariseErrors, file = paste0(base_dir,"/mismatchLatLong.csv"))

# write out the new dataframe of refined data 
write.csv(x = df2, file = paste0(base_dir,"/refinedBotanicalGarden.csv"))

