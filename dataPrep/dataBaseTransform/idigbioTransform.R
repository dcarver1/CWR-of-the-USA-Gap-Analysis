###
# reworking of idigbio data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "D:/cwrOfNA/occurrence_data2019_05_29/idigbio"

# Load in data 
csvPath <- paste0(base_dir,"/occurrence.csv")
data <- data.table::fread(csvPath, header = TRUE)
# Select necessary columns from dataset 
dataThin <- data %>%
  select("coreid","dwc:basisOfRecord", "gbif:canonicalName",
        "dwc:country","idigbio:isoCountryCode","dwc:genus", "idigbio:geoPoint")
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
df$taxon <- dataThin$`gbif:canonicalName`
df$genus <- dataThin$`dwc:genus`
df$species <- NA
df$latitude <- NA
df$longitude <- NA
df$databaseSource <- "idigbio " 
df$institutionCode <- NA
df$type <- "H"
df$uniqueID <- dataThin$coreid
df$sampleCategory <- dataThin$`dwc:basisOfRecord`
df$country <- dataThin$`dwc:country`
df$iso3 <- dataThin$`idigbio:isoCountryCode`
df$localityInformation <- NA

# define species by combining species and subspecies categories 
df$taxon2 <- df$taxon
df <- tidyr::separate(data = df, 'taxon2', c("Genus", "Species", "middle",
                                                                 "subSpe", "subSpe2",sep=" "))


# for(i in 1:10){
#   if(is.na(ele2$middle[i])){
#     df$species[i] <- ele2$Species[i]
#   }
#   if(!is.na(ele2$middle[i])){
#     df$species[i] <- paste(ele2$Species[i],ele2$middle[i],
#                            ele2$subSpe[i],ele2$subSpe2[i], sep="_")
#   }
# }

#rewriting this with no for loop              
setSpecies <- function(dataFrame){
  if(!is.na(dataFrame$middle)){
    dataFrame$species <- paste(dataFrame$Species,dataFrame$middle,
                               dataFrame$subSpe,dataFrame$subSpe2, sep="_")
  }
  if(is.na(dataFrame$middle)){
    dataFrame$species <- dataFrame$Species
  }
  return(dataFrame)
}
df4 <- setSpecies(df)

# split out the lat long data 
df6 <- str_remove_all(dataThin$`idigbio:geoPoint`, '"\"') %>%
  str_remove_all("lat:") %>%
  str_remove_all("lon:") %>%
  str_remove_all("\\{|\\}") %>%
  as.data.frame()
colnames(df6)<- "v1"

df6 <- separate(data = df6,col = "v1",into = c("lat","long"),sep = ",")
df6 
df4$latitude <- df6$lat
df4$longitude <- df6$long
#drop extra columns 
df4 <- subset(df4, select = -c(Genus,Species,middle,subSpe,
                             subSpe2))

df4<-subset(df4,select = -c(ncol(df4)))
head(df4)


# actual code 
testLatLong <<- df4 %>%
  dplyr::select(c("uniqueID","latitude", "longitude")) %>%
  mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "") %>%
  mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "") %>%
  mutate(hasLatLong = hasLat & hasLong)

summariseErrors <- testLatLong %>%
  filter(hasLat == TRUE & hasLong ==FALSE | hasLat == FALSE & hasLong ==TRUE)

print(paste0("there are ", nrow(summariseErrors)," miss matach lat long pairs."))
write.csv(x = summariseErrors, file = paste0(base_dir,"/mismatchLatLong.csv"))


# write out the new dataframe 
write.csv(x = df4, file = paste0(base_dir,"/refinedIdigBio.csv"))
