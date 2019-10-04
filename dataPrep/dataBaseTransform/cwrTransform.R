###
# reworking of wiews data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "D:/cwrOfNA/occurrence_data2019_05_29/cwr_occ/colin_list2"

# Load in data 
csvPath <- paste0(base_dir,"/colin_list2.csv")
data <- data.table::fread(csvPath, header = FALSE)
# Select necessary columns from dataset 
dataThin <- data %>%
  select("V2","V3","V12","V14","V15",
        "V41", "V69", "V75", "V81")
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
df$taxon <- dataThin$V41
df$genus <- NA
df$species <- NA
df$latitude <- dataThin$V75
df$longitude <- dataThin$V81
df$databaseSource <- dataThin$V3
df$institutionCode <- NA
df$type <- dataThin$V2
df$uniqueID <- dataThin$V12
df$sampleCategory <- NA
df$country <- NA
df$iso3 <- NA
df$localityInformation <- dataThin$V69

# Spilt name to get at genus and species 
#test <- df[1:100,]
df$name <- df$taxon
df <- tidyr::separate(data = df, "name",into =c('genus','spec','sub1','sub2','sub3'),sep=' ')
#View(df[1:1000,])

#Function to split full name into taxon/species              
setSpecies <- function(dataFrame){
  if(!is.na(dataFrame$sub1)){
    dataFrame$species <- paste(dataFrame$spec,dataFrame$sub1,
                               dataFrame$sub2,dataFrame$sub3, sep="_")
  }
  if(is.na(dataFrame$sub1)){
    dataFrame$species <- dataFrame$spec
  }
  return(dataFrame)
}
df4 <- setSpecies(df)
##
# for what ever reason the first clause of this funtion is not working. I'm leaving it for now because
# I dont think we actaully need a acurate species to do the analysis
##
#View(df4[1:1000,])
# remove all NA 
df6 <- str_remove_all(df4$species, 'NA') %>%
  str_remove_all("__")
#View(df6)  
df4$species <- df6
#View(test)

df4 <- subset(x = df4, select = -c(genus,spec,sub1,sub2,sub3) )
#View(df4[1:1000,])

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
write.csv(x = df4, file = paste0(base_dir,"/cwrOCC.csv"))
