###
# reworking of idigbio data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "D:/cwrOfNA/occurrence_data2019_05_29/WIEWS/wiews"

# Load in data 
csvPath <- paste0(base_dir,"/colin_wiews.csv")
data <- data.table::fread(csvPath, header = FALSE)

# names of columns 
cName <- c("Year", "Country ISO3 code",	"Country name",
          "Holding institute code",	"Holding institute name",
          "Accession number",	"Taxon",	"Acquisition date (YYYY/MM)",
          "Country of origin (ISO3)",	"Country of origin",
          "Biological status",	"Genebank(s) holding safety duplications - code",
          "Genebank(s) holding safety duplications",	"Latitude of collecting site (decimal degrees format)",
          "Longitude of collecting site (decimal degrees format)",
          "Collecting/acquisition source",	"Type of germplasm storage",
          "Status under the Multilateral System",	'Data owner',	"Data owner details",
          "Source of information")
# apply names to data 
colnames(data)<-cName
#View(data)
# Select necessary columns from dataset 
dataThin <- data %>%
  dplyr::select("Country ISO3 code","Country name", "Holding institute code",
        "Accession number","Taxon","Latitude of collecting site (decimal degrees format)",
        "Longitude of collecting site (decimal degrees format)", "Type of germplasm storage",
        "Source of information", "Collecting/acquisition source")
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
df$taxon <- dataThin$Taxon
df$genus <- NA
df$species <- NA
df$latitude <- dataThin$`Latitude of collecting site (decimal degrees format)`
df$longitude <- dataThin$`Longitude of collecting site (decimal degrees format)`
df$databaseSource <- dataThin$`Source of information` 
df$institutionCode <- dataThin$`Holding institute code`
df$type <- "G"
df$uniqueID <- dataThin$`Accession number`
df$sampleCategory <- dataThin$`Type of germplasm storage`
df$country <- dataThin$`Country name`
df$iso3 <- dataThin$`Country ISO3 code`
df$localityInformation <- dataThin$`Collecting/acquisition source`

# Spilt name to get at genus and species 
#test <- df[1:100,]
df$name <- df$taxon
df <- tidyr::separate(data = df, "name",into =c('genus','spec','sub1','sub2'),sep=' ')
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
#View(df6)  
df4$species <- df6
#View(head(df4))

df4 <- subset(x = df4, select = -c(spec,sub1,sub2) )
#View(df4)


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
write.csv(x = df4, file = paste0(base_dir,"/refinedWiews.csv"))
