###
# reworking of wiews data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "D:/cwrOfNA/occurrence_data2019_05_29/GBIF"

# Load in data 
csvPath <- paste0(base_dir,"/occurrences_may2019.csv")
data <- data.table::fread(csvPath, header = FALSE)
# Select necessary columns from dataset 
dataThin <- data %>%
  select("V1","V9", "V10","V12",
        "V11","V14","V15",
        "V17","V18", "V31")
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
df$taxon <- 'NA'
df$genus <- dataThin$V9
df$species <- NA
df$latitude <- dataThin$V17
df$longitude <- dataThin$V18
df$databaseSource <- NA
df$institutionCode <- NA
df$type <- NA
df$uniqueID <- dataThin$V1
df$sampleCategory <- dataThin$V31
df$country <- dataThin$V14
df$iso3 <- NA
df$localityInformation <- dataThin$V15
df$extra <- dataThin$V10
df$extra1 <- dataThin$V11
df$extra2 <- dataThin$V12

# split the taxon name into two on space 
df <- tidyr::separate(data = df,extra,c("gen", "spec1","spec2"))


test1 <- df[1:10,]


# R function
f = function(x) {
  if(x[17] == "SPECIES"){
    x[1] <- x[15]
  }else{
    x[1] <- paste0(x[15],"_subsp._",x[16])
  }
  #print(x[1])
}

t2 <- apply(df, 1, f)
install.packages("beepr")
library(beepr)
beep()
#add t2 to species column in df
df$species <- t2 

# remove unwanted columns 
df <- subset(x = df, select = -c(14:17) )



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

# The species column is not quite right. This is ok for now because we're really most concerned with 
# taxon/genus 
# Will probably need to fix this but I want to wait till after I get some feedback from colin about
# database structure. 


# write out the new dataframe 
write.csv(x = df, file = paste0(base_dir,"/refinedGBIF.csv"))
