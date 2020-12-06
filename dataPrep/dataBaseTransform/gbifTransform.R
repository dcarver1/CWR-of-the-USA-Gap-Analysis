###
# reworking of wiews data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)
library(readr)
library(sqldf)
library(MazamaSpatialUtils)
#set base dir
base_dir <- "F:/nrelD/cwrNA/occurrence_data2019_05_29/GBIF"

# Load in data 
header <- read.csv(paste0(base_dir,"/wetransfer-51b480/head.csv"), header = TRUE, fileEncoding = 'UTF-16',sep = '\t')

# g2 <- data.table::fread(input = "F:/nrelD/cwrNA/occurrence_data2019_05_29/GBIF/occurrences_may2019.csv")

csvPath <- paste0(base_dir,"/cleanGBIF20191119.csv")


# ### attempting a join to get at the date information. 
# dim(df2)
# dim(g2)
# vals <- as.numeric(df2$gbifID)
# 
# g2a <- g2[as.numeric(g2$V1) %in% vals,]
# g2a$gbifID1 <- as.numeric(g2a$V1)
# df2$gbifID1 <- vals
# d3 <- dplyr::left_join(x = df2, y = g2a, by= "gbifID1")
# 
# d3a <- d3[!is.na(d3$decimalLatitude),]
# 
# d4 <- d3a %>%
#   dplyr::group_by(V28)%>%
#   dplyr::summarise(count = n())
# 
# View(d4)
# 
# write.csv(x = d4, file = paste0(base_dir,"/yearcount.csv"))

# #old dataset 
# csv2 <- paste0(base_dir,"/occurrences_may2019.csv")
# # contain 46 columns so if need be we could apply header to this database with a few assumptions
# dOld <- data.table::fread(csv2, header= FALSE)
# #fread -- will not read in utf-16 
# data <- data.table::fread(csvPath, header = FALSE,)
# #readr - reads in the information but does writes it all as NA 
# ds <- readr::read_tsv(file = csvPath, n_max = 500,col_names = names(header))
# # rsql - still based on read.table so this did not work 
# df3 <- sqldf::read.csv.sql(file = csvPath, header = FALSE, sep ="\t", nrows = 1000)


# ### iterative process using read.csv https://stackoverflow.com/questions/9352887/strategies-for-reading-in-csv-files-in-pieces/30403877#30403877
# # establishing a connection to the file  
# con <- file(csvPath, "r", encoding = 'UTF-16')
# #close(con)
# # create a dataframe to bind outputs to 
# df2 <- data.frame()
# 
# rows <- 10000
# x =1 
# while(rows ==10000){
#   df <- read.csv(con,header = FALSE,fileEncoding = 'UTF-16',sep = '\t',nrows = 10000)
#   rows <- nrow(df)
#   print(rows)
#   colnames(df) <- names(header)
#   dataThin <- df %>%
#     dplyr::select("gbifID", "genus", "species", "infraspecificEpithet", "taxonRank",
#                   "countryCode", "locality", "stateProvince", "decimalLatitude", 
#                   "decimalLongitude", "basisOfRecord", "institutionCode" )
#   df2 <- rbind(df2, dataThin)
#   x = x+1 
#   print(x)
# }
# # write out df2 as new GBIF dataset 
# write.csv(x = df2, file = paste0(base_dir, '/cleanGBIF20191119.csv'))
# dim(df2)

### read in clean data 
df2 <- data.table::fread(input = csvPath, header = TRUE)

# send colin a list of unique Basis of records so he can filter 
instituteCodes <- sort(unique(df2$institutionCode))
write.csv(x = instituteCodes, file = paste0(base_dir, '/uniqueInstituteCodesGBIF20191120.csv'))
# excluse values based on colins recommidation 
exclude <- read.csv(paste0(base_dir, '/uniqueInstituteCodesGBIF20191120_ck.csv')) %>%
  dplyr::filter(exclude=="Y")

df3 <- df2[!df2$institutionCode %in% exclude$x,]
dim(df3)
# delete basis of records == 'fossil specimen'
df3 <- df3[df3$basisOfRecord != "FOSSIL_SPECIMEN",]
dim(df3)
# replace ISO2 with ISO3; lots of steps but it seems to be working 
naCC <- df3 %>%
  dplyr::filter(is.na(countryCode))
nonNACC <- df3 %>% 
  dplyr::filter(!is.na(countryCode))
iso2s <- nonNACC %>%
  dplyr::filter(nchar(countryCode) == 2)
iso2s$countryCode <- MazamaSpatialUtils::iso2ToIso3(iso2s$countryCode)
badIso2 <- nonNACC %>%
  dplyr::filter(nchar(countryCode)>2)
badIso2$countryCode <- NA 

df4 <- rbind(naCC, iso2s, badIso2)

# remove values based on lat long limits 
nolatLong <- df4 %>%
  filter(is.na(decimalLatitude) | is.na(decimalLongitude))
latLong <- df4 %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  filter(decimalLatitude > 10)%>%
  filter(decimalLongitude < -50)

df5 <- rbind(nolatLong, latLong)


# construct Locality 
df5 <- tidyr::unite(data = df5, 'local2', stateProvince, locality, sep = " -- ")

### construct species -- this process drops some rows where taxonRank is not species, subspecies, or varity 
# I beleive that most of those are data errors anyway so I'm ok with it. 
# for species only data 
sp1 <- df5 %>%
  filter(taxonRank == "SPECIES")
sp1$taxon <- sp1$species
sp1 <- sp1 %>% tidyr::separate(col = species, c("genus1", "species1"), sep = " " )
sp1$species <- sp1$species1

# for subsp and var 
sp2 <- df5 %>%
  filter(taxonRank %in% c("SUBSPECIES", "VARIETY"))
  
sp2$taxonRank <- gsub(pattern = "SUBSPECIES", replacement = "subsp.",x = sp2$taxonRank)
sp2$taxonRank <- gsub(pattern = "VARIETY", replacement = "var.",x = sp2$taxonRank)

sp2 <- sp2 %>% tidyr::separate(col = species, c("genus1", "species1"), sep = " " )
sp2$species <- paste0(sp2$species1, " ", sp2$taxonRank, " ", sp2$infraspecificEpithet)

sp2$taxon <- paste0(sp2$genus, " ", sp2$species)

# join the two datasets 
df5 <- rbind(sp1, sp2)

# construct taxon 





nr <- nrow(df5)
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
df$taxon <- df5$taxon
df$genus <- df5$genus
df$species <- df5$species
df$latitude <- df5$decimalLatitude
df$longitude <- df5$decimalLongitude
df$databaseSource <- "GBIF"
df$institutionCode <- df5$institutionCode
df$type <- NA
df$uniqueID <- as.factor(df5$gbifID)
df$sampleCategory <- df5$basisOfRecord
df$country <- NA
df$iso3 <- df5$countryCode
df$localityInformation <- NA
df$biologicalStatus <- NA
df$collectionSource <- NA
df$finalOriginStat <- NA 


# construct type 
# Add type field. Make type = G when basisofrecord = living specimen; otherwise H
h<- df %>%
  filter(sampleCategory != "LIVING_SPECIMEN")
h$type <- "H"
g <- df %>%
  filter(sampleCategory == "LIVING_SPECIMEN")
g$type <- "G"

df <- rbind(h,g)

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
write.csv(x = df, file = paste0(base_dir,"/refinedGBIF.csv"))

