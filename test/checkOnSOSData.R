### 
# Check occurrence data against the current list of SOS accessions, see how many are missing lat long
# carverd@colostate.edu
# 20200720
###
library(tidyverse)
library(data.table)
# read in USDA data 
d1 <- data.table::fread("F:/nrelD/cwrNA/occurrence_data2019_05_29/USDA_NPGS_GRINGlobal/USDA_CWRofUSA.csv",
                        header = TRUE)
head(d1)
View(d1)

# read in blm data 
b1 <- data.table::fread("F:/nrelD/usda/grin/blmSOSdata_20200609.csv",
                        header = TRUE)
View(b1)
# read in species list 
s1 <- read.csv("F:/nreld/cwrNA/speciesList/CWRoftheUSA_synonyms20191114.csv", header = TRUE)
head(s1)



# read in new SOS data from renee
sos1 <- read.csv("F:/nrelD/cwrNA/occurrence_data2019_05_29/checkAgainstSOSData/SOSAccessionsInGrin20200721.csv", header=TRUE)

# join to full USDA GRIN pull 
join2 <- dplyr::left_join(x = d1, y = sos1, by = "accession_number",Keep = TRUE)
View(join2)
join2a <- join2 %>%
  dplyr::filter(!is.na(Taxon.y))

%>%
  dplyr::filter(is.na(latitude))
View(join2a)
# join on blm data 
join3 <- dplyr::left_join(x = join2a, y = b1, by =c("plant_name" = "ACC_NUM"))
View(join3)

#occurrence to alter 
alter <- join3[!is.na(join3$LATITUDE_DECIMAL),]
noInformation <- join3[is.na(join3$LATITUDE_DECIMAL),]

# double check for join on topname 
t2 <- dplyr::left_join(x = noInformation, y = b1, by = c("topname" = "ACC_NUM"))


# summarise edits 
a2 <- alter %>% 
  dplyr::group_by(Taxon.x) %>%
  dplyr::summarise(count = n())

a2$rmSpec <- a2$Taxon.x %in% rmSpec
write.csv(x = a2, file = "F:/nrelD/cwrNA/occurrence_data2019_05_29/checkAgainstSOSData/numberOfLatLongUpdatesReplace.csv")

View(a2)

### replace the 94 lat long values in the final occurrence dataset 
base_dir <- "F:/nrelD/cwrNA"
occ_dir <- paste0(par_dir, "/occurenceData")
occData <- data.table::fread(paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2020-07-21.csv"),
                             header = TRUE)
occData <- occData[,2:ncol(occData)]
dim(occData)
oc1 <- occData[!occData$uniqueID %in% alter$accession_number,]
oc2 <- occData[occData$uniqueID %in% alter$accession_number,]

for(i in 1:nrow(oc2)){
  t1 <- alter[alter$accession_number == oc2$uniqueID[i],]
  oc2$latitude[i] <- t1$LATITUDE_DECIMAL
  oc2$longitude[i] <- t1$LONGITUDE_DECIMAL
}

ocData <- rbind(oc1, oc2)
View(ocData)



#rename database occurrences 
currentNames <- c("cwrofnorthamericabook","GBIF MO",
                  "GBIF Observations", "Global Crop Diversity Trust 2019b  (cwrocc)",
                  "midwestHerbarium", "Reilley")
newNames <- c(
  "Greene et al. 2019",
  "GBIF 2019",
  "GBIF 2019",  
  "Global Crop Diversity Trust 2019b  (Cwr Occ)",
  "Midwest Herbaria 2019",
  "Greene et al. 2019"
)

t1 <- ocData[ocData$databaseSource %in% currentNames, ]
t2 <- ocData[!ocData$databaseSource %in% currentNames, ]

temp3 <- t1

for(i in 1:length(currentNames)){
             t3 <- t1[t1$databaseSource == currentNames[i],]
             t3a <- t1[!t1$databaseSource == currentNames[i],]
             t3$databaseSource <- newNames[i]
             t1 <- rbind(t3a, t3)
}

ocD <- rbind(t1, t2)


write.csv(x = ocD, paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2020-07-21a.csv"))

dsCount <- ocD %>%
  dplyr::group_by(databaseSource) %>%
  dplyr::summarise(count = n())
View(dsCount)
write.csv(dsCount, paste0(base_dir, "/occurrence_data2019_05_29/occurrenceCountsPerDataSource20200721.csv"))


library(dplyr)
dplyr::left_join()

j1 <- dplyr::left_join(x = d1, y = b1, by = c("topname" = "ACC_NUM"))

j2 <- j1[!is.na(j1$NAME),]
j2 <- j2 %>% dplyr::filter(nchar(topname)> 0)
View(j2)

j3 <- j2 %>%
  dplyr::filter(is.na(latitude))
View(j3)

countSum <- j3 %>%
  dplyr::group_by(Taxon) %>%
  dplyr::summarise(totalWithLatLongUpdate = n())

countSum
View(countSum)


# generate counts of total records and G values for these species 
base_dir <- "F:/nrelD/cwrNA"
occ_dir <- paste0(par_dir, "/occurenceData")
occData <- data.table::fread(paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2020-04-07.csv"),
                             header = TRUE)
occData <- occData[,2:ncol(occData)]

d3 <- occData %>% 
  dplyr::filter(taxon %in% countSum$Taxon) %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::filter(!is.na(longitude))
dim(d3)

d4 <- d3 %>% 
  dplyr::group_by(taxon, type)%>%
  dplyr::summarise(count = n())
View(d4)

d5 <- dplyr::left_join(countSum, d4, by = c("Taxon" = "taxon"))
View(d5)


## test if the species has been drop
rmSpec <- c("Phaseolus acutifolius","Phaseolus leptostachyus","Elymus elymoides","Leymus mollis","Phaseolus maculatus","Hordeum jubatum","Helianthus petiolaris","Ribes sanguineum","Phaseolus polystachios","Prunus serotina","Elymus trachycaulus","Hordeum brachyantherum","Ribes roezlii","Rubus hispidus","Ribes hudsonianum","Helianthus nuttallii","Helianthus pauciflorus","Humulus lupulus","Allium geyeri","Ribes oxyacanthoides","Fragaria x ananassa","Helianthus occidentalis","Fragaria virginiana","Elymus lanceolatus","Fragaria vesca","Helianthus niveus","Helianthus praecox","Prunus fasciculata","Ribes malvaceum","Rubus arcticus","Vitis rotundifolia","Fragaria chiloensis","Ribes aureum","Acer saccharum","Allium victorialis","Elymus stebbinsii","Helianthus debilis","Ipomoea ternifolia","Lactuca tatarica","Prunus ilicifolia","Prunus pumila","Ribes californicum","Rubus idaeus","Saccharum brevibarbe","Vitis aestivalis","Vitis cinerea","Zizania aquatica","Zizania palustris", "Allium schoenoprasum","Elymus glabriflorus",
            "Elymus glaucus","Ipomoea cordatotriloba","Juglans major","Juglans microcarpa","Leymus salina","Prunus virginiana","Ribes cereum","Rubus ursinus","Tripsacum dactyloides","Vaccinium crassifolium","Vaccinium erythrocarpum","Vaccinium ovalifolium"
)
d5$removedSpecies <- d5$Taxon %in% rmSpec
View(d5)
write.csv(x = d5, file = "F:/nrelD/cwrNA/occurrence_data2019_05_29/checkAgainstSOSData/countsOfNoneLatLongValues.csv")

