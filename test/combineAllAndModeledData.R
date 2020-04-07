###
# attempt to join current allData with previous modeled date. 
# 20200401
# dan.carver@carverd.com
###
library(data.table)
library(dplyr)

## read in 
ad<-data.table::fread("D:/cwrNA/occurrence_data2019_05_29/combinedOccurance2020-03-18.csv",
                      header = TRUE)

## read in sheets review by colin 
gw <- read.csv("D:/cwrNA/occurrence_data2019_05_29/troubleshootingDuplicates/duplicatedWiews-Genesys_ckforDan.csv")

ar <- read.csv("D:/cwrNA/occurrence_data2019_05_29/troubleshootingDuplicates/allGsFrom2020-03-18Data_ckforDantoremove.csv")

### filter out values from ad based on the key value. 
gw1 <- gw %>% 
  dplyr::filter(action == "Remove")
dim(gw1)
#filter the full dataset 
ad1 <- ad[!ad$V1 %in% unique(gw1$V1),]

#repeat for second file 
ar1 <- ar %>%
  dplyr::filter(action == "Remove")
dim(ar1)
# filter full dataset 
ad2 <- ad1[!ad1$V1 %in% unique(ar1$V1),]
#drop v1 column 
ad2<- ad2[,-1]
names(ad2)
#write out the full occurrence dataset 
write.csv(x = ad2, file = "D:/cwrNA/occurrence_data2019_05_29/combinedOccurance2020-04-03.csv")

# write out the G dataset 
ad3 <- ad2[ad2$type == "G",]
dim(ad3)
write.csv(x = ad3, file = "D:/cwrNA/occurrence_data2019_05_29/gOccurance2020-04-03.csv")

allG <- ad[ad$type == "G", ]
nrow(allG)-nrow(ad3)

### generate a specieslist to rerun 
spl <- data.frame(matrix(nrow = 0,ncol = 1))
colnames(spl) <- "taxon"
spl <- gw %>% dplyr::select(taxon)
spl
spl2 <- ar %>% dplyr::select(taxon)
spl3 <- rbind(spl, spl2) %>%
  dplyr::distinct()
write.csv(x = spl3, file = "D:/cwrNA/occurrence_data2019_05_29/troubleshootingDuplicates/speciesToReRunSRSex.csv")



### nothing down here should need to be repeated. 
### work to detect points of issue 

# filter for G points 
gs <- ad %>%
  dplyr::filter(type=="G")
gs$dID <- duplicated(gs$uniqueID)
gs$dIC <- duplicated(gs$institutionCode)
g1 <- gs %>% dplyr::filter(dID == TRUE & dIC ==TRUE)

# occurrence with duplicated unique id and institution code. 
gDup <- g1 %>%
  dplyr::group_by(taxon)%>%
  dplyr::summarise(count = n())
# per taxon test for two duplicates 
sp <- unique(g1$taxon)
nW <- data.frame(matrix(nrow=0,ncol=21))
for(i in sp){
  q1 <- gs %>% 
    dplyr::filter(taxon == i, databaseSource == c("Genesys", "wiews"))%>%
    arrange(databaseSource)
  q1$dID <- duplicated(q1$uniqueID)
  q1$dIC <- duplicated(q1$institutionCode)
  q1$remove <- q1$dID == TRUE & q1$dIC == TRUE
  q1$latLong <- !is.na(q1$latitude) & !is.na(q1$longitude)
  nW <- rbind(nW,q1)
  print(i)
}
write.csv(x = nW, file = "D:/cwrNA/occurrence_data2019_05_29/duplicatedWiews-Genesys.csv")

# summarize what is to be removed. 
s1 <- nW %>%
  dplyr::filter(remove == TRUE)%>%
  dplyr::group_by(taxon,latLong)%>%
  dplyr::summarise(totals = n())
write.csv(x = s1, file = "D:/cwrNA/occurrence_data2019_05_29/valuesToRemoveWithLatLong.csv")


g2 <- left_join(gDup, gCount, by = "taxon")
g2$percent <- (g2$count / g2$total)*100
View(g2)
write.csv(x = g2, file = "D:/cwrNA/occurrence_data2019_05_29/duplicatedUniqueIDWithPercents.csv")

write.csv(x = gs, file = "D:/cwrNA/occurrence_data2019_05_29/allGsFrom2020-03-18Data.csv")

# work on a means of identifying what occurrence data was used in the modeling process
# summarize at the genus level 

# define run 
run <- "test20200203"
base <- "D:/cwrNA"
# all genera 
genera <- unique(ad$genus)
for(i in 1:length(genera)){
  g <- genera[i]
  sp <- unique(ad %>%
    dplyr::filter(genus == g)%>%
    dplyr::select(taxon))[,1]
  dfRaw <- data.frame(matrix(nrow = 0, ncol = 17))
  colnames(dfRaw) <- c("V1","taxon","genus", "species","latitude","longitude","databaseSource","institutionCode",
                       "type","uniqueID","sampleCategory","country","iso3","localityInformation", "biologicalStatus","collectionSource","finalOriginStat")
  dfNA <- data.frame(matrix(nrow = 0, ncol = 8))
  colnames(dfNA) <- c("taxon","latitude","longitude","type","databaseSource" ,"hasLat","hasLong","hasLatLong")
  dfNAWithID <- data.frame(matrix(nrow = 0, ncol=21))
  colnames(dfNAWithID) <- c("taxon","latitude","longitude","type","databaseSource",
                            "genus", "species", "institutionCode", "uniqueID", "sampleCategory","country",
                            "iso3","localityInformation","biologicalStatus", "collectionSource","finalOriginStat")
  for(s in sp){
    print(s)
    #pull in raw data 
    path <- paste0(base, "/gap_analysis/",genera[i], "/", s, "/", run, "/occurrences/rawData.csv")
    if(file.exists(path)){
      t1 <- read.csv(path)
      t1$latitude <- as.numeric(as.character(t1$latitude))
      t1$longitude <- as.numeric(as.character(t1$longitude))
      dfRaw <- rbind(dfRaw, t1)
    }
    #pull in data in NA 
    path <- paste0(base, "/gap_analysis/",genera[i], "/", s, "/", run, "/occurrences/rawDataForNA.csv")
    if(file.exists(path)){
      t2 <- read.csv(path)
      dfNA <- rbind(dfNA, t2)
      
    }
    # join to connect all data to NA data 
    d2 <- t2 %>% dplyr::mutate(ID = row_number())
    dfAll <- dplyr::left_join(d2, dfRaw, by = c("taxon" = "taxon",
                                                  "latitude" = "latitude",
                                                  "longitude" = "longitude",
                                                "type"="type",
                                                "databaseSource" = "databaseSource"))
    #drop values based on repeat ID 
    dfAll <- dfAll[!duplicated(dfAll$ID),]%>%
      dplyr::select("taxon","latitude","longitude","type","databaseSource",
"genus", "species", "institutionCode", "uniqueID", "sampleCategory","country",
"iso3","localityInformation","biologicalStatus", "collectionSource","finalOriginStat")
  dfNAWithID <- rbind(dfNAWithID, dfAll)
  }
  write.csv(x = dfRaw, file = paste0(base, "/gap_analysis/",genera[i],"/compiledRawData.csv"))
  write.csv(x = dfNA, file = paste0(base, "/gap_analysis/",genera[i],"/compiledNorthAmericanData.csv"))
  write.csv(x = dfNAWithID, file = paste0(base, "/gap_analysis/",genera[i],"/compiledNADataWithAttributes.csv"))
}


#### 20200402 
# we are going to work with older occurrence datasets 

# read in modeling data 
md <- data.table::fread("D:/cwrNA/runSummaries/allspeciesOccurrenceData2020-03-26.csv")
# read in current all data 
ad <- data.table::fread("D:/cwrNA/occurrence_data2019_05_29/combinedOccurance2020-04-01.csv", header = TRUE)
ad$latitude <- as.numeric(ad$latitude)
ad$longitude <- as.numeric(ad$longitude)
ad$dbSourceTemp <- ad$databaseSource
# attempt a join 
dd <- dplyr::left_join(md, ad, by=c("taxon"="taxon", "latitude"="latitude", 
                                    "longitude"="longitude"))
# because of duplicated lat long some records are regenerated 
# remote those records. 
di <- dd[!duplicated(dd$V1.x),]

d1 <- di %>% dplyr::select("taxon",
"latitude","longitude","type.x","dbSourceTemp",
"hasLat","hasLong","hasLatLong","iso3_check",
"StateTest","genus","species","institutionCode",
"uniqueID","sampleCategory","country","iso3",
"localityInformation","biologicalStatus",
"collectionSource","finalOriginStat")
colnames(d1) <- c("taxon",
                  "latitude","longitude","type","databaseSource",
                  "hasLat","hasLong","hasLatLong","iso3_check",
                  "StateTest","genus","species","institutionCode",
                  "uniqueID","sampleCategory","country","iso3",
                  "localityInformation","biologicalStatus",
                  "collectionSource","finalOriginStat")
write.csv(x = d1, file = "D:/cwrNA/runSummaries/modelDataWithSoruce2020-04-01.csv")
