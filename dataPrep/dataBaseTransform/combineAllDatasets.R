###
# The goal of this script is to compile all the csv from individual sources into
# a single element. 
# Not sure if this is really the beset structure to work with but I'm going to try
# it and see what happens 
### 

library(tidyverse)
library(data.table)


base_dir <- "D:/cwrNA/occurrence_data2019_05_29"

# read in all csvs 
files <- list.files(path = base_dir, pattern = ".csv",full.names = TRUE,recursive = TRUE)
# select all those that have 'refined' in the name 
refined <- files[grepl(pattern = 'refined', x = files)]
# drop idigbio for now 
refined <- refined[-grep(pattern = "refinedIdigBio", x = refined)]

# create an empty df with same structure 
df1 <- data.frame(taxon=character(),
                 genus=character(),
                 species=character(),
                 latitude=double(),
                 longitude=double(),
                 databaseSource=character(),
                 institutionCode=character(),
                 type=factor(),
                 uniqueID=factor(),
                 sampleCategory=character(),
                 country=character(),
                 iso3=character(),
                 localityInformation=character(),
                 biologicalStatus = character(), 
                 collectionSource = character(),
                 finalOriginStat = character(),
                 stringsAsFactors=FALSE)

# test for duplicates with GBIF data 


# test for invalid lat long 
# troubleshooting --- removing gbif from refined list 
gbif <- fread(refined[grep(pattern = "refinedGBIF", x = refined)], header = TRUE)
gbif$uniqueID <- as.factor(gbif$uniqueID)
gbif <- gbif %>%dplyr::select("taxon","genus","species","latitude","longitude",
                              "databaseSource","institutionCode","type","uniqueID",
                              "sampleCategory","country","iso3","localityInformation", 
                              "biologicalStatus", "collectionSource","finalOriginStat")

#pull capsicum and cucurbita out,   
capsicum <- fread(refined[grep(pattern = "refinedCapsicum", x = refined)], header = TRUE)%>%
  dplyr::select("taxon","genus","species","latitude","longitude",
                "databaseSource","institutionCode","type","uniqueID",
                "sampleCategory","country","iso3","localityInformation", 
                "biologicalStatus", "collectionSource","finalOriginStat")
cucurbita <- fread(refined[grep(pattern = "refinedCucurbita", x = refined)], header = TRUE)%>%
  dplyr::select("taxon","genus","species","latitude","longitude",
                "databaseSource","institutionCode","type","uniqueID",
                "sampleCategory","country","iso3","localityInformation", 
                "biologicalStatus", "collectionSource","finalOriginStat")


refined <- refined[-grep(pattern = "refinedGBIF", x = refined)]
refined <- refined[-grep(pattern = "refinedCapsicum", x = refined)]
refined <- refined[-grep(pattern = "refinedCucurbita", x = refined)]


# create function 
# read in one csv, r bind it to df, drop csv from memory 
appendTable <- function(dataframe, pathToData){
  data <- data.table::fread(pathToData, header = TRUE,)%>%
    dplyr::select("taxon","genus","species","latitude","longitude",
                 "databaseSource","institutionCode","type","uniqueID",
                 "sampleCategory","country","iso3","localityInformation", 
                 "biologicalStatus", "collectionSource","finalOriginStat")
  dataframe <- rbind(dataframe, data)
  rm(data)
  return(dataframe)
}

# apply function to list of csvs 
for(i in 1:length(refined)){
  path <- refined[i]
  df1 <- appendTable(dataframe = df1, pathToData = path)
  print(paste0(path, " has been added"))
  print(dim(df1))
}
# add gbif
df1 <- rbind(df1, gbif)

# remove all cucurbita and capsicum rows.
df1 <- df1[df1$genus != "Capsicum",]
df1 <- df1[df1$genus != "Cucurbita",]


# add clean capsicum and cucurbita 
df1 <- rbind(df1, cucurbita,capsicum)


# read in zizania texana data 
zT <- read.csv("D:/cwrNA/occurrence_data2019_05_29/zizania/Zizania_texana.csv")
# create an empty df with same structure 
nr <- nrow(zT)
df2 <- data.frame(taxon=character(nr),
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
# compile zazania texana to match the sctructure 
df2$taxon <- "Zizania texana"
df2$genus <- "Zizania"
df2$species <- "texana"
df2$latitude <- zT$lat
df2$longitude <- zT$lon
df2$databaseSource <- zT$Source
df2$uniqueID <- zT$id
df2$type <- zT$Type

#add the addational rice data to the whole set. 
df1 <- rbind(df1, df2)




# impliment taxonomic changes from Colins removal of species 20200226 

# remove all species that are not being using in the CWR species list for this study from the occurrence data 
sRemove <- c("Capsicum annuum", "Cucurbita okeechobeensis","Cucurbita pepo","Cucurbita pepo var. ozarkana",
"Cucurbita pepo var. texana","Persea borbonia var. borbonia","Ribes cereum var. inebrians") 

#none of these species have records so, deleting them doesn't change much, still it is good for record keeping
# they were removed from the cwr_NA list csv as well. 
df1 <- df1[!df1$taxon %in% sRemove,]

#convert Persea borbonia var. pubescens to Persea palustris 
## pull all records
t1 <- df1 %>% filter(taxon == "Persea borbonia var. pubescens")

## convert taxon and species colums 
t1$taxon <- "Persea palustris"
t1$species <- "palustris"

## remove all records from df1 
df2 <- df1 %>% filter(taxon != "Persea borbonia var. pubescens")

## add new records to df1 
df1 <- rbind(df2, t1)

# Convert specific species to intraspecific 
## convert Vaccinium ovalifolium to Vaccinium ovalifolium var. ovalifolium
## pull all records
t1 <- df1 %>% filter(taxon == "Vaccinium ovalifolium")
## convert taxon and species colums 
t1$taxon <- "Vaccinium ovalifolium var. ovalifolium"
t1$species <- "ovalifolium var. ovalifolium"
## remove all records from df1 
df2 <- df1 %>% filter(taxon != "Vaccinium ovalifolium")
## add new records to df1 
df1 <- rbind(df2, t1)

## convert Vaccinium erythrocarpum to Vaccinium erythrocarpum subsp. erythrocarpum
## pull all records
t1 <- df1 %>% filter(taxon == "Vaccinium erythrocarpum")
## convert taxon and species colums 
t1$taxon <- "Vaccinium erythrocarpum subsp. erythrocarpum"
t1$species <- "erythrocarpum subsp. erythrocarpum"
## remove all records from df1 
df2 <- df1 %>% filter(taxon != "Vaccinium erythrocarpum")
## add new records to df1 
df1 <- rbind(df2, t1)


## convert Allium schoenoprasum to Allium schoenoprasum subsp. schoenoprasum
## pull all records
t1 <- df1 %>% filter(taxon == "Allium schoenoprasum")
## convert taxon and species colums 
t1$taxon <- "Allium schoenoprasum subsp. schoenoprasum"
t1$species <- "schoenoprasum subsp. schoenoprasum"
## remove all records from df1 
df2 <- df1 %>% filter(taxon != "Allium schoenoprasum")
## add new records to df1 
df1 <- rbind(df2, t1)


## Remove all species that could have been change and replace with edits from the spreadsheet
spList <- c("Leymus salina","Leymus salina subsp. mojavensis","Leymus salina subsp. salina",
            "Leymus salina subsp. salmonis","Persea borbonia","Persea borbonia var. borbonia",
            "Persea borbonia var. pubescens","Saccharum brevibarbe","Saccharum brevibarbe var. brevibarbe",
            "Saccharum brevibarbe var. contortum","Vaccinium crassifolium","Vaccinium crassifolium subsp. crassifolium" ,
            "Vaccinium crassifolium subsp. sempervirens",'Elymus glabriflorus','Elymus glabriflorus var. australis',
            'Elymus glabriflorus var. glabriflorus','Elymus glaucus','Elymus glaucus subsp. glaucus',
            'Elymus glaucus subsp. mackenziei','Elymus glaucus subsp. virescens','Ipomoea cordatotriloba',
            'Ipomoea cordatotriloba var. cordatotriloba','Ipomoea cordatotriloba var. torreyana',
            'Juglans major','Juglans major var. major','Juglans microcarpa','Juglans microcarpa var. microcarpa',
            'Leymus mollis','Leymus mollis subsp. mollis','Leymus mollis subsp. villosissimus',
            'Phaseolus leptostachyus','Phaseolus leptostachyus var. leptostachyus','Prunus virginiana',
            'Prunus virginiana var. demissa','Prunus virginiana var. virginiana','Ribes cereum',
            'Ribes cereum var. cereum','Ribes cereum var. colubrinum','Rubus ursinus',
            'Rubus ursinus subsp. macropetalus','Rubus ursinus subsp. ursinus','Tripsacum dactyloides',
            'Tripsacum dactyloides var. dactyloides'
)
## select all species that are not in the list 
t1 <- df1 %>% filter(!taxon %in% spList)
## read in and join altered taxon lists 
t2 <- read.csv("D:/cwrNA/parameters/USA_cropWildRelativeInventory/intraspecificAlterations/intraSpecificSpecsNewTaxonEdits.csv")
t3 <- read.csv("D:/cwrNA/parameters/USA_cropWildRelativeInventory/intraspecificAlterations/intraSpecificSpecsList2NewTaxonEdits.csv")
t4 <- rbind(t2, t3)


###**there is 11 more occurrence in this re combined dataset... I don't know exactly why that is 
# I'm rolling with it for now 
df2 <- rbind(t1, t4)

# redefine genus and species to account for the duplication of values 
df3 <- df2 %>%
  dplyr::mutate(t0 = taxon)%>%
  tidyr::separate(col = t0,into = c("t1","t2", "t3","t4","t5","t6"),
                  sep = " ")
df3$genus <- df3$t1
df4 <- df3[,18:22]
# was assigning this to a column in a second dataframe and it was causeing quite a few issues. 
# keep it seperate seemed to fix the issues. 
df4 <- df4 %>% tidyr::unite("z", na.rm = TRUE , sep = " ")



df3$species <- df4$z

df2a <- df3[,1:16]
# colnames(df2) <- c("taxon","genus" ,             
# "species","latitude",
# "longitude","databaseSource","institutionCode","type","uniqueID","sampleCategory",
# "country","iso3", "localityInformation", "biologicalStatus","collectionSource",
# "finalOriginStat")
# write out final csv 
write.csv(x = df2a, file = paste0(base_dir,"/combinedOccurance", Sys.Date(), ".csv"))

