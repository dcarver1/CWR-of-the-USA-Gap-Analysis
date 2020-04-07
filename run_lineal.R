### 
# Primary script for running CWRNA. This should be the only location where users have
# to edit information.
# 20190830
# carver.dan1@gmail.com
### 

library(tidyverse)
library(sp)
library(raster)
library(rgdal)
library(tmap)
library(devtools)
#install_github("DFJL/SamplingUtil")
library(geobuffer)
library(SamplingUtil)
library(velox)
tmap::tmap_mode("view")
library(rgeos)
library(randomForest)
library(VSURF)
library(modelr)
library(maxnet)
library(pROC)
library(dismo)
library(redlistr)
library(fasterize)

# set all standard directories
base_dir <<- "D:/cwrNA"
repo_dir <<- paste0(base_dir , "/src")
gap_dir <<- paste0(base_dir , "/gap_analysis")
par_dir <<- paste0(base_dir , "/parameters")
occ_dir <<- paste0(par_dir, "/occurenceData")
temp_dir <<- paste0(base_dir , "/TEMP")

#set name of the run version 
run_version <<- "test20200203"

#set adjustable parameters 
numPoints <<- 2000 # maximun number of points used in model (use in subSampleCountry.R)
bufferDist <<- 50000 # used to define buffer distance in gBuffer.r ## had to change from 0.5 when
#swtiched to geobuffer package for SF object generation. 
set.seed(1234)

# set all primary file sources
bioVars <<- readRDS(paste0(par_dir,"/bioLayer_2.5/climate_vx.RDS"))
countrySHP <<- readOGR(paste0(par_dir,"/ne_10m_admin/ne_10m_admin_0_countries.shp"),verbose = FALSE)
# exculing pacific territories- runs near all species faster 
#naSHP <<- readOGR(paste0(par_dir,"/northAmericaArea/northAmericaArea.shp"),verbose = FALSE)
# include pacific territories 
naSHP <<- readOGR(paste0(par_dir, "/allUSAArea/NorthAmerica_AllUSA.shp"), verbose = FALSE)

#naSHP@data <- naSHP@data %>% dplyr::select(-c(1:94))# not sure what this is for?? 
ecoReg <<- readOGR(paste0(par_dir,"/ecoregions/tnc_terr_ecoregions.shp"),verbose = FALSE)
occData <<- data.table::fread("D:/cwrNA/occurrence_data2019_05_29/combinedOccurance2020-04-03.csv",
                              header = TRUE)
proArea <<- raster::raster(paste0(par_dir, "/protectedAreas/wdpa_reclass.tif"))
layerDescription <<- read.csv(paste0(par_dir, "/layerDesrciptions.csv"))
statesData <<- read.csv(paste0(par_dir, "/statePerTaxon/CWRofUSA_nativeareas_2020_1_30.csv"))
statesSpObject <<- readRDS(paste0(par_dir, "/statesByCountry/gadmCanUsaMex_sp.rds"))

# 20200317 issues with duplicate taxon presenting in different genera. 
# Split the taxon name and use that to define the genus - species 

occData1 <- occData %>%
  tidyr::spread(key= )

# Load the sources scripts
source.files = list.files(repo_dir, ".[rR]$", full.names = TRUE, recursive = T)
source.files = source.files[ !grepl("dataBaseTransform", source.files) ]
source.files = source.files[ !grepl("test", source.files) ]
source.files = source.files[ !grepl("lineal", source.files) ]
source.files = source.files[ !grepl("summaryMarkdown", source.files) ]



#lapply(source.files, source)
for(i in 1:length(source.files)){
  cat(i,"\n")
  source(source.files[i])
  
}
# set counts csv 
dfCounts <<- data.frame(matrix(NA, nrow = 1, ncol = 13))
colnames(dfCounts) <-  c("species","totalRecords",	"hasLat", "hasLong","totalUseful", 	"totalGRecords",
                         "totalGUseful","totalHRecords",	"totalHUseful","numberOfUniqueSources", 
                         "NA_occurrences","NA_GUseful" ,"NA_HUseful")
# set loop at genus level
genera <- sort(unique(occData$genus))
testGen <- genera[1:length(genera)]
troubleGen <- genera[3:3]
# Are you testing? Yes or No 
# if testing all code will run regradless of if the output exists or not. 
# if false code will test expected output, if that exist it will not run function. 


#create list for error species 
### 20200206 I should change this out to 
lowOccurence <- list()
lowOccurenceAll <- list()
notModeled <- list()
notModeledAll <- list()
fullModelProcess <- list()
ModeledAll <- list()




### species list for issues on 20200303 
x2<- allSummary %>%
  dplyr::filter(is.na(GRS.x)) %>%
  dplyr::distinct(species) # %>%
  #filter(species == "Castanea x neglecta")
spList <- unique(x2$species)[2:nrow(x2)]


spList <- as.character(spl3$taxon)
class(spList)
  


df5 <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(df5) <-  c(
  "taxon","varNames","importance","includeInFinal"
) 

vector <- c()
n = 1
# select all species at the genus level and apply master script to run process
beepr::beep_on_error(
  for(i in genera){
    t2a <- Sys.time()
    genus <<- i
    if (!file.exists(paste0(gap_dir,"/summaryDocs"))) {dir.create(paste0(gap_dir,"/summaryDocs"),recursive=T)}
    allSpec <- occData %>%
      dplyr::filter(genus == i)
    # generate a folder within the gap analysis
    folder <- paste0(occ_dir, "/",i)
    if (!file.exists(folder)) {dir.create(paste0(folder),recursive=T)}
    # test for genus level folder.
    genFolder <- paste0(gap_dir, "/", i)
    if (!file.exists(genFolder)) {dir.create(paste0(genFolder),recursive=T)}
    write.csv(allSpec, paste0(folder, "/", "raw",i,".csv"), row.names = FALSE)
    genusOcc <<- read.csv(paste0(folder, "/", "raw",i,".csv"))
    speciesList <<- sort(unique(allSpec$taxon))
    write.csv(x = speciesList, file = paste0(gap_dir,'/', genus, "/", 'speciesList.csv'))
    
    #test
    ### 20200227 here for trouble specific species in spList 
    speciesList <- speciesList[speciesList %in% spList]
    
    
    #useful code storing here at the moment 
    #for(k in speciesList){
    #   files <- list.files(path = paste0(gap_dir,"/", genus,"/",k,"/",run_version, "/modeling/maxent/"),
    #                       pattern = "predictorImportance.csv",
    #                       recursive = TRUE, full.names = TRUE)
    #   if(length(files) == 1){
    #     t5 <- read.csv(files[1]) %>%
    #       dplyr::mutate(taxon = k)%>%
    #       dplyr::select(taxon,varNames,	importance,	includeInFinal)
    #     df5 <- rbind(df5, t5)
    #     }
    #   }
    # #}
     
      #testList <- speciesList[1:1]
      #species1 <<- "Helianthus angustifolius"
      #result_master = lapply(testList, master_run)
      result_master = lapply(speciesList, master_run)
      # need to include a call for the summary of all features at the genus level
      # before the loop repeats
      #dfCounts <<- dfCounts[-1,]
      #write.csv(dfCounts, file = paste0(occ_dir,"/allCountsSummary",run_version,".csv"))
      # call the summarize script for all runs
      ### speciesList <<- testList
      # try(rmarkdown::render(paste0(repo_dir, "/summaryMarkdown/summaryOfGenus.rmd"),  # file 2
      #                          output_file =  paste("SummaryReport_", genus , Sys.Date(), ".html", sep=''),
      #                          output_dir = paste0(gap_dir,"/", genus,"/summaryDocs")))
      # t2b <- Sys.time()
      # totalTime <- t2b-t2a
      # print(paste0("the genus ", genus," includes ",
      #              length(speciesList), " in a total of ", totalTime," minutes."))
      # # #append error species to list
      # if(length(na.omit(lowOccurence)) >0 ){
      #   lowOccurenceAll <- append(lowOccurenceAll, na.omit(lowOccurence))
      # }
      # if(length(na.omit(notModeled)) >0 ){
      #   notModeledAll <- append(notModeledAll, na.omit(notModeled))
      # }
      # if(length(na.omit(fullModelProcess)) >0 ){
      #   ModeledAll <- append(ModeledAll, na.omit(fullModelProcess))
      # }
      # #Clear List of error species, as this information is saved on the
      # lowOccurence <- list()
      # notModeled <- list()
      # fullModelProcess <- list()
      
    }
 #else{
 #     print("no species of interest")
    # }
 # }
)
colnames(df5) <- c("Taxon", "Predictor",	"Explanatory weight",	"Included in modeling"
)
write.csv(x = df5, 
          file = paste0(base_dir, "/runSummaries/topPredictorsAll",Sys.Date(),".csv"))


# there are duplicates in the df counts data. removing them here 
dfC <- dfCounts%>% dplyr::distinct(species, .keep_all = TRUE)
#write out dfCounts 
write.csv(x = dfC, file = paste0(base_dir, "/runSummaries/allCounts.csv"))






#20200326 
# code for generating all species map

# if(length(speciesList) >=1){
#DANGER ---- delete existing model results
# for(name in 1:length(speciesList)){
#   path <- paste0(gap_dir,"/",genus, "/", speciesList[name],"/" , run_version)
#   unlink(x = path,recursive = TRUE)
# }
# speciesList <- speciesList[!speciesList %in% rmSpec]
# for(k in speciesList){
#   files <- list.files(path = paste0(gap_dir,"/", genus,"/",k,"/",run_version, "/"),
#                       pattern = "spdist_thrsld_median.tif",
#                       recursive = TRUE, full.names = TRUE)
#   if(length(files)>0){
#     vector[n] <- k
#     n = n+1
#   }
# }
#write.csv(x = vector, file = paste0(base_dir, "/runSummaries/speciesWithMaps.csv"))
### the vector list generated here is then used to run the summary of run function once all no cumulative species are removed


#20200313 
#useful code storing here at the moment 
# for(k in speciesList){
#   files <- list.files(path = paste0(gap_dir,"/", genus,"/",k,"/",run_version, "/"),
#                       pattern = "cleanedModelingData.csv",
#                       recursive = TRUE, full.names = TRUE)
#   if(length(files) == 1){
#     t5 <- read.csv(files[1])
#     if(ncol(t5)==10){
#       df5 <- rbind(df5,t5)
#     }
#     if(ncol(t5) == 9){
#       t5$StateTest = NA
#       df5 <- rbind(df5,t5)
#     }
#     if(class(files)=="character"){
#       t5 <- data.frame(matrix(nrow = 1, ncol=10))
#       colnames(t5) <-  c(
#         "taxon","latitude", "longitude","type","databaseSource", 
#         "hasLat","hasLong","hasLatLong","iso3_check"
#       ) 
#       t5$taxon=k
#       t5$latitude=NA 
#       t5$longitude=NA
#       t5$type=NA
#       t5$databaseSource=NA
#       t5$hasLat=NA
#       t5$hasLong=NA
#       t5$hasLatLong=NA
#       t5$iso3_check =NA
#     }
#   }
# }









#### 20200210 - I'd like to move this to either a 
# write out model/not modeled lists 

lowOccurenceAll1 <- lapply(lowOccurenceAll, function(x) x[!is.na(x)])
lowOccurenceAll1 <- lowOccurenceAll1[lengths(lowOccurenceAll1) > 0]

notModeledAll1 <- lapply(notModeledAll, function(x) x[!is.na(x)])
notModeledAll1 <- notModeledAll1[lengths(notModeledAll1) > 0]

ModeledAll1 <- lapply(ModeledAll, function(x) x[!is.na(x)])
ModeledAll1 <- ModeledAll1[lengths(ModeledAll1) > 0]
  
  

maxLength <- max(c(length(lowOccurenceAll1), 
                   length(notModeledAll1),
                   length(ModeledAll1)))

df2 <- data.frame(matrix(ncol=3, nrow= maxLength,data = NA))
colnames(df2) <- c("lowOccurrenceSpecies",
                   "speciesNotModeled",
                   "speciesSuccessfullyModeled"
)

lo2 <- c(lowOccurenceAll1, rep(NA, nrow(df2)-length(lowOccurenceAll1)))
nMA <- c(notModeledAll1, rep(NA, nrow(df2)-length(notModeledAll1)))
sSM <- c(ModeledAll1, rep(NA, nrow(df2)-length(ModeledAll1)))

for(i in 1:length(lo2)){
  df2$lowOccurrenceSpecies[i] <- lo2[[i]]
  df2$speciesNotModeled[i] <- nMA[[i]]
  df2$speciesSuccessfullyModeled[i] <- sSM[[i]]
}
                            
                                    
write.csv(x = df2, file = paste0(gap_dir, "/summaryDocs/speciesModeledAndNot", Sys.Date(),".csv"))

### combine the all species list, with counts of North American, then the columns of the bins 
# list of all species names of interest 
x1 <- read.csv("D:/cwrNA/parameters/statePerTaxon/CWRofUSA_nativeareas_2020_1_30.csv")
x1 <- as.data.frame(unique(x1$name))

#read in counts for all species
x2 <- read.csv("D:/cwrNA/parameters/occurenceData/allCountsSummary.csv") %>%
  dplyr::select(species, NorthAmericanPoint)
## for some reason some species are making it on to this list twice. Dropping them here 
x2 <- x2[!duplicated(x2$species),]
x2 <- distinct(x2) 

# join x1 and x2 by species name 
t1 <- dplyr::left_join(x = x1, y = x2, by = c("unique(x1$name)" = "species"))
dim(t1) # this is about 14 values more, meaning there are some 

## there might be a better way to do this but I want to move on here 
l2 <- as.data.frame(df2$lowOccurrenceSpecies[!is.na(df2$lowOccurrenceSpecies)]) %>%
  dplyr::mutate(lowOcc = 1) %>%
  distinct()
l3 <- as.data.frame(df2$speciesNotModeled[!is.na(df2$speciesNotModeled)]) %>%
  dplyr::mutate(notModeled = 1)%>%
  distinct()
l4 <- as.data.frame(df2$speciesSuccessfullyModeled[!is.na(df2$speciesSuccessfullyModeled)]) %>%
  dplyr::mutate(ModeledSuccessfully = 1) %>%
  distinct()


t2 <- dplyr::left_join(x = t1, y = l2, by = c("unique(x1$name)" = "df2$lowOccurrenceSpecies[!is.na(df2$lowOccurrenceSpecies)]"))
t2 <- dplyr::left_join(x = t2, y = l3, by = c("unique(x1$name)" = "df2$speciesNotModeled[!is.na(df2$speciesNotModeled)]"))
t2 <- dplyr::left_join(x = t2, y = l4, by = c("unique(x1$name)" = "df2$speciesSuccessfullyModeled[!is.na(df2$speciesSuccessfullyModeled)]"))

write.csv(x = t2, file = paste0(gap_dir, "/summaryDocs/modelErrorsSummary", Sys.Date(),".csv"))


## base directory is getting redefined to gap_dir somewhere. 

# this is a big process it will take time... 
try(rmarkdown::render(paste0(repo_dir, "/summaryMarkdown/summaryOfRun.rmd"),  # file 2
                      output_file =  paste("SummaryReport_", run_version , Sys.Date(), ".html", sep=''),
                      output_dir = paste0(base_dir,"/runSummaries")))


#### troubleshooting content, most species list 
#rerunning intraspecific species due to occurrence compiled from species level 
# spList <- c("Leymus salina subsp. salina","Leymus salina subsp. salmonis", "Persea palustris",
#              "Vaccinium crassifolium subsp. crassifolium", "Elymus glabriflorus var. australis",
# "Elymus glabriflorus var. glabriflorus","Elymus glaucus subsp. mackenziei","Ipomoea cordatotriloba var. cordatotriloba",
# "Juglans major var. major","Juglans microcarpa var. microcarpa"," Prunus virginiana var. demissa",
# "Ribes cereum var. cereum","Rubus ursinus subsp. macropetalus","Rubus ursinus subsp. ursinus",
# "Tripsacum dactyloides var. dactyloides")

# # species for issues 3, delete current folders and rerun completely 
# # test with Vanilla mexicana before deleting any more runs 
# spList <- c("Vanilla mexicana","Acer saccharum subsp. ozarkense","Rubus abactus","Rubus ostryifolius",
#             "Fragaria x ananassa","Artocarpus altilis","Lactuca ludoviciana","Psidium guajava")

# Species for issue 4, try re running the models... watch how they fail. It could be there are just
# not enough points
# spList <- c("Allium bigelovii","Leymus salina subsp. mojavensis","Rubus kennedyanus","Vaccinium crassifolium subsp. crassifolium",
#             "Fragaria chiloensis subsp. sandwicensis","Juglans jamaicensis","Helianthus verticillatus",
#             "Ipomoea littoralis","Rubus x neglectus","Helianthus praecox subsp. praecox","Manihot walkerae","Rubus arundelanus","Allium gooddingii",
#             "Rubus orarius","Solanum nelsonii","Vitis aestivalis var. linsecomii","Elymus stebbinsii subsp. stebbinsii","Rubus neglectus","Elymus glabriflorus var. australis","Helianthus arizonensis",
#             "Vitis x novae-angliae","Elymus interruptus","Ipomoea dumetorum","Helianthus debilis subsp. tardiflorus","Elymus glabriflorus var. glabriflorus","Xanthosoma sagittifolium",
#             "Juglans cinerea","Prunus andersonii","Ribes quercetorum","Gossypium hirsutum"
# )



