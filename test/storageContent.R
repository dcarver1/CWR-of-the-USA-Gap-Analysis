###
# storage for notes and useful one time code chunks during development 
# 20200414
# dan.carver@carverd.com
### 


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
# spList <- c(
"Leymus salina subsp. salina",
"Leymus salina subsp. salmonis",
"Persea palustris",
"Vaccinium crassifolium subsp. crassifolium",
"Elymus glabriflorus var. australis",
"Elymus glabriflorus var. glabriflorus",
"Elymus glaucus subsp. mackenziei",
"Ipomoea cordatotriloba var. cordatotriloba",
"Juglans major var. major",
"Juglans microcarpa var. microcarpa",
" Prunus virginiana var. demissa",
"Ribes cereum var. cereum",
"Rubus ursinus subsp. macropetalus",
"Rubus ursinus subsp. ursinus",
"Tripsacum dactyloides var. dactyloides")

# # species for issues 3, delete current folders and rerun completely 
# # test with Vanilla mexicana before deleting any more runs 
# spList <- c("Vanilla mexicana",
"Acer saccharum subsp. ozarkense",
"Rubus abactus",
"Rubus ostryifolius",
#             "Fragaria x ananassa",
"Artocarpus altilis",
"Lactuca ludoviciana",
"Psidium guajava")

# Species for issue 4, try re running the models... watch how they fail. It could be there are just
# not enough points
# spList <- c("Allium bigelovii",
"Leymus salina subsp. mojavensis",
"Rubus kennedyanus",
"Vaccinium crassifolium subsp. crassifolium",
#             "Fragaria chiloensis subsp. sandwicensis",
"Juglans jamaicensis",
"Helianthus verticillatus",
#             "Ipomoea littoralis",
"Rubus x neglectus",
"Helianthus praecox subsp. praecox",
"Manihot walkerae",
"Rubus arundelanus",
"Allium gooddingii",
#             "Rubus orarius",
"Solanum nelsonii",
"Vitis aestivalis var. linsecomii",
"Elymus stebbinsii subsp. stebbinsii",
"Rubus neglectus",
"Elymus glabriflorus var. australis",
"Helianthus arizonensis",
#             "Vitis x novae-angliae",
"Elymus interruptus",
"Ipomoea dumetorum",
"Helianthus debilis subsp. tardiflorus",
"Elymus glabriflorus var. glabriflorus",
"Xanthosoma sagittifolium",
#             "Juglans cinerea",
"Prunus andersonii","Ribes quercetorum",
"Gossypium hirsutum"
# )

######
# species that had issues with redlist values 

List <- c(  "Malus fusca", 
            "Vaccinium uliginosum", 
            "Leymus mollis",
            "Vaccinium vitis-idaea", 
            "Ribes triste", 
            "Vigna luteola", 
            "Gossypium hirsutum", 
            "Vaccinium ovalifolium var. ovalifolium",
            "Xanthosoma sagittifolium", 
            "Psidium guajava", 
            "Ipomoea cordatotriloba",  
            "Ribes howellii",
            "Acer saccharum subsp. ozarkense", 
            "Oryza latifolia",
            "Ipomoea dumetorum",
            "Elymus trachycaulus",
            "Elymus glaucus",
            "Ipomoea leucantha",
            "Juglans jamaicensis",
            "Ribes aureum",
            "Ribes lacustre",
            "Vitis aestivalis var. linsecomii",
            "Allium cernuum",
            "Allium textile",
            "Leymus x multiflorus",
            "Leymus salina subsp. salina",
            "Hordeum jubatum",
            "Prunus emarginata",
            "Leymus cinereus",
            "Ribes cereum",
            "Nicotiana obtusifolia",
            "Leymus ambiguus",
            "Leymus simplex",  
            "Carya illinoinensis", 
            "Leymus innovatus", 
            "Leymus pacificus", 
            "Lemus californicus", 
            "Vaccinium scoparium",  
            "Rubus ursinus",  
            "Rubus spectabilis",
            "Rubus orarius",  
            "Rubus nutkanus", 
            "Rubus hispidus var. obovalis", 
            "Rubus chamaemorus",  
            "Vaccinium crassifolium subsp. sempervirens", 
            "Solanum xanti", 
            "Daucus pusillus",  
            "Fragaria vesca", 
            "Dasiphora fruticosa", 
            "Elymus canadensis", 
            "Elymus glabriflorus", 
            "Allium gooddingii", 
            "Elymus glaucus subsp. glaucus",  
            "Solanum douglasii",  
            "Leymus salina", 
            "Helianthus annuus", 
            "Prunus virginiana var. demsa",
            "Hordeum brachyantherum", 
            "Elymus elymoides",
            "Pseudoroegneria spicata",
            "Juglans microcarpa",
            "Allium bigelovii", 
            "Vaccinium erythrocarpum subsp. erythrocarpum",
            "Fragaria x ananassa", 
            "Fragaria virginiana", 
            "Juglans major",  
            "Leymus mollis subsp. villosissimus",
            "Tripsacum dactyloides", 
            "Allium schoenoprasum subsp. schoenoprasum",
            "Allium schoenoprasum", 
            "Fragaria x bringhurstii", 
            "Phaseolus leptostachyus var. leptostachyus", 
            "Psidium longipes",  "Rubus abactus", 
            "Saccharum brevibarbe var. brevibarbe", 
            "Vaccinium crassifolium", 
            "Vaccinium erythrocarpum",  
            "Vaccinium ovalifolium",  
            "Vanilla mexicana"
            
)

