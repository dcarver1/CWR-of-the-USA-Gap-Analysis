### 
# Primary script for running CWRNA. This should be the only location where users have
# to edit information.
# 20200414
# dan.carver@carverd.com
### 
pacman::p_load(tidyverse, sp, raster,rgdal, tmap, devtools,
                randomForest,rgeos,VSURF,modelr,maxnet,
               pROC,dismo,redlistr,fasterize, devtools, DT)
#devtools::install_github("DFJL/SamplingUtil")
#install_github("ccsosa/GapAnalysis")
# devtools::install_github("hunzikp/velox")
# devtools::install_github("valentinitnelav/geobuffer")
`library(geobuffer)
library(velox)
library(SamplingUtil)
tmap::tmap_mode("view")
`
# set all standard directories
base_dir <<- "F:/nrelD/cwrNA"
repo_dir <<- paste0(base_dir , "/src")
gap_dir <<- paste0(base_dir , "/gap_analysis")
par_dir <<- paste0(base_dir , "/parameters")
occ_dir <<- paste0(par_dir, "/occurenceData")
temp_dir <<- paste0(base_dir , "/TEMP")

#set name of the run version 
run_version <<- "temp20200915"

#set adjustable parameters 
numPoints <<- 2000 # maximun number of points used in model (use in subSampleCountry.R)
bufferDist <<- 50000 # used to define buffer distance in gBuffer.r ## had to change from 0.5 when
#swtiched to geobuffer package for SF object generation. 
set.seed(1234)

# set all primary file sources
# bioVars <<- readRDS(paste0(par_dir,"/bioLayer_2.5/climate_vx.RDS")) # need to install velox via dev tools. 

### velox object is not working 
rasters <- list.files(path = "D:/generalSpatialData/worldclim/30arcSec", full.names = TRUE, recursive = TRUE)
rList <- c()
for(i in 1:length(rasters)){
  rList  <- append(x = rList, raster::raster(rasters[i]))
}
bioVars <<- raster::stack(rList)


countrySHP <<- rgdal::readOGR(paste0(par_dir,"/ne_10m_admin/ne_10m_admin_0_countries.shp"),verbose = FALSE)
# exculing pacific territories- runs near all species faster 
#naSHP <<- readOGR(paste0(par_dir,"/northAmericaArea/northAmericaArea.shp"),verbose = FALSE)
# include pacific territories 
naSHP <<- rgdal::readOGR(paste0(par_dir, "/allUSAArea/NorthAmerica_AllUSA.shp"), verbose = FALSE)
ecoReg <<- rgdal::readOGR(paste0(par_dir,"/ecoregions/tnc_terr_ecoregions.shp"),verbose = FALSE)
occData <- data.table::fread(paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2020-07-21a.csv"),
                             header = TRUE)
occData <<- occData[,2:ncol(occData)]
proArea <<- raster::raster(paste0(par_dir, "/protectedAreas/wdpa_reclass.tif"))
### running with the PAUD dataset 
#proArea <<- raster::raster(paste0(par_dir, "/protectedAreas/PAUDrasters/allAreas.tif"))

layerDescription <<- read.csv(paste0(par_dir, "/layerDesrciptions.csv"))
statesData <<- read.csv(paste0(par_dir, "/statePerTaxon/CWRofUSA_nativeareas_2020_1_30.csv"))
statesSpObject <<- readRDS(paste0(par_dir, "/statesByCountry/gadmCanUsaMex_sp.rds"))

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


# set loop at genus level
genera <- sort(unique(occData$genus))
testGen <- genera[c(32, 25, 47 )]
testGen <- genera[49]
testGen
# three H. species to re run for fcsex Helianthus argophyllus Helianthus praecox subsp. hirtus Helianthus winteri

# temp removal ,"Comarum palustre"  , "Daucus pusillus"                       

### rerun for html. 
# 2. For the 15 new htmls you sent me, the ex situ and in situ tables generally seem ok (for actual conservation metrics, and for those two in situ count fields), 
# but the other summary counts werent updated (i.e. at top of html, and also as some of first fields in other tables). Could you update those and send them again? 
spList <- c(
  "Psidium guajava",
  "Artocarpus altilis",
  "Oryza latifolia",
  "Solanum xanti",
  "Allium textile",
  "Elymus glaucus subsp. glaucus",
  "Solanum douglasii",
  "Elymus canadensis",
  "Allium cernuum",
  "Allium acuminatum",
  "Nicotiana obtusifolia",
  "Dasiphora fruticosa",
  "Xanthosoma sagittifolium",
  "Gossypium hirsutum",
  "Rubus ursinus",
  "Rubus ursinus subsp. ursinus",
  "Tripsacum dactyloides var. dactyloides",
  "Daucus pusillus",
  "Leymus salina")
# removed because run has been completed.   "Rubus ursinus subsp. macropetalus",


# generate new html docs as values do not 
# 1. There are 5 taxa that need new htmls. Dont know why but their numbers in new and old are different. Im assuming the new results are good, thus need new htmls for them. 

spList <- c(
  "Rubus ursinus",
  "Rubus ursinus subsp. ursinus",
  "Tripsacum dactyloides var. dactyloides",
  "Daucus pusillus",
  "Leymus salina"
)

spList <- "Solanum jamesii"

#source("F:/nrelD/cwrNA/src/test/generateERSMaps.R")

# drop all species that are not part of th 594 
rmSpec <- c("Phaseolus acutifolius","Phaseolus leptostachyus","Elymus elymoides","Leymus mollis","Phaseolus maculatus","Hordeum jubatum","Helianthus petiolaris","Ribes sanguineum","Phaseolus polystachios","Prunus serotina","Elymus trachycaulus","Hordeum brachyantherum","Ribes roezlii","Rubus hispidus","Ribes hudsonianum","Helianthus nuttallii","Helianthus pauciflorus","Humulus lupulus","Allium geyeri","Ribes oxyacanthoides","Fragaria x ananassa","Helianthus occidentalis","Fragaria virginiana","Elymus lanceolatus","Fragaria vesca","Helianthus niveus","Helianthus praecox","Prunus fasciculata","Ribes malvaceum","Rubus arcticus","Vitis rotundifolia","Fragaria chiloensis","Ribes aureum","Acer saccharum","Allium victorialis","Elymus stebbinsii","Helianthus debilis","Ipomoea ternifolia","Lactuca tatarica","Prunus ilicifolia","Prunus pumila","Ribes californicum","Rubus idaeus","Saccharum brevibarbe","Vitis aestivalis","Vitis cinerea","Zizania aquatica","Zizania palustris", "Allium schoenoprasum","Elymus glabriflorus",
            "Elymus glaucus","Ipomoea cordatotriloba","Juglans major","Juglans microcarpa","Leymus salina","Prunus virginiana","Ribes cereum","Rubus ursinus","Tripsacum dactyloides","Vaccinium crassifolium","Vaccinium erythrocarpum","Vaccinium ovalifolium"
)
occData <- occData[occData$taxon %in% rmSpec,]

### running htmls for the non priority species 
spList <- rmSpec


# select all species at the genus level and apply master script to run process
beepr::beep_on_error(
  for(i in testGen){
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
    #write.csv(allSpec, paste0(folder, "/", "raw",i,".csv"), row.names = FALSE)
    genusOcc <<- read.csv(paste0(folder, "/", "raw",i,".csv"))
    speciesList <<- sort(unique(allSpec$taxon))
    #write.csv(x = speciesList, file = paste0(gap_dir,'/', genus, "/", 'speciesList.csv'))
    
    #test
    ### 20200227 here for trouble specific species in spList 
    speciesList <- speciesList[speciesList %in% spList]
    if(!is.na(speciesList[1])){
      #calls the master function 
      result_master = lapply(speciesList[1: length(speciesList)], master_run)
    }


    # try(rmarkdown::render(paste0(repo_dir, "/summaryMarkdown/summaryOfGenus.rmd"),  # file 2
    #                          output_file =  paste("SummaryReport_", genus , Sys.Date(), ".html", sep=''), 
    #                          output_dir = paste0(gap_dir,"/", genus,"/summaryDocs")))
    t2b <- Sys.time()
    totalTime <- t2b-t2a
    print(paste0("the genus ", genus," includes ",
                 length(speciesList), " in a total of ", totalTime," minutes."))

  }
)


# pull all htmls from 15 species 


# compile a list of all fcs data for troublesome species 
fcs <- list.files(path = base_dir,pattern = "_Run20200203_2020-08-18.html", full.names = TRUE,  recursive = TRUE)
folder <- "F:/nrelD/cwrNA/runSummaries/speciesLevelHTML"
fcs <- include(theList = fcs, toMatch = spList)
for(i in fcs){
  ## currently set to only include the trouble shooting I've been working on today
    file.copy(i, folder)
}

### test to see what species are included
ot <- sort(unique(occData$taxon))
ot <- data.frame(taxon = sort(unique(occData$taxon)), summaryDoc = NA)
n = 1 
for(i in ot$taxon){
   a <- include(fcs, i)
   if(length(a) > 0){
     ot$summaryDoc[n] <- a
   }
   n = n+1
}

reRun <- rmSpec[!rmSpec %in% ot$taxon]

# Ficus 

# interestingly no real issue with Elymus lanceolatus subsp. lanceolatus, Helianthus praecox subsp. praecox, 
# , Ipomoea violacea

# compile a list of all fcs data for troublesome species 
fcs <- list.files(path = base_dir,pattern = "summary.csv", full.names = TRUE,  recursive = TRUE)
# function for flitering list based on character values
include <- function (theList, toMatch){
  matches <- unique (grep(paste(toMatch,collapse="|"),
                          theList, value=TRUE))
  return(matches)
}

fcs <- include(theList = fcs, toMatch = spList)
fcs <-fcs[grepl(pattern = "insitu",x = fcs)]
fcs <- fcs[grepl(pattern = "test20200203", x = fcs)]
fcs
for(i in 1:length(fcs)){
  if(i == 1){
    all <- read.csv(fcs[i])
  }else{
    all <- rbind(all, read.csv(fcs[i]))
  }
}
write.csv(x = all, file = "F:/nrelD/cwrNA/troubleshooting/srsInsituWDPAreruns20200813.csv")
