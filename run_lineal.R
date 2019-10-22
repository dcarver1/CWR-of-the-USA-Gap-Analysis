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
library(tmap)
library(devtools)
#install_github("DFJL/SamplingUtil")
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

# set all standard directories
base_dir <<- "D:/cwrNA"
repo_dir <<- paste0(base_dir , "/src")
gap_dir <<- paste0(base_dir , "/gap_analysis")
par_dir <<- paste0(base_dir , "/parameters")
occ_dir <<- paste0(par_dir, "/occurenceData")
temp_dir <<- paste0(base_dir , "/TEMP")

#set name of the run version 
run_version <<- "test20190827"

#create list for error species 
lowOccurence <- c()
notModeled <- c()

#set adjustable parameters 
numPoints <<- 2000 # maximun number of points used in model (use in subSampleCountry.R)
bufferDist <<- 0.5 # used to define buffer distance in gBuffer.r 
set.seed(1234)

# set all primary file sources
bioVars <<- readRDS(paste0(par_dir,"/bioLayer_2.5/climate_vx.RDS"))
countrySHP <<- readOGR(paste0(par_dir,"/ne_10m_admin/ne_10m_admin_0_countries.shp"),verbose = FALSE)
naSHP <<- readOGR(paste0(par_dir,"/northAmericaArea/northAmericaArea.shp"),verbose = FALSE)
naSHP@data <- naSHP@data %>% dplyr::select(-c(1:95))#
ecoReg <<- readOGR(paste0(par_dir,"/ecoregions/wwf_terr_ecos.shp"),verbose = FALSE)
occData <<- data.table::fread(paste0(par_dir, "/modelingData2019-08-30.csv"),header = TRUE)
proArea <<- raster::raster(paste0(par_dir, "/protectedAreas/wdpa_reclass.tif"))

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
dfCounts <<- data.frame(matrix(NA, nrow = 1, ncol = 11))
colnames(dfCounts) <-  c("species","totalRecords",	"hasLat", "hasLong","totalUseful", 	"totalGRecords",
                         "totalGUseful","totalHRecords",	"totalHUseful","numberOfUniqueSources", "NorthAmericanPoint" )
# set loop at genus level
genera <<- unique(occData$genus)
testGen <- genera[2]

# Are you testing? Yes or No 
# if testing all code will run regradless of if the output exists or not. 
# if false code will test expected output, if that exist it will not run function. 
Testing <<- TRUE

# select all species at the genus level and apply master script to run process
for(i in genera){
  t2a <- Sys.time()
  genus <<- i
  allSpec <- occData %>%
    filter(genus %in% i)
  # generate a folder within the gap analysis
  folder <- paste0(occ_dir, "/",i)
  dir.create(folder, showWarnings = FALSE) # https://stackoverflow.com/questions/4216753/check-existence-of-directory-and-create-if-doesnt-exist
  write.csv(allSpec, paste0(folder, "/", "raw",i,".csv"), row.names = FALSE)
  genusOcc <<- read.csv(paste0(folder, "/", "raw",i,".csv"))
  speciesList <<- unique(allSpec$taxon)
  print(speciesList)
  #test
  #testList <- speciesList[13:length(speciesList)]
  # species1 <- "Vaccinium vitis-idaea"
  #result_master = lapply(testList, master_run)
  result_master = lapply(speciesList, master_run)
                         # need to include a call for the summary of all features at the genus level
                         # before the loop repeats
  dfCounts <<- dfCounts[-1,]
  write.csv(dfCounts, file = paste0(occ_dir,"/allCountsSummary.csv"))
  # call the summarize script for all runs
  # rmarkdown::render(paste0(repo_dir, "/summaryMarkdown/summaryOfGenus.rmd"),  # file 2
  #                  output_file =  paste("SummaryReport_", genus , Sys.Date(), ".html", sep=''),
  #                  output_dir = paste0(gap_dir, "/", genus))
}
t2b <- Sys.time()
totalTime <- t2b-t2a 
