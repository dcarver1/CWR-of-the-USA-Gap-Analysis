###
# Tranform the PAUD dataset to a raster and run new insitu metrics 
# dan.carver@carverd.com
# 20200811
###

library(raster)
library(sp)
library(dplyr)
library(fasterize)
library(tmap)


# read in occurrence data
base_dir <<- "F:/nrelD/cwrNA"
gap_dir <- paste0(base_dir , "/gap_analysis")
par_dir <- paste0(base_dir , "/parameters")

occData <- data.table::fread(paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2020-07-21a.csv"),
                             header = TRUE)
occData <- occData[,2:ncol(occData)]

ecoReg <- rgdal::readOGR(paste0(par_dir,"/ecoregions/tnc_terr_ecoregions.shp"),verbose = FALSE)

 
proArea <- raster::raster(paste0(par_dir, "/protectedAreas/wdpa_reclass.tif"))


paud <- raster::shapefile("F:/nrelD/cwrNA/parameters/protectedAreas/PADUS2_0_Shapefiles/PADUS2_0Designation.shp")
paud1 <- spTransform(x = paud, CRSobj = crs(ecoReg))
paud1 <- sf::st_as_sf(paud1)
paud2 <- fasterize::fasterize(sf = paud1, raster = bioVars[[1]])
writeRaster(x = paud2, filename = "F:/nrelD/cwrNA/parameters/protectedAreas/PAUDrasters/PADUS2_0Designation.tif") 

paudTrans <- function(shp){
  paud1 <- spTransform(x = shp, CRSobj = crs(ecoReg))
  print(1)
  paud2 <- sf::st_as_sf(paud1)
  print(2)
  paud3 <- fasterize::fasterize(sf = paud2, raster = bioVars[[1]])
  print(3)
  return(paud3)
}


paudFee <- raster::shapefile("F:/nrelD/cwrNA/parameters/protectedAreas/PADUS2_0_Shapefiles/PADUS2_0Fee.shp")
pFee <- paudTrans(paudFee)
writeRaster(x = pFee, filename = "F:/nrelD/cwrNA/parameters/protectedAreas/PAUDrasters/PADUS2_0Fee.tif", ) 
qtm(pFee)

paudEase <- raster::shapefile("F:/nrelD/cwrNA/parameters/protectedAreas/PADUS2_0_Shapefiles/PADUS2_0Easement.shp")
pEase <- paudTrans(paudEase)
writeRaster(x = pEase, filename = "F:/nrelD/cwrNA/parameters/protectedAreas/PAUDrasters/PADUS2_0Easement.tif") 

paudProc <- raster::shapefile("F:/nrelD/cwrNA/parameters/protectedAreas/PADUS2_0_Shapefiles/PADUS2_0Proclamation.shp")
pProc <- paudTrans(paudProc)
qtm(pProc)
writeRaster(x = pProc, filename = "F:/nrelD/cwrNA/parameters/protectedAreas/PAUDrasters/PADUS2_0Proclamation.tif") 

pFee1 <- pFee
pEase1 <- pEase
paud21 <- paud2
pProc1 <- pProc

pFee1[is.na(pFee1)[]] <-0 
pEase1[is.na(pEase1)[]] <-0 
paud21[is.na(paud21)[]] <-0

pProc1[is.na(pProc1)[]] <-0


allAreas <- pFee1 + pEase1 + paud21
allAreas

allAreas[allAreas[] > 0]<-1

qtm(allAreas)

writeRaster(x = allAreas,
            filename = paste0("F:/nrelD/cwrNA/parameters/protectedAreas/PAUDrasters/allAreas", Sys.Date(),".tif")) 


### with the 


# drop all species that are not part of th 594 
rmSpec <- c("Phaseolus acutifolius","Phaseolus leptostachyus","Elymus elymoides","Leymus mollis","Phaseolus maculatus","Hordeum jubatum","Helianthus petiolaris","Ribes sanguineum","Phaseolus polystachios","Prunus serotina","Elymus trachycaulus","Hordeum brachyantherum","Ribes roezlii","Rubus hispidus","Ribes hudsonianum","Helianthus nuttallii","Helianthus pauciflorus","Humulus lupulus","Allium geyeri","Ribes oxyacanthoides","Fragaria x ananassa","Helianthus occidentalis","Fragaria virginiana","Elymus lanceolatus","Fragaria vesca","Helianthus niveus","Helianthus praecox","Prunus fasciculata","Ribes malvaceum","Rubus arcticus","Vitis rotundifolia","Fragaria chiloensis","Ribes aureum","Acer saccharum","Allium victorialis","Elymus stebbinsii","Helianthus debilis","Ipomoea ternifolia","Lactuca tatarica","Prunus ilicifolia","Prunus pumila","Ribes californicum","Rubus idaeus","Saccharum brevibarbe","Vitis aestivalis","Vitis cinerea","Zizania aquatica","Zizania palustris", "Allium schoenoprasum","Elymus glabriflorus",
            "Elymus glaucus","Ipomoea cordatotriloba","Juglans major","Juglans microcarpa","Leymus salina","Prunus virginiana","Ribes cereum","Rubus ursinus","Tripsacum dactyloides","Vaccinium crassifolium","Vaccinium erythrocarpum","Vaccinium ovalifolium"
)
ocd <- occData[!occData$taxon %in% rmSpec,]

remotes::install_github("CIAT-DAPA/GapAnalysis")

run_version <-"test20200203"
# GapAnalysis::GetDatasets()

# build a datframe that captures the total run time for a process.
time_df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(time_df) <- c("species", "runTime")

# define genera 
genera <- sort(unique(ocd$genus))

df2 <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(df2) <- c("species","SRSin","GRSin","ERSin","FCSin","FCSin_class")

for(i in genera[2:length(genera)]){
  genus <- i 
  allSpec <- ocd %>%
    dplyr::filter(genus == i)
  speciesList <<- sort(unique(allSpec$taxon))
  for(species in speciesList){
    print(species)
    t1a <- Sys.time()
    sp_dir <- paste0(gap_dir,"/",genus, "/",species,"/",run_version)
    if(file.exists(paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))){
      sdm <- raster::raster(paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))
      d1 <- read.csv(paste0( sp_dir, "/cleanedModelingData.csv")) %>%
        dplyr::select(taxon, type, latitude, longitude)
      colnames(d1) <- c("species", "type", "latitude", "longitude")
      d1 <- d1[,c(1,3,4,2)]
      
      # crop the protected areas file to the extent of model 
      pro1 <- raster::crop(x = allAreas, y = sdm)
      
      # exsitu 
      # srsIn <- try(GapAnalysis::SRSin(Species_list = species, 
      #                                 Occurrence_data = d1, 
      #                                 Raster_list = sdm, 
      #                                 Pro_areas = pro1, 
      #                                 Gap_Map = FALSE))
      # ersIn <- GapAnalysis::ERSin(Species_list = species, 
      #                             Occurrence_data = d1, 
      #                             Raster_list = sdm, 
      #                             Pro_areas = pro1,
      #                             Ecoregions_shp = ecoReg)
      # grsIn <- GapAnalysis::GRSin(Species_list = species,
      #                             Occurrence_data = d1,
      #                             Raster_list = sdm,
      #                             Pro_areas = pro1)
      
      fcsIn <- try(GapAnalysis::FCSin(Species_list = species,
                                      Occurrence_data = d1,
                                      Raster_list = sdm,
                                      Ecoregions_shp = ecoReg, 
                                      Gap_Map = FALSE, 
                                      Pro_areas = pro1)
                                      )
      write.csv(x = fcsIn, file = paste0(sp_dir,"/gap_analysis/insitu/paudFCSResults.csv"))
      df2 <- rbind(df2,fcsIn)
      endTime <- Sys.time() - t1a
      time_df <- rbind(time_df, data.frame(species=species, runTime = endTime))
      
    }
  }
}

