###
# compile ERS gaps using gap analysis code base 
# dan.carver@carverd.com  
# 20200724
###
# install.packages("raster")


### need a method for compiling all ERS data 

# read in occurrence data
occData <- data.table::fread(paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2020-07-21a.csv"),
                             header = TRUE)
occData <<- occData[,2:ncol(occData)]

# drop all species that are not part of th 594 
rmSpec <- c("Phaseolus acutifolius","Phaseolus leptostachyus","Elymus elymoides","Leymus mollis","Phaseolus maculatus","Hordeum jubatum","Helianthus petiolaris","Ribes sanguineum","Phaseolus polystachios","Prunus serotina","Elymus trachycaulus","Hordeum brachyantherum","Ribes roezlii","Rubus hispidus","Ribes hudsonianum","Helianthus nuttallii","Helianthus pauciflorus","Humulus lupulus","Allium geyeri","Ribes oxyacanthoides","Fragaria x ananassa","Helianthus occidentalis","Fragaria virginiana","Elymus lanceolatus","Fragaria vesca","Helianthus niveus","Helianthus praecox","Prunus fasciculata","Ribes malvaceum","Rubus arcticus","Vitis rotundifolia","Fragaria chiloensis","Ribes aureum","Acer saccharum","Allium victorialis","Elymus stebbinsii","Helianthus debilis","Ipomoea ternifolia","Lactuca tatarica","Prunus ilicifolia","Prunus pumila","Ribes californicum","Rubus idaeus","Saccharum brevibarbe","Vitis aestivalis","Vitis cinerea","Zizania aquatica","Zizania palustris", "Allium schoenoprasum","Elymus glabriflorus",
            "Elymus glaucus","Ipomoea cordatotriloba","Juglans major","Juglans microcarpa","Leymus salina","Prunus virginiana","Ribes cereum","Rubus ursinus","Tripsacum dactyloides","Vaccinium crassifolium","Vaccinium erythrocarpum","Vaccinium ovalifolium"
)
ocd <- occData[!occData$taxon %in% rmSpec,]

genera <- sort(unique(ocd$genus))

run_V <- "test20200203"

n=1
for(i in genera){
  #select all species in genera 
  oc1 <- ocd %>% 
    dplyr::filter(genus == i)
  spList2 <- unique(oc1$taxon)
  for(j in spList2){
    sp_dir <- paste0("F:/nrelD/cwrNA/gap_analysis/",i,"/",j,"/",run_V)
    # test for file and read it as object 
    if(file.exists(paste0(sp_dir, "/gap_analysis/insitu/ersInEcoregions.csv"))){
      ersin <- read.csv(paste0(sp_dir, "/gap_analysis/insitu/ersInEcoregions.csv"))
      ersin <- ersin[,c(2,3,4,7)] 
      if(n==1){
        ersinAll <- ersin 
      }else{
        ersinAll <- dplyr::bind_rows(ersinAll, ersin)
      }
    }
    if(file.exists(paste0(sp_dir, "/gap_analysis/exsitu/ersExEcoregions.csv"))){
      ersex <- read.csv(paste0(sp_dir, "/gap_analysis/exsitu/ersExEcoregions.csv"))
      ersex <- ersex[,c(2,3,4,7)]
      if(n==1){
        ersexAll <- ersex 
      }else{
        ersexAll <- dplyr::bind_rows(ersexAll, ersex)
      }
    }
    n = n +1
  }

}

outDir <- "F:/nrelD/cwrNA/parameters/ecoregions/gapEcoRegions"
write.csv(x = ersexAll, file = paste0(outDir, "/allERSExEcoregionData.csv"))
write.csv(x = ersinAll, file = paste0(outDir, "/allERSExEcoregionData.csv"))



# generate a count of species not conserved exsitu in each ecoregion 
ex <- ersexAll %>%
  dplyr::filter(Conserved.Ex.Situ == "Not Conserved")%>%
  dplyr::group_by(ECO_ID_U)%>%
  dplyr::summarise("Exsitu_Con" = n())
View(ex)  
write.csv(x = ex, file = paste0(outDir, "/ERSexNonConservedCounts.csv"))

# generate a count of species not conserved exsitu in each ecoregion 
ins <- ersinAll %>%
  dplyr::filter(Conserved.In.Situ == "Not Conserved")%>%
  dplyr::group_by(ECO_ID_U)%>%
  dplyr::summarise("Insitu_Con" = n())
View(ins)  
write.csv(x = ins, file = paste0(outDir, "/ERSinNonConservedCounts.csv"))

# join information to shapefile of ecoregions 
ecoReg <- rgdal::readOGR(paste0(par_dir,"/ecoregions/tnc_terr_ecoregions.shp"),verbose = FALSE)

eco1 <- sp::merge(x = ecoReg, y = ex, by = "ECO_ID_U")
eco1 <- sp::merge(x = eco1, y = ins, by = "ECO_ID_U")

View(eco1@data)


rgdal::writeOGR(obj = eco1, dsn = outDir, layer = "gap_ecoregions", driver ="ESRI Shapefile",overwrite_layer = TRUE )

#### 20200724 - something was not working well with the GapAnalysis Functions I may come back to this but for now 
# I'm tying in the table generate to the 
#### 20200728 - this works but it is running very slow. I running the method via the runlineal code now. 

remotes::install_github("CIAT-DAPA/GapAnalysis")


run_version <-"test20200203"
GapAnalysis::GetDatasets()

# build a datframe that captures the total run time for a process.
time_df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(time_df) <- c("species", "runTime")

# define genera 
genera <- sort(unique(ocd$genus))
                                     
base_dir <<- "F:/nrelD/cwrNA"
gap_dir <<- paste0(base_dir , "/gap_analysis")

for(i in genera){
  genus <<- i 
  allSpec <- ocd %>%
    dplyr::filter(genus == i)
  speciesList <<- sort(unique(allSpec$taxon))
  for(species in speciesList){
    print(species)
    t1a <- Sys.time()
    sp_dir <- paste0(gap_dir,"/",genus, "/",species,"/",run_version)
    if(file.exists(paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))){
      sdm <- raster::raster(paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))
      # d1 <- read.csv(paste0( sp_dir, "/cleanedModelingData.csv")) %>%
      #   dplyr::select(taxon, type, latitude, longitude)
      # colnames(d1) <- c("species", "type", "latitude", "longitude")
      # d1 <- d1[,c(1,3,4,2)]
      # exsitu 
      # ersEx <- try(GapAnalysis::ERSex(Species_list = species,
      #                             Occurrence_data = d1,
      #                             Raster_list = sdm,
      #                             Ecoregions_shp = ecoReg, 
      #                             Buffer_distance = 50000,
      #                             Gap_Map = TRUE))
      # try(raster::writeRaster(x = ersEx$gap_maps[[1]],
      #                         filename = paste0(sp_dir, "/gap_analysis/exsitu/",species,"_ersEx.tif"),overwrite = TRUE))
      if(!file.exists(paste0(sp_dir, "/modeling/alternatives/ga50.tif"))){
        try(raster::writeRaster(x = sdm,
                                filename = paste0(sp_dir, "/gap_analysis/exsitu/",species,"_grsEx.tif"),overwrite = TRUE))    
       }#else{
      #   ersEx <- try(raster::raster(x = paste0(sp_dir, "/modeling/alternatives/ga50.tif")))
      #   ersEx[is.na(ersEx)] <- 0
      #   sdm[sdm[]==0]<-NA
      #   ersEx1 <- try(sdm - ersEx)
      #   
      #   try(raster::writeRaster(x = ersEx1,
      #                           filename = paste0(sp_dir, "/gap_analysis/exsitu/",species,"_grsEx.tif"),overwrite = TRUE))    
      #   
      }

      
      # insitu 
      # ersIn <- try(GapAnalysis::ERSin(Species_list = species,
      #                             Occurrence_data = d1,
      #                             Raster_list = sdm,
      #                             Pro_areas = proArea,
      #                             Ecoregions_shp = ecoReg,
      #                             Gap_Map = TRUE))
      # try(raster::writeRaster(x = ersIn$gap_maps[[1]],
      #                         filename = paste0(sp_dir, "/gap_analysis/insitu/",species,"_ersIn.tif"), overwrite = TRUE))
      # endTime <- Sys.time() - t1a
      # time_df <- rbind(time_df, data.frame(species=species, runTime = endTime))
                                           
    }
  }
}
  
