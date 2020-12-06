###
# Use the gap analysis R library to generate maps for the three species of interest 
# dan.carver@carverd.com
# 20200728
###
remotes::install_github("CIAT-DAPA/GapAnalysis")


### pull in datasets
# ecoregion 
ecoReg <- rgdal::readOGR(paste0(par_dir,"/ecoregions/tnc_terr_ecoregions.shp"),verbose = FALSE)
# occurrence data
occData <- data.table::fread(paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2020-07-21a.csv"),
                             header = TRUE)
occData <<- occData[,2:ncol(occData)]
#protected areas
proArea <- raster::raster(paste0(par_dir, "/protectedAreas/wdpa_reclass.tif"))




run_version <-"test20200203"
GapAnalysis::GetDatasets()


speciesList <- c("Phaseolus filiformis", "Vitis monticola", "Helianthus pumilus")
genera <- unique(d1$genus)
run_version <-"test20200203"

# filter occurrence data 
d1 <- occData %>%
  dplyr::filter(taxon %in% speciesList)

# output folder 
outFolder <- "F:/nrelD/cwrNA/gap_analysis/exampleSpecies"

for(i in genera[2:3]){
  # filter to species of interest and format data 
  genus <- as.character(i) 
  allSpec <- d1 %>%
    dplyr::filter(genus == i) %>%
    dplyr::select(taxon, type, latitude, longitude)
  species <- allSpec$taxon[1]
  #set file location 
  sp_dir <- paste0(gap_dir,"/",genus, "/",species,"/",run_version)
  if(file.exists(paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))){
    sdm <- raster::raster(paste0(sp_dir, "/modeling/spdist_thrsld_median.tif"))
    d2 <- read.csv(paste0( sp_dir, "/cleanedModelingData.csv")) %>%
      dplyr::select(taxon, type, latitude, longitude)
    colnames(d2) <- c("species", "type", "latitude", "longitude")
    d2 <- d2[,c(1,3,4,2)]
    
    # this work but the rasters are not being writen. Add as an issue on the github page 
    # GapAnalysis::SummaryHTML(Species_list = species, 
    #                          Occurrence_data = d2,
    #                          Raster_list = sdm,
    #                          Buffer_distance = 50000,
    #                          Ecoregions_shp = ecoReg, 
    #                          Pro_areas = proArea,
    #                          Output_Folder = "F:/nrelD/cwrNA/gap_analysis/exampleSpecies",
    #                          writeRasters = TRUE)
    ### ERS maps
    print("ers")
    # exsitu 
    ersEx <- GapAnalysis::ERSex(Species_list = species,
                                Occurrence_data = d2,
                                Raster_list = sdm,
                                Ecoregions_shp = ecoReg, 
                                Buffer_distance = 50000,
                                Gap_Map = TRUE)
    try(raster::writeRaster(x = ersEx$gap_maps[[1]], filename = paste0(outFolder, "/",species,"_ersEx.tif"),overwrite = TRUE))
    # insitu 
    ersIn <- GapAnalysis::ERSin(Species_list = species,
                                Occurrence_data = d2,
                                Raster_list = sdm,
                                Pro_areas = proArea,
                                Ecoregions_shp = ecoReg,
                                Gap_Map = TRUE)
    try(raster::writeRaster(x = ersIn$gap_maps[[1]], filename = paste0(outFolder, "/",species,"_ersIn.tif"), overwrite = TRUE))
    ### GRS maps 
    print("grs")
    #exsitu 
    grsex <- GapAnalysis::GRSex(Species_list = species,
                                Occurrence_data = d2,
                                Raster_list = sdm,
                                Buffer_distance = 50000,
                                Gap_Map = TRUE)
    try(raster::writeRaster(x = grsex$gap_maps[[1]], filename = paste0(outFolder, "/",species,"_grsEx.tif"), overwrite = TRUE))
    
    #insitu
    grsin <- GapAnalysis::GRSin(Species_list = species,
                                Occurrence_data = d2,
                                Raster_list = sdm,
                                Pro_areas = proArea,
                                Gap_Map = TRUE)
    try(raster::writeRaster(x = grsin$gap_maps[[1]], filename = paste0(outFolder, "/",species,"_grsIn.tif"), overwrite = TRUE))
    
    ### SRS maps
    print("srs")
    srsin <- GapAnalysis::SRSin(Species_list = species,
                                Occurrence_data = d2,
                                Raster_list = sdm,
                                Gap_Map = TRUE)
    try(raster::writeRaster(x = srsin$gap_maps[[1]], filename = paste0(outFolder, "/",species,"_srsIn.tif"), overwrite = TRUE))
    
  }
}

