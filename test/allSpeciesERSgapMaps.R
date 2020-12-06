###
# Generating a summary map for ERS 
# dan.carver@carverd.com
# 20200729
###
library(raster)


# function for flitering list based on character values
include <- function (theList, toMatch){
  matches <- unique (grep(paste(toMatch,collapse="|"),
                          theList, value=TRUE))
  return(matches)
}

# function for replacing na values with 0
removeNA = function(rasterPath){
  r1 <- raster::raster(rasterPath)
  r1[is.na(r1)] <- 0
  return(r1)
}

# function for extending all rasters to an equal extent
extend_all =function(rasters){
  extent(Reduce(extend,rasters))
}

# function for adding all rasters together
sum_all = function(rasters, extent){
  re = lapply(rasters, function(r){extend(r, extent, value=0)})
  Reduce("+",re)
}

base_dir <- "F:/nrelD/cwrNA"

occData <- data.table::fread(paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2020-07-21a.csv"),
                             header = TRUE)
occData <<- occData[,2:ncol(occData)]

# drop all species that are not part of th 594 
rmSpec <- c("Phaseolus acutifolius","Phaseolus leptostachyus","Elymus elymoides","Leymus mollis","Phaseolus maculatus","Hordeum jubatum","Helianthus petiolaris","Ribes sanguineum","Phaseolus polystachios","Prunus serotina","Elymus trachycaulus","Hordeum brachyantherum","Ribes roezlii","Rubus hispidus","Ribes hudsonianum","Helianthus nuttallii","Helianthus pauciflorus","Humulus lupulus","Allium geyeri","Ribes oxyacanthoides","Fragaria x ananassa","Helianthus occidentalis","Fragaria virginiana","Elymus lanceolatus","Fragaria vesca","Helianthus niveus","Helianthus praecox","Prunus fasciculata","Ribes malvaceum","Rubus arcticus","Vitis rotundifolia","Fragaria chiloensis","Ribes aureum","Acer saccharum","Allium victorialis","Elymus stebbinsii","Helianthus debilis","Ipomoea ternifolia","Lactuca tatarica","Prunus ilicifolia","Prunus pumila","Ribes californicum","Rubus idaeus","Saccharum brevibarbe","Vitis aestivalis","Vitis cinerea","Zizania aquatica","Zizania palustris", "Allium schoenoprasum","Elymus glabriflorus",
            "Elymus glaucus","Ipomoea cordatotriloba","Juglans major","Juglans microcarpa","Leymus salina","Prunus virginiana","Ribes cereum","Rubus ursinus","Tripsacum dactyloides","Vaccinium crassifolium","Vaccinium erythrocarpum","Vaccinium ovalifolium"
)
ocd <- occData[!occData$taxon %in% rmSpec,]
taxons <- unique(ocd$taxon)
genera <- sort(unique(ocd$genus))

run_V <- "test20200203"

### 20200813 
# regenerating ERS and GRS gap maps to troubleshoot 
#### testing the ERSex gap map issue 
# determine the number of GRSex elements 
gex <- list.files(path = base_dir, pattern = "_grsEx.tif",recursive = TRUE, full.names = TRUE)
gex1 <- gex[grepl(pattern = "test20200203", x = gex)]
gex2 <- include(gex1,toMatch = taxons)
length(gex2)

gEx <- list()
eEx <- list()
for(i in 1:length(gex2)){
  gEx[[i]] <- try(removeNA(gex2[i]))
}
allgEx <- sum_all(gEx, extend_all(gEx))
allgEx[allgEx[] == 0] <- NA
raster::writeRaster(x = allgEx, filename = paste0(outdir2, "grsExGapSummary", Sys.Date(),".tif") , overwrite = TRUE)

qtm(allgEx)

  "F:\nrelD\cwrNA\gap_analysis\Cucurbita\Cucurbita digitata\test20200203\modeling\alternatives\ga50.tif"
#determine the number of ERSex elements 
eex <- list.files(path = base_dir, pattern = "_ersEx.tif",recursive = TRUE, full.names = TRUE)
eex1 <- eex[grepl(pattern = "test20200203", x = eex)]
eex2 <- include(theList = eex1, toMatch = taxons)
length(eex2)

for(i in 1:length(eex2)){
   sdm <- try(removeNA(eex2[i]))
   sdm[sdm[] > 0] <- 1
   eEx[[i]] <- sdm
}


alleEx <- sum_all(eEx, extend_all(eEx))
alleEx[alleEx[]==0]<-NA
raster::writeRaster(x = alleEx, filename = paste0(outdir2, "ersExGapSummary", Sys.Date(),".tif") , overwrite = TRUE)


### double check on the GRSin map 
# determine the number of GRSin elements 
gin <- list.files(path = base_dir, pattern = "spdist_thrsld_median",recursive = TRUE, full.names = TRUE)
gin1 <- gin[grepl(pattern = "test20200203", x = gin)]
gin2 <- include(gin1,toMatch = taxons)
length(gin2)

gIn <- list()
for(i in 1:length(gin2)){
  gIn[[i]] <- try(removeNA(gin2[i]))
}
allgin <- sum_all(gIn, extend_all(gIn))
allgin[allgin[] == 0] <- NA
raster::writeRaster(x = allgin, filename = paste0(outdir2, "grsinGapSummary", Sys.Date(),".tif") , overwrite = TRUE)

qtm(allgin)










#compare to older version 
r1 <- raster::raster("F:/nrelD/cwrNA/gap_analysis/ersExCumulativeGapMap2020-08-11.tif")
r1
alleEx

### pre 20200813 workflow 
gIns <- c()
gExs <- c()
for(i in genera){
  print(i)
  #select all species in genera 
  oc1 <- ocd %>% 
    dplyr::filter(genus == i)
  spList2 <- unique(oc1$taxon)
  inRast <- c()
  exRast <- c()
  for(j in spList2){
    sp_dir <- paste0("F:/nrelD/cwrNA/gap_analysis/",i,"/",j,"/",run_V)
    # test for file and read it as object 
    ex <- paste0("/gap_analysis/exsitu/",j,"_ersEx.tif")
    ins <- paste0("/gap_analysis/insitu/",j,"_ersIn.tif")
                 
    if(file.exists(paste0(sp_dir, ex))){
      ersex <-removeNA(paste0(sp_dir, ex))
      ersex[ersex[] > 0] <- 1 
      exRast <- append(exRast, ersex)
    }
    # if(file.exists(paste0(sp_dir, ins))){
    #   ersin <-removeNA(paste0(sp_dir, ins))
    #   ersin[ersin[] > 0] <- 1 
    #   inRast <- append(inRast, ersin)
    # }
  }
  
  outdir <-  paste0("F:/nrelD/cwrNA/gap_analysis/",i,"/")
  # sum the rasters and write out result
  if(length(exRast)> 0){
    allEx <- sum_all(exRast, extend_all(exRast))
    gExs <- append(gExs, allEx)
    raster::writeRaster(x = allEx, filename = paste0(outdir, "ersExGapSummary", Sys.Date(),".tif") , overwrite = TRUE)
  }

  # if(length(inRast)> 0){
  #   allIn <- sum_all(inRast, extend_all(inRast))
  #   gIns <- append(gIns, allIn)
  #   raster::writeRaster(x = allIn, filename = paste0(outdir, "ersInGapSummary", Sys.Date(),".tif"), overwrite = TRUE)
  # }
}
# Compile the full species level gap maps 
outdir2 <-  "F:/nrelD/cwrNA/gap_analysis/"
fullEx <- sum_all(gExs, extend_all(gExs))
raster::writeRaster(x = fullEx, filename = paste0(outdir2, "ersExCumulativeGapMap",Sys.Date(),".tif"))

fullIn <- sum_all(gIns, extend_all(gIns))
raster::writeRaster(x = fullIn, filename = paste0(outdir2, "ersInCumulativeGapMap",Sys.Date(),".tif"))
tmap::qtm(fullEx)
tmap::qtm(fullIn)
