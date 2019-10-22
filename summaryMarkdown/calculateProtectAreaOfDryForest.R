library(rgdal)
library(sp)
library(raster)
library(dplyr)
library(tmap)
library(rgeos)
tmap::tmap_mode("view")

# load in tnc shp 
ecoReg <- readOGR("D:/cwrNA/parameters/ecoregions/tnc_terr_ecoregions.shp")

# filter for only locations in dry forest biome 
ecoRInter <- ecoReg@data %>%
  dplyr::filter(WWF_MHTNAM == "Tropical and Subtropical Dry Broadleaf Forests")
ecoNames <- unique(ecoRInter$ECO_NAME)

# select all polygons that have one of these names 
dryFor <- ecoReg[ecoReg$ECO_NAME %in% ecoNames,]
dryFor$area <- raster::area(dryFor)/1000000
totalDryForArea <- sum(dryFor$area)
# load in protected areas 
proArea <- raster::raster("D:/cwrNA/parameters/protectedAreas/wdpa_reclass.tif")

# cell size of protect area is 0.041667 this 
areaOfCell <- (0.041667 * 111)^2
# set values to equal 1 or NA 
proArea[proArea != 1] <- NA
crs(proArea)
# multiple the rasster by the shapefile 
test2 <- raster::mask(x = proArea, mask = dryFor)


# read in the americas shp to use to mask protect areas again 
americas <- readOGR("D:/cwrNA/parameters/New folder/theAmericas.shp")

# mask test2 to americas 
test3 <- raster::mask(x = test2, mask = americas)
# get a count of the number of cells with values 
overlaps <- length(test3[test3==1,])
percentInProtectAreas <- ((overlaps * areaOfCell)/totalDryForArea)*100
percentInProtectAreas

