###
# test for species occurring within protected areas 
# 20200625
# dan.carver@carverd.com
### 
library(raster)
library(sp)
library(dplyr)
library(tmap)
library(sf)
library(readxl)
tmap::tmap_mode("view")
# two steps 
# 1. what occurrence fall within protected aras 
# 2. what predicted distributions fall within protects areas 

#ensure that the protected areas are part of the WPDA 
wdpa <- raster::shapefile("F:/nrelD/cwrNA/parameters/protectedAreas/WDPA_Mar2020-shapefile/WDPA_na.shp")
wdpa <- wdpa[wdpa$ISO3 == "USA", ]
View(wdpa@data)


proA <- read.csv("F:/nrelD/cwrNA/parameters/protectedAreasValidation/protectedAreaListToCheck.csv")
proA <- proA[!is.na(proA$WDPA.id),]
# get unique names 

#read in occurrence data 
base_dir <- "F:/nrelD/cwrNA"
occData <- data.table::fread(paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2020-07-21a.csv"),
                             header = TRUE)
occData <- occData[,2:ncol(occData)]

# drop all species that are not part of th 594 
rmSpec <- c("Phaseolus acutifolius","Phaseolus leptostachyus","Elymus elymoides","Leymus mollis","Phaseolus maculatus","Hordeum jubatum","Helianthus petiolaris","Ribes sanguineum","Phaseolus polystachios","Prunus serotina","Elymus trachycaulus","Hordeum brachyantherum","Ribes roezlii","Rubus hispidus","Ribes hudsonianum","Helianthus nuttallii","Helianthus pauciflorus","Humulus lupulus","Allium geyeri","Ribes oxyacanthoides","Fragaria x ananassa","Helianthus occidentalis","Fragaria virginiana","Elymus lanceolatus","Fragaria vesca","Helianthus niveus","Helianthus praecox","Prunus fasciculata","Ribes malvaceum","Rubus arcticus","Vitis rotundifolia","Fragaria chiloensis","Ribes aureum","Acer saccharum","Allium victorialis","Elymus stebbinsii","Helianthus debilis","Ipomoea ternifolia","Lactuca tatarica","Prunus ilicifolia","Prunus pumila","Ribes californicum","Rubus idaeus","Saccharum brevibarbe","Vitis aestivalis","Vitis cinerea","Zizania aquatica","Zizania palustris", "Allium schoenoprasum","Elymus glabriflorus",
            "Elymus glaucus","Ipomoea cordatotriloba","Juglans major","Juglans microcarpa","Leymus salina","Prunus virginiana","Ribes cereum","Rubus ursinus","Tripsacum dactyloides","Vaccinium crassifolium","Vaccinium erythrocarpum","Vaccinium ovalifolium"
)
ocd <- occData[!occData$taxon %in% rmSpec,]

d3$rmspec <- d3$speciesNativeRange %in% rmSpec
View(d3)
# # test individual locations 
# qtm(wdpa[wdpa$WDPAID ==969,])
# 
# ### read in NYC data 
# nyc <- st_read("F:/nrelD/cwrNA/parameters/protectedAreasValidation/kmlData/central park.kml")
# nyc <- st_zm(nyc, drop = TRUE)
# nyc <-  as(nyc, 'Spatial')
# qtm(nyc)
# 
# # repeat for gwc park 
# gwc <- st_read("F:/nrelD/cwrNA/parameters/protectedAreasValidation/kmlData/george washington Carver park.kml")
# gwc <- st_zm(gwc, drop = TRUE)
# gwc <-  as(gwc, 'Spatial')
# qtm(gwc)
# # repeat for voyageurs NP 
# sp <- raster::shapefile("F:/nrelD/cwrNA/parameters/protectedAreasValidation/nps_boundary/nps_boundary.shp")
# voy <- sp[sp@data$UNIT_CODE == "VOYA",]
# crs(voy) <- crs(wdpa)
# qtm(voy)
# 
# # find values within the WDPA 
areaIds <- unique(proA$WDPA.id)
wpdaNames <- unique(proA$Name)

wdpaIndex <- wdpa[wdpa$WDPAID %in% areaIds, ]

dim(wdpaIndex)
View(wdpaIndex@data)



### 
# Test all preserves
# 
#
###



# allProAreas <- wdpa@data$NAME
# write.csv(x = wdpa@data, file = "F:/nrelD/cwrNA/parameters/protectedAreasValidation/usaProtectedArea.csv")
### 1. what occurrence fall within protected areas 
base_dir <<- "F:/nrelD/cwrNA"
occData <- data.table::fread(paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2020-04-07.csv"),
                             header = TRUE)

occData <- occData[,2:ncol(occData)]
ocd <- occData[complete.cases(occData$latitude),]

output <- data.frame(matrix(nrow = 0, ncol =2 ))
colnames(output) <- c("protected area", "taxon")


for(i in areaIds){
  area <- wdpaIndex[wdpaIndex$WDPAID == i,]
  print(area@data$NAME)
  qtm(area)
  # addation for central park 
  # i <- "Central Park"
  # area <- nyc
  ex <- raster::extent(area)
  d1 <- ocd %>% 
    dplyr::filter(latitude > ex[3] & latitude < ex[4] & longitude < ex[1] & longitude > ex[2])
  
  if(nrow(d1)!=0){
    # none of my examples have points with in them 
    coords <- data.frame(matrix(nrow = nrow(d1), ncol = 2))
    colnames(coords) <- c("longitude","latitude")
    coords$latitude <- as.numeric(d1$latitude)
    coords$longitude <- as.numeric(d1$longitude)
    points <- sp::SpatialPointsDataFrame(coords = coords,data = d1, proj4string = crs(wdpa))
    # visual test for intersection of points 
    tm_shape(area)+tm_polygons()+tm_shape(points)+tm_dots()
    points$inProtectAreas <- sp::over(points, sp::SpatialPolygons(area@polygons, proj4string = crs(points)))
    inP <- points@data[!is.na(points$inProtectAreas),]
    if(nrow(inP)>0 ){
      samples <- unique(inP$taxon)
      samples <- samples[!is.na(samples)]
    }else{
      samples<-NA
    }
    df <- data.frame(matrix(nrow = length(samples), ncol=2))
    colnames(df) <- c("protected area", "taxon")
    df$`protected area` <- rep(i, length(samples))
    df$taxon <- samples
    output <- rbind(output, df)
  }else{
    samples <- NA  
    df <- data.frame(matrix(nrow = length(samples), ncol=2))
    colnames(df) <- c("protected area", "taxon")
    df$`protected area` <- rep(i, length(sample))
    df$taxon <- samples
    output <- rbind(output, df)
  }
  #test species observed against species collected 
}
output <- dplyr::left_join(x = output,y= proA, by = c("protected area" = "WDPA.id"))
# for kmls 
output$Name <- "Central Park"
View(output)

qs <- output %>% 
  group_by(Name)%>%
  summarise(count = n())
View(qs)
### nyc elements 
# output$`protected area` <- "Central Park"

write.csv(x = output, file = "F:/nrelD/cwrNA/parameters/protectedAreasValidation/occurrencesDataFromProtectedAreas.csv")

# Pull in all occurrence data 
# Filter by extent of the protect area 
# generate sp object with what is left over 
# extract values to the protect area shp 
# write out the resulting species list 

###todo occurrences 
# index by id not name 
# add a counts column to the samples data 



### 2. 


#load total predicted diversity maps
richM <- raster::raster("F:/nrelD/cwrNA/runSummaries/richnessMap_2020-03-18.tif")
# test it against all areas 

# dataframe to see if protected area makes it to the next step
d2 <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(d2) <- c("proArea", "speciesPresent")

for(i in areaIds){
  a <- wdpaIndex[wdpaIndex$WDPAID == i,]
  # nyc test 
  # a <- gwc
  time <- Sys.time()
  t1 <- raster::crop(x = richM, y = a)
  #t1 <- raster::mask(x = richM, mask = a) # this adds a lot of time. I'm only going to mask at the species level.
  vals <- unique(values(t1))
  vals <- vals[!is.na(vals)]
  df <- data.frame(matrix(nrow = 1, ncol = 2))
  colnames(df) <- c("proArea", "speciesPresent")
  df$proArea <- i 
  if(length(vals)>0){
    df$speciesPresent <- TRUE
  }else{
    df$speciesPresent <- FALSE
  }
  d2 <- rbind(d2,df)
  time <- Sys.time() - time
  print(paste0(i, " took ", time , " time to generate"))
}

#check d2 to ensure that all 
d2


d3 <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(d3) <- c("proArea", "speciesNativeRange", "speciesPredictedPresent")

# load on genera summary maps 
genera <- sort(unique(occData$genus))

#i <- 4444407
for(i in areaIds){
  a <- wdpaIndex[wdpaIndex$WDPAID == i,]
  # for kml locations 
  # a <- nyc
  # print(a$Name)
  time <- Sys.time()
  for(g in genera){
    print(g)
    dir <- paste0(base_dir, "/gap_analysis/", g)
    files <- list.files(path = dir, pattern = "richnessMap_2020", full.names = TRUE)
    files <- sort(files, decreasing = TRUE)
    map <- try(raster::raster(files[1]))
    t2 <- tryCatch(!is.null(raster::values(raster::crop(x = map, y = a))), error=function(e) return(FALSE)) 
    if(t2 == TRUE){
      spList <- occData %>% dplyr::filter(genus == g) %>% dplyr::distinct(taxon)
      for(s in spList$taxon){
        dir2 <- paste0(dir,"/", s,"/test20200203/" )
        files <- list.files(path = dir2, pattern = "spdist_thrsld_median.tif", full.names = TRUE, recursive = TRUE)
        map <- try(raster::raster(files[1]))
        # check for no map or no overlaping extent
        t3 <- tryCatch(!is.null(raster::values(raster::crop(x = map, y = a))), error=function(e) return(FALSE))
        if(t3 == TRUE){
          print(s)
          # reduce the extent 
          r2 <- raster::crop(x = map, y = a)
          if(1 %in% values(r2) | 0 %in% values(r2)){
            # convert to polygon to test for overlap 
            r3 <- raster::rasterToPolygons(x = r2)
            # test for intersect 
            t1 <- sp::over(x = a, y = r3,returnList = TRUE)
            
            # # try making a raster of protected area 
            # a1 <- rasterize(x = a, y= proArea, field =1)
            
            #pull unique values 
            vals <- unique(t1[[1]]$spdist_thrsld_median)
            vals <- vals[!is.na(vals)]
            # set df for data storage
            df <- data.frame(matrix(nrow = 1, ncol = 3))
            colnames(df) <- c("proArea", "speciesNativeRange", "speciesPredictedPresent")
            df$proArea <- i 
            df$speciesNativeRange <- s
            # test for predicted presense
            if(length(vals) != 0){
              ifelse(test = 1 %in% vals,
                     yes = df$speciesPredictedPresent <- TRUE,
                     no = df$speciesPredictedPresent <- FALSE)  
              d3 <- rbind(d3, df)
              print(paste0(s," row ", nrow(d3)))
            } 
          }
        }
        try(rm(map))
      }
    }
  } 
  time <- Sys.time() - time
}
d3
View(d3)
     #join to protected areas names 
d4 <- dplyr::left_join(x = d3, y = proA, by = c("proArea"= "WDPA.id"))
write.csv(x = d4, file = "F:/nrelD/cwrNA/parameters/protectedAreasValidation/nineAreasPredictedSpecies.csv" )

#for kml 
d3$Name <- "Central Park"
write.csv(x = d3,
          file = "F:/nrelD/cwrNA/parameters/protectedAreasValidation/CentralParkPredictedSpecies.csv")


View(d4)
# read in xlsx file colin create to do a full join between datasets 
#install.packages("readxl")
library("readxl")

files <- list.files("F:/nrelD/cwrNA/parameters/protectedAreasValidation/reprotectedareasvalidationnewresource/originalData", 
                    full.names = TRUE, pattern = ".xlsx")
# drop 

d5 <- d4 %>%
  dplyr::arrange(Name)


o2 <- output %>%
  dplyr::arrange(Name)

locs <- sort(proA$Name)
df2 <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(df2) <- c("Location", "Taxon Observed via seinet", "species predicted present and observed by seinet", "species collected and observed by scinet",
                   "species predicted present and not observed by seinet", "species collected and not observed by seinet",
                   "species predicted and collect but not observed by seinet")
n = 1
for(i in wpdaNames){
  d1 <- files[grep(pattern = i, x = files)]
  d1 <- read_excel(d1)
  d2 <- d5[d5$Name == i,]
  #for kml areas
  d1 <- read_excel(files[4])
  d2 <- d3
  j1 <- dplyr::full_join(x = d1,y=d2, by = c("Taxon" = "speciesNativeRange"))
  o2 <- output
  o2$"speciesOccurrence" <- 1
  j2 <- dplyr::full_join(j1, o2, by = c("Taxon" = "taxon"), keep = TRUE)
  write.csv(x = j2, file = paste0("F:/nrelD/cwrNA/parameters/protectedAreasValidation/reprotectedareasvalidationnewresource/",
                                  "CentralPark","_joinedValidationData",Sys.Date(),".csv"))
  tes <- j2 %>% dplyr::filter(`Included in summary metrics` == "Y")
  tS <- nrow(tes) 
  tP <- nrow(tes %>% dplyr::filter(speciesPredictedPresent == TRUE))
  tC <- nrow(tes %>% dplyr::filter(`speciesOccurrence` == 1))
  aP <- nrow(j2 %>% dplyr::filter(speciesPredictedPresent == TRUE & is.na(Genus)))
  aC <- nrow(j2 %>% dplyr::filter(`speciesOccurrence` == 1) %>%
               dplyr::filter(is.na(Genus)))
  aB <- nrow(j2 %>% dplyr::filter(speciesPredictedPresent == TRUE & is.na(Genus) & `speciesOccurrence` == 1))
  df2[1,] <- c("CentralPark", tS,tP,tC,aP,aC, aB)
  
  write.csv(x = df2,
            file = paste0("F:/nrelD/cwrNA/parameters/protectedAreasValidation/reprotectedareasvalidationnewresource/validationSummaryCentralPark",Sys.Date(),".csv"))
  
  
  
  # process for the standardize data. 
  j1 <- dplyr::full_join(x = d1,y=d2, by = c("Taxon" = "speciesNativeRange"))
  o3 <- o2[o2$Name == i,] 
  o3$`Occurences Data Present in Preserve` <- TRUE
  j2 <- dplyr::full_join(j1, o3, by = c("Taxon" = "taxon"), keep = TRUE)
  write.csv(x = j2, file = paste0("F:/nrelD/cwrNA/parameters/protectedAreasValidation/reprotectedareasvalidationnewresource/", 
                                  i,"_joinedValidationData",Sys.Date(),".csv"))
  
  tes <- j2 %>% dplyr::filter(`Included in summary metrics` == "Y")
  tS <- nrow(tes)
  tP <- nrow(tes %>% dplyr::filter(speciesPredictedPresent == TRUE))
  tC <- nrow(tes %>% dplyr::filter(`Occurences Data Present in Preserve` == TRUE))
  aP <- nrow(j2 %>% dplyr::filter(speciesPredictedPresent == TRUE & is.na(Family)))
  aC <- nrow(j2 %>% dplyr::filter(`Occurences Data Present in Preserve` == TRUE) %>%
               dplyr::filter(is.na(Family)))
  aB <- nrow(j2 %>% dplyr::filter(speciesPredictedPresent == TRUE & is.na(Family) & `Occurences Data Present in Preserve` == TRUE))
  df2[n,] <- c(i, tS,tP,tC,aP,aC, aB)
  n = n +1 
}

View(df2)
write.csv(x = df2,
          file = paste0("F:/nrelD/cwrNA/parameters/protectedAreasValidation/reprotectedareasvalidationnewresource/validationSummary",Sys.Date(),".csv"))
sort(f1)

###########
# predicted area process for locations not within the wdpa 




####
# test for occurrences and predicted values within protected areas
#### 

proA <- unique(wdpa$WDPAID)
# prep data to remove taxon with intraspecific species from this method. 
rmSpec <- c("Phaseolus acutifolius","Phaseolus leptostachyus","Elymus elymoides","Leymus mollis","Phaseolus maculatus","Hordeum jubatum","Helianthus petiolaris","Ribes sanguineum","Phaseolus polystachios","Prunus serotina","Elymus trachycaulus","Hordeum brachyantherum","Ribes roezlii","Rubus hispidus","Ribes hudsonianum","Helianthus nuttallii","Helianthus pauciflorus","Humulus lupulus","Allium geyeri","Ribes oxyacanthoides","Fragaria x ananassa","Helianthus occidentalis","Fragaria virginiana","Elymus lanceolatus","Fragaria vesca","Helianthus niveus","Helianthus praecox","Prunus fasciculata","Ribes malvaceum","Rubus arcticus","Vitis rotundifolia","Fragaria chiloensis","Ribes aureum","Acer saccharum","Allium victorialis","Elymus stebbinsii","Helianthus debilis","Ipomoea ternifolia","Lactuca tatarica","Prunus ilicifolia","Prunus pumila","Ribes californicum","Rubus idaeus","Saccharum brevibarbe","Vitis aestivalis","Vitis cinerea","Zizania aquatica","Zizania palustris", "Allium schoenoprasum","Elymus glabriflorus",
            "Elymus glaucus","Ipomoea cordatotriloba","Juglans major","Juglans microcarpa","Leymus salina","Prunus virginiana","Ribes cereum","Rubus ursinus","Tripsacum dactyloides","Vaccinium crassifolium","Vaccinium erythrocarpum","Vaccinium ovalifolium"
)
ocd <- occData[!occData$taxon %in% rmSpec,]
unique(ocd$taxon)
# Im saving the index value so we can use this to reference back to any occcurrences in a potected
# area at a later point 
write.csv(x = ocd, file = "F:/nrelD/cwrNA/parameters/protectedAreas/only594Occurrence.csv")

# read in full species prediction
allCWR <- raster::raster("F:/nrelD/cwrNA/runSummaries/richnessMap_2020-07-23.tif" )
allCWR
qtm(allCWR)

#empty data to hold location count outputs 
df <- data.frame(matrix(nrow = 0,ncol = 5))
colnames(df) <- c("Location", "total occurrences", 
                  "total unique species", "index value", "totalPredictedSpecies")
n = 1
for(i in proA){
  print(n)
  #empty df
  df1 <- data.frame(matrix(nrow = 1,ncol = 5))
  colnames(df1) <- c("Location", "total occurrences", 
                     "total unique species", "index value", "totalPredictedSpecies" )
  #pull location 
  loc <- wdpa[wdpa$WDPAID == i,]
  df1$Location <- loc$WDPAID
  # test if occurrence are within the extent 
  ex <- extent(loc)
  # d1 <- ocd %>% 
  #   dplyr::filter(latitude > ex[3] & latitude < ex[4] & longitude < ex[1] & longitude > ex[2])
  # if true, extract data from the polygon to test for physical overlap
  # if(nrow(d1)>=1){
  #   d1$latitude <- as.numeric(d1$latitude)
  #   d1$longitude <- as.numeric(d1$longitude)
  #   s1 <- sp::SpatialPointsDataFrame(coords = d1[,c(6,5)], data = d1, proj4string = crs(loc))
  #   s1$InPro <- raster::extract(x = loc,y = s1)
  #   s1 <- s1[!is.na(s1$InPro$WDPAID),]
  #   # if overlap does occur, add information to main dataframe. 
  #   if(nrow(s1) >= 1){
  #     df1$`total occurrences` <- nrow(s1)
  #     df1$`total unique species` <- length(unique(s1$taxon))
  #     df1$`index value` <- list(s1$V1)
  #   }else{
  #     df1$`total occurrences` <- 0
  #     df1$`total unique species` <- 0
  #     df1$`index value` <- NA
  #   }
  # }else{
  #   df1$`total occurrences` <- 0
  #   df1$`total unique species` <- 0
  #   df1$`index value` <- NA
  # }
  # extract the maximum value from raster 
  r1 <- try(raster::crop(x = allCWR, y = ex))
  
  
  if(class(r1)=="try-error"){
    df1$totalPredictedSpecies <- 0
    
  }else{
    vals <- values(r1)
    vals <- vals[!is.na(vals)]
    if(length(vals)>0){
      r3 <- raster::rasterToPolygons(x = r1)
      r3 <- sp::over(x = loc, y = r3, returnList = TRUE)
      df1$totalPredictedSpecies <- max(r3[[1]]$richnessMap_2020.07.23,na.rm = TRUE)
    }else{
      df1$totalPredictedSpecies <- 0
    }
  }
  
  df <- rbind(df, df1)
  n=n+1
}

### join to full wdpa dataset. 
View(df)


df3 <- df
df3 <- df3 %>% 
  dplyr::left_join(y = wdpa@data, by = c("Location" = "WDPAID"))

# read in previous version and join the improved protected areas assessment to it 
pa <- read.csv("F:/nrelD/cwrNA/parameters/protectedAreasValidation/overlapAllProtectedAres.csv")
# reassgn based on new values 
pa$totalPredictedSpecies <- df$totalPredictedSpecies
# join with the full wdpa data 
df4 <- dplyr::left_join(x = pa[,1:6], y= wdpa@data, by = c("Location"= "WDPAID"))

View(df4)
df4 <- df4[,c(2,3,4,5,12,13:39)]
names(df4)

View(df4)


write.csv(x = df4, 
          file = paste0("F:/nrelD/cwrNA/parameters/protectedAreasValidation/overlapAllProtectedAres",Sys.Date(),".csv"))



df3$`index value` <- as.character(df3$`index value`)

parkNames <- wdpa@data[,c(1,4,6)]
df3 <- df3 %>% dplyr::left_join(parkNames, by = c("Location"= "WDPAID"))
fr <- df3 %>% 
  dplyr::group_by(totalPredictedSpecies) %>%
  dplyr::summarise(count = n())
View(fr)

write.csv(x = df3, 
          file = paste0("F:/nrelD/cwrNA/parameters/protectedAreasValidation/predictedPresenceAllProtectedAres",Sys.Date(),".csv"))




