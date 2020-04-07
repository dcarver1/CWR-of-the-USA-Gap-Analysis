###
# generate counts of all  species and species in North america 
# 20191112
# carver.dan1@gmail.com
### 
library(tidyverse)
library(sp)
library(rgdal)
library(rgeos)
library(raster)

base_dir <<- "D:/cwrNA"
par_dir <<- paste0(base_dir , "/parameters")
occ_dir <<- paste0(par_dir, "/occurenceData")

# read in raw data 
occData <<- data.table::fread(paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2019-12-13.csv"),header = TRUE)
naSHP <<- readOGR(paste0(par_dir,"/northAmericaArea/northAmericaArea.shp"),verbose = FALSE)
naSHP@data <- naSHP@data %>% dplyr::select(-c(1:95))#

# check for duplicates with latlong and 


# getting counts of species across the globe. 
d1 <- occData %>%
  mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "") %>%
  mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "") %>%
  mutate(hasLatLong = hasLat & hasLong)
d2 <- d1 %>%
  group_by(taxon, hasLatLong)%>%
  dplyr::summarise(count = n())
write.csv(d2,file=paste0(occ_dir, "/allDataCounts", Sys.Date(),".csv"))

# filter to the general area of the USA to drop points before intersect 
d1a <- d1 %>%
  filter(latitude > 10)%>%
  filter(longitude < -50)

coord <- d1a %>%
  filter(hasLatLong == TRUE) %>%
  dplyr::select(longitude,latitude)
coord[] <- lapply(coord, function(x) as.numeric(x))

c1 <- coord[complete.cases(coord),]

d3 <- filter(d1a, hasLatLong == TRUE) 


spPoint <- SpatialPointsDataFrame( coords = c1 ,data = d3)

#proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
if( nrow(spPoint@data) == 0){
  print("there are no coodinate pairs for this species")
  spPoint <<- "no data available"
}
# mask to North America 
crs(spPoint) <- crs(naSHP)


intersect1 <- intersect(spPoint, naSHP)
write.csv(intersect1@data,file=paste0(occ_dir, "/allNorthAmericaOccuenceData", Sys.Date(),".csv"))


d2a <- intersect1@data %>%
  group_by(taxon)%>%
  dplyr::summarise(count = n())

write.csv(d2a,file=paste0(occ_dir, "/allNorthAmericaCounts", Sys.Date(),".csv"))

