###
# reworking of wiews data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)
#install.packages("ff")
library(ff)
install.packages("readr")
library(readr)

#set base dir
base_dir <- "D:/cwrOfNA/occurrence_data2019_05_29/cwr_occ"


# names <- "id	old_id	taxon_id	metadata_id	field_collected_data	data_public_access	filename	username	source	provider_institute_id	provider_name	institute_id	institute_name	is_expert	collection	source_url	botrecat	availability	image	final_image	unique_number	barcode	vno_1	vno_2	voucher_id	x1_family	x1_genus	x1_sp1	x1_author1	x1_rank1	x1_sp2	x1_author2	x1_rank2	x1_sp3	x1_author3	x1_detby	x1_detdate	x1_detdd	x1_detmm	x1_detyy	x1_detstat	x2_family	x2_genus	x2_sp1	x2_author1	x2_rank1	x2_sp2	x2_author2	x2_rank2	x2_sp3	x2_author3	x2_detby	x2_detdate	x2_detdd	x2_detmm	x2_detyy	x2_detstat	x3_family	x3_genus	x3_sp1	x3_author1	x3_rank1	x3_sp2	x3_author2	x3_rank2	x3_sp3	x3_author3	x3_detby	x3_detdate	x3_detdd	x3_detmm	x3_detyy	x3_detstat	is_hybrid	hybrid_memo	tnrs_overall_score	tnrs_source	tnrs_x1_family	tnrs_final_taxon	tnrs_author1	taxon_final	f_x1_genus	f_x1_sp1	f_x1_rank1	f_x1_sp2	f_x1_rank2	f_x1_sp3	annotated_specimen	type	type_memo	collector	addcoll	collnumber	prefix	number	suffix	colldate	colldd	collmm	collyy	final_country	country	iso	final_iso2	adm1	adm2	adm3	adm4	local_area	locality	coord	lat_deg	lat_min	lat_sec	ns	final_ns	latitude	long_deg	long_min	long_sec	ew	final_ew	longitude	llorig	lldatum	georef_history_id	latitude_georef	longitude_georef	distance_georef	georef_flag	alt	final_alt	alt_max	final_alt_max	cult_stat	final_cult_stat	origin_stat	final_origin_stat	soil	slope	aspect	habitat_txt	plant_desc	frequency	fl_code	fr_code	inflo_graminea	vernacular	language	uses	dups	notes	comments	timestamp	coord_check_old	citation	final_lat	final_lon	coord_source	taxstand_family	taxstand_genus	taxstand_sp1	taxstand_sp2	taxstand_author1	taxstand_final_taxon	temp_id	grin_final_taxon	x1_taxon	gbif_genus	gbif_species	collection_code	gbif_references	datasetKey	gbif_rank	origin_stat_inv	iso2	taxon_source	f_x1_family	quality_row	visibility"
# names <- gsub(pattern = "[[:blank:]]+",replacement = ",", x = names)
# names <- unlist(strsplit(x = names, split = ","))
# 
# 
# names(data) <- names
# Load in data 
csvPath <- paste0(base_dir,"/colin_list2/colin_list2.csv")

# testing the FF library 
#system.time(data2 <- ff::read.csv.ffdf(file=csvPath))

system.time(dataReadR <- readr::read_table(file=csvPath,sep="|"))


data <- data.table::fread(csvPath, header = FALSE,)
View(data[1:100])
# Select necessary columns from dataset 
dataThin <- data %>%
  dplyr::select("V2",	"V3",	"V15",	"V16",	"V39",	"V62",	"V81",	"V82")
nr <- nrow(dataThin)

dataThin2 <- dataThin %>%
  filter(V81 == "\\N")

#convert lat long classes to numeric 
dataThin$V81 <- as.factor(dataThin$V81)
dataThin$V82 <- as.numeric(dataThin$V82)

# define structure of the empty dataframe 
df <- data.frame(taxon=character(nr),
                 genus=character(nr),
                 species=character(nr),
                 latitude=numeric(nr),
                 longitude=numeric(nr),
                 databaseSource=character(nr),
                 institutionCode=character(nr),
                 type=factor(nr),
                 uniqueID=factor(nr),
                 sampleCategory=character(nr),
                 country=character(nr),
                 iso3=character(nr),
                 localityInformation=character(nr),
                 stringsAsFactors=FALSE)

# assign columns to location in empty dataframe
df$taxon <- dataThin$V39
df$genus <- dataThin$V15
df$species <- dataThin$V16
df$latitude <- dataThin$V82
df$longitude <- dataThin$V81
df$databaseSource <- dataThin$V3
df$institutionCode <- NA
df$type <- dataThin$V2
df$uniqueID <- NA
df$sampleCategory <- NA
df$country <- dataThin$V62
df$iso3 <- NA
df$localityInformation <- NA


test1 <- df[1:10,]
View(test1)

# test 
testLatLong <<- dataThin %>%
  dplyr::select(c("V81", "V82")) %>%
  mutate(hasLat = !is.na(V81) & V81 != "\\N" & V81 != "") %>%
  mutate(hasLong = !is.na(V82) & V82 != "\\N"& V82 != "") %>%
  mutate(hasLatLong = hasLat & hasLong)

summariseErrors <- testLatLong %>%
  filter(hasLat == TRUE & hasLong ==FALSE | hasLat == FALSE & hasLong ==TRUE)

print(paste0("there are ", nrow(summariseErrors)," miss matach lat long pairs."))


# actual code 
testLatLong <<- df %>%
  dplyr::select(c("latitude", "longitude")) %>%
  mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "") %>%
  mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "") %>%
  mutate(hasLatLong = hasLat & hasLong)

summariseErrors <- testLatLong %>%
  filter(hasLat == TRUE & hasLong ==FALSE | hasLat == FALSE & hasLong ==TRUE)

print(paste0("there are ", nrow(summariseErrors)," miss matach lat long pairs."))




# write out the new dataframe 
write.csv(x = df, file = paste0(base_dir,"/refinedcwrOcc.csv"))
