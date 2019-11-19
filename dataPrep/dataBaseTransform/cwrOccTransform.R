###
# reworking of cwrOCC data
# 20190815
# carver.dan1@gmail.com
### 

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "D:/cwrNA/occurrence_data2019_05_29/cwr_occ/20191114"

# Load in data 
csvPath <- paste0(base_dir,"/colin_dan_data.csv")

data <- data.table::fread(csvPath, header = TRUE,)
#View(data)
dim(data)
# filter by filename 
namesToRemove <- c("GBIF",                                            
                   "raw_ATGGC_ff.xlsx",                              
                   "raw_AVRDC_ff.xlsx",
                    "raw_Bioversity_EUR_SING_2013_1b_ff.xlsx",      
                    "raw_Bioversity_EUR_SING_2013_1c_ff.xlsx",         
                    "raw_Bioversity_EUR_SING_2013_1d_ff.xlsx",         
                    "raw_Bioversity_EUR_SING_2013_1e_ff.xlsx",         
                   "raw_Bioversity_EUR_SING_2013_1a_ff.xlsx",         
                   "raw_Bioversity_EUR_SING_2013_2a_ff.xlsx",         
                   "raw_Bioversity_EUR_SING_2013_2b_ff.xlsx",         
                   "raw_Bioversity_EUR_SING_2013_2c_ff.xlsx",         
                   "raw_Bioversity_EUR_SING_2013_2d_ff.xlsx", 
                  "raw_CATIE_2014_ff.xlsx", 
                  "raw_CK_USDA_NPGS_GRIN_USCWR.xlsx",  
                  "raw_Helianthus_Marek_newdata_ff",
                 "raw_IRRI_newdata_ff.xlsx",
                  "raw_USCWRExtras_CS_FF.xlsx",                      
                  "raw_USDA_NPGS_GRIN_extras_ff.xlsx",               
                  "raw_USDA_NPGS_GRIN_FF.xlsx",                      
                  "raw_USDA_NPGS_GRIN_Hijmans_ff.xlsx") 

data <- data[!data$filename %in% namesToRemove,]
#View(data)
dim(data)



# Select necessary columns from dataset 
dataThin <- data %>%
  dplyr::select("source",	"institute_id",	"f_x1_genus",
                "f_x1_sp1", "f_x1_rank1","f_x1_sp2","f_x1_rank2", "f_x1_sp3",
                "taxstand_final_taxon", "final_country", "final_lat", "final_lon", 
                "adm1","adm2","adm3","adm4", "locality",
                "final_cult_stat","final_origin_stat") 
              

# filter by final cult status- not   "cultivated"
dataThin <- dataThin[!dataThin$final_cult_stat %in% c("cultivated"),]
dim(dataThin)


# build species from "f_x1_sp1", "f_x1_rank1","f_x1_sp2","f_x1_rank2", "f_x1_sp3",
dataThin <- tidyr::unite(data = dataThin, "species",f_x1_sp1, f_x1_rank1,f_x1_sp2,f_x1_rank2, f_x1_sp3,sep=' ')
dataThin$species <- gsub(pattern = 'NULL', replacement = "",x = dataThin$species)

# build locality from "adm1","adm2","adm3","adm4", "locality",
dataThin <- tidyr::unite(data = dataThin, "locality",adm1,adm2,adm3,adm4,locality,sep=' -- ')

nr <- nrow(dataThin)


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
                 biologicalStatus = character(nr), 
                 collectionSource = character(nr),
                 finalOriginStat = character(nr),
                 stringsAsFactors=FALSE)

# assign columns to location in empty dataframe
df$taxon <- dataThin$taxstand_final_taxon
df$genus <- dataThin$f_x1_genus
df$species <- dataThin$species
df$latitude <- dataThin$final_lat
df$longitude <- dataThin$final_lon
df$databaseSource <- "cwrOCC"
df$institutionCode <- dataThin$institute_id
df$type <- dataThin$source
df$uniqueID <- NA
df$sampleCategory <- NA
df$country <- dataThin$final_country
df$iso3 <- NA
df$localityInformation <- dataThin$locality
df$biologicalStatus <- NA
df$collectionSource <- dataThin$final_cult_stat
df$finalOriginStat <- dataThin$final_origin_stat

# remove the underscore from taxon column 
df$taxon <- gsub(pattern = '_', replacement = " ",x = df$taxon)

# pull in checkSynomyn function and apply it 
source(file="D:/cwrNA/src/dataPrep/dataBaseTransform/checkSynonymsFunction.R")
df <- checkSynonym(df)


# test 
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
