###
# reworking of wiews data
# 20190815
# carver.dan1@gmail.com
###

library(tidyverse)
library(data.table)

#set base dir
base_dir <- "D:/cwrNA/occurrence_data2019_05_29/cwr_occ/20191114"
# Load in data
csvPath <- paste0(base_dir,"/colin_dan_data.csv")
data <- data.table::fread(csvPath, header = TRUE)
d1 <- head(data)


#filter by filename 
exclude <- read.csv(file = "D:/cwrNA/occurrence_data2019_05_29/cwr_occ/ExcludefromCWROCC.csv", header=TRUE) %>%
  filter(Exclude == "Y")

listExclude<- c("GBIF","raw_ATGGC_ff.xlsx","raw_AVRDC_ff.xlsx",
                "raw_Bioversity_EUR_SING_2013_1a_ff.xlsx",
                "raw_Bioversity_EUR_SING_2013_1b_ff.xlsx",
                "raw_Bioversity_EUR_SING_2013_1c_ff.xlsx",
                "raw_Bioversity_EUR_SING_2013_1d_ff.xlsx",
                "raw_Bioversity_EUR_SING_2013_1e_ff.xlsx",
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


data <- data[!data$filename %in% listExclude,]
#View(data)
dim(data)


# Select necessary columns from dataset
dataThin <- data %>%
  dplyr::select("id","source","provider_institute_id","f_x1_genus","f_x1_sp1","taxstand_final_taxon",
        "final_lon", "final_lat", "adm1", "adm2", "adm3", "adm4", "locality", "cultivated")
nr <- nrow(dataThin)

# filter by final cult status- not   "cultivated"
dataThin <- dataThin[!dataThin$final_cult_stat %in% c("cultivated"),]
dim(dataThin)

# define structure of the empty dataframe
df <- data.frame(taxon=character(nr),
                 genus=character(nr),
                 species=character(nr),
                 latitude=double(nr),
                 longitude=double(nr),
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
df$taxon <- dataThin$taxstand_final_taxon
df$genus <- dataThin$f_x1_genus
df$species <- NA
df$latitude <- dataThin$final_lat
df$longitude <- dataThin$final_lon
df$databaseSource <- "cwr_occ"
df$institutionCode <- dataThin$provider_institute_id
df$type <- dataThin$source
df$uniqueID <- dataThin$id
df$sampleCategory <- NA
df$country <- NA
df$iso3 <- NA
df$localityInformation <- dataThin$locality
df$biologicalStatus <- NA
df$collectionSource <- NA
df$finalOriginStat <- NA
   


# Spilt name to get at genus and species
#test <- df[1:100,]
df$name <- df$taxon
df <- tidyr::separate(data = df, "name",into =c('genus1','spec','sub1','sub2','sub3'),sep='_')
#View(df[1:1000,])



# for what ever reason the first clause of this funtion is not working. I'm leaving it for now because
# I dont think we actaully need a acurate species to do the analysis
##
#View(df4[1:1000,])
# remove all NA
df6 <- str_replace_all(df$taxon, pattern = "_", replacement = " " )
#View(df6)
df$taxon <- df6
df1 <- df

df <- df1
# compile species 
for(i in 1:nrow(df)){
  if(is.na(df$sub1[i])){
    sps <- df$spec[i]
  }else{
    if(is.na(df$sub2[i])){
      sps <- paste(df$spec[i], df$sub1[i], sep = "_")
    }else{
    if(is.na(df$sub3[i])){
      sps <- paste(df$spec[i], df$sub1[i], df$sub2[i], sep = "_")
    }else{
      sps <- paste(df$spec[i], df$sub1[i], df$sub2[i],df$sub3[i], sep = "_")
      }
    }
  }
  sps <- str_replace_all(sps, pattern = "NA", replacement = "")
  sps <- str_replace_all(sps, pattern = "_", replacement = " ")
  df$species[i] <- sps
  print(i)
}

#View(test)

df2 <- subset(x = df, select = -c(genus1, spec,sub1,sub2,sub3) )
#View(df4[1:1000,])

# actual code
testLatLong <<- df2 %>%
  dplyr::select(c("uniqueID","latitude", "longitude")) %>%
  mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "") %>%
  mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "") %>%
  mutate(hasLatLong = hasLat & hasLong)

summariseErrors <- testLatLong %>%
  filter(hasLat == TRUE & hasLong ==FALSE | hasLat == FALSE & hasLong ==TRUE)

print(paste0("there are ", nrow(summariseErrors)," miss matach lat long pairs."))
write.csv(x = summariseErrors, file = paste0(base_dir,"/mismatchLatLong.csv"))


# complete the check synonym process
source(file="D:/cwrNA/src/dataPrep/dataBaseTransform/checkSynonymsFunction.R")
df3 <- checkSynonym(df2)

# write out the new dataframe
write.csv(x = df2, file = paste0(base_dir,"/refinedcwrOCC.csv"))
