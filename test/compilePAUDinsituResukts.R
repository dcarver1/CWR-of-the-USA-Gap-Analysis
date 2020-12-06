###
# script to pull all paud relate eris information 
# 20200812
# dan.carver@carverd.com
###
library(dplyr)

base_dir <<- "F:/nrelD/cwrNA"


# list all SRS and FCS files
fcs <- list.files(path = base_dir,pattern = "summaryPAUD.csv", full.names = TRUE,  recursive = TRUE)


# pull full species list from occurrence data 
occData <- data.table::fread(paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2020-07-21a.csv"),
                             header = TRUE)
occData <- occData[,2:ncol(occData)]

# drop all species that are not part of th 594 
rmSpec <- c("Phaseolus acutifolius","Phaseolus leptostachyus","Elymus elymoides","Leymus mollis","Phaseolus maculatus","Hordeum jubatum","Helianthus petiolaris","Ribes sanguineum","Phaseolus polystachios","Prunus serotina","Elymus trachycaulus","Hordeum brachyantherum","Ribes roezlii","Rubus hispidus","Ribes hudsonianum","Helianthus nuttallii","Helianthus pauciflorus","Humulus lupulus","Allium geyeri","Ribes oxyacanthoides","Fragaria x ananassa","Helianthus occidentalis","Fragaria virginiana","Elymus lanceolatus","Fragaria vesca","Helianthus niveus","Helianthus praecox","Prunus fasciculata","Ribes malvaceum","Rubus arcticus","Vitis rotundifolia","Fragaria chiloensis","Ribes aureum","Acer saccharum","Allium victorialis","Elymus stebbinsii","Helianthus debilis","Ipomoea ternifolia","Lactuca tatarica","Prunus ilicifolia","Prunus pumila","Ribes californicum","Rubus idaeus","Saccharum brevibarbe","Vitis aestivalis","Vitis cinerea","Zizania aquatica","Zizania palustris", "Allium schoenoprasum","Elymus glabriflorus",
            "Elymus glaucus","Ipomoea cordatotriloba","Juglans major","Juglans microcarpa","Leymus salina","Prunus virginiana","Ribes cereum","Rubus ursinus","Tripsacum dactyloides","Vaccinium crassifolium","Vaccinium erythrocarpum","Vaccinium ovalifolium"
)
ocd <- occData[!occData$taxon %in% rmSpec,]

df <- ocd %>% dplyr::select(taxon) %>%
  dplyr::distinct()

# Compile all FCS data 
n = 1 
for(i in fcs){
  if(n == 1){
    fcsall <- read.csv(i)
    n = 2
  }else{
    fcsall <- rbind(fcsall, read.csv(i))
  }
}

# to full list of species from occurrence data 
fc <- dplyr::left_join(df, fcsall, by = c("taxon" = "ID"))
View(fc)

sp <- fc[is.na(fc$SRS.NTOTAL),]

sp2 <- fc[fc$taxon %in% spList,]
View(sp2)
write.csv(x = fc, file = paste0("F:/nrelD/cwrNA/gap_analysis/compiledFCSIn", Sys.Date(),".csv"))
