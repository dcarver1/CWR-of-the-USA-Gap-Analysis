# occurrence data from data prep step 
d1 <- read.csv("D:/cwrNA/parameters/USA_cropWildRelativeInventory/CWRofUSA_Inventory_2020_02_26.csv", header = TRUE)
fL <- d1 %>% dplyr::select(Taxon_GRIN.Global_2019.final)

# pull in original data from the project 
CWRuslist <- read.csv("D:/cwrNA/speciesList/CWRoftheUSA_synonyms20191114.csv")
tL <- CWRuslist %>% dplyr::select(Taxon_GRIN.Global_2019.final)

# join to select species that are on both list 
fullList <- dplyr::inner_join(x = fL, y= tL, by = "Taxon_GRIN.Global_2019.final") %>%
  dplyr::distinct()

#genera List 
genera <- sort(unique(occData$genus))
# set run version 
run_v <- "test20200203"
## pull counts, gap analysis scores, and redlist scores from the model run folder 

# create empty dataframe for counts CSV 
df1 <- data.frame(matrix(nrow = nrow(fullList), ncol = 1))
colnames(df1) <- "species"
df1$species <- as.character(sort(fullList$Taxon_GRIN.Global_2019.final))
# loop over all species append values 
n=1
for(i in genera){
  #select all species in genera 
  oc1 <- occData %>% 
    dplyr::filter(genus == i)
  spList2 <- df1[grep(pattern = i,x = df1$species),]
  for(j in spList2){
    sp_dir <- paste0("D:/cwrNA/gap_analysis/",i,"/",j,"/",run_v)
    # test for file and read it as object 
    if(file.exists(paste0(sp_dir, "/counts.csv"))){
      ct <- read.csv(paste0(sp_dir, "/counts.csv"))
      if(n==1){
        ctAll <- ct 
      }else{
        ctAll <- dplyr::bind_rows(ctAll, ct)
      }
    }
    if(file.exists(paste0(sp_dir, "/gap_analysis/exsitu/summary.csv"))){
      gE <- read.csv(paste0(sp_dir, "/gap_analysis/exsitu/summary.csv"))
      #assign classes 
      if (gE$FCS < 25) {
        gE$Exsitu_Score <- "HP"
      } else if (gE$FCS >= 25 & gE$FCS < 50) {
        gE$Exsitu_Score <- "MP"
      } else if (gE$FCS >= 50 & gE$FCS < 75) {
        gE$Exsitu_Score <- "LP"
      } else {
        gE$Exsitu_Score <- "SC"
      }
      if(n==1){
        gEAll <- gE 
      }else{
        gEAll <- dplyr::bind_rows(gEAll, gE)
      }
    }
    if(file.exists(paste0(sp_dir, "/gap_analysis/insitu/summary.csv"))){
      gI <- read.csv(paste0(sp_dir, "/gap_analysis/insitu/summary.csv")) %>%
        dplyr::select("ID","SRS.NTOTAL",	"SRS.ProTotal","SRS.SRS","SRS.SRS","GRS","ERS","FCS")
      if (gI$FCS < 25) {
        gI$Insitu_Score <- "HP"
      } else if (gI$FCS >= 25 & gI$FCS < 50) {
        gI$Insitu_Score <- "MP"
      } else if (gI$FCS >= 50 & gI$FCS < 75) {
        gI$Insitu_Score <- "LP"
      } else {
        gI$Insitu_Score <- "SC"
      }
      if(n==1){
        gIAll <- gI 
      }else{
        gIAll <- dplyr::bind_rows(gIAll, gI)
      }
    }
    if(file.exists(paste0(sp_dir, "/gap_analysis/combined/fcs_combined.csv"))){
      gF <- read.csv(paste0(sp_dir, "/gap_analysis/combined/fcs_combined.csv"))
      if(n==1){
        gFAll <- gF 
      }else{
        gFAll <- dplyr::bind_rows(gFAll, gF)
      }
    }
    if(file.exists(paste0(sp_dir, "/gap_analysis/redList/listingValues.csv"))){
      rL <- read.csv(paste0(sp_dir, "/gap_analysis/redList/listingValues.csv"))
      if(n==1){
        rlAll <- rL 
      }else{
        rlAll <- dplyr::bind_rows(rlAll, rL)
      }
    }
    n= n+1
  }
  print(paste0(i, " have been compiled"))
}

# generate the combined score for redList Values 
rlAll$aVal <- NA
rlAll$eVal <- NA
# add numeric values based on status 
for(i in 1:nrow(rlAll)){
  if (rlAll$AOO.Status[i] == "Least Concern (LC)"){
    rlAll$aVal[i] <- 1 }
  if (rlAll$AOO.Status[i] == "Possible Near Threatened (NT)"){
    rlAll$aVal[i] <- 2 }
  if (rlAll$AOO.Status[i] == "Vulnerable (VU)"){
    rlAll$aVal[i] <- 3 }
  if (rlAll$AOO.Status[i] == "Endangered (EN)"){ 
    rlAll$aVal[i] <- 4 }
  if (rlAll$AOO.Status[i] == "Critically Endangered (CR)"){
    rlAll$aVal[i] <- 5 }
  # EOO values 
  if (rlAll$EOO.Status[i] == "Least Concern (LC)"){
    rlAll$eVal[i] <- 1 }
  if (rlAll$EOO.Status[i] == "Possible Near Threatened (NT)"){
    rlAll$eVal[i] <- 2 }
  if (rlAll$EOO.Status[i] == "Vulnerable (VU)"){
    rlAll$eVal[i] <- 3 }
  if (rlAll$EOO.Status[i] == "Endangered (EN)"){ 
    rlAll$eVal[i] <- 4 }
  if (rlAll$EOO.Status[i] == "Critically Endangered (CR)"){
    rlAll$eVal[i] <- 5 }

  if(rlAll$eVal[i] >= rlAll$aVal[i]){
    stat <- rlAll$EOO.Status[i]
  }else{
    stat <- rlAll$AOO.Status[i]
  }
  
  rlAll$`Combined Status`[i] <- stat
}

rlAll <- rlAll %>% dplyr::select(c("taxon","EOO.Area.km2","EOO.Status","AOO",
                                    "AOO.adjusted.Minimum","AOO.Status","Combined Status"))
# join based on full specices list to identify non present species 
ctFull <- dplyr::full_join(x = df1,y=ctAll, by = "species")

gEFull <- dplyr::full_join(x = ctFull,y=gEAll, by = c("species" = "ID"))

gIFull <- dplyr::full_join(x = gEFull,y=gIAll, by =  c("species" = "ID"))

gFFull <- dplyr::full_join(x = gIFull,y=gFAll, by =  c("species" = "ID"))

allSummary <- dplyr::full_join(x = gFFull,y=rlAll, by = c("species" = "taxon"))


# add field based on if speciecs will be included in the higher level analysis 
noSS <- c("Phaseolus acutifolius","Phaseolus leptostachyus","Elymus elymoides","Leymus mollis","Phaseolus maculatus","Hordeum jubatum","Helianthus petiolaris","Ribes sanguineum","Phaseolus polystachios","Prunus serotina","Elymus trachycaulus","Hordeum brachyantherum","Ribes roezlii","Rubus hispidus","Ribes hudsonianum","Helianthus nuttallii","Helianthus pauciflorus","Humulus lupulus","Allium geyeri","Ribes oxyacanthoides","Fragaria x ananassa","Helianthus occidentalis","Fragaria virginiana","Elymus lanceolatus","Fragaria vesca","Helianthus niveus","Helianthus praecox","Prunus fasciculata","Ribes malvaceum","Rubus arcticus","Vitis rotundifolia","Fragaria chiloensis","Ribes aureum","Acer saccharum","Allium victorialis","Elymus stebbinsii","Helianthus debilis","Ipomoea ternifolia","Lactuca tatarica","Prunus ilicifolia","Prunus pumila","Ribes californicum","Rubus idaeus","Saccharum brevibarbe","Vitis aestivalis","Vitis cinerea","Zizania aquatica","Zizania palustris", "Allium schoenoprasum","Elymus glabriflorus",
          "Elymus glaucus","Ipomoea cordatotriloba","Juglans major","Juglans microcarpa","Leymus salina","Prunus virginiana","Ribes cereum","Rubus ursinus","Tripsacum dactyloides","Vaccinium crassifolium","Vaccinium erythrocarpum","Vaccinium ovalifolium"
)
allSummary$`Included in Summaries` <- !allSummary$species %in% noSS 


# change column names 
allSummary <- allSummary %>% dplyr::select(-X, -Exsitu_Score)


View(allSummary)
#drop NA row 
allSummary <- allSummary[2:nrow(allSummary),]

newCols <- c("Species",
  "Total Records",	"Records with latitude",	"Records with longitude",	
  "Records with coordinates",
 " Total G records",	'Total G records with coordinates',	
 "Total H Records",	"Total H with coordinates",
 "Number of unique data sources",
 "Total occurrences in North America",	"Total G occurrences in North America",
 "Total H occurrences in North America",	
 "SRSex",	"GRSex",	"ERSex",	"FCSex", "Exsitu Conservation Score",
 "Total occurrences in modeled area",	
 "Total occcurrens in modeled area in protected areas",
 "SRSin",	"GRSin",	"ERSin",	"FCSin", "Insitu Conservation Score",	
 "FCSex_value",	"FCSin_value",	"FCSc_min",	"FCSc_max",	"FCSc mean",
 "FCSc_min priority category",	"FCSc_max priority category",	
 "FCSc mean priority category",
 "EOO area km2", "EOO status",	"AOO",	"AOO adjusted minimum",	"AOO status",
 "Combined status", "Included in Summaries"
)

colnames(allSummary) <- newCols

#write.csv(x = allSummary, file = paste0("D:/cwrNA/runSummaries/allMetricData", Sys.Date(), ".csv"))


# :) run from here

# inport the CWR inventory and join the priority level and the crop type for futher summaries 
cwrIn <- d1 %>% dplyr::select("Taxon_GRIN.Global_2019.final","Crop.or.WUS.use_general", "Priority.2019","Crop.or.WUS.use_1")

cwrIn$name <- as.character(cwrIn$Taxon_GRIN.Global_2019.final) 
useGroup <- dplyr::left_join(x = allSummary ,y= cwrIn, by= c("Species" = "name"))
View(useGroup)
# drop NA columns 
#useGroup <- useGroup[useGroup$species != "",]
#write.csv(x = useGroup, file = paste0("D:/cwrNA/runSummaries/allMetricData", Sys.Date(), ".csv"))

### adding the median model data to this data to double check true model runs for each species 
allM <- read.csv("D:/cwrNA/runSummaries/median_summary_test20200203.csv")
dt2 <- dplyr::left_join(useGroup, allM, by = c("Species" = "species"))
write.csv(x = dt2, file = paste0("D:/cwrNA/runSummaries/allMetricData_withRuns", Sys.Date(), ".csv"))


dt2a <- dt2 %>%
  dplyr::filter("Total occurrences in North America" >= 25)%>%
  group_by("Included in Summaries", "Valid")%>%
  dplyr::summarise()
View(dt2a)
# 20200225 
# pulling data for intraspecific species 
spList2 <- 
iSL <- occData %>%
  filter(taxon %in% spList2)

a <- iSL %>%
  dplyr::group_by(taxon)%>%
  dplyr::summarise(count = n())

sort(unique(iSL$taxon))
View(iSL)
write.csv(x = iSL, file = "D:/cwrNA/troubleshooting/intraSpecificSpecsList2.csv")

### pre 20200224


spCount <- occData %>%
  group_by(taxon, type)%>%
  dplyr::summarise(count = n())
write.csv(x = spCount, file = "D:/temp/spOccurrenceCount.csv")






# list on all known species 
d1 <- read.csv("D:/cwrNA/speciesList/CWRoftheUSA_synonyms20191114.csv", header = TRUE)
fullList <- unique(d1$Taxon_GRIN.Global_2019.final)
write.csv(x = fullList, file = "D:/temp/speciesInCWRlist.csv")

# find missing species 
missingSpecies <- fullList[!fullList %in% spList2]

# species present in occurence data that are not listed in CWR list 
extraSpecies <- spList2[!spList2 %in% fullList]
