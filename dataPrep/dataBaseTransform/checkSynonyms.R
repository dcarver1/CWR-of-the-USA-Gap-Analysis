####
# thin the combined data so it contains only known species of interest. 
# once select, I want to rename all the synonyms to match a single taxon 
# The output dataset will be something that can be used for modeling 
# carver.dan1@gmail.com 
# 20190822
###


library(tidyverse)
library(data.table)
data <- data.table::fread("D:/cwrOfNA/occurrence_data2019_05_29/combinedOccurance2019-08-29.csv", header = TRUE)

base_dir <- "D:/cwrOfNA"

# read in data and synonym list 
#data <- data.table::fread(file = paste0(base_dir, "/occurrence_data2019_05_29/combinedOccurance2019-08-29.csv"), header = TRUE)

syn <- data.table::fread(file = paste0(base_dir, "/speciesList/CWRoftheUSA_synonyms.csv") ,header = TRUE)

# so the taxon/synonym sheet do not have"_" between values. I need to replace this with space 
data$taxon <-gsub("_", " ", data$taxon, fixed=TRUE)

# pull data that is from the true data list. 
primary <- data[data$taxon %in% unique(syn$`Taxon_GRIN Global_2019 final`),]

# create a list of unique synonyms
synList <- syn[which(syn$synonym != ""),]
synList <- unique(synList$synonym)

#Select all data where the taxon will need to be changed. 
secondary <- data[data$taxon %in% synList,] 


syn2 <- syn[which(syn$synonym != ""),]

# Issues with join, some species have the same synonym for mutliple taxon. There is no real way of knowing which is which
# So i think the best option is either droping those points or duplicating them? One can tell them apart by elements which 
# have duplicated V1 values 
### example of the join issue 
syn2$syn1 <- syn2$synonym
join1 <- dplyr::left_join(x = secondary[1800:1815,], y = syn2 ,by = c("taxon" = "synonym"), keep=TRUE) 
### 



syn2$syn1 <- syn2$synonym
join2 <- dplyr::left_join(x = secondary, y = syn2 ,by = c("taxon" = "synonym"), keep=TRUE) 

# if synonym in not NA, replace taxon with Taxon GRIN Global 
join2$taxon <- join2$`Taxon_GRIN Global_2019 final`
# add step to replace genus... not sure how 

### duplicate issues 
issues <- join2 %>% 
  group_by(V1)%>%
  count()%>%
  filter(n != 1)

iss2 <- secondary[secondary$V1 %in% issues$V1, ] 
speciecsOfIssue <- unique(iss2$taxon)
### 


df1 <- dplyr::select(.data = join2,-c("Taxon_GRIN Global_2019 final","taxonomy_species_id","name","note","syn1"))
df2 <- rbind(primary, df1)

# write out the modeling data 
write.csv(x = df2, file = paste0(base_dir,"/modelingData", Sys.Date(), ".csv"))
