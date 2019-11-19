####
# thin the combined data so it contains only known species of interest. 
# once select, I want to rename all the synonyms to match a single taxon 
# The output dataset will be something that can be used for modeling 
# carver.dan1@gmail.com 
# 20190822
###


checkSynonym <- function(data){
  l1 <- ncol(data)
  d1 <- read.csv("D:/cwrNA/speciesList/CWRoftheUSA_synonyms20191114.csv", header = TRUE)
  syn <- d1 %>% filter(note == "synonym" )
  alt <- d1 %>% filter(note == "alt name")
  acp <- d1 %>% filter(note == "accepted")
  
  # test for alt names and replace them with synonym 
    # join and replace 
  alt$alt.syn <- as.character(alt$alt.syn)
  alt$synonym <- as.character(alt$synonym)
  
  dataJoin <- dplyr::left_join(data, alt, by = c("taxon" = "alt.syn" ))
  n = 0 
  for(i in 1:nrow(dataJoin)){
    if(!is.na(dataJoin$synonym[i])){
      dataJoin$taxon[i] <- dataJoin$synonym[i]
      n = n+1
    }
  }
  print(paste0(n, " number of occurences had an alt name that were replaced with a synonym"))
  
  # drop columns from the join
  dataJoin <- dataJoin[1:l1]
  
  # test for synonym replace them with accepted names 
    # join and replace
  syn$synonym <- as.character(syn$synonym) 
  syn$Taxon_GRIN.Global_2019.final <- as.character(syn$Taxon_GRIN.Global_2019.final)
  
  dataJoin2 <- dplyr::left_join(data, syn, by = c("taxon" = "synonym" ))
  n = 0 
  for(i in 1:nrow(dataJoin2)){
    if(!is.na(dataJoin2$name[i])){
      dataJoin2$taxon[i] <- dataJoin2$Taxon_GRIN.Global_2019.final[i]
      n = n+1
    }
  }
  print(paste0(n, " number of occurences had an alt name that were replaced with a synonym"))
  
  # drop columns from the join
  dataJoin2 <- dataJoin2[1:l1]
  
  # select all rows where taxon == an acceped name 
    #FILTER 
  primary <- dataJoin2[dataJoin2$taxon %in% unique(acp$Taxon_GRIN.Global_2019.final),]
  print(paste0("there were ", nrow(data)-nrow(primary) ," removed for not being accecpt crop wild relative species"))
  
  return(primary) 
}
