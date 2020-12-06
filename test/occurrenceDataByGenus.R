###
# compile genus level data for all species based on the run data from 202002 model runs 
# dan.carver@carverd.com
# 20200414
###
library(tidyverse)
#simple option 
# use existing occurrence database 

d1 <- data.table::fread("D:/cwrNA/occurrence_data2019_05_29/combinedOccurance2020-04-07.csv",
                             header = TRUE)
d1 <- d1[,2:ncol(d1)]
genera <- sort(unique(d1$genus))

#replace source for midwest herdarium 
t1 <- d1[d1$databaseSource == "midwestHerbarium ",]
t1$databaseSource <- "Consortium of Midwest Herbaria (2019)"
t2 <- d1[d1$databaseSource != "midwestHerbarium ",]
d1 <- rbind(t1,t2)



o1 <- "D:/cwrNA/occurrence_data2019_05_29/genusOccurrences"
c1 <- data.frame(matrix(nrow = 0,ncol=2))
colnames(c1) <- c("taxon", "count")
# loop over genera 
for(i in genera){
  d2 <- d1[d1$genus == i,]
  write.csv(x = d2, file = paste0(o1, "/",i,Sys.Date(),".csv"))
  d3 <- d2 %>%
    dplyr::group_by(taxon) %>%
    dplyr::summarise(count = n())
  c1 <- rbind(c1,d3)
}
# pull the data summary document and join to check species total occurrences 
d4 <- read.csv("D:/cwrNA/runSummaries/allMetricData2020-04-12forFigures.csv")
d4 <- d4[,c(1,8,11)]

j1 <- dplyr::left_join(c1, d4, by=c("taxon"="Taxon")) %>%
  dplyr::mutate(differnce = count - Total.Records)
View(j1)
write.csv(x = j1, file = paste0(o1, "/summaryCounts",Sys.Date(),".csv"))
