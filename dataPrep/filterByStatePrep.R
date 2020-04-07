###
# filtering the state level occurence data by genus, and generate unique names of occurences per species 
# 20200127 
# dan.carver@carverd.com 
### 

### commented out because this is not a necessary step each time the models is ran 
# 
# library(tidyverse)
# library(sp)
# library(tmap)
# tmap::tmap_mode("view")
# 
# # load in country SHP 
# 
# # load in state gadm 
# rds <- list.files(path = "D:/cwrNA/parameters/statesByCountry",pattern = ".rds", full.names = TRUE )
# 
# # find a way to read in all and create on file 
# for(i in 1:length(rds)){
#   print(i)
#   if(i == 1){
#     states <- readRDS(rds[i])
#   }else{
#     v <- readRDS(rds[i])
#     states <- rbind(states, v) 
#     print(paste0(rds[i]," has been added"))
#   }
# }
# 
# # write out sp object 
# saveRDS(object = states, file = "D:/cwrNA/parameters/statesByCountry/gadmCanUsaMex_sp.rds")
