###
# pulls all the species summaries and copies content into a new folder 
# dan.carver@carverd.com
# 20200418 
###

baseDir <- "D:/cwrNA"


# list all html summary files 
# list all CSV files recursively through each sub-folder
f1 <- list.files(path = paste0(baseDir, "/gap_analysis"), 
           pattern = "20200203_2020-04-21.html", 
           recursive = TRUE,
           full.names = TRUE)
f2 <- list.files(path = paste0(baseDir, "/gap_analysis"), 
                 pattern = "test20200203_2020-04-18.html", 
                 recursive = TRUE,
                 full.names = TRUE)
length(f1)
length(f2)
f3 <- append(f1, f2)# there are duplicates in f2, but I will find the manually

#loop through all files to copy 
for(i in f1){
  file.copy(i,paste0(baseDir, "/runSummaries/speciesLevelHTML/"))
}

f2 <- list.files(paste0(baseDir, "/runSummaries/speciesLevelHTML/"))
setwd(paste0(baseDir, "/runSummaries/speciesLevelHTML"))
# drop first test item 
f2 <- f2[2:length(f2)]
for(i in f2){
  # drop front 
  j <- str_sub(i, start = 8, end = -29)
  j <- paste0(j, "Run20200203_2020-04-20.html")
  file.rename(i, j)
  
}
