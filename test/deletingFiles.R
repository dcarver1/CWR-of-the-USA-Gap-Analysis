### 
# a script that will clean all older model run from the computer 
# 20200108 
### 

# background 


baseDir <- "D:/cwrNA/gap_analysis"

oldFiles <- list.dirs(path = baseDir, full.names = TRUE, recursive = TRUE) 

old2 <- oldFiles[grep(pattern = "test20200203$", x = oldFiles)]
#filter files by the species list 
old3 <- old2[speciesList %in% old2]

n <- list.files(old2[1], recursive = TRUE)

unlink(x = old2[1], recursive = TRUE)


speciesList<- c("Leymus salina subsp. salina",
                "Leymus salina subsp. salmonis",
                "Persea palustris",
                "Vaccinium crassifolium subsp. crassifolium",
                "Elymus glabriflorus var. australis",
                "Elymus glabriflorus var. glabriflorus",
                "Elymus glaucus subsp. mackenziei",
                "Ipomoea cordatotriloba var. cordatotriloba",
                "Juglans major var. major",
                "Juglans microcarpa var. microcarpa",
                "Prunus virginiana var. demissa",
                "Ribes cereum var. cereum",
                "Rubus ursinus subsp. macropetalus",
                "Rubus ursinus subsp. ursinus",
                "Tripsacum dactyloides var. dactyloides"
)


allFiles <- list.files(path="D:/cwrNA/gap_analysis/Capsicum", full.names = TRUE, recursive = TRUE) 
oldFiles <- allFiles[grep(pattern = "2019-11-06"| "2019-11-05", x = allFiles)]
unlink(x=oldFiles) 


deleteALot <- function(directory,pattern){
  allFolders <- list.dirs(path = directory, full.names = TRUE, recursive = TRUE)
  oldFolders <- allFolders[grep(pattern = "test20191206$", x = allFolders)]
  unlink(x = oldFolders, recursive = TRUE)
  print(paste0("All files and folders containing ", pattern, " are gone forever."))
}


listOfPatterns <- c("test20191206$","test20191207$","test20191208$")

for(i in listOfPatterns){
  deleteALot(directory = "D:/cwrNA/gap_analysis/temp", pattern = i)
}
