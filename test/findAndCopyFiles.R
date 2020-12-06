###
# Pull all summary html docments into a single location 
# 20200725
# dan.carver@carverd.com
###
install.packages(raster, dependencies = TRUE)

baseDir <- "F:/nrelD/cwrNA"

occData <- data.table::fread(paste0(baseDir, "/occurrence_data2019_05_29/combinedOccurance2020-07-21a.csv"),
                             header = TRUE)
occData <- occData[,2:ncol(occData)]


# function for filtering list based on character values
include <- function (theList, toMatch){
  matches <- unique (grep(paste(toMatch,collapse="|"),
                          theList, value=TRUE))
  return(matches)
}


# list all html files 
files <- list.files(path = baseDir, pattern = ".html", full.names = TRUE, recursive = TRUE)
run1 <- include(files, "Run20200203_2020-07-27")
run2 <- include(files, "Run20200203_2020-07-28")

run4 <- append(run1,run2)
run4 <- append(run4, run3)


d1 <- occData %>%
  dplyr::group_by(genus) %>%
  dplyr::summarize(total = length(unique(taxon)))
View(d1)
d1$numberOfFiles <- 0

spList1 <- sort(unique(occData$taxon))
genera <- sort(unique(occData$genus))

df <- data.frame(species = sort(spList1))
head(df)
for(i in genera){
  j <- as.character(i)
  folder <- paste0(baseDir,"/gap_analysis/compiledSummaryDocs/",j)
  if (!file.exists(folder)){
    dir.create(folder,recursive=T)
  }
  ## currently set to only include the trouble shooting I've been working on today 
  t1 <- include(run4, j)
  if(length(t1)>0){
    file.copy(t1, folder)
  }
  
}

for(g in 1:length(genera)){
  folder <- paste0(baseDir,"/gap_analysis/compiledSummaryDocs/",as.character(genera[g]))
  d1$numberOfFiles[g] <- length(list.files(folder))
}

d1$match <- d1$total == d1$numberOfFiles
View(d1)


umSpec <- d1 %>% dplyr::filter(match == FALSE)

reRuns <- c()

for(k in umSpec$genus){
  sp <- occData[occData$genus == k,]
  spList <- unique(sp$taxon)
  for(s in spList){
    st <- paste0(s, "_Run20200203_2020-07-25.html")
    st1 <- paste0(s, "_Run20200203_2020-07-24.html")
    st2 <- paste0(s, "_Run20200203_2020-07-27.html")
    t1 <- include(run4, st)
    t2 <- include(run4, st1)
    t3 <- include(run4, st2)
    if(length(t1)==0 & length(t2)==0 & length(t3)==0){
     reRuns <- append(reRuns, s)
    }
  }
  
}
reRuns

