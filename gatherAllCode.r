### 
# code for compiling all code into a single doc. I want to do this to seach for when elements are being defined 
# 20191212
# carver.dan1@gmail.com
### 

outLocation <- "F:/nrelD/GapAnalysis"
setwd(outLocation)
files <- sort(list.files(path = outLocation, pattern = "\\.r$",full.names = TRUE,recursive = TRUE,ignore.case = TRUE))

sink(file = "textOut.txt")

for(i in 1:length(files)){
  file <- readLines(files[i])
  cat(files[i], "\n\n")
  cat(file, sep = "\n")
}

sink()
