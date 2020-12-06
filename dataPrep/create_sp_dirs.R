###
# Test for existing file stucture and generates sturcture if needed.
# dan.carver@carverd.com
# 20200414
###

create_sp_dirs <- function(species) {
  #create species dir
  sp_dir <<- paste0(gap_dir,"/",genus, "/",species,"/",run_version)
  if (!file.exists(sp_dir)) {dir.create(sp_dir,recursive=T)}

  #create other directories
  #if (!file.exists(paste(sp_dir,"/bioclim",sep=""))) {dir.create(paste(sp_dir,"/bioclim",sep=""))}
  if (!file.exists(paste(sp_dir,"/gap_analysis/combined",sep=""))) {dir.create(paste(sp_dir,"/gap_analysis/combined",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/gap_analysis/exsitu",sep=""))) {dir.create(paste(sp_dir,"/gap_analysis/exsitu",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/gap_analysis/insitu",sep=""))) {dir.create(paste(sp_dir,"/gap_analysis/insitu",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/gap_analysis/redList",sep=""))) {dir.create(paste(sp_dir,"/gap_analysis/redList",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/modeling/alternatives",sep=""))) {dir.create(paste(sp_dir,"/modeling/alternatives",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/modeling/maxent",sep=""))) {dir.create(paste(sp_dir,"/modeling/maxent",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/modeling/nativeArea",sep=""))) {dir.create(paste(sp_dir,"/modeling/nativeArea",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/modeling/replicates",sep=""))) {dir.create(paste(sp_dir,"/modeling/replicates",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/occurrences",sep=""))) {dir.create(paste(sp_dir,"/occurrences",sep=""),recursive=T)}

  #return
  return(species)
}
