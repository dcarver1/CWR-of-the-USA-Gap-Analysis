###
# script to produce the raw data for each species
###

#troubleshooting
#species <- species1

developRaw <- function(species) {
    # from the occurence data at the species level select specific species and write out rew dataset
    rawData <<- genusOcc[genusOcc$taxon == species, ]
    write.csv(rawData, file = paste0(sp_dir,"/occurrences/rawData.csv"),row.names = FALSE)
}

