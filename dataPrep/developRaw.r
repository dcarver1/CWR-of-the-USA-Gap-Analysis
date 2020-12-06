###
# Produces the raw data for each species
# dan.carver@carved.com
# 20200414
###

developRaw <- function(species) {
    # from the occurence data select specific species and write out raw dataset
    rawData <<- genusOcc[genusOcc$taxon == species, ]
    write.csv(rawData, file = paste0(sp_dir,"/occurrences/rawData.csv"),row.names = FALSE)
}
