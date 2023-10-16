# ==========================================================================
# Convert all files in a directory to gpkg or parquet
# ==========================================================================

librarian::shelf(tidyverse, data.table, sf, qs, fst)

setwd("~/Downloads/data")

#
# For csv files
# --------------------------------------------------------------------------
csv_files <- list.files(pattern = "\\.csv$", ignore.case=TRUE, include.dirs = TRUE, recursive = TRUE)

map(csv_files, function(file){
x <- fread(file)
unlink(paste0(substr(file, 1, nchar(file) -4), ".*"))
write.fst(
    x, 
    paste0(substr(file, 1, nchar(file) -4), ".fst"), compress = 100)
})

#
# for gpkq files
# --------------------------------------------------------------------------
gpkg_files <- list.files(pattern = "\\.gpkg$", ignore.case=TRUE, include.dirs = TRUE, recursive = TRUE)

map(gpkg_files, function(file){
x <- st_read(file)
unlink(paste0(substr(file, 1, nchar(file) -5), ".*"))
qsave(
    x, 
    paste0(substr(file, 1, nchar(file) -5), ".qs"))
})