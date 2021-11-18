# ==========================================================================
# Convert all files in a directory to gpkg or parquet
# ==========================================================================

pacman::p_load(tidyverse, data.table, sf, qs)

setwd("~/Downloads/Alex")

csv_files <- list.files(pattern = "\\.csv$", ignore.case=TRUE, include.dirs = TRUE, recursive = TRUE)

map(csv_files, function(file){
x <- read_csv(file)
unlink(paste0(substr(file, 1, nchar(file) -4), ".*"))
qsave(
    x, 
    paste0(substr(file, 1, nchar(file) -4), ".qs"))
})

gpkg_files <- list.files(pattern = "\\.gpkg$", ignore.case=TRUE, include.dirs = TRUE, recursive = TRUE)

map(gpkg_files, function(file){
x <- st_read(file)
unlink(paste0(substr(file, 1, nchar(file) -5), ".*"))
qsave(
    x, 
    paste0(substr(file, 1, nchar(file) -5), ".qs"))
})