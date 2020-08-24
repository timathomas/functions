# ==========================================================================
# Convert all files in a directory to gpkg or parquet
# ==========================================================================

library(tidyverse)
library(data.table)
library(sf)
library(arrow)

setwd("~/git/sparcc")

files <- list.files(pattern = "\\.shp$", ignore.case=TRUE, include.dirs = TRUE, recursive = TRUE)

map(files, function(file){
x <- st_read(file)
unlink(paste0(substr(file, 1, nchar(file) -4), ".*"))
st_write(
    x, 
    paste0(substr(file, 1, nchar(file) -4), ".gpkg"), 
    driver = "gpkg")
})

files <- list.files(pattern = "\\.csv$", ignore.case=TRUE, include.dirs = TRUE, recursive = TRUE)

map(files, function(file){
x <- fread(file)
unlink(file)
write_parquet(
    x, 
    paste0(substr(file, 1, nchar(file) -4), ".parquet"))
})

files <- list.files(pattern = "\\.csv.bz2$", ignore.case=TRUE, include.dirs = TRUE, recursive = TRUE)

map(files, function(file){
x <- fread(file)
unlink(file)
write_parquet(
    x, 
    paste0(substr(file, 1, nchar(file) -4), ".parquet"))
})

files <- list.files(pattern = "\\.rds$", ignore.case=TRUE, include.dirs = TRUE, recursive = TRUE)

map(files, function(file){
x <- readRDS(file)
unlink(file)
write_parquet(
    x, 
    paste0(substr(file, 1, nchar(file) -4), ".parquet"))
})

