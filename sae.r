# ==========================================================================
# Small area estimation 
# ==========================================================================

pacman::p_load(googledrive, data.table, tidyverse)

options(gargle_oob_default = TRUE, scipen=10, width=Sys.getenv("COLUMNS"), tigris_use_cache = TRUE) # avoid scientific notation, oob = out-of-bound auth, set to TRUE when using RStudio Server
drive_auth(use_oob = TRUE)

# ==========================================================================
# Download data 
# ==========================================================================

drive_download("https://drive.google.com/file/d/1d3ZTD1ERiOhf-3Nn7cFEUvmiVpMvTiMs/view?usp=sharing", path = "~/data/ig/validation/panel65.csv.gz")

df <- fread("~/data/ig/validation/panel65.csv.gz")