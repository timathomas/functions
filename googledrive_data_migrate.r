# ==========================================================================
# Migrate date from google drive
# ==========================================================================

#
# Libraries
# --------------------------------------------------------------------------
if (!require(pacman)) install.packages('pacman')
pacman::p_load(googledrive, tidyverse)
options(gargle_oob_default = TRUE) # oob = out-of-bound auth, set to TRUE when using RStudio Server
drive_auth(use_oob = TRUE)

#
# Pulling from shared drives
# --------------------------------------------------------------------------

source("~/users/timthomas/git/functions/functions.r")
ipak(c("tidyverse","googledrive"))
drive <- shared_drive_get("udpdata")
subdirectory <- drive_get("projects/displacement_measure", shared_drive = drive)
files_to_download <- drive_ls(path=subdirectory, recursive = T)
map2(files_to_download$id, files_to_download$name, function(id, name){
  drive_download(id, path = paste0("~/data/projects/displacement_measure/BayTracts_INLA_Smoothed/", name), overwrite = TRUE)
})

#
# Set the working directory where the files will be saved.
# --------------------------------------------------------------------------
# setwd("/data/infogroup/raw")
setwd("/net/proj/hprm/data/ig/")
#
# Transfer files from `/Volumes/GoogleDrive/My Drive/data/hprm_data/infogroup` working directory
# --------------------------------------------------------------------------
# Note: If you get this message:
# The googledrive package is requesting access to your Google account. Select a pre-authorised account or enter '0' to obtain a new token. Press Esc/Ctrl + C to abort.
# choose 0

# How to download from shared drives
drive_download("https://drive.google.com/file/d/1KAgbuDWId_7HZVnEll50ZtUJNAv5M2Jj/view?usp=sharing")

# how to download from my drive (you can still use URL's)

drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2019.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2018.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2017.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2016.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2015.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2014.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2013.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2012.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2011.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2010.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2009.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2008.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2007.fst", overwrite = TRUE)
drive_download("~/data/hprm_data/infogroup/output/fst/US_Consumer_5_File_2006.fst", overwrite = TRUE)

#
# DO NOT RUN
# upload from your laptop
# --------------------------------------------------------------------------
# path <- drive_find("extract")
# drive_upload("/Users/ajramiller/Downloads/info2019_Aug3.csv.gz", "info2019_Aug3.csv.gz")
# --------------------------------------------------------------------------
# END DO NOT RUN
