# ==========================================================================
# Move data from Google Drive to UDP
# ==========================================================================

source("~/users/timthomas/git/functions/functions.r")
ipak(c("tidyverse", "googledrive"))
options(gargle_oob_default = TRUE) # oob = out-of-bound auth, set to TRUE when using RStudio Server
drive_auth(use_oob = TRUE)
1
#
# pull data from shared drives
# --------------------------------------------------------------------------

# get the udpdata shared drive location credentials on GD
drive <- shared_drive_get("udpdata") 

#
# Download smoothed data
# --------------------------------------------------------------------------
# get the subdirectory for displacement measure data
subdirectory <- drive_get("projects/displacement_measure", shared_drive = drive)

# create list of files to download
files_to_download <- drive_ls(path=subdirectory, recursive = T)

# loop GD file download onto udp EML server (this replaces all prior data)
map2(
	files_to_download$id, files_to_download$name, 
	function(id, name){
  		drive_download(
  			id, 
  			path = paste0("~/data/projects/displacement_measure/BayTracts_INLA_Smoothed/", name), 
  			overwrite = TRUE)
})

#
# Download infogroup data
# --------------------------------------------------------------------------

subd <- drive_get("raw/infogroup/fst", shared_drive = drive)

files_to_download <- drive_ls(path = subd, recursive = TRUE)

map2(
	files_to_download$id[1], files_to_download$name[1], 
	function(id, name){
		drive_download(
			id, 
			path = paste0("~/data/raw/dataaxel/", name), overwrite = TRUE)
		})