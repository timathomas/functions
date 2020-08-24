# ==========================================================================
# Code for setting up R on unix
# ==========================================================================

# ==========================================================================
# debian Prep
# ==========================================================================
	# change permissions to write and install packages

	# cd /usr/local/lib/R
	# sudo chmod o+w site-library
	# ls -l

# ==========================================================================
# !! JUST DON'T !! install mran
# ==========================================================================
	# # https://docs.microsoft.com/en-us/machine-learning-server/r-client/install-on-linux

	# # Install as root or sudo
	# sudo su

	# # If your system does not have the https apt transport option, add it now
	# apt-get install apt-transport-https

	# # Download
	# wget https://mran.blob.core.windows.net/install/mro/3.4.3/microsoft-r-open-3.4.3.tar.gz

	# tar -xf microsoft-r-open-3.4.3.tar.gz

	# cd microsoft-r-open/

	# sudo ./install.sh

	# # Register the repo.
	# dpkg -i packages-microsoft-prod.deb

	# # Check for microsoft-prod.list configuration file to verify registration.
	# ls -la /etc/apt/sources.list.d/

	# # Update packages on your system
	# apt-get update

	# # Install the packages
	# apt-get install microsoft-r-client-packages-3.4.3

	# # List the packages
	# ls /opt/microsoft/rclient/


# ==========================================================================
# Alternatives
# ==========================================================================
	# find / -name alternatives
	# sudo echo $PATH

# ==========================================================================
# Package insall
# ==========================================================================

options(repos="https://cloud.r-project.org/")

#
# Update packages
# --------------------------------------------------------------------------

 update.packages(ask = FALSE)
Yes
#
# install packages
# --------------------------------------------------------------------------

packages <-
	c("stringr","sf","rgdal","rgeos","RColorBrewer","gridExtra","bartMachine","knitr","magrittr","kableExtra","scales","rmarkdown","limma","circlize","lubridate","tidyverse","ggforce","circlize",	"ggthemes",	"wesanderson", "lcmm", "seg", "spdep", "reshape2", "mlmRev", "lme4", "rstanarm", "textreg")



install_packs <- packages[!packages %in% installed.packages()]
for(lib in install_packs) install.packages(lib,dependencies=TRUE, quiet = TRUE, ask = FALSE)

	if(!require(limma)){
	    source("http://www.bioconductor.org/biocLite.R")
		biocLite("limma")}

# ==========================================================================
# special install
# ==========================================================================

	devtools::install_github("jalvesaq/colorout")
	devtools::install_github("walkerke/tidycensus")
	devtools::install_github("mnpopcenter/ipumsr")
