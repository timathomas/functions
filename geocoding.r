# ==========================================================================
# Package load or install and load 
# ==========================================================================

source("~/git/timathomas/functions/functions.r")
ipak_gh("Datactuariat/Rpostal")
ipak(c("censusxy", "readxl", "tidyverse"))

# ==========================================================================
# Read data
# ==========================================================================

adds <- read_excel("/Users/timthomas/Downloads/Pierce County Add.xlsx")

postal_parse(adds[1])