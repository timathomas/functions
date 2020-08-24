# ==========================================================================
# Package load 
# ==========================================================================

pack <- function(pkg){
    update.packages(ask = FALSE)
    if(!require(pkg)) install.packages(as.character(pkg), dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)    
}
