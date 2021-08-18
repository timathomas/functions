# ==========================================================================
# Package load or install and load 
# ==========================================================================

ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

ipak_gh <- function(pkg){
    new.pkg <- pkg[!(sub('.*/', '', pkg) %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        remotes::install_github(new.pkg, dependencies = TRUE)
    sapply(sub('.*/', '', pkg), require, character.only = TRUE)
}

# Example
# ipak_gh(c("lmullen/gender", "mtennekes/tmap", "jalvesaq/colorout", "timathomas/neighborhood", "arthurgailes/rsegregation"))