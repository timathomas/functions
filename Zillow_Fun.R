# ==========================================================================
# Zillow read and prep
# ==========================================================================

if(!require(dplyr)){
	    install.packages("dplyr")
	    require(dplyr)
	}

# ==========================================================================
# Pull in data
# ==========================================================================

	z_year <- function(path,year){
		
		if(!exists("z.rent")) {
		  load(path)
		}
		
		rent <- z.rent %>% 
				filter(date.rent>=paste(year,"-01-01", sep=""), 
					   date.rent<=paste(year,"-12-31", sep="")) %>% 
				group_by(GEO2010) %>% 
				summarise(ZRI=median(ZRI)) %>% 
				mutate(GEO2010=factor(GEO2010))

		homes <- z.homes %>% 
				filter(date.home>=paste(year,"-01-01", sep=""), 
					   date.home<=paste(year,"-12-31", sep="")) %>% 
				group_by(GEO2010) %>% 
				summarise(ZHVI=median(ZHVI)) %>% 
				mutate(GEO2010=factor(GEO2010))

		z_value <- left_join(rent, homes, by = "GEO2010") %>% 
				   na.omit()
		return(z_value)
	}