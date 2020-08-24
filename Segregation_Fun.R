# ==========================================================================
# Segregation measures for both Macro (county) and micro (neighborhood) scales
# Neighborhood specific contribution to segregation
# Drawn from Corey Sparks
# https://rpubs.com/corey_sparks/116032
# ==========================================================================

# ==========================================================================
# Load packages
# ==========================================================================

	options(repos="https://cloud.r-project.org/")
	update.packages(ask = FALSE)
	Yes

	library(colorout)

	if(!require(sf)){
	   install.packages("sf")
	   require(sf)
	  }
	if(!require(tidycensus)){
    	install.packages("tidycensus")
    	require(tidycensus)
	}

	if(!require(tidyverse)){
    	install.packages("tidyverse")
    	require(tidyverse)
	}
options(tigris_use_cache = TRUE)
census_api_key("63217a192c5803bfc72aab537fe4bf19f6058326", install = TRUE)

# ==========================================================================
# Tidycensus variables
# ==========================================================================

#
# Race variables
# --------------------------------------------------------------------------
race_vars <- c(
	totrace = "B03002_001",
	White = "B03002_003",
	Black = "B03002_004",
	Asian = "B03002_006",
	Latinx = "B03002_012")

#
# Tidycensus functions
# --------------------------------------------------------------------------

	acs5yr <- c(2017,2009)
	# acs1year <- rep(2012:2017, 1)
	acs <-
		function(
			geography = "tract",
			variables,
			state,
			county = NULL,
			geometry = FALSE,
			survey = "acs5",
			year = 2011){
	acs <-
		get_acs(
			geography = geography,
			variables = race_vars,
			survey = survey,
			state = state,
			county = county,
			output = "tidy",
			year = year,
			geometry = geometry,
			cache_table = TRUE) %>%
		select(-moe) %>%
		replace(is.na(.), 0) %>%
		mutate(year = year,
			   survey = survey) %>%
		spread(variable, estimate) %>%
		group_by(GEOID) %>%
		mutate(Other = totrace - sum(White,Latinx,Black,Asian),
			   LAO = sum(Latinx, Asian, Other),
			   pWhite = White/totrace,
			   pBlack = Black/totrace,
			   pAsian = Asian/totrace,
			   pLatinx = Latinx/totrace,
			   pOther = Other/totrace,
			   pLAO = LAO/totrace) %>%
		mutate(COID = substr(GEOID,1,5)) %>%
		ungroup()
	}

king <-
	acs(variables = race_vars,
		county = "king",
		state = "WA")

wayne <-
	acs(variables = race_vars,
		county = "wayne",
		state = "MI")

madison <-
	acs(variables = race_vars,
		county = "madison",
		state = "AL")

anderson <-
	acs(variables = race_vars,
		county = "anderson",
		state = "tx")


# ggplot(wayne) +
# 	geom_sf()

# ggplot(king) +
# 	geom_sf()

# ggplot(madison) +
# 	geom_sf()

# ggplot(anderson) +
# 	geom_sf()

# ==========================================================================
# Segregation measures
# ==========================================================================

#
# County totals
# --------------------------------------------------------------------------

anderson_co <-
	anderson %>%
 	group_by(COID) %>%
	mutate(co_total = sum(totrace),
		   co_White = sum(White),
		   co_Black = sum(Black),
		   co_Asian = sum(Asian),
		   co_Latinx = sum(Latinx),
		   co_Other = sum(Other),
		   co_LAO = sum(Latinx, Asian, Other),
		   co_pWhite = co_White/co_total,
		   co_pBlack = co_Black/co_total,
		   co_pAsian = co_Asian/co_total,
		   co_pLatinx = co_Latinx/co_total,
		   co_pOther = co_Other/co_total,
		   co_pLAO = co_LAO/co_total,
		   co_ent = co_pWhite*(log(1/co_pWhite))+
		   			co_pBlack*(log(1/co_pBlack))+
		   			co_pLAO*(log(1/co_pLAO)),
		   co_ent = if_else(is.na(co_ent), 0, co_ent)) %>%
	group_by(GEOID) %>%
	mutate(d.wb = .5*(abs(White/co_White - Black/co_Black)),
		   int.wb = Black/co_Black * White/totrace) %>%
	group_by(COID) %>%
	mutate(co_d.wb = sum(d.wb, na.rm = TRUE),
		   co_int.wb = sum(int.wb, na.rm = TRUE)) %>%
	ungroup()

glimpse(anderson_co)

# ==========================================================================
# !!!WORKING!!!
# Segregation measures for sf and sp packages
# https://www.rdocumentation.org/packages/dplyr/versions/0.7.3/topics/summarise_all
# ==========================================================================



library(seg)
library(dplyr)

	US.race <- US %>%
			   select(GEOID, GEOID10, White, Black, Asian, Latino, Other) %>%
			   na.omit()

	US.race.sp <- as(US.race,"Spatial")

	dat <- US.race.sp
	cos <- unique(dat$GEOID10)

	spsegout<-function(dat, cos){
	  theil<-numeric(length(cos))
	  diss<-numeric(length(cos))
	  div<-numeric(length(cos))
	  isol<-numeric(length(cos))
	  inter<-numeric(length(cos))
	  cbsa<-NULL
	for (i in 1:length(cos)){
	  #Modify the two groups in the next line, this usees NH White and Hispanic
	  out<-spseg(dat[dat$GEOID10==cos[i],],
	  			 data =dat@data[dat$GEOID10==cos[i],c("White", "Black")],
	  			 method="all",
	  			 useC=T,
	  			 negative.rm = T)

	  div[i]<-out@r #Diversity
	  diss[i]<-out@d #Dissimilarity
	 theil[i]<-out@h #Theil Entrophy
	 isol[i]<-out@p[2,2] #Isolation for Hispanics
	 inter[i]<-out@p[1,2] #Interaction for Whites and Hispanics
	  cbsa[i]<-as.character(cos[i])
	}
	list(theil=theil, dissim=diss, diversity=div,isolation=isol, interaction=inter,GEOID10=cbsa)
	}

	spsegs<-data.frame(spsegout(dat,cos))

### Join seg indicies

	US.indi <- left_join(US.indi,spsegs)