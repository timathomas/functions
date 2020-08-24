# ==========================================================================
# AMI Plots
# ==========================================================================

rm(list=ls()) #reset
options(scipen=10) # avoid scientific notation
gc()

library(colorout)
library(lubridate)
library(tigris)
library(purrr)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(tidyverse)

# ==========================================================================
# Data
# ==========================================================================
# load("H_Drive/Academe/Data/Zillow/RData/zillow.rdata")

# v15 <- load_variables(2016, "acs5", cache = TRUE)
# View(v15)

# v00 <- load_variables(2000, "sf3", cache = TRUE)
# View(v00)

# v90 <- load_variables(1990, "sf3", cache = TRUE)
# View(v90)

# ==========================================================================
# Racial economic stratification
# ==========================================================================

#
# Functions
# --------------------------------------------------------------------------
	acs <- function(geography = "county",
					variables,
					year = 2016,
					state = "WA",
					county,
					survey = "acs1"){
		bind_rows(get_acs(geography = geography,
					   variables = variables,
					   cache_table = TRUE,
					   year = year,
					   output = "tidy",
					   state = state,
					   county = county,
					   geometry = FALSE,
					   survey = survey),
				get_acs(geography = "state", # state
					   variables = variables,
					   cache_table = TRUE,
					   year = year,
					   output = "tidy",
					   state = state,
					   geometry = FALSE,
					   survey = survey))
	}

#
# Variables
# --------------------------------------------------------------------------

	ACSMovVars <- c(total = "B07004A_001",
					whnl_total = "B07004H_001",
					whnl_smcnty = "B07004H_003",
					whnl_difcntysmst = "B07004H_004",
					whnl_difst = "B07004H_005",
					whnl_abroad = "B07004H_006",
					bl_total = "B07004B_001",
					bl_smcnty = "B07004B_003",
					bl_difcntysmst = "B07004B_004",
					bl_difst = "B07004B_005",
					bl_abroad = "B07004B_006",
					as_total = "B07004D_001",
					as_smcnty = "B07004D_003",
					as_difcntysmst = "B07004D_004",
					as_difst = "B07004D_005",
					as_abroad = "B07004D_006",
					la_total = "B07004I_001",
					la_smcnty = "B07004I_003",
					la_difcntysmst = "B07004I_004",
					la_difst = "B07004I_005",
					la_abroad = "B07004I_006")




	cnty <- c("Island", "Snohomish", "King", "Pierce", "Thurston","Mason","Kitsap")

#
# ACS Output
# --------------------------------------------------------------------------

	acs16 <- get_acs(geography = "tract",
					variables = ACSMovVars,
					year = 2016,
					state = "WA",
					survey = "acs5",
					output = "wide",
					geometry = TRUE,
					cache_table = TRUE) %>%
			 select(-ends_with("M")) %>%
			 mutate(year = 2016,
					pwhnl_smcnty = ifelse(whnl_totalE < 200, NA, (whnl_smcntyE/whnl_totalE)),
					pwhnl_difcntysmst = ifelse(whnl_totalE < 200, NA, (whnl_difcntysmstE/whnl_totalE)),
					pwhnl_outst = ifelse(whnl_totalE < 200, NA, ((whnl_difstE+whnl_abroadE)/whnl_totalE)),

					pbl_smcnty = ifelse(bl_totalE < 200, NA, (bl_smcntyE/bl_totalE)),
					pbl_difcntysmst = ifelse(bl_totalE < 200, NA, (bl_difcntysmstE/bl_totalE)),
					pbl_outst = ifelse(bl_totalE < 200, NA, ((bl_difstE+bl_abroadE)/bl_totalE)),

					pas_smcnty = ifelse(as_totalE < 200, NA, (as_smcntyE/as_totalE)),
					pas_difcntysmst = ifelse(as_totalE < 200, NA, (as_difcntysmstE/as_totalE)),
					pas_outst = ifelse(as_totalE < 200, NA, ((as_difstE+as_abroadE)/as_totalE)),

					pla_smcnty = ifelse(la_totalE < 200, NA, (la_smcntyE/la_totalE)),
					pla_difcntysmst = ifelse(la_totalE < 200, NA, (la_difcntysmstE/la_totalE)),
					pla_outst = ifelse(la_totalE < 200, NA, ((la_difstE+la_abroadE)/la_totalE)))

	acs15 <- get_acs(geography = "tract",
					variables = ACSMovVars,
					year = 2015,
					state = "WA",
					survey = "acs5",
					output = "wide",
					geometry = TRUE,
					cache_table = TRUE) %>%
			 select(-ends_with("M")) %>%
			 mutate(year = 2015,
					pwhnl_smcnty = ifelse(whnl_totalE < 200, NA, (whnl_smcntyE/whnl_totalE)),
					pwhnl_difcntysmst = ifelse(whnl_totalE < 200, NA, (whnl_difcntysmstE/whnl_totalE)),
					pwhnl_outst = ifelse(whnl_totalE < 200, NA, ((whnl_difstE+whnl_abroadE)/whnl_totalE)),

					pbl_smcnty = ifelse(bl_totalE < 200, NA, (bl_smcntyE/bl_totalE)),
					pbl_difcntysmst = ifelse(bl_totalE < 200, NA, (bl_difcntysmstE/bl_totalE)),
					pbl_outst = ifelse(bl_totalE < 200, NA, ((bl_difstE+bl_abroadE)/bl_totalE)),

					pas_smcnty = ifelse(as_totalE < 200, NA, (as_smcntyE/as_totalE)),
					pas_difcntysmst = ifelse(as_totalE < 200, NA, (as_difcntysmstE/as_totalE)),
					pas_outst = ifelse(as_totalE < 200, NA, ((as_difstE+as_abroadE)/as_totalE)),

					pla_smcnty = ifelse(la_totalE < 200, NA, (la_smcntyE/la_totalE)),
					pla_difcntysmst = ifelse(la_totalE < 200, NA, (la_difcntysmstE/la_totalE)),
					pla_outst = ifelse(la_totalE < 200, NA, ((la_difstE+la_abroadE)/la_totalE)))

	acs14 <- get_acs(geography = "tract",
					variables = ACSMovVars,
					year = 2014,
					state = "WA",
					survey = "acs5",
					output = "wide",
					geometry = TRUE,
					cache_table = TRUE) %>%
			 select(-ends_with("M")) %>%
			 mutate(year = 2014,
					pwhnl_smcnty = ifelse(whnl_totalE < 200, NA, (whnl_smcntyE/whnl_totalE)),
					pwhnl_difcntysmst = ifelse(whnl_totalE < 200, NA, (whnl_difcntysmstE/whnl_totalE)),
					pwhnl_outst = ifelse(whnl_totalE < 200, NA, ((whnl_difstE+whnl_abroadE)/whnl_totalE)),

					pbl_smcnty = ifelse(bl_totalE < 200, NA, (bl_smcntyE/bl_totalE)),
					pbl_difcntysmst = ifelse(bl_totalE < 200, NA, (bl_difcntysmstE/bl_totalE)),
					pbl_outst = ifelse(bl_totalE < 200, NA, ((bl_difstE+bl_abroadE)/bl_totalE)),

					pas_smcnty = ifelse(as_totalE < 200, NA, (as_smcntyE/as_totalE)),
					pas_difcntysmst = ifelse(as_totalE < 200, NA, (as_difcntysmstE/as_totalE)),
					pas_outst = ifelse(as_totalE < 200, NA, ((as_difstE+as_abroadE)/as_totalE)),

					pla_smcnty = ifelse(la_totalE < 200, NA, (la_smcntyE/la_totalE)),
					pla_difcntysmst = ifelse(la_totalE < 200, NA, (la_difcntysmstE/la_totalE)),
					pla_outst = ifelse(la_totalE < 200, NA, ((la_difstE+la_abroadE)/la_totalE)))

	acs13 <- get_acs(geography = "tract",
					variables = ACSMovVars,
					year = 2013,
					state = "WA",
					survey = "acs5",
					output = "wide",
					geometry = TRUE,
					cache_table = TRUE) %>%
			 select(-ends_with("M")) %>%
			 mutate(year = 2013,
					pwhnl_smcnty = ifelse(whnl_totalE < 200, NA, (whnl_smcntyE/whnl_totalE)),
					pwhnl_difcntysmst = ifelse(whnl_totalE < 200, NA, (whnl_difcntysmstE/whnl_totalE)),
					pwhnl_outst = ifelse(whnl_totalE < 200, NA, ((whnl_difstE+whnl_abroadE)/whnl_totalE)),

					pbl_smcnty = ifelse(bl_totalE < 200, NA, (bl_smcntyE/bl_totalE)),
					pbl_difcntysmst = ifelse(bl_totalE < 200, NA, (bl_difcntysmstE/bl_totalE)),
					pbl_outst = ifelse(bl_totalE < 200, NA, ((bl_difstE+bl_abroadE)/bl_totalE)),

					pas_smcnty = ifelse(as_totalE < 200, NA, (as_smcntyE/as_totalE)),
					pas_difcntysmst = ifelse(as_totalE < 200, NA, (as_difcntysmstE/as_totalE)),
					pas_outst = ifelse(as_totalE < 200, NA, ((as_difstE+as_abroadE)/as_totalE)),

					pla_smcnty = ifelse(la_totalE < 200, NA, (la_smcntyE/la_totalE)),
					pla_difcntysmst = ifelse(la_totalE < 200, NA, (la_difcntysmstE/la_totalE)),
					pla_outst = ifelse(la_totalE < 200, NA, ((la_difstE+la_abroadE)/la_totalE)))

	acs12 <- get_acs(geography = "tract",
					variables = ACSMovVars,
					year = 2012,
					state = "WA",
					survey = "acs5",
					output = "wide",
					geometry = TRUE,
					cache_table = TRUE) %>%
			 select(-ends_with("M")) %>%
			 mutate(year = 2012,
					pwhnl_smcnty = ifelse(whnl_totalE < 200, NA, (whnl_smcntyE/whnl_totalE)),
					pwhnl_difcntysmst = ifelse(whnl_totalE < 200, NA, (whnl_difcntysmstE/whnl_totalE)),
					pwhnl_outst = ifelse(whnl_totalE < 200, NA, ((whnl_difstE+whnl_abroadE)/whnl_totalE)),

					pbl_smcnty = ifelse(bl_totalE < 200, NA, (bl_smcntyE/bl_totalE)),
					pbl_difcntysmst = ifelse(bl_totalE < 200, NA, (bl_difcntysmstE/bl_totalE)),
					pbl_outst = ifelse(bl_totalE < 200, NA, ((bl_difstE+bl_abroadE)/bl_totalE)),

					pas_smcnty = ifelse(as_totalE < 200, NA, (as_smcntyE/as_totalE)),
					pas_difcntysmst = ifelse(as_totalE < 200, NA, (as_difcntysmstE/as_totalE)),
					pas_outst = ifelse(as_totalE < 200, NA, ((as_difstE+as_abroadE)/as_totalE)),

					pla_smcnty = ifelse(la_totalE < 200, NA, (la_smcntyE/la_totalE)),
					pla_difcntysmst = ifelse(la_totalE < 200, NA, (la_difcntysmstE/la_totalE)),
					pla_outst = ifelse(la_totalE < 200, NA, ((la_difstE+la_abroadE)/la_totalE)))

	acs11 <- get_acs(geography = "tract",
					variables = ACSMovVars,
					year = 2011,
					state = "WA",
					survey = "acs5",
					output = "wide",
					geometry = TRUE,
					cache_table = TRUE) %>%
			 select(-ends_with("M")) %>%
			 mutate(year = 2011,
					pwhnl_smcnty = ifelse(whnl_totalE < 200, NA, (whnl_smcntyE/whnl_totalE)),
					pwhnl_difcntysmst = ifelse(whnl_totalE < 200, NA, (whnl_difcntysmstE/whnl_totalE)),
					pwhnl_outst = ifelse(whnl_totalE < 200, NA, ((whnl_difstE+whnl_abroadE)/whnl_totalE)),

					pbl_smcnty = ifelse(bl_totalE < 200, NA, (bl_smcntyE/bl_totalE)),
					pbl_difcntysmst = ifelse(bl_totalE < 200, NA, (bl_difcntysmstE/bl_totalE)),
					pbl_outst = ifelse(bl_totalE < 200, NA, ((bl_difstE+bl_abroadE)/bl_totalE)),

					pas_smcnty = ifelse(as_totalE < 200, NA, (as_smcntyE/as_totalE)),
					pas_difcntysmst = ifelse(as_totalE < 200, NA, (as_difcntysmstE/as_totalE)),
					pas_outst = ifelse(as_totalE < 200, NA, ((as_difstE+as_abroadE)/as_totalE)),

					pla_smcnty = ifelse(la_totalE < 200, NA, (la_smcntyE/la_totalE)),
					pla_difcntysmst = ifelse(la_totalE < 200, NA, (la_difcntysmstE/la_totalE)),
					pla_outst = ifelse(la_totalE < 200, NA, ((la_difstE+la_abroadE)/la_totalE)))

	acs10 <- get_acs(geography = "tract",
					variables = ACSMovVars,
					year = 2010,
					state = "WA",
					survey = "acs5",
					output = "wide",
					geometry = TRUE,
					cache_table = TRUE) %>%
			 select(-ends_with("M")) %>%
			 mutate(year = 2010,
					pwhnl_smcnty = ifelse(whnl_totalE < 200, NA, (whnl_smcntyE/whnl_totalE)),
					pwhnl_difcntysmst = ifelse(whnl_totalE < 200, NA, (whnl_difcntysmstE/whnl_totalE)),
					pwhnl_outst = ifelse(whnl_totalE < 200, NA, ((whnl_difstE+whnl_abroadE)/whnl_totalE)),

					pbl_smcnty = ifelse(bl_totalE < 200, NA, (bl_smcntyE/bl_totalE)),
					pbl_difcntysmst = ifelse(bl_totalE < 200, NA, (bl_difcntysmstE/bl_totalE)),
					pbl_outst = ifelse(bl_totalE < 200, NA, ((bl_difstE+bl_abroadE)/bl_totalE)),

					pas_smcnty = ifelse(as_totalE < 200, NA, (as_smcntyE/as_totalE)),
					pas_difcntysmst = ifelse(as_totalE < 200, NA, (as_difcntysmstE/as_totalE)),
					pas_outst = ifelse(as_totalE < 200, NA, ((as_difstE+as_abroadE)/as_totalE)),

					pla_smcnty = ifelse(la_totalE < 200, NA, (la_smcntyE/la_totalE)),
					pla_difcntysmst = ifelse(la_totalE < 200, NA, (la_difcntysmstE/la_totalE)),
					pla_outst = ifelse(la_totalE < 200, NA, ((la_difstE+la_abroadE)/la_totalE)))

	# acs09 <- get_acs(geography = "tract",
	# 				variables = ACSMovVars,
	# 				year = 2009,
	# 				state = "WA",
	# 				survey = "acs5",
	# 				geometry = FALSE) %>%
	# 				mutate(year = 2009)

mig <- rbind(acs16,
				 acs15,
				 acs14,
				 acs13,
				 acs12,
				 acs11,
				 acs10)
sf::st_write(mig, "H_Drive/Academe/Data/USCensus/Shapefiles/Long/Migration/mig.shp")
# ==========================================================================
#
# ==========================================================================
	acs <- get_acs(geography = "tract",
					variables = ACSMovVars,
					year = 2016,
					state = "WA",
					survey = "acs5",
					geometry = FALSE) %>%
					mutate(year = 2016)

	acs15 <- acs(geography = "tract",
				variables = ACSMovVars,
				year = 2015,
				state = "WA",
				county = cnty,
				survey = "acs1") %>%
				mutate(year = 2015,
					   estimate = estimate*1.05)

	acs14 <- acs(geography = "tract",
				variables = ACSMovVars,
				year = 2014,
				state = "WA",
				county = cnty,
				survey = "acs1") %>%
				mutate(year = 2014,
					   estimate = estimate*1.05)

	acs13 <- acs(geography = "tract",
				variables = ACSMovVars,
				year = 2013,
				state = "WA",
				county = cnty,
				survey = "acs1") %>%
			mutate(year = 2013,
				   estimate = estimate*1.07)

	acs12 <- acs(geography = "tract",
				variables = ACSMovVars,
				year = 2012,
				state = "WA",
				county = cnty,
				survey = "acs1") %>%
			mutate(year = 2012,
				   estimate = estimate*1.08)

	acs10 <- acs(geography = "tract",
				variables = ACSMovVars,
				year = 2010,
				state = "WA",
				county = cnty,
				survey = "acs5") %>%
			mutate(year = 2007,
				   estimate = estimate*1.21)

	#
	# 1 year ACS not available before 2012
	#

#
# Decennial
# --------------------------------------------------------------------------

	decvars00 <- c(medianinc = "P053001",
				   white = "P152A001",
				   black = "P152B001",
				   aian = "P152C001",
				   asian = "P152D001",
				   nhop = "P152E001",
				   other = "P152F001",
				   two = "P152G001",
				   latinx = "P152H001",
				   whitenl = "P152I001")

	dec00 <- bind_rows(get_decennial(geography = "county",
						   variables = decvars00,
						   cache_table = TRUE,
						   year = 2000,
						   sumfile = "sf1",
						   state = "WA",
						   county = cnty,
						   geometry = FALSE,
						   output = "tidy") %>%
			 mutate(year = 2000,
			 		value = value*1.50) %>%
			 rename(estimate = value),
			 get_decennial(geography = "state",
						   variables = decvars00,
						   cache_table = TRUE,
						   year = 2000,
						   sumfile = "sf1",
						   state = "WA",
						   # county = cnty,
						   geometry = FALSE,
						   output = "tidy") %>%
			 mutate(year = 2000,
			 		value = value*1.50) %>%
			 rename(estimate = value))

#
# Bind df together
# --------------------------------------------------------------------------
	# hud <- read.table(header=T, text='
	# GEOID 	NAME 	variable	estimate	moe	year
	# 53033	King 	medianinc	103400		NA	2018
	# 53033	King 	medianinc	97920		NA	2017
	# 53	Washington	medianinc	81100		NA	2018
	# 53	Washington	medianinc	76500		NA	2017
	# ', stringsAsFactors = FALSE) %>%
	# mutate(GEOID = as.character(GEOID))

	# ==========================================================================
	# HUD uses family income whereas household income is more inclusive # for renting sake
	# ==========================================================================

	econ <- bind_rows(#hud,
					  acs16,
					  acs15, # 2015 and 2013 look odd so removing
					  acs14,
					  acs13,
					  acs12,
					  acs10,
					  dec00)

	races <- c("medianinc", "asian", "black", "latinx", "whitenl")

	econ.sub <- econ %>%
				filter(variable %in% races) %>%
				mutate(variable = factor(variable,
										 levels = c("medianinc",
										 			"whitenl",
										 			"asian",
										 			"latinx",
										 			"black")))

	wa.econ.sub <- econ %>%
				filter(variable %in% races,
						NAME == "Washington") %>%
				mutate(variable = factor(variable,
										 levels = c("medianinc",
										 			"whitenl",
										 			"asian",
										 			"latinx",
										 			"black")))

	pug.econ.sub <- econ %>%
				filter(variable %in% races,
						NAME != "Washington") %>%
				mutate(variable = factor(variable,
										 levels = c("medianinc",
										 			"whitenl",
										 			"asian",
										 			"latinx",
										 			"black"))) %>%
				group_by(year, variable) %>%
				summarize(estimate = mean(estimate, na.rm = T)) %>%
				arrange(desc(year))

	wa.econ.pov <- econ %>%
				filter(variable == "medianinc",
						NAME == "Washington") %>%
				mutate(low = .8*estimate,
					   vlow = .5*estimate,
					   exlow = .3*estimate) %>%
				gather(pov, value, low:exlow) %>%
				mutate(pov = factor(pov,
									levels = c("low", "vlow","exlow")),
					   pov = ifelse(pov == "low", "Low\nIncome",
							 ifelse(pov == "vlow", "Very Low\nIncome", "Ex. Low\nIncome"))) %>%
				arrange(desc(year))

	pug.econ.pov <- pug.econ.sub %>%
				filter(variable == "medianinc")%>%
				mutate(low = .8*estimate,
					   vlow = .5*estimate,
					   exlow = .3*estimate) %>%
				gather(pov, value, low:exlow) %>%
				mutate(pov = factor(pov,
									levels = c("low", "vlow","exlow")),
					   pov = ifelse(pov == "low", "Low\nIncome",
							 ifelse(pov == "vlow", "Very Low\nIncome", "Ex. Low\nIncome"))) %>%
				arrange(desc(year))

	cnty.econ.pov <- econ.sub %>%
				filter(variable == "medianinc")%>%
				mutate(low = .8*estimate,
					   vlow = .5*estimate,
					   exlow = .3*estimate) %>%
				gather(pov, value, low:exlow) %>%
				mutate(pov = factor(pov,
									levels = c("low", "vlow","exlow")),
					   pov = ifelse(pov == "low", "Low\nIncome",
							 ifelse(pov == "vlow", "Very Low\nIncome", "Ex. Low\nIncome"))) %>%
				arrange(desc(year))

# ==========================================================================
# Plots
# ==========================================================================

#
# Washington Plot
# --------------------------------------------------------------------------

wa_medinc <- ggplot() +
		  geom_smooth(data = wa.econ.pov,
				aes(y = value,
					x = year,
					color = pov,
					group = pov),
				color = "grey70",
				se = FALSE,
				# linetype = "F1",
				alpha = .5,
				size = 3) +
		  geom_text(data = wa.econ.pov %>% filter(year == 2016),
		  			aes(label = pov,
                        x = year + 1,
                        y = value,
                        group = pov),
		  			size = 2.7,
		  			# check_overlap = TRUE,
		  			color = "grey60") +
		  geom_line(data = wa.econ.sub,
					  aes(y = estimate,
						  x = year,
						  color = variable),
					  size = .8
					  ) +
		  theme_minimal() +
		  # theme(legend.position='bottom') +
		  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
		  scale_color_brewer(palette = "Set1",
							    direction=-1,
							    name = "Household\nMedian Income",
							    breaks=c("medianinc",
										 "whitenl",
										 "asian",
										 "latinx",
										 "black"),
							    labels=c("Overall",
										 "NL White",
										 "Asian",
										 "Latinx",
										 "African American")) +
			labs(title = "Washington Racial and Ethnic Differences\nin Median Household Income 2000 - 2016",
				  y = "2018 Dollars",
				  x = "Year") +
			ylim(10000, 104000) +
			scale_x_continuous(breaks=c(2000,2007,2012,2014,2016))

wa_medinc

#
# Puget Sound Plot
# --------------------------------------------------------------------------

pug_medinc <- ggplot() +
		  geom_smooth(data = pug.econ.pov,
				aes(y = value,
					x = year,
					color = pov,
					group = pov),
				color = "grey70",
				se = FALSE,
				# linetype = "F1",
				alpha = .5,
				size = 3,) +
		  geom_text(data = pug.econ.pov %>% filter(year == 2016),
		  			aes(label = pov,
                        x = year + 1,
                        y = value,
                        group = pov),
		  				size = 2.7,
		  			# check_overlap = TRUE,
		  			color = "grey60")+
		  geom_line(data = pug.econ.sub,
					  aes(y = estimate,
						  x = year,
						  color = variable),
					  size = .8
					  ) +
		  theme_minimal() +
		  # theme(legend.position='bottom') +
		  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
		  scale_color_brewer(palette = "Set1",
							    direction=-1,
							    name = "Household\nMedian Income",
							    breaks=c("medianinc",
										 "whitenl",
										 "asian",
										 "latinx",
										 "black"),
							    labels=c("Overall",
										 "NL White",
										 "Asian",
										 "Latinx",
										 "African American")) +
			labs(title = "Puget Sound Racial and Ethnic Differences\nin Median Household Income 2000 - 2016",
				  y = "2018 Dollars",
				  x = "Year") +
			ylim(10000, 104000) +
			scale_x_continuous(breaks=c(2000,2007,2012,2014,2016))

pug_medinc

# ggsave(filename = "Sync/Academe/Presentations/180507_CityTalk/SeaRaceIncDiff_Tiers.pdf",
	   # width = 7,
	   # height = 3.5)

#
# County Specific
# --------------------------------------------------------------------------

cnty_medinc <- function(county_name){
		ggplot() +
		  geom_smooth(data = cnty.econ.pov %>%
		  					filter(grepl(county_name, NAME)),
				aes(y = value,
					x = year,
					color = pov,
					group = pov),
				color = "grey70",
				se = FALSE,
				# linetype = "F1",
				alpha = .5,
				size = 3,) +
		  geom_text(data = cnty.econ.pov %>%
		  					filter(grepl(county_name, NAME),
		  						  year == 2016),
		  			aes(label = pov,
                        x = year + 1,
                        y = value,
                        group = pov),
		  				size = 2.7,
		  			# check_overlap = TRUE,
		  			color = "grey60")+
		  geom_line(data = econ.sub %>%
		  					filter(grepl(county_name, NAME)),
					  aes(y = estimate,
						  x = year,
						  color = variable),
					  size = .8,
					  position=position_dodge(width=1)) +
		  geom_pointrange(data = econ.sub %>%
		  						 filter(grepl(county_name, NAME)),
		  				 aes(x = year,
		  				 	y = estimate,
		  				 	ymin=estimate-moe,
		  				 	ymax=estimate+moe,
		  				 	color = variable),
		  				 alpha = .5,
		  				 position=position_dodge(width=1)) +
		  theme_minimal() +
		  # theme(legend.position='bottom') +
		  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
		  scale_color_brewer(palette = "Set1",
							    direction=-1,
							    name = "Household\nMedian Income",
							    breaks=c("medianinc",
										 "whitenl",
										 "asian",
										 "latinx",
										 "black"),
							    labels=c("Overall",
										 "NL White",
										 "Asian",
										 "Latinx",
										 "African American")) +
			labs(title = paste(county_name, "Racial and Ethnic Differences\nin Median Household Income 2000 - 2016"),
				  y = "2018 Dollars",
				  x = "Year") +
			ylim(10000, 104000) +
			scale_x_continuous(breaks=c(2000,2007,2012,2014,2016))
}

Island <- cnty_medinc("Island"); Island
Snohomish <- cnty_medinc("Snohomish"); Snohomish
King <- cnty_medinc("King"); King #
Pierce <- cnty_medinc("Pierce"); Pierce #
Thurston <- cnty_medinc("Thurston"); Thurston
Mason <- cnty_medinc("Mason"); Mason
Kitsap <- cnty_medinc("Kitsap"); Kitsap

# ==========================================================================
# Rent through time
# ==========================================================================

### Seattle tracts ###
	sea <- sf::st_read("/Users/timothythomas/Academe/Data/USCensus/Shapefiles/2010/Washington/Seattle/SeaTr2010.shp")

	rent <- z.rent %>%
			mutate(GEOID10 = factor(GEO2010),
		   		   date.rent = ymd(date.rent),
		   		   year = year(date.rent)) %>%
			filter(GEOID10 %in% sea$GEOID10) %>%
			group_by(year) %>%
			summarize(rent = median(ZRI, na.rm = T))
	glimpse(rent)

	rb <- rent %>%
		  mutate(rb30 = (rent*12)/.3,
		  		 rb50 = (rent*12)/.5)

# Plot
	rbplot <- ggplot() +
		  geom_smooth(data = econ.pov %>% filter(pov =="low"),
				aes(y = value,
					x = year,
					color = pov),
				color = "grey80",
				se = FALSE,
				# linetype = "F1",
				alpha = .5,
				size = 3,) +
		  geom_smooth(data = econ.pov %>% filter(pov =="vlow"),
				aes(y = value,
					x = year,
					color = pov),
				color = "grey80",
				se = FALSE,
				# linetype = "F1",
				alpha = .5,
				size = 3,) +
		  geom_smooth(data = econ.pov %>% filter(pov =="exlow"),
				aes(y = value,
					x = year,
					color = pov),
				color = "grey80",
				se = FALSE,
				# linetype = "F1",
				alpha = .5,
				size = 3,) +
		  geom_line(data = econ.sub,
					  aes(y = estimate,
						  x = year,
						  color = variable),
					  size = .8
					  ) +
		  geom_point(data = econ.sub,
					  aes(y = estimate,
						  x = year,
						  color = variable),
					  alpha = .5) +
		  geom_smooth(data = rb,
		  			  aes(y = rb30,
		  			  	  x = year),
		  			  color = "red",
		  			  linetype = "dashed",
		  			  se = FALSE) +
		  geom_smooth(data = rb,
		  			  aes(y = rb50,
		  			  	  x = year),
		  			  color = "blue",
		  			  linetype = "dashed",
		  			  se = FALSE) +
		  theme_minimal() +
		  # theme(legend.position='bottom') +
		  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
		  scale_color_brewer(palette = "Set1",
							    direction=-1,
							    name = "Household\nMedian Income",
							    breaks=c("medianinc",
										 "whitenl",
										 "asian",
										 "latinx",
										 "black"),
							    labels=c("Overall",
										 "NL White",
										 "Asian",
										 "Latinx",
										 "African American")) +
			labs(#title = "King County Racial and Ethnic Differences\nin Median  Household Income 2000 - 2016",
				  y = "2018 Dollars",
				  x = "Year") +
			ylim(20000, 104000) +
			scale_x_continuous(breaks=c(2000,2007,2012,2014,2016,2018))


ggsave(filename = "/Users/timothythomas/Academe/Presentations/180604_RentersCommissionTalk.pdf",
	   width = 7,
	   height = 3.5)


# ==========================================================================
# HUD Values
# ==========================================================================

hud <- read.delim(file = "H_Drive/Temp/NCP/Data/HUD/combined_years_HUD_AMI.txt")


# ==========================================================================
# NEXT:
# * add group specific income values (how many at what economic level) to
# show who falls bove and below rent burden.
# ==========================================================================
