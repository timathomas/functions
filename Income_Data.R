# ==========================================================================
# Pull Variables
# ==========================================================================

# ==========================================================================
# AMI Plots
# Steps:
# create area median income
# ==========================================================================

# rm(list=ls()) #reset
options(scipen=10) # avoid scientific notation
gc()

# update.packages(ask = FALSE)
source("~/git/functions/functions.r")
# library(lubridate)
ipak(c("tigris","tidycensus", "tidyverse", "tigris"))
options(tigris_use_cache = TRUE)


# ==========================================================================
# Variables
# ==========================================================================

path <- "~/git/functions/" # adjust as needed
var <- "Variables.R"

source("~/git/functions/Variables.R")

data(fips_codes)

states <- 
	fips_codes %>% 
	filter(!state_name %in% c("American Samoa", "Guam", "Northern Mariana Islands", "Puerto Rico", "U.S. Minor Outlying Islands", "U.S. Virgin Islands")) %>% 
	select(state) %>% distinct() %>% pull(state)

#
# Functions
# --------------------------------------------------------------------------

	acs5yr <- c(2019,2009)
	acs1year <- rep(2012:2019, 1)
	acs <-
		function(
			geography = "county",
			variables,
			state = states,
			county = NULL,
			geometry = FALSE,
			survey,
			year){
	acs <-
		get_acs(
			geography = geography,
			variables = variables,
			survey = survey,
			state = states,
			county = county,
			output = "tidy",
			year = year,
			geometry = geometry,
			cache_table = TRUE) %>%
		select(-moe)
	}

# medincacs3yr <-
# 	get_acs(geography = "county",
# 			variables = acsmedinc_vars,
# 			survey = "acs3",
# 			state = "WA",
# 			year = 2012,
# 			geometry = FALSE,
# 			cache_table = TRUE)

# ==========================================================================
# Median household income
# ==========================================================================
	medincacs5yr <-
	map_df(acs5yr, function(x){
		acs(variables = acsmedinc_vars, survey = "acs5", year = x) %>%
		replace(is.na(.), 0) %>%
		mutate(year = x,
			   survey = "acs5") %>%
		spread(variable, estimate)
	})

	medincacs1yr <-
		map_df(acs1year, function(x){
			acs(variables = acsmedinc_vars, survey = "acs1", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs1") %>%
			spread(variable, estimate)
		})

	medincdec00 <-
		get_decennial(geography = "county",
					  variables = decmedinc_vars,
		  			  cache_table = TRUE,
		  			  year = 2000,
		  			  sumfile = "sf3",
		  			  # state = "WA",
		  			  output = "wide") %>%
		mutate(year = 2000,
			   survey = "sf3")

	medinc <-
		bind_rows(medincdec00, medincacs5yr, medincacs1yr) %>%
		left_join(., cpi) %>% 
		mutate(
			mhhinc_cpi = mhhinc * CPI,
			mhhinc_wht_cpi = mhhinc_wht * CPI,
			mhhinc_blk_cpi = mhhinc_blk * CPI,
			mhhinc_aian_cpi = mhhinc_aian * CPI,
			mhhinc_asi_cpi = mhhinc_asi * CPI,
			mhhinc_nhop_cpi = mhhinc_nhop * CPI,
			mhhinc_oth_cpi = mhhinc_oth * CPI,
			mhhinc_two_cpi = mhhinc_two * CPI,
			mhhinc_lat_cpi = mhhinc_lat * CPI,
			mhhinc_whtnl_cpi = mhhinc_whtnl * CPI) %>% 
		# mutate_at(vars(mhhinc:mhhinc_whtnl), funs = list(cpi = .*CPI)) %>%
		mutate(AMI80_cpi = mhhinc_cpi*.8,
			   AMI50_cpi = mhhinc_cpi*.5,
			   AMI30_cpi = mhhinc_cpi*.3,
			   NAME = gsub(' County.*', '', NAME))

	wa_medincacs1yr <-
	map_df(acs1year, function(x){
		acs(variables = acsmedinc_vars,
			survey = "acs1",
			year = x,
			geography = "state") %>%
		replace(is.na(.), 0) %>%
		mutate(year = x,
			   survey = "acs1") %>%
		spread(variable, estimate)
	})

	wa_medincacs5yr <-
		get_acs(variables = acsmedinc_vars,
			survey = "acs5",
			state = "WA",
			year = 2009,
			geography = "state") %>%
		replace(is.na(.), 0) %>%
		mutate(year = 2009,
			   survey = "acs5") %>%
		select(-moe) %>%
		spread(variable, estimate)

	wa_medincdec00 <-
		get_decennial(geography = "state",
					  variables = decmedinc_vars,
		  			  cache_table = TRUE,
		  			  year = 2000,
		  			  sumfile = "sf3",
		  			  state = "WA",
		  			  output = "wide") %>%
		mutate(year = 2000,
			   survey = "sf3")
	wa_medinc <-
		bind_rows(wa_medincacs1yr, wa_medincacs5yr, wa_medincdec00) %>%
		left_join(., cpi) %>%
		mutate_at(vars(mhhinc:mhhinc_whtnl), .funs = funs(cpi = .*CPI)) %>%
		mutate(AMI80_cpi = mhhinc_cpi*.8,
			   AMI50_cpi = mhhinc_cpi*.5,
			   AMI30_cpi = mhhinc_cpi*.3)

# ==========================================================================
# Household income
# ==========================================================================

	hhincacs5yr10 <-
	map_df(acs5yr, function(x){
			acs(variables = acshhinc_vars, survey = "acs5", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs5")
		})

	hhincacs1yr <-
		map_df(acs1year, function(x){
			acs(variables = acshhinc_vars, survey = "acs1", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs1")
		})

	hhincdec00 <-
		get_decennial(geography = "county",
					  variables = dechhinc_vars,
		  			  cache_table = TRUE,
		  			  year = 2000,
		  			  sumfile = "sf3",
		  			  state = "WA",
		  			  output = "tidy") %>%
		mutate(year = 2000,
			   survey = "sf3") %>%
		rename(estimate = value)

	hhinc <-
		bind_rows(hhincacs5yr10, hhincacs1yr, hhincdec00) %>%
		mutate(NAME = gsub(' County.*', '', NAME))

	inc_join <- left_join(hhinc, medinc %>%
				select(GEOID, year, survey, mhhinc))

	hhinc_totals <-
		inc_join %>%
		filter(grepl("_Total", variable)) %>%
		separate(variable, c("type", "total"), sep = "_") %>%
		select(-total) %>%
		rename(HHInc_Total = estimate)

	hhinc <-
		inc_join %>%
		filter(!grepl("_Total", variable)) %>%
		separate(variable, c("type", "income"), sep = "_") %>%
		left_join(., hhinc_totals) %>%
		left_join(., cpi) %>%
		mutate(NAME = gsub(' County.*', '', NAME),
			   income = as.numeric(income)*1000,
			   income_cpi = income * CPI,
			   mhhinc_cpi = CPI * mhhinc,
			   AMI80_cpi = mhhinc_cpi * .8,
			   AMI50_cpi = mhhinc_cpi * .5,
			   AMI30_cpi = mhhinc_cpi * .3) %>%
		group_by(GEOID, type, year, survey) %>%
		mutate(AMI80_pop = if_else(income_cpi <= AMI80_cpi &
			   					   lead(income_cpi) >= AMI80_cpi,
			   					   round((1-((income_cpi - AMI80_cpi)/(lead(income_cpi) - income_cpi))*estimate), 0),
			   			   if_else(AMI80_cpi >= income_cpi, estimate,
			   					   0)),
			   AMI50_pop = if_else(income_cpi <= AMI50_cpi &
			   					   lead(income_cpi) >= AMI50_cpi,
			   					   round((1-((income_cpi - AMI50_cpi)/(lead(income_cpi) - income_cpi))*estimate), 0),
			   			   if_else(AMI50_cpi >= income_cpi, estimate,
			   					   0)),
			   AMI30_pop = if_else(income_cpi <= AMI30_cpi &
			   					   lead(income_cpi) >= AMI30_cpi,
			   					   round((1-((income_cpi - AMI30_cpi)/(lead(income_cpi) - income_cpi))*estimate), 0),
			   			   if_else(AMI30_cpi >= income_cpi, estimate,
			   					   0))) %>%
		mutate(AMI80_prop = sum(AMI80_pop)/HHInc_Total,
			   AMI50_prop = sum(AMI50_pop)/HHInc_Total,
			   AMI30_prop = sum(AMI30_pop)/HHInc_Total) %>%
		ungroup() %>%
		mutate(year = if_else(year == 2009, 2007, year))

# ==========================================================================
# Household income by tenure
# ==========================================================================

	hhinctenacs5yr <-
	map_df(acs5yr, function(x){
			acs(variables = acs_hhincten, survey = "acs5", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs5")
		})

	hhinctenacs1yr <-
		map_df(acs1year, function(x){
			acs(variables = acs_hhincten, survey = "acs1", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs1")
		})

	hhinctendec00 <-
		get_decennial(geography = "county",
					  variables = dec00_hhincten,
		  			  cache_table = TRUE,
		  			  year = 2000,
		  			  sumfile = "sf3",
		  			  state = "WA",
		  			  output = "tidy") %>%
		mutate(year = 2000,
			   survey = "sf3") %>%
		rename(estimate = value)

	hhincten <-
		bind_rows(hhinctenacs5yr, hhinctenacs1yr, hhinctendec00) %>%
		mutate(NAME = gsub(' County.*', '', NAME),
			   GEOID = as.numeric(GEOID)) %>%
		unite(s_y, survey, year, remove = FALSE) %>%
		left_join(., cpi) %>%
		filter(!variable %in% c("HHIncTen_Total", "HHIncTenOwn", "HHIncTenRent")) %>%
		separate(variable, c("type", "income"), sep = "_") %>%
		mutate(income = as.numeric(income)*1000) %>%
		left_join(., medinc %>%
					 mutate(GEOID = as.numeric(GEOID)) %>%
					 select(GEOID, year, survey, CPI, mhhinc, mhhinc_cpi, AMI80_cpi:AMI30_cpi)) %>%
		mutate(income_cpi = income*CPI) %>%
		mutate(AMI80_pop = if_else(income_cpi <= AMI80_cpi &
			   					   lead(income_cpi) >= AMI80_cpi,
			   					   round((1-((income_cpi - AMI80_cpi)/(lead(income_cpi) - income_cpi))*estimate), 0),
			   			   if_else(AMI80_cpi >= income_cpi, estimate,
			   					   0)),
			   AMI50_pop = if_else(income_cpi <= AMI50_cpi &
			   					   lead(income_cpi) >= AMI50_cpi,
			   					   round((1-((income_cpi - AMI50_cpi)/(lead(income_cpi) - income_cpi))*estimate), 0),
			   			   if_else(AMI50_cpi >= income_cpi, estimate,
			   					   0)),
			   AMI30_pop = if_else(income_cpi <= AMI30_cpi &
			   					   lead(income_cpi) >= AMI30_cpi,
			   					   round((1-((income_cpi - AMI30_cpi)/(lead(income_cpi) - income_cpi))*estimate), 0),
			   			   if_else(AMI30_cpi >= income_cpi, estimate,
			   					   0))) %>%
		group_by(NAME, s_y, type) %>%
		mutate(AMI80_prop = sum(AMI80_pop)/sum(estimate),
			   AMI50_prop = sum(AMI50_pop)/sum(estimate),
			   AMI30_prop = sum(AMI30_pop)/sum(estimate),
			   AMI80_pop = sum(AMI80_pop),
			   AMI50_pop = sum(AMI50_pop),
			   AMI30_pop = sum(AMI30_pop),
			   HHIncTen_Total = sum(estimate)) %>%
		ungroup() %>%
		distinct() %>%
		mutate(year = if_else(year == 2009, 2007, year))

# ==========================================================================
# Write Data
# ==========================================================================

# date <- format(Sys.time(), "%Y-%m-%d")
# file.path <- "/data/census/"
	saveRDS(hhinc, "~/git/functions/data/household_income.rds")
	saveRDS(medinc, "~/git/functions/data/medinc.rds")
	saveRDS(wa_medinc, "~/git/functions/data/wa_medinc.rds")
	saveRDS(hhincten, "~/git/functions/data/hhincten.rds")

# ==========================================================================
# Edits
# ==========================================================================
