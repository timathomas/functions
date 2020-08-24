race_vars12 <-
	c(totrace = "B03002_001",
	  White = "B03002_003",
	  Black = "B03002_004",
	  Asian = "B03002_006",
	  Latinx = "B03002_012"
)

race_vars11<-
	c(totrace = "B03002_001",
	  White = "B03002_003",
	  Black = "B03002_004",
	  Asian = "B03002_006",
	  Latinx = "B03002_012"
		)

race_vars00 <-
	c(
totrace = "P008001", # HISPANIC:TotalHispanic'&'NotHispanic
# = P008002, # HISPANIC:Total NotHispanic or Latino
White = "P008003", # HISPANIC:NotHisp:White alone
Black = "P008004", # HISPANIC:NotHisp:Bl/AfAm alone
# = P008005, # HISPANIC:NotHisp:AmInd/AK alone
Asian = "P008006", # HISPANIC:NotHisp:Asian alone
# = P008007, # HISPANIC:NotHisp:HI alone
# = P008008, # HISPANIC:NotHisp:Other alone
# = P008009, # HISPANIC:NotHisp:Two+races
Latinx = "P008010"#, # HISPANIC:Total Hispanic or Latino
# = P008011, # HISPANIC:Hisp:White alone
# = P008012, # HISPANIC:Hisp:Bl/AfAm alone
# = P008013, # HISPANIC:Hisp:AmInd/AK alone
# = P008014, # HISPANIC:Hisp:Asian alone
# = P008015, # HISPANIC:Hisp:HI alone
# = P008016, # HISPANIC:Hisp:Other alone
# = P008017 # HISPANIC:Hisp:Two+ races
		)

# ==========================================================================
# Race and Population Growth
# ==========================================================================

library(lubridate)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(tidyverse)

### View variables ###
# vars_acs5_2018 <- load_variables(2018, "acs5", cache = TRUE)
# vars_acs1_2018 <- load_variables(2018, "acs1", cache = TRUE)
# vars_acs1_2016 <- load_variables(2016, "acs1", cache = TRUE)
# vars_acs1_2015 <- load_variables(2015, "acs1", cache = TRUE)
# vars_acs1_2014 <- load_variables(2014, "acs1", cache = TRUE)
# vars_acs1_2013 <- load_variables(2013, "acs1", cache = TRUE)
# vars_acs1_2012 <- load_variables(2012, "acs1", cache = TRUE)
# vars_acs5_2012 <- load_variables(2012, "acs5", cache = TRUE)
# vars_acs3_2011 <- load_variables(2012, "acs3", cache = TRUE)
# vars_acs5_2011 <- load_variables(2011, "acs5", cache = TRUE)
# vars_dec10 <- load_variables(2010, "sf1", cache = TRUE)
# vars_dec10sf2 <- load_variables(2010, "sf2", cache = TRUE)
# vars_dec00sf3 <- load_variables(2000, "sf3", cache = TRUE)
# vars_dec00sf1 <- load_variables(2000, "sf1", cache = TRUE)
# View(vars_acs5_2018)
# View(vars_acs1_2018)
# View(vars_acs1_2016)
# View(vars_acs1_2015)
# View(vars_acs1_2014)
# View(vars_acs1_2013)
# View(vars_acs1_2012)
# View(vars_acs5_2012)
# View(vars_acs3_2011)
# View(vars_acs5_2011)
# View(vars_dec10)
# View(vars_dec10sf2)
# View(vars_dec00sf3)
# View(vars_dec00sf1)


# ==========================================================================
# Variables
# ==========================================================================

# ==========================================================================
# Pull data
# ==========================================================================

	acs5yr <- c(2018,2009)
	acs1year <- rep(2012:2018, 1)
	acs <-
		function(
			geography = "county",
			variables,
			state = "WA",
			county = NULL,
			geometry = TRUE,
			survey,
			year){
	acs <-
		get_acs(
			geography = geography,
			variables = variables,
			survey = survey,
			state = state,
			county = county,
			output = "tidy",
			year = year,
			geometry = geometry,
			cache_table = TRUE) %>%
		select(-moe)
	}

race5yr <- get_acs(geography = "block group",
			variables = race_vars12,
			survey = "acs5",
			state = "WA",
			county = "King",
			output = "tidy",
			year = 2018,
			geometry = TRUE,
			cache_table = TRUE) %>%
		select(-moe) %>%
		spread(variable, estimate)


	race5yr <-
	map_df(acs5yr, function(x){
		acs(variables = race_vars12, survey = "acs5", year = x) %>%
		replace(is.na(.), 0) %>%
		mutate(year = x,
			   survey = "acs5") %>%
		spread(variable, estimate)
	})

	race1yr <-
		map_df(acs1year, function(x){
			acs(variables = race_vars12, survey = "acs1", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs1") %>%
			spread(variable, estimate)
		})

	race00 <-
		get_decennial(geography = "tract",
					  variables = race_vars00,
		  			  cache_table = TRUE,
		  			  year = 2000,
		  			  sumfile = "sf1",
		  			  state = "WA",
		  			  county = "King",
		  			  output = "wide",
		  			  geometry = TRUE) %>%
		mutate(year = 2000,
			   survey = "sf1")

	race_census <-
		bind_rows(race5yr,race1yr,race00) %>%
		unite(survey, survey, year, remove = FALSE) %>%
		ungroup() %>%
		mutate(NAME = gsub(' County.*', '', NAME),
			   Other = totrace - Asian - Black - Latinx - White)

# date <- format(Sys.time(), "%Y-%m-%d")
file.path <- "~/data/census/"
	write_csv(race_census,paste0(file.path,"race_census", ".csv.bz2"))
