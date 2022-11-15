# ==========================================================================
# Rent
# ==========================================================================
source("~/git/timathomas/functions/functions.r")
ipak_gh(c("jalvesaq/colorout"))
ipak(c(
"data.table",
"tidyverse",
"tidycensus",
"purrr",
"tigris"))

state_abb <- "WA"

#
# Census variables
# --------------------------------------------------------------------------

#
# Pull variables
# --------------------------------------------------------------------------
source("~/git/timathomas/functions/Variables.R")

	acs5yr <- c(2019,2009)
	acs1year <- rep(2012:2019, 1)
	acs <-
		function(
			geography = "county",
			variables,
			state = state_abb,
			county = NULL,
			geometry = FALSE,
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

	rentersacs5yr <-
	map_df(acs5yr, function(x){
			acs(variables = acsrent_vars, survey = "acs5", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs5") %>%
			spread(variable, estimate)
		})

	rentersacs1yr <-
		map_df(acs1year, function(x){
			acs(variables = acsrent_vars, survey = "acs1", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs1") %>%
			spread(variable, estimate)
		})

	rentersdec00 <-
		get_decennial(geography = "county",
					  variables = decrent_vars00,
		  			  cache_table = TRUE,
		  			  year = 2000,
		  			  sumfile = "sf3",
		  			  state = state_abb,
		  			  output = "wide") %>%
		mutate(year = 2000,
			   survey = "sf3",
			   tottenAIAN = sum(totownAIAN, totrentAIAN, na.rm = TRUE),
			   tottenASI = sum(totownASI, totrentASI, na.rm = TRUE),
			   tottenBLK = sum(totownBLK, totrentBLK, na.rm = TRUE),
			   tottenLAT = sum(totownLAT, totrentLAT, na.rm = TRUE),
			   tottenNHOP = sum(totownNHOP, totrentNHOP, na.rm = TRUE),
			   tottenOTH = sum(totownOTH, totrentOTH, na.rm = TRUE),
			   tottenTWO = sum(totownTWO, totrentTWO, na.rm = TRUE),
			   tottenWHT = sum(totownWHT, totrentWHT, na.rm = TRUE),
			   tottenWHTNL = sum(totownWHTNL, totrentWHTNL, na.rm = TRUE))
	# rentersdec20 <-
	# 	get_decennial(geography = "county",
	# 				  variables = decrent_vars00,
	# 	  			  cache_table = TRUE,
	# 	  			  year = 2020,
	# 	  			  sumfile = "sf3",
	# 	  			  state = state_abb,
	# 	  			  output = "wide") %>% glimpse()
	# 	mutate(year = 2000,
	# 		   survey = "sf3",
	# 		   tottenAIAN = sum(totownAIAN, totrentAIAN, na.rm = TRUE),
	# 		   tottenASI = sum(totownASI, totrentASI, na.rm = TRUE),
	# 		   tottenBLK = sum(totownBLK, totrentBLK, na.rm = TRUE),
	# 		   tottenLAT = sum(totownLAT, totrentLAT, na.rm = TRUE),
	# 		   tottenNHOP = sum(totownNHOP, totrentNHOP, na.rm = TRUE),
	# 		   tottenOTH = sum(totownOTH, totrentOTH, na.rm = TRUE),
	# 		   tottenTWO = sum(totownTWO, totrentTWO, na.rm = TRUE),
	# 		   tottenWHT = sum(totownWHT, totrentWHT, na.rm = TRUE),
	# 		   tottenWHTNL = sum(totownWHTNL, totrentWHTNL, na.rm = TRUE))

	# cpi <- function(x){cpi*x}
	renters <-
		bind_rows(rentersdec00, rentersacs5yr, rentersacs1yr) %>%
		left_join(., cpi) %>%
		mutate(mgrent = CPI*mgrent)

	rbacs5yr <-
	map_df(acs5yr, function(x){
			acs(variables = acsrb_vars, survey = "acs5", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs5")
			})

	rbacs1yr <-
		map_df(acs1year, function(x){
			acs(variables = acsrb_vars, survey = "acs1", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs1")
		})

	rbdec00 <-
		get_decennial(geography = "county",
					  variables = decrb_vars,
		  			  cache_table = TRUE,
		  			  year = 2000,
		  			  sumfile = "sf3",
		  			  state = state_abb,
		  			  output = "tidy") %>%
		mutate(year = 2000,
			   survey = "sf3") %>%
		rename(estimate = value)

	rb <-
		bind_rows(rbacs5yr,rbacs1yr,rbdec00) %>%
		spread(variable, estimate)

runits5yr2019 <-
	get_acs(geography = "county",
			state = state_abb,
			variables = runits_acs2015_vars,
			cache_table = TRUE,
			year = 2019,
			survey = "acs5",
			geometry = FALSE)

	runits5yr2019 <-
	get_acs(geography = "county",
			state = state_abb,
			variables = runits_acs2015_vars,
			cache_table = TRUE,
			year = 2019,
			survey = "acs5",
			geometry = FALSE)

runits5yr2009 <-
	get_acs(geography = "county",
			state = state_abb,
			variables = runits_acs2014_vars,
			cache_table = TRUE,
			year = 2009,
			survey = "acs5",
			geometry = FALSE)

runits1yr2015_2019 <- # added categories in 2015
	map_df(c(2015,2016,2017, 2018, 2019), function(x){
			acs(variables = runits_acs2015_vars, survey = "acs1", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs1")
		})

runits1yr2012_2014 <-
	map_df(c(2012,2013,2014), function(x){
			acs(variables = runits_acs2014_vars, survey = "acs1", year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs1")
		})

runitsdec2000 <-
		get_decennial(geography = "county",
					  variables = runits_dec2000_vars,
		  			  cache_table = TRUE,
		  			  year = 2000,
		  			  sumfile = "sf3",
		  			  state = state_abb,
		  			  output = "tidy") %>%
		mutate(year = 2000,
			   survey = "sf3") %>%
		rename(estimate = value)

#
# Affordable housing units
# --------------------------------------------------------------------------
medinc <- readRDS("~/git/timathomas/functions/data/medinc.rds")

runits1yr_adj2019 <-
	runits1yr2015_2019 %>% 
	filter(variable %in% c("ru_tot", "ru_cash", "ru_nocash")) %>%
	spread(variable, estimate) %>%
	left_join(., runits1yr2015_2019 %>%
				 filter(!variable %in% c("ru_tot", "ru_cash", "ru_nocash"))) %>%
	left_join(., cpi) %>% 
	mutate(NAME = gsub(' County.*', '', NAME),
		   variable = as.numeric(str_sub(variable, 4)),
		   cpi_rent_cost = CPI*variable,
		   annual_income_at30rb = cpi_rent_cost*12/.3) %>% 
	rename(count = estimate,
		   rent_cost = variable) %>%
	left_join(., medinc %>%
				 filter(survey == "acs1") %>%
				 select(GEOID, year, mhhinc,mhhinc_cpi, AMI80_cpi:AMI30_cpi)) %>%
	mutate(afforable_ami30 = if_else(annual_income_at30rb < AMI30_cpi, count, 0),
		   afforable_ami50 = if_else(annual_income_at30rb < AMI50_cpi, count, 0))


runits1yr_adj2014 <-
	runits1yr2012_2014 %>%
	filter(variable %in% c("ru_tot", "ru_cash", "ru_nocash")) %>%
	spread(variable, estimate) %>%
	left_join(., runits1yr2012_2014 %>%
				 filter(!variable %in% c("ru_tot", "ru_cash", "ru_nocash"))) %>%
	left_join(., cpi) %>%
	mutate(NAME = gsub(' County.*', '', NAME),
		   variable = as.numeric(str_sub(variable, 4)),
		   cpi_rent_cost = CPI*variable,
		   annual_income_at30rb = cpi_rent_cost*12/.3) %>%
	rename(count = estimate,
		   rent_cost = variable) %>%
	left_join(., medinc %>%
				 filter(survey == "acs1") %>%
				 select(GEOID, year, mhhinc,mhhinc_cpi, AMI80_cpi:AMI30_cpi)) %>%
	mutate(afforable_ami30 = if_else(annual_income_at30rb < AMI30_cpi, count, 0),
		   afforable_ami50 = if_else(annual_income_at30rb < AMI50_cpi, count, 0))

runits5yr_adj2009 <-
	runits5yr2009 %>%
	select(-moe) %>%
	filter(variable %in% c("ru_tot", "ru_cash", "ru_nocash")) %>%
	spread(variable, estimate) %>%
	left_join(., runits5yr2009 %>%
				 filter(!variable %in% c("ru_tot", "ru_cash", "ru_nocash")) %>%
				 select(-moe)) %>%
	mutate(year = 2009) %>%
	left_join(., cpi) %>%
	mutate(NAME = gsub(' County.*', '', NAME),
		   variable = as.numeric(str_sub(variable,4)),
		   cpi_rent_cost = CPI*variable,
		   annual_income_at30rb = cpi_rent_cost*12/.3) %>%
	rename(count = estimate,
		   rent_cost = variable) %>%
	left_join(., medinc %>%
				 filter(survey == "acs5") %>%
				 select(GEOID, year, mhhinc,mhhinc_cpi, AMI80_cpi:AMI30_cpi)) %>%
	mutate(afforable_ami30 = if_else(annual_income_at30rb < AMI30_cpi, count, 0),
		   afforable_ami50 = if_else(annual_income_at30rb < AMI50_cpi, count, 0))

runitsdec_2000adj <-
	runitsdec2000 %>%
	select(-survey) %>%
	filter(variable %in% c("ru_tot", "ru_cash", "ru_nocash")) %>%
	spread(variable, estimate) %>%
	left_join(., runitsdec2000 %>%
				 select(-survey) %>%
				 filter(!variable %in% c("ru_tot", "ru_cash", "ru_nocash"))) %>%
	left_join(., cpi) %>%
	mutate(NAME = gsub(' County.*', '', NAME),
		   variable = as.numeric(str_sub(variable, 4)),
		   cpi_rent_cost = CPI*variable,
		   annual_income_at30rb = cpi_rent_cost*12/.3) %>%
	rename(count = estimate,
		   rent_cost = variable) %>%
	left_join(., medinc %>%
				 filter(survey == "sf3") %>%
				 select(GEOID, year, mhhinc,mhhinc_cpi, AMI80_cpi:AMI30_cpi)) %>%
	mutate(afforable_ami30 = if_else(annual_income_at30rb < AMI30_cpi, count, 0),
		   afforable_ami50 = if_else(annual_income_at30rb < AMI50_cpi, count, 0))

runits <-
	bind_rows(runits1yr_adj2019, runits1yr_adj2014, runits5yr_adj2009, runitsdec_2000adj) %>%
	mutate(Rent = if_else(cpi_rent_cost <= 499, "Less than $500",
					  if_else(cpi_rent_cost > 499 &
					  		  cpi_rent_cost <= 799, "$500 to $799",
					  if_else(cpi_rent_cost > 799 &
					  		  cpi_rent_cost <= 999, "$800 to $999",
					  if_else(cpi_rent_cost > 999 &
					  		  cpi_rent_cost <= 1499, "$1,000 to $1,499",
					  if_else(cpi_rent_cost > 1499 &
					  		  cpi_rent_cost <= 1999, "$1,500 to $1,999",
					  if_else(cpi_rent_cost > 1999, "$2,000 or more", NA_character_)))))))

ps_runits <-
	runits %>%
	filter(NAME %in% c("King", "Snohomish", "Pierce", "Kitsap", "Thurston", "Skagit", "Island")) %>%
	group_by(Rent, year) %>%
	summarise(count = sum(count),
			  NAME = "Puget Sound") %>%
	ungroup()

wa_runits <-
	runits %>%
	group_by(Rent, year) %>%
	summarise(count = sum(count),
			  NAME = "Washington") %>%
	ungroup()

wa_noking_runits <-
	runits %>%
	filter(NAME != "King") %>%
	group_by(Rent, year) %>%
	summarise(count = sum(count),
			  NAME = "WA No King") %>%
	ungroup()

runits <-
	bind_rows(runits, ps_runits, wa_runits, wa_noking_runits)


# 	%>%
# 	group_by(GEOID, NAME, year, tot)

# 	%>%
# 	summarise(afforable_ami30 = sum(afforable_ami30),
# 			  afforable_ami50 = sum(afforable_ami50))

# runits %>% filter(NAME == "King") %>%
# mutate(afforable_ami30/tot*1000)



### For neighborhood level rent ###
#
# Zillow data
# --------------------------------------------------------------------------

	load("/Users/timthomas/git/neigh_health/Data/zillow.rdata")
	# load("/Users/timothythomas/Academe/Data/Zillow/RData/zillow.rdata")

#
# Zillow Rent
# --------------------------------------------------------------------------
z_years <- rep(2010:2015)
wa_co <- rentersdec00$GEOID

z_rent <-
	map_df(z_years, function(x) {
			z.rent %>%
			mutate(GEOID = substring(GEO2010, 1,5)) %>%
  			filter(date.rent >= paste0(x,"-01-01"),
				   date.rent <= paste0(x,"-12-31")) %>%
			group_by(GEOID) %>%
			summarise(med_z_rent = median(ZRI, na.rm = TRUE)) %>%
			mutate(year = x)
})

	z_change <- z_rent %>%

				ungroup() %>%
				left_join(., cpi) %>%
				mutate(year = paste0("rent",year),
					   med_z_rent = CPI*med_z_rent) %>%
				select(-CPI) %>%
				spread(year, med_z_rent) %>%
				filter(str_detect(GEOID, "^53")) %>%
				mutate(ChangeRent1011 = (rent2011 - rent2010),
					   ChangeRent1112 = (rent2012 - rent2011),
					   ChangeRent1213 = (rent2013 - rent2012),
					   ChangeRent1314 = (rent2014 - rent2013),
					   ChangeRent1415 = (rent2015 - rent2014),
					   pChangeRent1011 = (rent2011 - rent2010)/rent2010,
					   pChangeRent1112 = (rent2012 - rent2011)/rent2011,
					   pChangeRent1213 = (rent2013 - rent2012)/rent2012,
					   pChangeRent1314 = (rent2014 - rent2013)/rent2013,
					   pChangeRent1415 = (rent2015 - rent2014)/rent2014)

	c_rent <- renters %>%
			  select(GEOID, year, survey, mgrent)

# ==========================================================================
# Clean HUD FMR data
# ==========================================================================

HUDFMR_2003 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FMR2003F_County.xls") %>%
	mutate(fips2000 = paste0(str_pad(State, 2, pad = 0), str_pad(County, 3, pad = 0))) %>%
	select(fips2000, fmr_0 = FMR0, fmr_1 = FMR1, fmr_2 = FMR2, fmr_3 = FMR3, fmr_4 = FMR4, AreaName = msaname, countyname = CountyName, State = State_Alpha) %>%
	group_by(fips2000) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2003) %>%
	ungroup()

HUDFMR_2004 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FMR2004F_County.xls") %>%
	mutate(fips2000 = paste0(str_pad(State, 2, pad = 0), str_pad(County, 3, pad = 0))) %>%
	select(fips2000, fmr_0 = New_FMR0, fmr_1 = New_FMR1, fmr_2 = New_FMR2, fmr_3 = New_FMR3, fmr_4 = New_FMR4, AreaName = MSAName, countyname = CountyName, State = State_Alpha) %>%
	group_by(fips2000) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2004) %>%
	ungroup()

HUDFMR_2005 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/Revised_FY2005_CntLevel.xls") %>%
	select(fips2000 = stco, fmr_0 = FMR_0Bed, fmr_1 = FMR_1Bed, fmr_2 = FMR_2Bed, fmr_3 = FMR_3Bed, fmr_4 = FMR_4Bed, AreaName = MSAName, countyname = CountyName, State = State_Alpha) %>%
	group_by(fips2000) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2005) %>%
	ungroup()

HUDFMR_2006 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2006_County_Town.xls") %>%
	select(fips2000 = fips, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5)) %>%
	group_by(fips2000) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2006) %>%
	ungroup()


HUDFMR_2007 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2007F_County_Town.xls") %>%
	select(fips2000 = fips, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5)) %>%
	group_by(fips2000) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2007) %>%
	ungroup()

HUDFMR_2008 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/HUDFMR_2008.xls") %>%
	select(fips2000 = fips, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = Areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5)) %>%
	group_by(fips2000) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2008) %>%
	ungroup()

HUDFMR_2009 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2009_4050_Rev_Final.xls") %>%
	select(fips2000 = FIPS, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = Areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5)) %>%
	group_by(fips2000) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2009) %>%
	ungroup()

HUDFMR_2010 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2010_4050_Final_PostRDDs.xls") %>%
	select(fips2000 = FIPS, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = Areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5)) %>%
	group_by(fips2000) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2010) %>%
	ungroup()


HUDFMR_2011 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2011_4050_Final.xls") %>%
	select(fips2000 = FIPS, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = Areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5)) %>%
	group_by(fips2000) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2011) %>%
	ungroup()

HUDFMR_2012 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2012_4050_Final.xls") %>%
	select(fips2000 = FIPS, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = Areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5)) %>%
	group_by(fips2000) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2012) %>%
	ungroup()

HUDFMR_2013 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2013_4050_Final.xls") %>%
	select(fips2010, fips2000, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = Areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5),
		   fips2010 = substr(fips2010, 1, 5)) %>%
	group_by(fips2010) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2013) %>%
	ungroup()

HUDFMR_2014 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2014_4050_RevFinal.xls") %>%
	select(fips2010, fips2000, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = Areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5),
		   fips2010 = substr(fips2010, 1, 5)) %>%
	group_by(fips2010) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2014) %>%
	ungroup()

HUDFMR_2015 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2015_4050_RevFinal.xls") %>%
	select(fips2010, fips2000, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = Areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5),
		   fips2010 = substr(fips2010, 1, 5)) %>%
	group_by(fips2010) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2015) %>%
	ungroup()

HUDFMR_2016 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2016F-4050-RevFinal4.xlsx") %>%
	select(fips2010, fips2000, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5),
		   fips2010 = substr(fips2010, 1, 5)) %>%
	group_by(fips2010) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2016) %>%
	ungroup()

HUDFMR_2017 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2017-4050-County-Level_Data.xlsx") %>%
	select(fips2010, fips2000, fmr_0 = fmr0, fmr_1 = fmr1, fmr_2 = fmr2, fmr_3 = fmr3, fmr_4 = fmr4, AreaName = areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2000 = substr(fips2000, 1, 5),
		   fips2010 = substr(fips2010, 1, 5)) %>%
	group_by(fips2010) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2017) %>%
	ungroup()

HUDFMR_2018 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY18_4050_FMRs_rev.xlsx") %>%
	select(fips2010:fmr_4, AreaName = areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2010 = substr(fips2010, 1, 5)) %>%
	group_by(fips2010) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2018) %>%
	ungroup()

HUDFMR_2019 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY2019_4050_FMRs_rev2.xlsx") %>%
	select(fips2010:fmr_4, AreaName = areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2010 = substr(fips2010, 1, 5)) %>%
	group_by(fips2010) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2019) %>%
	ungroup()

HUDFMR_2020 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY20_4050_FMRs_rev.xlsx") %>%
	select(fips2010:fmr_4, AreaName = areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2010 = substr(fips2010, 1, 5)) %>%
	group_by(fips2010) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2020) %>%
	ungroup()

HUDFMR_2021 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY21_4050_FMRs_rev.xlsx") %>%
	select(fips2010:fmr_4, AreaName = areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2010 = substr(fips2010, 1, 5)) %>%
	group_by(fips2010) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2021) %>%
	ungroup()

HUDFMR_2022 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY22_FMRs_revised.xlsx") %>%
	select(fips2010:fmr_4, AreaName = areaname, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2010 = substr(fips2010, 1, 5), metro = as.numeric(metro)) %>%
	group_by(fips2010) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2022) %>%
	ungroup()

HUDFMR_2023 <-
	readxl::read_excel("~/git/timathomas/functions/data/hudfmr/FY23_FMRs.xlsx") %>%
	select(fips2010 = fips, fmr_0:fmr_4, AreaName = hud_area_name, countyname, county_town_name, State = state_alpha, metro) %>%
	mutate(fips2010 = substr(fips2010, 1, 5), metro = as.numeric(metro)) %>%
	group_by(fips2010) %>%
	mutate(fmr_avg = mean(c(fmr_0, fmr_1, fmr_2, fmr_3, fmr_4), na.rm = TRUE),
		   year = 2023) %>%
	ungroup()

hudfmr <- bind_rows(HUDFMR_2003, HUDFMR_2004, HUDFMR_2005, HUDFMR_2006, HUDFMR_2007, HUDFMR_2008, HUDFMR_2009, HUDFMR_2010, HUDFMR_2011, HUDFMR_2012, HUDFMR_2013, HUDFMR_2014, HUDFMR_2015, HUDFMR_2016, HUDFMR_2017, HUDFMR_2018, HUDFMR_2019,HUDFMR_2020,HUDFMR_2021,HUDFMR_2022, HUDFMR_2023) %>%
	left_join(., cpi) %>% 
	mutate_at(vars(fmr_0:fmr_4, fmr_avg), funs(cpi = .*CPI)) %>%
	mutate(countyname = gsub(' County.*', '', countyname),
		   rb30_income = (fmr_avg_cpi*12)/.3)

wa_hudfmr <-
	hudfmr %>%
	filter(State == state_abb) %>%
	group_by(year, countyname) %>%
	summarise(fmr_avg_cpi = fmr_avg_cpi,
			  rb30_income = (fmr_avg_cpi*12)/.3,
			  State = state_abb) %>%
	mutate(county =
		case_when(
			countyname %in% c("King", "Snohomish", "Pierce", "Spokane", "Clark", "Skamania", "Kitsap") ~ countyname, TRUE ~ "Washington"
			)) %>%
	group_by(year, county) %>%
	summarise(fmr_avg_cpi = median(fmr_avg_cpi))

hudfmr2 <-
	bind_rows(hudfmr, wa_hudfmr)

# filenames <- list.files("/data/hud_fmr/HUDFMR", pattern="*.xls", full.names=TRUE)
# ldf <- lapply(filenames, readxl::read_excel)
# names(ldf) <- substr(filenames, 22, 32)

# ==========================================================================
# Median Gross Rent by Year Built
# ==========================================================================

# mgryb_acsvars

# ==========================================================================
# Housing stock
# ==========================================================================

# Tenure
# ==========================================================================

us <- unique(fips_codes$state)[1:51]
us2 <- rep(us, 6)
acs1year2 <- sort(rep(acs1year, 51))

#
# County
# --------------------------------------------------------------------------
ten_acs1_st <- map2_dfr(us2, acs1year2,
		function(state = us2,year = acs1year2){
			get_acs(geography = "state",
					variables = tenure_varsACS,
					state = state,
					year = year,
					survey = "acs1") %>%
			mutate(year = year)
		}) %>%
	select(-moe) %>%
	spread(variable, estimate)


ten_acs2009_st <-
	map_df(us, function(x){
		get_acs(geography = "state",
				variables = tenure_varsACS,
				state = x,
				year = 2009) %>%
		mutate(year = 2007)
	}) %>%
	select(-moe) %>%
	spread(variable, estimate)

ten_1970_co <-
	fread("/data/census/Tenure1970_2010_county/nhgis0164_ds94_1970_county.csv") %>%
	rename(GEOID = GISJOIN, own = CB4001, rent = CB4002) %>%
	mutate(year = 1970)	%>%
	group_by(GEOID) %>%
	mutate(total = sum(own, rent)) %>%
	select(GEOID, year, STATE, COUNTY, own, rent, total) %>%
	ungroup()
ten_1980_co <-
	fread("/data/census/Tenure1970_2010_county/nhgis0164_ds104_1980_county.csv") %>%
	rename(GEOID = GISJOIN, own = C7W001, rent = C7W002) %>%
	mutate(year = 1980) %>%
	group_by(GEOID) %>%
	mutate(total = sum(own, rent)) %>%
	select(GEOID, year, STATE, COUNTY, own, rent, total) %>%
	ungroup()
ten_1990_co <-
	fread("/data/census/Tenure1970_2010_county/nhgis0164_ds120_1990_county.csv") %>%
	rename(GEOID = GISJOIN, own = ES1001, rent = ES1002) %>%
	mutate(year = 1990) %>%
	group_by(GEOID) %>%
	mutate(total = sum(own, rent)) %>%
	select(GEOID, year, STATE, COUNTY, own, rent, total) %>%
	ungroup()
ten_2000_co <-
	fread("/data/census/Tenure1970_2010_county/nhgis0164_ds146_2000_county.csv") %>%
	rename(GEOID = GISJOIN, own = FKN001, rent = FKN002) %>%
	mutate(year = 2000) %>%
	group_by(GEOID) %>%
	mutate(total = sum(own, rent)) %>%
	select(GEOID, year, STATE, COUNTY, own, rent, total) %>%
	ungroup()
ten_2010_co <-
	fread("/data/census/Tenure1970_2010_county/nhgis0164_ds172_2010_county.csv") %>%
	rename(GEOID = GISJOIN, total = IFF001, rent = IFF004) %>%
	group_by(GEOID) %>%
	mutate(own = sum(IFF002, IFF003, na.rm = TRUE),
		   year = 2010,
		   total = sum(own, rent),
		   COUNTY = gsub(" County", "", COUNTY)) %>%
	select(GEOID, year, STATE, COUNTY, own, rent, total) %>%
	ungroup()

ten_state <-
	bind_rows(ten_1970_co, ten_1980_co, ten_1990_co, ten_2000_co, ten_2010_co) %>%
	group_by(NAME = STATE, year) %>%
	summarise(own = sum(own),
			  rent = sum(rent),
			  total = sum(total)) %>%
	bind_rows(.,ten_acs1_st,ten_acs2009_st)  %>%
	select(-GEOID) %>%
	arrange(NAME, year)


#
# Place
# --------------------------------------------------------------------------
sea_acs1_pl <-
	map_df(acs1year,
		function(x){
		acs(geography = "place",
			variables = tenure_varsACS,
			survey = "acs1",
			state = state_abb,
			year = x) %>%
		mutate(year = x)
	}) %>%
	spread(variable, estimate) %>%
	separate(NAME, c("PLACE", "STATE"), sep = ", ")

ten_acs2009_pl <-
	acs(geography = "place",
		variables = tenure_varsACS,
		survey = "acs5",
		state = state_abb,
		year = 2009) %>%
	mutate(year = 2007) %>%
	spread(variable, estimate) %>%
	separate(NAME, c("PLACE", "STATE"), sep = ", ")


ten_1970_pl <-
	fread("/data/census/Tenure1970_2010_place/nhgis0165_ds94_1970_place.csv") %>%
	rename(GEOID = GISJOIN, own = CB4001, rent = CB4002) %>%
	group_by(GEOID) %>%
	mutate(year = 1970,
		   total = sum(own, rent)) %>%
	select(GEOID, year, STATE, PLACE, own, rent, total)
ten_1980_pl <-
	fread("/data/census/Tenure1970_2010_place/nhgis0165_ds104_1980_place.csv") %>%
	rename(GEOID = GISJOIN, own = C7W001, rent = C7W002) %>%
	group_by(GEOID) %>%
	mutate(year = 1980,
		   total = sum(own, rent)) %>%
	select(GEOID, year, STATE, PLACE, own, rent, total)
ten_1990_pl <-
	fread("/data/census/Tenure1970_2010_place/nhgis0165_ds120_1990_place.csv") %>%
	rename(GEOID = GISJOIN, own = ES1001, rent = ES1002) %>%
	group_by(GEOID) %>%
	mutate(year = 1990,
		   total = sum(own, rent)) %>%
	select(GEOID, year, STATE, PLACE, own, rent, total)
ten_2000_pl <-
	fread("/data/census/Tenure1970_2010_place/nhgis0165_ds146_2000_place.csv") %>%
	rename(GEOID = GISJOIN, own = FKN001, rent = FKN002) %>%
	group_by(GEOID) %>%
	mutate(year = 2000,
		   total = sum(own, rent)) %>%
	select(GEOID, year, STATE, PLACE, own, rent, total)
ten_2010_pl <-
	fread("/data/census/Tenure1970_2010_place/nhgis0165_ds172_2010_place.csv") %>%
	rename(GEOID = GISJOIN, total = IFF001, rent = IFF004) %>%
	group_by(GEOID) %>%
	mutate(own = sum(IFF002, IFF003, na.rm = TRUE),
		   year = 2010) %>%
	ungroup() %>%
	select(GEOID, year, STATE, PLACE, own, rent, total)

ten_place <-
	bind_rows(sea_acs1_pl, ten_acs2009_pl, ten_1970_pl, ten_1980_pl, ten_1990_pl, ten_2000_pl, ten_2010_pl) %>%
	filter(grepl("Seattle", PLACE), PLACE != "Seattle Hill-Silver Firs CDP") %>%
	arrange(year)



# ==========================================================================
# Save Data
# ==========================================================================

time <- paste0(format(Sys.time(), "%Y-%m-%d"),".csv.bz2")
results.path <- "~/git/timathomas/functions/data/"
	saveRDS(rb,"~/git/timathomas/functions/data/rent_burden.rds"))
	saveRDS(runits,"~/git/timathomas/functions/data/rental_units_cost.rds")
	saveRDS(ten_place,"~/git/timathomas/functions/data/tenure_place.rds")
	saveRDS(ten_county,"~/git/timathomas/functions/data/tenure_county.rds")
	saveRDS(runits,"~/git/timathomas/functions/data/rental_units_cost.rds")
	saveRDS(hudfmr2, "~/git/timathomas/functions/data/hudfmr.rds")
	saveRDS(wa_hudfmr, "~/git/timathomas/functions/data/wa_hudfmr.rds")


