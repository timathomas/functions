# ==========================================================================
# Rent Burden
# ==========================================================================
library(data.table)
library(tidyverse)

rb <- fread("/data/results/rent_burden.csv.bz2")
options(scipen=10) # avoid scientific notation
#
# State
# --------------------------------------------------------------------------
wa_rb <-
rb %>%
	filter(survey %in% c("sf3", "acs5")) %>%
	mutate(rb30 = rb_34.9+rb_39.9+rb_49.9,
		   rb50 = rb_55,
		   rb0 = rb_tot-rb30-rb50,
		   NAME = gsub(' County.*', '', NAME),
		   year = if_else(year == 2009, 2007, as.numeric(year))) %>%
	select(GEOID, NAME, year, rb30:rb0) %>%
	gather(rb, count, rb30:rb0) %>%
	group_by(year, rb) %>%
	summarise(count = sum(count)) %>%
	group_by(year) %>%
	mutate(prop = count/sum(count))

plot_wa_rb <- function(df,
					   y = "count",
					   labels = scales::comma,
					   y_axis = "Households"){
ggplot(df) +
	geom_area(aes_string(x = "year", y = y, fill = "rb")) +
	scale_y_continuous(labels = labels) +
	theme_minimal() +
	labs(title = "Washington Rent Burden",
		subtitle = "Percentage of income going to rent (rent burden = 30% or more)",
		 x = "Year",
		 y = y_axis,
		 caption = "Data source: US Census American Community Survey") +
	scale_fill_brewer(palette = "Set1",
					  direction = -1,
					  name = "Percent of Income\nGoing to Rent",
					  breaks=c("rb0",
					   			"rb30",
								"rb50"),
					  labels = c("Less than 30%",
					   			  "30% to 50%",
								  "Over 50%"))
}
# plot_wa_rb(wa_rb)
# plot_wa_rb(wa_rb, y = "prop", labels = scales::percent, y_axis = "")

#
# County
# --------------------------------------------------------------------------

co_rb <-
rb %>%
	ungroup() %>%
	mutate(rb30 = rb_34.9+rb_39.9+rb_49.9,
		   rb50 = rb_55,
		   rb0 = rb_tot-rb30-rb50,
		   NAME = gsub(' County.*', '', NAME),
		   year = if_else(year == 2009, 2007, as.numeric(year))) %>%
	unite(survey, survey, year, remove = FALSE) %>%
	select(GEOID:survey, rb30:rb0) %>%
	gather(rb, count, rb30:rb0) %>%
	spread(survey, count) %>%
	select(-acs5_2017) %>%
	group_by(NAME, rb) %>%
	summarise(`2000` = sum(sf3_2000),
			  `2007` = sum(acs5_2007),
			  `2012` = sum(acs1_2012),
			  `2013` = sum(acs1_2013),
			  `2014` = sum(acs1_2014),
			  `2015` = sum(acs1_2015),
			  `2016` = sum(acs1_2016),
			  `2017` = sum(acs1_2017)) %>%
	na.omit() %>%
	gather(year, count, `2000`:`2017`) %>%
	mutate(year = as.numeric(year)) %>%
	group_by(NAME, year) %>%
	mutate(prop = count/sum(count))

plot_co_rb <- function(county,
					   y = "count",
					   labels = scales::comma,
					   y_axis = "Households"){
ggplot(co_rb %>% filter(NAME %in% county)) +
	geom_area(aes_string(x = "year", y = y, fill = "rb")) +
	scale_y_continuous(labels = labels) +
	theme_minimal() +
	labs(title = paste0(county, " County Rent Burden"),
		 x = "Year",
		 y = y_axis,
		 caption = "Data source: US Census American Community Survey") +
	scale_fill_brewer(palette = "Set1",
					  direction = -1,
					  name = "Percent of Income\nGoing to Rent",
					  breaks=c("rb0",
					   			"rb30",
								"rb50"),
					  labels = c("Less than 30%",
					   			  "30% to 50%",
								  "Over 50%"))
}
# plot_co_rb(county = "King")
# plot_co_rb(county = "King", y = "prop", labels = scales::percent, y_axis = "")

#
# Number of households that are rent burdened
# --------------------------------------------------------------------------

hhinc <- fread("/data/census/household_income.csv.bz2")
hudfmr <- fread("/data/hud_fmr/hudfmr.csv.bz2")

rbhh_df <-
	left_join(hhinc %>% filter(type == "HHInc"),
	          hudfmr %>% mutate(GEOID = as.numeric(if_else(is.na(fips2010),
	                                                       fips2000,
	                                                       fips2010))) %>%
	                  select(GEOID, NAME = countyname, year, rb30_income)) %>%
	unite(survey, survey, year, remove = FALSE) %>%
	filter(survey != "acs5_2017") %>%
	mutate(rb30_count = ifelse(income_cpi <= rb30_income &
	                            lead(income_cpi) >= rb30_income,
	                     round((1-((income_cpi - rb30_income)/(lead(income_cpi) - income_cpi))*estimate), 0),
	                 ifelse(rb30_income >= income_cpi, estimate,
	                     0))) %>%
	filter(year != 2000) %>%
	select(NAME, rb30_count, estimate, year) %>%
	distinct() %>%
	group_by(NAME, year) %>%
	mutate(hh_total = sum(estimate), rb30_total = sum(rb30_count),
	       rb30_prop = round(rb30_total/hh_total*100, 1),
	       nonrb30_prop = round(100-rb30_prop, 1)) %>%
	select(NAME, year, hh_total, rb30_total, rb30_prop, nonrb30_prop) %>%
	distinct() %>%
	ungroup() %>%
	arrange(NAME, year)

wa_rbhh_df <-
	left_join(hhinc %>% filter(type == "HHInc"),
	          hudfmr %>% mutate(GEOID = as.numeric(if_else(is.na(fips2010),
	                                                       fips2000,
	                                                       fips2010))) %>%
	                  select(GEOID, NAME = countyname, year, rb30_income)) %>%
	filter(survey != "acs1") %>%
	mutate(rb30_count = ifelse(income_cpi <= rb30_income &
	                            lead(income_cpi) >= rb30_income,
	                     round((1-((income_cpi - rb30_income)/(lead(income_cpi) - income_cpi))*estimate), 0),
	                 ifelse(rb30_income >= income_cpi, estimate,
	                     0))) %>%
	filter(year != 2000) %>%
	select(NAME, rb30_count, estimate, year) %>%
	distinct() %>%
	group_by(year) %>%
	summarise(hh_total = sum(estimate),
		   	  rb30_total = sum(rb30_count)) %>%
	mutate(NAME = "Washington",
		   rb30_prop = round(rb30_total/hh_total*100,1),
		   nonrb30_prop = round(100-rb30_prop, 1))

co_rbhh_df <-
	bind_rows(rbhh_df, wa_rbhh_df)

rbhh <- function(county){
	ggplot(data = co_rbhh_df %>% filter(NAME == county)) +
	  theme_minimal() +
	  geom_line(aes(x = year, y = rb30_total)) +
	  scale_y_continuous(labels = scales::comma) +
	  scale_x_continuous(breaks=c(2007:2017)) +
	  theme(axis.text.x = element_text(angle = -45, hjust = 0),
	      panel.grid.minor.x = element_blank()) +
	  annotate("text",
	  		   x = 2017,
	  		   y = co_rbhh_df %>%
	  		   	   filter(NAME == county,
	  		   	   year == 2017) %>%
	  		   	   pull(rb30_total) * .98,
	  		   label = paste0(co_rbhh_df %>%
	  		   	   			  filter(NAME == county,
	  		   	   			  year == 2017) %>%
	  		   	   			  pull(rb30_prop), "%")) +
	  annotate("text",
	  		   x = 2007,
	  		   y = co_rbhh_df %>%
	  		   	   filter(NAME == county,
	  		   	   year == 2007) %>%
	  		   	   pull(rb30_total) * .98,
	  		   label = paste0(co_rbhh_df %>%
	  		   	   			  filter(NAME == county,
	  		   	   			  year == 2007) %>%
	  		   	   			  pull(rb30_prop), "%")) +
	  labs(title = paste0("The number of households facing rent burden in ",county),
	       subtitle = "rent burden = 30% of household income going to rent",
	       y = "Households",
	       x = "Year",
	       caption = "Data source: HUD Fair Market Rent data, the Bureau of Labor Statistics\nConsumer Price Index, & US Census American Community Survey")
}

# rbhh("Washington")
# rbhh("King")
# rbhh("Spokane")

#
# Income needed to avoid rent burden
# --------------------------------------------------------------------------

rbincome <- function(df, county, ylim = NULL){
	df %>%
	  mutate(rb30_income = (fmr_avg_cpi*12)/.3) %>%
	  filter(State == "WA", countyname == county) %>%
	ggplot() +
	  theme_minimal() +
	  geom_smooth(aes(x = year, y = rb30_income), se = FALSE, color = "cornflowerblue") +
	  labs(title = paste0("How much you need to make to avoid rent burden in ", county),
	       subtitle = "rent burden = 30% of household income going to rent",
	       y = "Income (2017 dollars)",
	       x = "Year",
	       caption = "Data source: HUD Fair Market Rent data, the Bureau of Labor Statistics\nConsumer Price Index, & US Census American Community Survey") +
	  coord_cartesian(ylim = ylim) +
	  scale_y_continuous(labels = scales::dollar) +
	  scale_x_continuous(breaks=c(2003:2019)) +
	  theme(axis.text.x = element_text(angle = -45, hjust = 0))
}

# rbincome(wa_hudfmr, "Washington")
# rbincome(hudfmr, "King", ylim = c(45000, 90000))
# rbincome(hudfmr, "Pierce")

#
# FMR
# --------------------------------------------------------------------------

fmryearly <- function(df, county, ylim = NULL){
	df %>%
	  filter(State == "WA", countyname == county) %>%
	ggplot() +
	  theme_minimal() +
	  geom_smooth(aes(x = year, y = fmr_avg_cpi), se = FALSE) +
	  labs(title = paste0("Average fair market rent for all bedroom types in ", county),
	       y = "Rent (2017 dollars)",
	       x = "Year",
	       caption = "Data source: HUD Fair Market Rent data & the Bureau of Labor Statistics Consumer Price Index") +
	  coord_cartesian(ylim = ylim) +
	  scale_y_continuous(labels = scales::dollar) +
	  scale_x_continuous(breaks=c(2003:2019)) +
	  theme(axis.text.x = element_text(angle = -45, hjust = 0),
	      panel.grid.minor.x = element_blank())
}

# fmryearly(wa_hudfmr, "Washington")
# fmryearly(hudfmr, "King", ylim = c(1200, 2250))
# ==========================================================================
# Plot runits
# ==========================================================================

runits <- fread("/data/census/rental_units_cost.csv.bz2")


plot_co_runits <- function(county){
runits %>%
	mutate(Rent = factor(Rent, levels = c("$2,000 or more",
										  "$1,500 to $1,999",
										  "$1,000 to $1,499",
										  "$800 to $999",
										  "$500 to $799",
										  "Less than $500"))) %>%
	group_by(NAME, year, Rent) %>%
	summarise(count = sum(count)) %>%
	mutate(prop = count/sum(count)) %>%
	filter(NAME == county,
		   !year %in% c(2013, 2014, 2015, 2016)) %>%
	ungroup() %>%
	select(NAME, year, count, prop, Rent) %>%
ggplot() +
	geom_area(aes(x = year, y = count, fill = Rent)) +
	theme_minimal() +
	scale_y_continuous(labels = scales::comma) +
	labs(title = paste0(county, " County Rental Units by Gross Rent"),
		 x = "Year",
		 y = "Rental Units",
		 caption = "US Census American Community Survey & the Bureau of Labor Statistics Consumer Price Index")
}
# ggpubr::ggarrange(
# plot_co_runits("King"),
# plot_co_runits("Pierce"),
# # ncol=2,
# nrow = 2,
# common.legend = TRUE,
# legend="right")

# ==========================================================================
# Tenure over time
# ==========================================================================

ten_co_fun <- function(county, state){
	ten_state %>%
		filter(NAME %in% c("Tennessee", "Minnesota")) %>%
		# filter(year != 2015) %>%
	ggplot() +
		geom_smooth(aes(x = year, y = rent, color = NAME), se = FALSE) +
		geom_point(aes(x = year, y = rent, color = NAME), alpha = .5) +
		geom_smooth(aes(x = year, y = own, color = NAME), se = FALSE) +
		geom_point(aes(x = year, y = own, color = NAME), alpha = .5) +
		theme_minimal() +
		scale_y_continuous(labels = scales::comma) +
		labs(title = paste0("Count of Occupied Owned and Rented Units in\nMinnesota & Tennessee"),
		 	 x = "Year",
		 	 y = "Housing Units",
		 	 caption = "US Census Decennial & American Community Survey") +
		geom_text(x = 2010, y = 1600000, label = "Own") +
		geom_text(x = 2010, y = 690000, label = "Rent")
}


# ten_place %>%
# 	mutate(PLACE = gsub(" city", "", PLACE)) %>%
# 	select(-total) %>%
# 	gather(Tenure, count, own:rent) %>%
# 	mutate(Tenure = factor(Tenure)) %>%
# 	ggplot() +
# 		geom_smooth(aes(x = year, y = count, color = Tenure), se = FALSE) +
# 		geom_point(aes(x = year, y = count, color = Tenure), alpha = .5) +
# 		theme_minimal() +
# 		scale_y_continuous(labels = scales::comma) +
# 		labs(title = paste0("Count of Occupied Owned and Rented Housing Units in Seattle"),
# 		 	 x = "Year",
# 		 	 y = "Rental Units",
# 		 	 caption = "US Census Decennial & American Community Survey")


# ten_county %>%
# 	filter(NAME == "New Jersey") %>%
# 	select(-total) %>%
# 	gather(Tenure, count, own:rent) %>%
# 	mutate(Tenure = factor(Tenure)) %>%
# 	ggplot() +
# 		geom_smooth(aes(x = year, y = count, color = Tenure), se = FALSE) +
# 		geom_point(aes(x = year, y = count, color = Tenure), alpha = .5) +
# 		theme_minimal() +
# 		scale_y_continuous(labels = scales::comma) +
# 		labs(title = paste0("Count of Occupied Owned and Rented Units in New Jersey State"),
# 		 	 x = "Year",
# 		 	 y = "Rental Units",
# 		 	 caption = "US Census Decennial & American Community Survey")


# ==========================================================================
# Plot HUD fmr
# ==========================================================================


# glimpse(hudfmr)

# ggplot() +
# 	geom_line(data = hudfmr %>%	filter(State == "WA"),
# 			  aes(x = year, y = fmr_avg_cpi, color = countyname)) +
# 	geom_line(data = z_change %>%
# 					 select(GEOID:rent2015) %>%
# 					 gather(year, rent, rent2010:rent2015) %>%
# 					 mutate(year = as.numeric(substr(year, 5,8))),
# 			  aes(x = year, y = rent, group = GEOID)) +
# 	geom_line(data = c_rent, aes(x = year, y = mgrent, group = GEOID), color = "red")

# ==========================================================================
# Seattle loss in rental units
# ==========================================================================

library(data.table)
library(tidyverse)
library(tidycensus)
library(tigris)
	source("/data/Functions/Variables.R")

	acs5yr <- c(2017,2009)
	acs1year <- rep(2012:2017, 1)
	acs <-
		function(
			geography = "county",
			variables,
			state = "WA",
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

# runits5yr2017 <-
# 	get_acs(geography = "place",
# 			state = "WA",
# 			variables = runits_acs2015_vars,
# 			cache_table = TRUE,
# 			year = 2017,
# 			survey = "acs5",
# 			geometry = FALSE)

# 	runits5yr2017 <-
# 	get_acs(geography = "place",
# 			state = "WA",
# 			variables = runits_acs2015_vars,
# 			cache_table = TRUE,
# 			year = 2017,
# 			survey = "acs5",
# 			geometry = FALSE)

runits5yr2009 <-
	get_acs(geography = "place",
			state = "WA",
			variables = runits_acs2014_vars,
			cache_table = TRUE,
			year = 2009,
			survey = "acs5",
			geometry = FALSE)

runits1yr2015_2017 <- # added categories in 2015
	map_df(c(2015,2016,2017), function(x){
			acs(geography = "place", 
				variables = runits_acs2015_vars, 
				survey = "acs1", 
				year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs1")
		})

runits1yr2012_2014 <-
	map_df(c(2012,2013,2014), function(x){
			acs(geography = "place", 
				variables = runits_acs2014_vars, 
				survey = "acs1", 
				year = x) %>%
			replace(is.na(.), 0) %>%
			mutate(year = x,
				   survey = "acs1")
		})

runitsdec2000 <-
		get_decennial(geography = "place",
					  variables = runits_dec2000_vars,
		  			  cache_table = TRUE,
		  			  year = 2000,
		  			  sumfile = "sf3",
		  			  state = "WA",
		  			  output = "tidy") %>%
		mutate(year = 2000,
			   survey = "sf3") %>%
		rename(estimate = value)

#
# Affordable housing units
# --------------------------------------------------------------------------
medinc <- fread("/data/census/medinc.csv.bz2")

runits1yr_adj2017 <-
	runits1yr2015_2017 %>%
	filter(variable %in% c("tot", "cash", "nocash")) %>%
	spread(variable, estimate) %>%
	left_join(., runits1yr2015_2017 %>%
				 filter(!variable %in% c("tot", "cash", "nocash"))) %>%
	left_join(., cpi) %>%
	mutate(GEOID = as.numeric(GEOID),
		   NAME = gsub(' County.*', '', NAME),
		   variable = as.numeric(variable),
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
	filter(variable %in% c("tot", "cash", "nocash")) %>%
	spread(variable, estimate) %>%
	left_join(., runits1yr2012_2014 %>%
				 filter(!variable %in% c("tot", "cash", "nocash"))) %>%
	left_join(., cpi) %>%
	mutate(GEOID = as.numeric(GEOID),
		   NAME = gsub(' County.*', '', NAME),
		   variable = as.numeric(variable),
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
	filter(variable %in% c("tot", "cash", "nocash")) %>%
	spread(variable, estimate) %>%
	left_join(., runits5yr2009 %>%
				 filter(!variable %in% c("tot", "cash", "nocash")) %>%
				 select(-moe)) %>%
	mutate(year = 2009) %>%
	left_join(., cpi) %>%
	mutate(GEOID = as.numeric(GEOID),
		   NAME = gsub(' County.*', '', NAME),
		   variable = as.numeric(variable),
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
	filter(variable %in% c("tot", "cash", "nocash")) %>%
	spread(variable, estimate) %>%
	left_join(., runitsdec2000 %>%
				 select(-survey) %>%
				 filter(!variable %in% c("tot", "cash", "nocash"))) %>%
	left_join(., cpi) %>%
	mutate(GEOID = as.numeric(GEOID),
		   NAME = gsub(' County.*', '', NAME),
		   variable = as.numeric(variable),
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
	bind_rows(runits1yr_adj2017, runits1yr_adj2014, runits5yr_adj2009, runitsdec_2000adj) %>%
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

total units <-
	runits %>%
	filter(grepl("Seattle city", NAME),
	       !year %in% c(2015,2016) ) %>%
	  group_by(NAME, year) %>%
	  summarise(count = sum(count))

100 <- 	    	
	runits %>%
	filter(grepl("Seattle city", NAME),
	       !year %in% c(2015,2016),
	       Rent %in% c(#"$2,000 or more",
	                   # "$1,500 to $1,999",
	                   # "$1,000 to $1,499",
	                   "$800 to $999",
	                   "$500 to $799",
	                   "Less than $500"
	                   )) %>%
	  group_by(NAME, year) %>%
	  summarise(count = sum(count)) %>% 
	  ungroup() %>% 
	  mutate(NAME = "Seattle")


    ggplot(data = plot_data,
          aes(x = year, y = count)) +
	    theme_minimal() +
	    scale_x_continuous(breaks=c(2000:2018)) +
	    scale_y_continuous(labels = scales::comma) +
	    geom_line(size = 1.5, color = "black") +
	    geom_point(size = 2.5, alpha = .5) +
	    coord_cartesian(xlim = c(2000:2018)) +
	    labs(title = "Seattle trends in affordable housing,\nhomelessness, and evictions",
	         subtitle = "Affordable rental units at $1000 or less (2017 dollars)",x = "", y = "Affordable Rental Units") +
	    theme(axis.text.x = element_text(angle = -45, hjust = 0))




