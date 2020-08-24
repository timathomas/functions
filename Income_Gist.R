# ==========================================================================
# Explore data
# ==========================================================================

library(data.table)
library(scales)
library(tidyverse)

hhinc <- fread("/data/census/household_income.csv.bz2")
hhincten <- fread("/data/census/hhincten.csv.bz2")
medinc <- fread("/data/census/medinc.csv.bz2")
wa_medinc <- fread("/data/census/wa_medinc.csv.bz2")

#
# Number of people by income category
# --------------------------------------------------------------------------

plot_inc_count <- function(category = "HHInc",
						   county,
						   title = paste(county, "HH Counts"),
						   ylim = c(0,880000)){
hhinc %>%
	unite(survey, survey, year, remove = FALSE) %>%
	filter(survey != "acs5_2017",
		   type == category,
		   NAME %in% county) %>%
	select(type, income_cpi, income, estimate, year) %>%
	mutate(inc_cat = ifelse(income_cpi < 30000, "$30k",
					ifelse(income_cpi >= 30000 &
						   income_cpi < 60000, "$60k",
					ifelse(income_cpi >= 60000 &
						   income_cpi < 100000, "$100k",
					ifelse(income_cpi >= 100000 &
						   income_cpi < 250000, "$250k",
						   "+ $250k")))),
		   inc_cat = factor(inc_cat,
		   					levels = c("+ $250k",
		   							   "$250k",
		   							   "$100k",
		   							   "$60k",
		   							   "$30k"))) %>%
	group_by(inc_cat, year) %>%
	summarise(estimate = sum(estimate)) %>%
	group_by(year) %>%
	mutate(prop = estimate/sum(estimate)) %>%
ggplot() +
	geom_line(aes(x = year, y = estimate, color = inc_cat),) +
	theme_minimal() +
	labs(title = title,
		 x = "Year",
		 y = "Count") +
	coord_cartesian(ylim = ylim) +
	scale_y_continuous(labels = scales::comma) +
	scale_fill_brewer(palette = "Spectral",
					   name = "Household Income",
					   breaks=c("+ $250k",
					   			"$250k",
								"$100k",
								"$60k",
								"$30k"),
					   labels = c("$250k or more",
					   			  "$100k or $250k",
								  "$60k to $100k",
								  "$30k to $60k",
								  "less than $30k"))
}

# plot_inc_count(county = "King", ylim = NULL)

# ggsave(income_cat,
# 	   file = "~/presentations/temp/income2_cat.pdf",
# 	   width = 8,
# 	   height = 5)
# browseURL('~/presentations/temp/income2_cat.pdf')
#
# Proportion in category
# --------------------------------------------------------------------------
plot_inc_prop <- function(category = "HHInc", county, title = paste(county, "HH Percentages")){
hhinc %>%
	unite(survey, survey, year, remove = FALSE) %>%
	filter(survey != "acs5_2017",
		   type == category,
		   NAME %in% county) %>%
	select(type, income_cpi, income, estimate, year) %>%
	mutate(inc_cat = ifelse(income_cpi < 30000, "$30k",
					ifelse(income_cpi >= 30000 &
						   income_cpi < 60000, "$60k",
					ifelse(income_cpi >= 60000 &
						   income_cpi < 100000, "$100k",
					ifelse(income_cpi >= 100000 &
						   income_cpi < 250000, "$250k",
						   "+ $250k")))),
		   inc_cat = factor(inc_cat,
		   					levels = c("+ $250k",
		   							   "$250k",
		   							   "$100k",
		   							   "$60k",
		   							   "$30k"))) %>%
	group_by(inc_cat, year) %>%
	summarise(estimate = sum(estimate)) %>%
	group_by(year) %>%
	mutate(prop = estimate/sum(estimate)) %>%
ggplot() +
	geom_area(aes(x = year, y = prop, fill = inc_cat),) +
	theme_minimal() +
	labs(title = title,
		 x = "Year",
		 y = "") +
	scale_y_continuous(labels = percent) +
	scale_fill_brewer(palette = "Spectral",
					   name = "Household Income",
					   breaks=c("+ $250k",
					   			"$250k",
								"$100k",
								"$60k",
								"$30k"),
					   labels = c("$250k or more",
					   			  "$100k or $250k",
								  "$60k to $100k",
								  "$30k to $60k",
								  "less than $30k"))
}

# plot_inc_prop(county = "King")

# ==========================================================================
# Multi graph plot
# ==========================================================================
# ggpubr::ggarrange(
# plot_inc_count(county = "King"),
# plot_inc_prop(county = "King"),
# plot_inc_count(county = "Snohomish"),
# plot_inc_prop(county = "Snohomish"),
# plot_inc_count(county = "Pierce"),
# plot_inc_prop(county = "Pierce"),
# ncol=2,
# nrow = 3,
# common.legend = TRUE,
# legend="right")

# ==========================================================================
# Income breakdowns by race
# ==========================================================================

#
# Washington income by race
# --------------------------------------------------------------------------

plot_wa_medinc <- function(){
ggplot() +
	geom_line(data = wa_medinc %>%
					   select(year, AMI80_cpi:AMI30_cpi) %>%
					   gather(ami, inc, AMI80_cpi:AMI30_cpi) %>%
					   mutate(ami = factor(ami,
					   				levels = c("AMI80_cpi",
					   						   "AMI50_cpi",
					   						   "AMI30_cpi"),
					   				labels = c("80% AMI",
					   						   "50% AMI",
					   						   "30% AMI"))),
			  aes(x = year, y = inc, group = ami), size = 4, alpha = .2) +
	geom_line(data = wa_medinc %>%
					 select(year, mhhinc_cpi, mhhinc_asi_cpi, mhhinc_blk_cpi, mhhinc_lat_cpi, mhhinc_whtnl_cpi) %>%
					 gather(race, inc, mhhinc_cpi:mhhinc_whtnl_cpi) %>%
					 filter(year != 2015),
			  aes(x = year, y = inc, color = race), size = 1.5) +
	scale_y_continuous(labels = scales::dollar) +
	labs(title = "Washington Median HH Income by Race",
		 caption = "Data source: Bureau of Labor Statistics Consumer Price Index &\nUS Census American Community Survey",
		 y = "2017 dollars",
		 x = "Year") +
	coord_cartesian(ylim = c(20000, 100000)) +
	scale_color_brewer(palette = "Set1",
					  direction = -1,
					  name = "Race",
					  breaks=c("mhhinc_cpi",
					  		   "mhhinc_asi_cpi",
					   		   "mhhinc_whtnl_cpi",
					   		   "mhhinc_lat_cpi",
					   		   "mhhinc_blk_cpi"),
					  labels = c("Overall Median Income",
					   			 "Asian",
								 "White Non-Latinx",
								 "Latinx",
								 "Black")) +
	theme_minimal() +
	annotate("text", x = 2007, y = 52600, label = "80% AMI") +
	annotate("text", x = 2007, y = 33000, label = "50% AMI") +
	annotate("text", x = 2007, y = 20000, label = "30% AMI")
}
# plot_wa_medinc()

plot_co_medinc <- function(county){
ggplot() +
	geom_line(data = medinc %>%
					 unite(survey, survey, year, remove = FALSE) %>%
					 filter(survey != "acs5_2017",
					 		NAME == county) %>%
					 select(NAME, year, AMI80_cpi:AMI30_cpi) %>%
					 gather(ami, inc, AMI80_cpi:AMI30_cpi) %>%
					 mutate(ami = factor(ami,
					   				levels = c("AMI80_cpi",
					   						   "AMI50_cpi",
					   						   "AMI30_cpi"),
					   				labels = c("80% AMI",
					   						   "50% AMI",
					   						   "30% AMI"))),
			  aes(x = year, y = inc, group = ami), size = 4, alpha = .2) +
	geom_line(data = medinc %>%
					 unite(survey, survey, year, remove = FALSE) %>%
					 filter(survey != "acs5_2017",
					 		NAME == county) %>%
					 select(NAME, year, mhhinc_cpi, mhhinc_asi_cpi, mhhinc_blk_cpi, mhhinc_lat_cpi, mhhinc_whtnl_cpi) %>%
					 gather(race, inc, mhhinc_cpi:mhhinc_whtnl_cpi) %>%
					 filter(year %in% c(2000, 2009, 2012, 2015, 2016, 2017)),
		### Consider replacing 2014 acs5 for 1 yr in that range
			  aes(x = year, y = inc, color = race), size = 1.5) +
	theme_minimal() +
	scale_y_continuous(labels = scales::dollar) +
	labs(title = paste0(county, " Median HH Income by Race"),
		 caption = "Data source: Bureau of Labor Statistics Consumer Price Index &\nUS Census American Community Survey",
		 y = "2017 dollars",
		 x = "Year") +
	coord_cartesian(ylim = c(20000, 100000)) +
	scale_color_brewer(palette = "Set1",
					  direction = -1,
					  name = "Race",
					  breaks=c("mhhinc_cpi",
					  		   "mhhinc_asi_cpi",
					   		   "mhhinc_whtnl_cpi",
					   		   "mhhinc_lat_cpi",
					   		   "mhhinc_blk_cpi"),
					  labels = c("Overall Median Income",
					   			 "Asian",
								 "White Non-Latinx",
								 "Latinx",
								 "Black"))
}

# plot_wa_medinc()

# plot_co_medinc(county = "Snohomish") +
# 	annotate("text", x = 2007, y = 60500, label = "80% AMI") +
# 	annotate("text", x = 2007, y = 38000, label = "50% AMI") +
# 	annotate("text", x = 2007, y = 22500, label = "30% AMI")

# plot_co_medinc(county = "King") +
# 	annotate("text", x = 2007, y = 62000, label = "80% AMI") +
# 	annotate("text", x = 2007, y = 39000, label = "50% AMI") +
# 	annotate("text", x = 2007, y = 23500, label = "30% AMI")

# plot_co_medinc(county = "Pierce") +
# 	annotate("text", x = 2007, y = 52500, label = "80% AMI") +
# 	annotate("text", x = 2007, y = 33000, label = "50% AMI") +
# 	annotate("text", x = 2007, y = 20000, label = "30% AMI")


#
# Income by tenure
# --------------------------------------------------------------------------

# ==========================================================================
# !!!!!!!!!!!!! BEGIN DEVELOPMENT !!!!!!!!!!!!!
# ==========================================================================


# hhincten %>%
# 	filter(s_y %in% c("acs5_2017", "acs5_2009", "sf3_2000"),
# 		   type == "HHIncTenRent") %>%
# 	group_by(year) %>%
# 	summarise_at(vars(AMI80_pop:AMI30_pop, HHIncTen_Total), funs(sum)) %>%
# 	mutate(AMI80_prop = AMI80_pop/HHIncTen_Total,
# 		   AMI50_prop = AMI50_pop/HHIncTen_Total,
# 		   AMI30_prop = AMI30_pop/HHIncTen_Total) %>%
# 	gather(type, prop, AMI80_pop:HHIncTen_Total) %>%
# ggplot() +
# 	geom_line(aes(x = year, y = prop, color = type))

co_income <- function(county){
hhincten %>%
	filter(!s_y %in% c("acs5_2017", "acs1_2015","acs1_2013", "acs1_2016"),
		   type == "HHIncTenRent",
		   NAME == county) %>%
	group_by(year, NAME) %>%
	mutate(inc_cat = ifelse(income_cpi <= 41000, "less than $40k",
					ifelse(income_cpi > 41000 &
						   income_cpi <= 65000, "$40k - $65k",
					ifelse(income_cpi > 65000 &
						   income_cpi <= 90000, "$65k - $90k",
						   "more than $90k"))),
		   inc_cat = factor(inc_cat,
		   					levels = c("more than $90k",
		   							   "$65k - $90k",
		   							   "$40k - $65k",
		   							   "less than $40k"))) %>%
	group_by(NAME, year, inc_cat) %>%
	summarise(estimate = sum(estimate)) %>%
	group_by(NAME, year) %>%
	mutate(prop = estimate/sum(estimate)) %>%
ggplot() +
	theme_minimal() +
	geom_area(aes(x = year, y = estimate, fill = inc_cat), alpha = .5) +
	geom_line(aes(x = year, y = estimate, color = inc_cat)) +
	geom_point(aes(x = year, y = estimate, color = inc_cat)) + 
	scale_fill_brewer("Income", palette = "Set1") +
	scale_color_brewer("Income", palette = "Set1") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks=c(2000,2007, 2012, 2017)) +
      theme(axis.text.x = element_text(angle = -45, hjust = 0),
          panel.grid.minor.x = element_blank()) + 
      labs(y = "Renting Households", 
      	   x = "Year", 
      	   title = paste(county, "County"))
}

# 	summarise_at(vars(AMI80_pop:AMI30_pop, HHIncTen_Total), funs(sum)) %>%
# 	mutate(AMI80_prop = AMI80_pop/HHIncTen_Total,
# 		   AMI50_prop = AMI50_pop/HHIncTen_Total)

# hhincten %>%
# 	mutate(inc_cat = ifelse(income_cpi < 30000, "$30k",
# 					ifelse(income_cpi >= 30000 &
# 						   income_cpi < 60000, "$60k",
# 					ifelse(income_cpi >= 60000 &
# 						   income_cpi < 100000, "$100k",
# 					ifelse(income_cpi >= 100000 &
# 						   income_cpi < 250000, "$250k",
# 						   "+ $250k")))),
# 		   inc_cat = factor(inc_cat,
# 		   					levels = c("+ $250k",
# 		   							   "$250k",
# 		   							   "$100k",
# 		   							   "$60k",
# 		   							   "$30k"))) %>%
# 	filter(!s_y %in% c("acs5_2017", "acs1_2015"),
# 		   type == "HHIncTenRent",
# 		   NAME %in% c("King", "Pierce", "Snohomish", "Clark", "Spakane")) %>%
# 	group_by(year, NAME) %>%
# 	summarise_at(vars(AMI80_pop:AMI30_pop, HHIncTen_Total), funs(sum)) %>%
# 	mutate(AMI80_prop = AMI80_pop/HHIncTen_Total,
# 		   AMI50_prop = AMI50_pop/HHIncTen_Total,
# 		   AMI30_prop = AMI30_pop/HHIncTen_Total) %>%
# 	gather(type, prop, AMI80_pop:HHIncTen_Total) %>%
# 	filter(NAME == "King") %>%
# ggplot() +
# 	geom_line(aes(x = year, y = prop, color = type))

	#
	# Seems that the number of undividuals that are low-income are not increasing while the number of the overall renting population is increasing. This could be because there are not enough rentals that are affordable for low-income households to allow lo-income hosueholds to live there. If all the new rentals that are coming in are at a higher cost, then there's higher competition for those at a lower price.
	#


# ==========================================================================
# !!!!!!!!!!!!! END DEVELOPMENT !!!!!!!!!!!!!
# ==========================================================================
