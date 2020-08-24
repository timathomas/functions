# ==========================================================================
# Population growth
# This gist shows how different areas have changed over time. 
# ==========================================================================

if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, scales, tidyverse)
race_census <- fread("/data/census/race_census.csv.bz2")

#
# Raw Growth
# --------------------------------------------------------------------------
# fill in the gaps for rural counties


race_counts <- race_census %>%
	group_by(NAME) %>%
	mutate(co_type = if_else(min(totrace) <= 75000, "Low Pop", "High Pop")) %>%
	select(-year) %>%
	gather(race, count, Asian:Other) %>%
	spread(survey, count) %>% data.frame()

wa_counts <-
	race_counts %>%
	filter(race != "totrace") %>%
	group_by(race) %>%
	summarise(`2000` = sum(sf1_2000), `2009` = sum(acs5_2009), `2017` = sum(acs5_2017)) %>%
	gather(year, counts, `2000`:`2017`) %>%
	mutate(year = as.numeric(year),
		   race = factor(race, levels = c("White", "Other", "Black", "Asian", "Latinx"))) %>%
	group_by(year) %>%
	mutate(prop = counts/sum(counts))

# ==========================================================================
# Washington Plots
# ==========================================================================

plot_wa_race <- function(df, y = "counts", labels){
df %>%
ggplot() +
	geom_area(aes_string(x = "year", y = y, fill = "race")) +
	theme_minimal() +
	labs(title = "Washington", x = "Year", y = "Count") +
	scale_y_continuous(labels = labels) +
	scale_fill_brewer(palette = "Spectral",
					   name = "Race & Ethnicity")
}

# plot_wa_race(df = wa_counts, labels = scales::comma)
# plot_wa_race(df = wa_counts, y = "prop", labels = scales::percent)

# ==========================================================================
# County counts
# ==========================================================================

plot_co_race <- function(county,
						 title = county,
						 ylim = c(0,2250000),
						 labels,
						 y_axis = "Count",
						 y = "counts"){
race_counts %>%
	filter(co_type == "High Pop",
		   NAME == county,
		   race != "totrace") %>%
	select(-acs5_2017) %>%
	group_by(NAME, race) %>%
	summarise(`2000` = sum(sf1_2000),
			  `2007` = sum(acs5_2009),
			  `2012` = sum(acs1_2012),
			  `2013` = sum(acs1_2013),
			  `2014` = sum(acs1_2014),
			  `2015` = sum(acs1_2015),
			  `2016` = sum(acs1_2016),
			  `2017` = sum(acs1_2017)) %>%
	gather(year, counts, `2000`:`2017`) %>%
	mutate(year = as.numeric(year),
		   race = factor(race, levels = c("White", "Other", "Black", "Asian", "Latinx"))) %>%
	group_by(year) %>%
	mutate(prop = counts/sum(counts)) %>%
ggplot() +
	geom_area(aes_string(x = "year", y = y, fill = "race")) +
	theme_minimal() +
	coord_cartesian(ylim = ylim) +
	labs(title = title, x = "Year", y = y_axis) +
	scale_y_continuous(labels = labels) +
	scale_fill_brewer(palette = "Spectral",
					   name = "Race & Ethnicity")
}

# plot_co_race("King", labels = scales::comma)
# plot_co_race("King", y = "prop", ylim = NULL, labels = scales::percent, y_axis = "")

# ==========================================================================
# Place counts
# ==========================================================================

race_vars12 <-
	c(totrace = "B03002_001",
	  White = "B03002_003",
	  Black = "B03002_004",
	  Asian = "B03002_006",
	  Latinx = "B03002_012"
)

acs1year <-
	rep(2012:2017, 1)

race_1yr <-
	map_df(acs1year, function(x){
		get_acs(geography = "place",
				variables = race_vars12,
				survey = "acs1",
				year = x,
				state = "WA")
	})


ggplot(race00) +
	geom_sf() +
	geom_sf_label(aes(label = GEOID)) +
	coord_sf(xlim = c(122.4, 122.3), ylim = c(47.5, 47.4))

tracts <- c("Census Tract 78", "Census Tract 79", "Census Tract 77", "Census Tract 87", "Census Tract 88", "Census Tract 90", "Census Tract 89", "Census Tract 95")

r00 <- race00 %>%
filter(NAME %in% tracts) %>%
	mutate(tot00 = sum(totrace),
		   totblack00 = sum(Black),
		   pblack00 = totblack00/tot00) %>%
	group_by(GEOID) %>%
	mutate(ttot00 = sum(totrace),
		   ttotblack00 = sum(Black),
		   tpblack00 = ttotblack00/ttot00) %>% data.frame

tracts2 <- c("Census Tract 78, King County, Washington",
			"Census Tract 79, King County, Washington",
			"Census Tract 77, King County, Washington",
			"Census Tract 87, King County, Washington",
			"Census Tract 88, King County, Washington",
			"Census Tract 90, King County, Washington",
			"Census Tract 89, King County, Washington",
			"Census Tract 95, King County, Washington")

r17 <- race5yr %>%
	filter(NAME %in% tracts2) %>%
	mutate(tot17 = sum(totrace),
		   totblack17 = sum(Black),
		   pblack17 = totblack17/tot17) %>%
	group_by(GEOID) %>%
	mutate(ttot17 = sum(totrace),
		   ttotblack17 = sum(Black),
		   tpblack17 = ttotblack17/ttot17) %>% data.frame

r10 <- race5yr10 %>%
	filter(NAME %in% tracts2) %>%
	mutate(tot10 = sum(totrace),
		   totblack10 = sum(Black),
		   pblack10 = totblack10/tot10) %>%
	group_by(GEOID) %>%
	mutate(ttot10 = sum(totrace),
		   ttotblack10 = sum(Black),
		   tpblack10 = ttotblack10/ttot10) %>% data.frame

left_join(r00 %>% select(GEOID, tot00:tpblack00),
		  r10 %>% select(GEOID, tot10:tpblack10)) %>%
left_join(., r17 %>% select(GEOID, tot17:tpblack17)) %>%
ungroup() %>%
mutate(difftract = tpblack17 - tpblack00)  %>%
mutate_at(c("pblack00", "tpblack00", "pblack10", "tpblack10", "pblack17", "tpblack17", "difftract"), scales::percent)

