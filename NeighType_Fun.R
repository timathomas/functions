# ==========================================================================
# Neighborhood Typology
# By: Tim Thomas - timothyathomas@gmail.com
# Measures drawn from:
# 	Hall, Matthew, Kyle Crowder, and Amy Spring. 2015. “Neighborhood 
# 		Foreclosures, Racial/Ethnic Transitions, and Residential 
# 		Segregation.” American Sociological Review 80:526–549.
# 
# Process:
# 	1. Create a dataframe with the proportions for White, Black, Asian, 
# 		Latinx, and Other. 
# 	2. Run the `nt` function on that dataset to create a `NeighType` field
# 	3. Look at your neighborhood typology frequency and consider merging
# 		similar, or low frequency, neighborhoods into one or more common 
# 		categories (e.g., some of the four category cases could be considered
# 		white-mixed--see the article to see how the authors consolidated 
# 		categories).
# ==========================================================================

nt <- function(df, pWhite = "pWhite", pBlack = "pBlack", pAsian = "pAsian", pLatinx = "pLatinx", pOther = "pOther"){

	if (!require("pacman")) install.packages("pacman")
	pacman::p_load(tidyverse)

	df %>%
	group_by(GEOID) %>%
			# create neighborhood typology
	mutate(NeighType =
		case_when(
			pWhite >=.9 ~ "All White",
			pBlack >=.9 ~ "All Black",
			pAsian >=.9 ~ "All Asian",
			pLatinx >=.9 ~ "All Latinx",
			pOther >=.9 ~ "All Other",

			# pBlack >=.7 ~ "Mostly Black",
			# pAsian >=.7 ~ "Mostly Asian",
			# pLatinx >=.7 ~ "Mostly Latinx",
			# pOther >=.7 ~ "Mostly Other",

			pWhite < .9 & pBlack < .1 & pAsian < .1 & pLatinx < .1 & pOther < .1 ~ "White-Shared",
			pBlack < .9 & pWhite < .1 & pAsian < .1 & pLatinx < .1 & pOther < .1 ~ "Black-Shared",
			pAsian < .9 & pBlack < .1 & pWhite < .1 & pLatinx < .1 & pOther < .1 ~ "Asian-Shared",
			pLatinx < .9 & pBlack < .1 & pAsian < .1 & pWhite < .1 & pOther < .1 ~ "Latinx-Shared",
			pOther < .9 & pBlack < .1 & pAsian < .1 & pLatinx < .1 & pWhite < .1 ~ "Other-Shared",

			pWhite >=.1 & pWhite < .9 & pBlack >=.1 & pBlack < .9 & pAsian < .1 & pLatinx < .1 & pOther < .1 ~ "White-Black",
			pWhite >=.1 & pWhite < .9 & pLatinx >=.1 & pLatinx < .9 & pAsian < .1 & pBlack < .1 & pOther < .1 ~ "White-Latinx",
			pWhite >=.1 & pWhite < .9 & pAsian >=.1 & pAsian < .9 & pBlack < .1 & pLatinx < .1 & pOther < .1 ~ "White-Asian",
			pWhite >= .1 & pWhite < .9 & pOther >=.1 & pOther < .9 & pBlack < .1 & pLatinx < .1 & pAsian < .1 ~ "White-Other",

			pBlack >=.1 & pBlack < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pLatinx < .1 & pAsian < .1 ~ "Black-Other",
			pBlack >=.1 & pBlack < .9 & pLatinx >=.1 & pLatinx < .9 & pWhite < .1 & pAsian < .1 & pOther < .1 ~ "Black-Latinx",

			pAsian >=.1 & pAsian < .9 & pBlack >=.1 & pBlack < .9 & pWhite < .1 & pLatinx < .1 & pOther < .1 ~ "Asian-Black",
			pAsian >=.1 & pAsian < .9 & pLatinx >=.1 & pLatinx < .9 & pWhite < .1 & pBlack < .1 & pOther < .1 ~ "Asian-Latinx",
			pAsian >= .1 & pAsian < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pLatinx < .1 & pBlack < .1 ~ "Asian-Other",
			
			pLatinx >= .1 & pLatinx < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pAsian < .1 & pBlack < .1 ~ "Latinx-Other",

			pBlack >=.1 & pAsian >=.1 & pLatinx >=.1 & pWhite < .1 & pOther < .1 ~ "Black-Asian-Latinx",
			pBlack >=.1 & pAsian >=.1 & pOther >=.1 & pWhite < .1 & pLatinx < .1 ~ "Black-Asian-Other",
			pBlack >=.1 & pLatinx >=.1 & pOther >=.1 & pWhite < .1 & pAsian < .1 ~ "Black-Latinx-Other",

			pAsian >=.1 & pLatinx >=.1 & pOther >=.1 & pWhite < .1 & pBlack < .1 ~ "Asian-Latinx-Other",

			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pLatinx < .1 & pOther < .1 ~ "White-Black-Asian",
			pWhite >=.1 & pBlack >=.1 & pLatinx >=.1 & pAsian < .1 & pOther < .1 ~ "White-Black-Latinx",
			pWhite >=.1 & pBlack >=.1 & pOther >=.1 & pLatinx < .1 & pAsian < .1 ~ "White-Black-Other",
			pWhite >=.1 & pAsian >=.1 & pLatinx >=.1 & pBlack < .1 & pOther < .1 ~ "White-Asian-Latinx",
			pWhite >=.1 & pAsian >=.1 & pOther >=.1 & pLatinx < .1 & pBlack < .1 ~ "White-Asian-Other",
			pWhite >=.1 & pLatinx >=.1 & pOther >=.1 & pAsian < .1 & pBlack < .1 ~ "White-Latinx-Other",
			pWhite >=.1 & pLatinx >=.1 & pOther >=.1 & pAsian < .1 & pBlack < .1 ~ "White-Latinx-Other",

			pBlack >=.1 & pAsian >=.1 & pLatinx >=.1 & pOther >=.1 & pWhite < .1 ~ "Black-Asian-Latinx-Other",
			pWhite >=.1 & pAsian >=.1 & pLatinx >=.1 & pOther >=.1 & pBlack < .1 ~ "White-Asian-Latinx-Other",
			pWhite >=.1 & pBlack >=.1 & pLatinx >=.1 & pOther >=.1 & pAsian < .1 ~ "White-Black-Latinx-Other",
			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pOther >=.1 & pLatinx < .1 ~ "White-Black-Asian-Other",
			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pLatinx >=.1 & pOther < .1 ~ "White-Black-Asian-Latinx",

			pWhite >.1 & pWhite <=.7 & pBlack >.1 & pAsian >.1 & pLatinx >.1 & pOther >.1 ~ "Integrated", 
			totraceE == 0 ~ "unpopulated tract")) %>% 
		ungroup()
	}

# ==========================================================================
# Optional download and create nt df
# ==========================================================================


ntdf <- function(state, geography = "tract", county = NULL, geometry = FALSE, cache_table = TRUE,output = "wide", year = 2017){

	if (!require("pacman")) install.packages("pacman")
	pacman::p_load(tidyverse, tidycensus)

race_vars <- 
	c('totrace' = 'B03002_001',
	  'White' = 'B03002_003',
	  'Black' = 'B03002_004',
	  'Asian' = 'B03002_006',
	  'Latinx' = 'B03002_012')

acs_data <- 
	get_acs(
		geography = geography,
		variables = race_vars,
		state = state,
		county = county,
		geometry = geometry,
		cache_table = cache_table,
		output = output,
		year = year, 
		cb = TRUE
		) %>%
	select(-ends_with("M")) %>% 
	group_by(GEOID) %>% 
	mutate(pWhite = WhiteE/totraceE, 
		   pAsian = AsianE/totraceE, 
		   pBlack = BlackE/totraceE, 
		   pLatinx = LatinxE/totraceE, 
		   pOther = (totraceE - sum(WhiteE, AsianE, BlackE, LatinxE, na.rm = TRUE))/totraceE)

 nt(df = acs_data)

}

ntcheck <- function(df) 
	df %>% group_by(NeighType) %>% count() %>% arrange(desc(n)) %>% data.frame()

#### example
# Baltimore_nt <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
# cal <- ntdf(state = "CA")
# ny <- ntdf(state = "NY")
# glimpse(Baltimore_nt)
# 
# ps_nt <- ntdf(state = "WA", county = c("Snohomish", "King", "Pierce"), geometry = TRUE)
####


### 
# Check to see if there are duplicate tract assumptions. 
# Baltimore_nt %>% 
# st_set_geometry(NULL) %>% 
# mutate(val = 1) %>%
# spread(NeighType, val, fill = 0) %>% 
# mutate_at(vars(`All Black`:`White-Shared`), list(as.numeric)) %>% 
# select(13:ncol(.)) %>% 
# mutate(rowsum = rowSums(.)) %>% 
# filter(rowsum > 1) %>% 
# glimpse()

# ==========================================================================
# Note: 
# After running the above code, look at the counts and consider concatenating 
# of reducing the outlying neighborhood types into something more manageable. 
# ==========================================================================

# ntcheck(Baltimore_nt)
# ntcheck(cal)
# ntcheck(ny)
# ntcheck(ps_nt)

