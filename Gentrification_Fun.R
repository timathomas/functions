# !!! NOTE: Look at Atkinson 2011 for modeling gentrification.

# ==========================================================================
# Elegant Package Function - NOT WORKING
# ==========================================================================

if(!require(tidyverse)){
	    install.packages("tidyverse")
	    require(tidyverse)
	}
if(!require(tidycensus)){
	    install.packages("tidycensus")
	    require(tidycensus)
	}

options(tigris_use_cache = TRUE)
# ==========================================================================
# Variable selection - creates a searchable list of census variables
# ==========================================================================
	# v16 <- load_variables(2016, "acs5", cache = TRUE)
	# View(v16)
# ==========================================================================
# ACS Gentrification Measures
# Cite: ...
# ==========================================================================

source("/Users/timothythomas/Git/censusvariables/gen_vars.R")

gen <- function(geography,variables,state,county,geometry = FALSE, survey = "acs5", year = 2017){
	gen <-
		# Get data from tidycensus
		#
		get_acs(geography = geography,
			variables = variables,
			survey = survey,
			state = state,
			county = county,
			output = "wide",
			year = year,
			geometry = geometry,
			cache_table = TRUE) %>%
		select(-ends_with("M")) %>%
	group_by(GEOID) %>%
	mutate(#pct = 100 * (estimate / summary_est))
			pWhite = White/totrace,
			pBlack = Black/totrace,
			pAsian = Asian/totrace,
			pLatinx = Latinx/totrace,
			Other = totrace-White-Black-Asian-Latinx,
			pOther = ifelse(Other/totrace>=0, Other/totrace,
						ifelse(is.na(Other/totrace), 0, 0)),
			pWelf = welf/totwelf,
			pPov = sum(povfamh, povnonfamh)/totpov,
			pUnemp = unemp/totunemp,
			pFemhh = sum(femfamhh, femnonfamhh)/totfemhh,
			pFemhhCh = sum(femfamheadch, femnonfamheadch)/totfhc,
			pChild = sum(Munder5,Mfiveto9,Mtento14,Mfiftto17,Funder5,Ffiveto9,Ftento14,Ffiftto17)/totage,
			pEld = sum(Mel1,Mel2,Mel3,Mel4,Mel5,Mel6,Fel1,Fel2,Fel3,Fel4,Fel5,Fel6)/totage,
			pCol = sum(bach, mas, pro, doc)/toted,
			# include medfaminc
			pRent = rent/totten,
			pOwn = own/totten,
			# include mgrent
			pManagers = sum(Mmgr,Fmgr)/totoccupation,
			pPrivSchl = sum(pschlM1,pschlM2,pschlM3,pschlM4,pschlM5,pschlF1,pschlF2,pschlF3,pschlF4,pschlF5)/totschool) %>%
	ungroup()
		return(gen)
	}


# ==========================================================================
# Freeman Measure
# ==========================================================================

load("~/data/NCDB/Raw/NCDB_1970_2010.RData")

ncdb %>% 
	names() %>% 
	grepl("msa", names(ncdb))

"CMSA99","MSACMA99","MSAPMA99","PMSA99"

# ==========================================================================
# Enterprise review of gentrification
# 	https://www.enterprisecommunity.org/policy-and-advocacy/policy-development-and-research/gentrification-comparison-tool
#
# Freeman model: 
# Freeman, L. (2005). Displacement or Succession?: Residential Mobility in Gentrifying Neighborhoods. Urban Affairs Review, 40(4), 463–491.
# 
# Eligible to Gentrify: Tract median household income AND share of housing built in prior 20 years both less than metro-wide values in first year of decade
# Gentrified: Eligible AND change in tract share of residents with college degree greater than metro-wide value AND any increase in real house prices between first and last year of decade
# 
# 
# Ellen and O'Regan
# Ellen, I.G., and O’Regan, K. (2008). Reversal of Fortunes? Lower-income Urban Neighbourhoods in the US in the 1990s. Urban Studies, 45(4), 845–869.

# Eligible to Gentrify: Ratio of tract-to-metro average household income at start of decade less than 0.7
# Gentrified: Eligible AND at least a 10-percentage-point increase in the ratio of tract-to-metro average household income over decade
# 
# 
# McKinnish et al: 
# McKinnish, T., Walsh, R., and White, T.K. (2010). Who Gentrifies Low-Income Neighborhoods? J Urban Econ, 67(2), 180–193.
# 
# ==========================================================================

# ==========================================================================
# Test the function
# ==========================================================================
	# wa16 <- gen(geography = "tract",
	# 		  state = "WA",
	# 		  county = NULL,
	# 		  geometry = TRUE)

	# wa15 <- gen(geography = "tract",
	# 		  state = "WA",
	# 		  county = NULL,
	# 		  year = 2015,
	# 		  geometry = TRUE)

	# wa14 <- gen(geography = "tract",
	# 		  state = "WA",
	# 		  county = NULL,
	# 		  year = 2014,
	# 		  geometry = TRUE)

	# wa13 <- gen(geography = "tract",
	# 		  state = "WA",
	# 		  county = NULL,
	# 		  year = 2013,
	# 		  geometry = TRUE)

	# wa12 <- gen(geography = "tract",
	# 		  state = "WA",
	# 		  county = NULL,
	# 		  year = 2012,
	# 		  geometry = TRUE)

	# wa11 <- gen(geography = "tract",
	# 		  state = "WA",
	# 		  county = NULL,
	# 		  year = 2011,
	# 		  geometry = TRUE)

	# wa10 <- gen(geography = "tract",
	# 		  state = "WA",
	# 		  county = NULL,
	# 		  year = 2010,
	# 		  geometry = TRUE)


# ==========================================================================
# End Test
# ==========================================================================

# ==========================================================================
# By Table
# ==========================================================================

	# table <- c("B03002","B19057","B17017","B23025","B09019","B01001","B15003","B19013","B25003","B25064","C24010","B11005","B14002")

	# get_acs(geography = "block groups",
	# 		table = table,
	# 		year = 2016,
	# 		survey = "acs5",
	# 		state = "WA",
	# 		county = "King",
	# 		output = "wide",
	# 		geometry = TRUE,
	# 		cache_table = TRUE)

### Old rename
	#
