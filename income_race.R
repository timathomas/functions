# ==========================================================================
# Housing Income by Race
# ==========================================================================

# Alameda, Berkeley, Oakland, Hayward, and San Francisco

pacman::p_load(tidycensus, tidyverse)

census_api_key(key = '63217a192c5803bfc72aab537fe4bf19f6058326', install = TRUE)

vars <- 
	c(
	'mhhinc' = 'B19013_001', 
	'HHInc_Total' = 'B19001_001', # Total HOUSEHOLD INCOME
	'HHInc_10' = 'B19001_002', # Less than $10,000 HOUSEHOLD INCOME
	'HHInc_15' = 'B19001_003', # $10,000 to $14,999 HOUSEHOLD INCOME
	'HHInc_20' = 'B19001_004', # $15,000 to $19,999 HOUSEHOLD INCOME
	'HHInc_25' = 'B19001_005', # $20,000 to $24,999 HOUSEHOLD INCOME
	'HHInc_30' = 'B19001_006', # $25,000 to $29,999 HOUSEHOLD INCOME
	'HHInc_35' = 'B19001_007', # $30,000 to $34,999 HOUSEHOLD INCOME
	'HHInc_40' = 'B19001_008', # $35,000 to $39,999 HOUSEHOLD INCOME
	'HHInc_45' = 'B19001_009', # $40,000 to $44,999 HOUSEHOLD INCOME
	'HHInc_50' = 'B19001_010', # $45,000 to $49,999 HOUSEHOLD INCOME
	'HHInc_60' = 'B19001_011', # $50,000 to $59,999 HOUSEHOLD INCOME
	'HHInc_75' = 'B19001_012', # $60,000 to $74,999 HOUSEHOLD INCOME
	'HHInc_100' = 'B19001_013', # $75,000 to $99,999 HOUSEHOLD INCOME
	'HHInc_125' = 'B19001_014', # $100,000 to $124,999 HOUSEHOLD INCOME
	'HHInc_150' = 'B19001_015', # $125,000 to $149,999 HOUSEHOLD INCOME
	'HHInc_200' = 'B19001_016', # $150,000 to $199,999 HOUSEHOLD INCOME
	'HHInc_250' = 'B19001_017', # $200,000 or more HOUSEHOLD INCOME
	'HHIncWht_Total' = 'B19001A_001', # Total HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_10' = 'B19001A_002', # Less than $10,000 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_15' = 'B19001A_003', # $10,000 to $14,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_20' = 'B19001A_004', # $15,000 to $19,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_25' = 'B19001A_005', # $20,000 to $24,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_30' = 'B19001A_006', # $25,000 to $29,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_35' = 'B19001A_007', # $30,000 to $34,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_40' = 'B19001A_008', # $35,000 to $39,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_45' = 'B19001A_009', # $40,000 to $44,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_50' = 'B19001A_010', # $45,000 to $49,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_60' = 'B19001A_011', # $50,000 to $59,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_75' = 'B19001A_012', # $60,000 to $74,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_100' = 'B19001A_013', # $75,000 to $99,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_125' = 'B19001A_014', # $100,000 to $124,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_150' = 'B19001A_015', # $125,000 to $149,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_200' = 'B19001A_016', # $150,000 to $199,999 HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	'HHIncWht_250' = 'B19001A_017' # $200,000 or more HOUSEHOLD INCOME (WHITE ALONE HOUSEHOLDER)
	)

acs_co <- 
	get_acs(
		geography = "county", 
		state = "CA", 
		county = c("San Francisco", "Alameda"), 
		variables = vars, 
		output = "wide", 
		keep_geo_vars = TRUE
		)

acs_place <- 
	get_acs(
		geography = "place", 
		state = "CA", 
		variables = vars, 
		output = "wide", 
		keep_geo_vars = TRUE
		) %>% filter(NAME %in% c("San Francisco city, California", "Berkeley city, California", "Oakland city, California", "Hayward city, California", "Alameda city, California"))

coli <- 
	acs_co %>% 
	group_by(NAME) %>% 
	summarise(
		co_mhhinc = mhhincE, 
		co_LI_val = .8*co_mhhinc
		)


place_li <- 
	acs_place %>% 
	group_by()

df_vli <-
	df %>%
	select(GEOID,
		   COUNTY,
		   mhhinc,
		   HHInc_10:HHInc_75) %>%
	group_by(COUNTY) %>%
	mutate(co_mhhinc = median(mhhinc, na.rm = TRUE),
		   co_LI_val = .8*co_mhhinc,
		   co_VLI_val = .5*co_mhhinc,
		   co_ELI_val = .3*co_mhhinc) %>%
	ungroup() %>%
	gather(medinc_cat, medinc_count, HHInc_10:HHInc_75) %>%
	mutate(medinc_cat = recode(medinc_cat, !!!inc_names17)) %>%
	mutate(bottom_inccat = case_when(medinc_cat == 9999 ~ medinc_cat - 9999,
									 medinc_cat > 9999 &
									 medinc_cat <= 49999 ~ medinc_cat - 4999,
									 medinc_cat == 59999 ~ medinc_cat - 9999,
									 medinc_cat > 59999 &
									 medinc_cat <= 149999 ~ medinc_cat - 24999,
									 medinc_cat >= 199999 ~ medinc_cat - 49999,
								  TRUE ~ NA_real_),
		top_inccat = medinc_cat,
		LI = case_when(co_LI_val >= top_inccat ~ 1,
					   co_LI_val <= top_inccat &
					   co_LI_val >= bottom_inccat ~
			   		   (co_LI_val - bottom_inccat)/(top_inccat - bottom_inccat),
			   		   TRUE ~ 0),
		VLI = case_when(co_VLI_val >= top_inccat ~ 1,
					    co_VLI_val <= top_inccat &
			   			co_VLI_val >= bottom_inccat ~
			   			(co_VLI_val - bottom_inccat)/(top_inccat - bottom_inccat),
			   			TRUE ~ 0),
		ELI = case_when(co_ELI_val >= top_inccat ~ 1,
						co_ELI_val <= top_inccat &
						co_ELI_val >= bottom_inccat ~
						(co_ELI_val - bottom_inccat)/(top_inccat - bottom_inccat),
						TRUE ~ 0)) %>%
	group_by(GEOID) %>%
	mutate(tr_totinc_count = sum(medinc_count, na.rm = TRUE),
		   tr_LI_count = sum(LI*medinc_count, na.rm = TRUE),
		   tr_VLI_count = sum(VLI*medinc_count, na.rm = TRUE),
		   tr_ELI_count = sum(ELI*medinc_count, na.rm = TRUE),
		   tr_LI_prop = tr_LI_count/tr_totinc_count,
		   tr_VLI_prop = tr_VLI_count/tr_totinc_count,
		   tr_ELI_prop = tr_ELI_count/tr_totinc_count) %>%
	select(GEOID:co_ELI_val, tr_totinc_count:tr_ELI_prop) %>%
	distinct()


