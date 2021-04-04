# ==========================================================================
# Race Estimation for Berit Schaus
# Idaho Evictions
# ==========================================================================

# ==========================================================================
# Load libraries
# ==========================================================================
remotes::install_github("lmullen/gender") # only way to get genderdata
if(!require(pacman)) install.packages('pacman')
pacman::p_load(wru, gender, readxl, sf, tigris, tidyverse)

# ==========================================================================
# Pull in raw data
# ==========================================================================

df <- read_xlsx("~/Google Drive/projects/berit_schaus_id_evictions/data/idaho_eviction_Records.xlsx")

# ==========================================================================
# Prep data for estimation
# ==========================================================================

id_tracts <- tracts(state = "ID")

clean_df <- 
	df %>% 
	separate(Defendant, c("firstname", "lastname"), sep = " ", remove = FALSE) %>% 
	mutate(Coords = gsub("[()]", "", Coords)) %>% # remove parentheses
	separate(Coords, c("Y", "X"), sep = ", ") %>% # split into X and Y coords
	st_as_sf(coords = c("X", "Y")) %>% # make df spatial feature
 	st_set_crs(4326) %>% # set the crs of the spatial feature
 	st_transform(st_crs(id_tracts)) %>% # match crs to census tract crs
 	st_join(id_tracts) %>% # spatial join tracts to get fips code for tract
 	mutate(
 		county = factor(COUNTYFP), 
 		tract = factor(TRACTCE), 
 		surname = factor(lastname), 
 		state = "ID") # mutates for wru race estimation

# ==========================================================================
# Estimate gender and race
# ==========================================================================

### gender ###
gen <- 
	gender(
		unique(clean_df$firstname),
			method='ssa',
			countries='United States') %>%
	as.data.frame() %>%
	distinct()

### Get tract data
tract.data <-
	get_census_data(key = "63217a192c5803bfc72aab537fe4bf19f6058326",
					"ID",
      				census.geo = "tract")

	saveRDS(tract.data, "~/Google Drive/projects/berit_schaus_id_evictions/data/tract.data.rds") # sometimes the census API is down, always good to save

### race ###
race <- 
	predict_race(
		clean_df, 
		census.geo = "tract", 
		census.data = tract.data
		) 

# ==========================================================================
# Final df join and cleanup
# ==========================================================================

final_df <- 
	race %>% 
	left_join(gen, by = c("firstname" = "name")) %>% 
	mutate(race=colnames(.[,26:30])[max.col(.[,26:30],ties.method="first")]) %>% 
	mutate(race = 
		case_when(
			race == "pred.asi" ~ "asian", 
			race == "pred.bla" ~ "black", 
			race == "pred.his" ~ "latinx", 
			race == "pred.oth" ~ "other", 
			race == "pred.whi" ~ "white", 
			))

# ==========================================================================
# Save final file
# ==========================================================================

write_csv(final_df, "/Users/timthomas/Google Drive/projects/berit_schaus_id_evictions/data/estimated_race_gender_idaho.csv")