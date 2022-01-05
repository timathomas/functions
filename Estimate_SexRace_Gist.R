# ==========================================================================
# Code for estimating sex and race if individuals
# Tim Thomas
# timthomas@berkeley.edu
# ==========================================================================

# NOTE: While most eviction data are "public", individuals who face eviction
# 		are vulnerable to lasting housing and economic struggles. Therefore,
# 		we make it a rule to omit names from any searchable databases (e.g.
# 		GitHub).

# ==========================================================================
# Libraries - load or install if not on your machine
# ==========================================================================

	if(!require(pacman)){
	    install.packages("pacman", dependencies = TRUE)
	    require(pacman)
	}

	pacman::p_load(devtools, gender, wru, data.table, censusxy, tigris, sf, tidyverse)

# ==========================================================================
# Load data
# ==========================================================================

	# This is a sample dataframe.
	# Your data frame should look something like this.
		# NOTE: It is very likely you'll have to clean the names and
		# 		separate companies from your raw dataframe.
	firstname <- c("John","Nia","Lupe","Conner","Jamal")
	surname <- c("Smith","Carter","Rodriguez","Miller","Williams")
	age <- c(30, 30, 30, 30, 30) # if you don't have age, use 30
	address <-
		c(
			"2001 NW Market St",
			"6306 30th Ave SW",
			"400 23rd Ave Se",
			"1410 NE 66th St",
			"5997 Rainier Ave S")
	city <- rep('Seattle', 5)
	state <- rep('WA', 5)
	zip <- c('98107', '98126', '98122', '98115', '98118')
	df <- data.frame(firstname,surname,age,address,city,state,zip, stringsAsFactors = FALSE)
	df$id <- rownames(df)

	# Preview data frame
	df

# ==========================================================================
# Geocode addresses
# ==========================================================================

	# Geocode using census
	geo <-
		cxy_geocode(
			df,
			street = 'address',
			city = 'city',
			state = 'state',
			zip = 'zip',
			output = 'full', # get accuracy
			class = 'sf' # make it spatial
			)

	# Preview data frame
	geo

		# Note: You want to pay attention to geocoding quality.
		#  		The cxy_quality returns exact and non-exact matches.
		# 		Double check non-exact matches to make sure they are
		# 		what you're looking for. The example above has one
		# 		non-exact match in line 3.

	# Combine names and tract locations
	df_geo <- geo %>%
			  st_join(., tracts(state = "WA"))# get tract fips for each point


# ==========================================================================
# Estimate Sex of Names
# ==========================================================================

	# Estimate names
	sex <- gender(unique(df_geo$firstname),
				  method = "ssa",
				  countries = "United States") %>%
		   data.frame() %>%
		   distinct()

	# Join sex probabilities to
	df_geo_sex <- left_join(df_geo,
							sex,
							by = c("firstname" = "name"))

	# see if any and which names were unidentifiable
	df_geo_sex %>%
	filter(is.na(gender)) %>%
	data.frame()
		# Up to you to decide what you do with unidentifiable names: toss or
		# salvage. One method I used was to search facebook for these names
		# and take the first 10 profiles I found and averaged the gender
		# based on these 10.

# ==========================================================================
# Estimate Race
# ==========================================================================

		# You'll need a US Census API. Go here to sign up for one
		# https://api.census.gov/data/key_signup.html

	race <-
		df_geo_sex %>%
		select(
			id,
			surname,
			county = COUNTYFP,
			tract = TRACTCE,
			state) %>%
		st_drop_geometry() %>%
		predict_race(
			.,
			census.geo = "tract",
			census.key = '4c26aa6ebbaef54a55d3903212eabbb506ade381' # https://api.census.gov/data/key_signup.html
			) %>%
		arrange(id) %>%
		rename(white = pred.whi, # for easier interpretation
			   black = pred.bla,
			   latinx = pred.his,
			   asian = pred.asi,
			   other = pred.oth) %>%
		mutate(race = colnames(.[,6:10])[max.col(.[,6:10],
										ties.method="first")])

# ==========================================================================
# Create final dataframe
# ==========================================================================
	df_geo_sex_race <-
		left_join(df_geo_sex, race %>% select(id, surname, white:race)) %>%
		select(address:id, cxy_status:cxy_matched_address, GEOID, proportion_male:race)
	# NOTE: Names ommitted in final dataframe.

# ==========================================================================
# Save files
# ==========================================================================
	# as csv
	write_csv(df_geo_sex_race %>% st_drop_geometry(), 'path/df_geo_sex_race.csv')

	# as a spatial dataframe
	st_write(df_geo_sex_race, '~/Downloads/df_geo_sex_race.gpkg') # can also use the .shp format