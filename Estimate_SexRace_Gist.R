# ==========================================================================
# Code for estimating sex and race if individuals
# Tim Thomas
# t77@uw.edu
# ==========================================================================

# ==========================================================================
# Libraries - install if not
# ==========================================================================

	if(!require(pacman)){
	    install.packages("pacman", dependencies = TRUE)
	    require(pacman)
	}
pacman::p_load(devtools,gender,wru,data.table,gdata,tigris,sf,tidyverse)

# ==========================================================================
# Load data
# ==========================================================================

	# Your data frame should look something like this
	firstname <- c("John","Nia","Lupe","Conner","Jamal")
	surname <- c("Smith","Carter","Rodriguez","Miller","Williams")
	age <- c(30, 30, 30, 30, 30) # if you don't have age, use 30
	address <- c("2001 NW Market St, Seattle, WA, 98107","6306 30th Ave SW, Seattle, WA, 98126","400 23rd Ave Se, Seattle, WA, 98122","1410 NE 66th St, Seattle, WA, 98115","5997 Rainier Ave S, Seattle, WA, 98118")
	df <- data.frame(firstname,surname,age, address, stringsAsFactors = FALSE)
	df$id <- rownames(df)

	df

# ==========================================================================
# Geocode addresses
# ==========================================================================

	# Beginning June 11, 2018, Google requires an API key. Go here to register for an API key (requires credit card but your trial should cover most geocodes)
	# https://developers.google.com/maps/documentation/javascript/get-api-key#step-1-get-an-api-key-from-the-google-cloud-platform-console

	# Register your API
	register_google(key = "your Google API Code")
	ggmap_credentials()

	# Geocode
	geo <- geocode(df$address, output = "more") %>%
		   select(lon, lat, loctype)

		# Pay attention to the loctype, "approximation" is very
		# braod and not a good geocode.
		# read: http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS4_geocoding.html#loctype

	# Combine names and locations
	df_geo <- cbind(df,geo) %>%
			  st_as_sf(coords = c("lon", "lat")) %>%
			  st_set_crs(4326) %>% # google is 4326, ArcGIS is 
			  st_transform(4269) %>%
			  st_join(., tracts(state = "WA", # get tract fips for each point
			  			 year = 2010) %>%
			  			 st_as_sf())

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

	# see which names were unidentifiable
	df_geo_sex %>%
	filter(is.na(gender)) %>%
	data.frame()
		# Up to you to decide what you do with these. One method I used was to
		# search facebook for these names and take the first 10 profiles I found
		# and averaged the gender based on these 10

# ==========================================================================
# Estimate Race
# ==========================================================================

		# You'll need a US Census API. Go here to sign up for one
		# https://api.census.gov/data/key_signup.html

	race <- df_geo_sex %>%
			select(id,
				   surname,
				   county = COUNTYFP10,
				   tract = TRACTCE10) %>%
			mutate(state = "WA") %>%
			st_set_geometry(NULL) %>%
			predict_race(.,
						 census.geo = "tract",
						 census.key = "Your census api here") %>%
			arrange(id) %>%
			rename(white = pred.whi, # for easier interpretation
				   black = pred.bla,
				   latinx = pred.his,
				   asian = pred.asi,
				   other = pred.oth) %>%
			mutate(race = colnames(.[,6:10])[max.col(.[,6:10],
										ties.method="first")])

	df_geo_sex_race <- left_join(df_geo_sex, race %>% select(id, surname, race))

