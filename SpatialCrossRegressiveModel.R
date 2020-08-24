# ==========================================================================
# Spatial Cross-Regressive Modeling
# Tim Thomas - t77@uw.edu
# 18.07.03
# ==========================================================================

	rm(list = ls())
	options(scipen = 10) # avoid scientific notation

	library(spdep)
	library(tigris)
	options(tigris_use_cache = TRUE) # for caching shapefiles
	library(tidyverse)

# ==========================================================================
# Data
# ==========================================================================

	load("H_Drive/Academe/Data/SPD/Data/Temp/nb_complaints.RData")

	wa_tracts <- tracts(state = "WA", cb = TRUE)

	wa_tracts@data <- left_join(wa_tracts@data,
								mod9.1_data,
						   		by = c("GEOID" = "tract"))

# ==========================================================================
# Create lag variables
# ==========================================================================

		coords <- coordinates(wa_tracts)
		IDs <- row.names(as(wa_tracts, "data.frame"))
		wa_tracts_nb <- poly2nb(wa_tracts) # nb
		lw_bin <- nb2listw(wa_tracts_nb, style = "B",zero.policy = TRUE)
		kern1 <- knn2nb(knearneigh(coords, k = 1), row.names=IDs)
		dist <- unlist(nbdists(kern1, coords)); summary(dist)
		max_1nn <- max(dist)
		dist_nb <- dnearneigh(coords, d1=0, d2 = .75*max_1nn, row.names = IDs)
		### listw object
		set.ZeroPolicyOption(TRUE)
		set.ZeroPolicyOption(TRUE)
		dists <- nbdists(dist_nb, coordinates(wa_tracts))
		idw <- lapply(dists, function(x) 1/(x^2))
		lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")

	### lag values
		wa_tracts$pED15.lag <- lag.listw(lw_dist_idwW,wa_tracts$pED15)
		wa_tracts$pPOV00.lag <- lag.listw(lw_dist_idwW,wa_tracts$pPOV00)
		wa_tracts$pPOV15.lag <- lag.listw(lw_dist_idwW,wa_tracts$pPOV15)
		wa_tracts$pRB30Plus_15.lag <- lag.listw(lw_dist_idwW,wa_tracts$pRB30Plus_15)
		wa_tracts$pRB50Plus_15.lag <- lag.listw(lw_dist_idwW,wa_tracts$pRB50Plus_15)

		glimpse(wa_tracts@data)
