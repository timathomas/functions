# ==========================================================================
# Various Spatial Functions
# ==========================================================================

# ==========================================================================
# Erase areas, such as water zones in tracts.
# ==========================================================================

st_erase <- function(x, y) {
			sf::st_difference(x, sf::st_union(sf::st_combine(y)))
			}

	# ==========================================================================
	# DO NOT RUN
		kc_water <- tigris::area_water("WA", "King", class = "sf")
		kc <- tigris::tracts("WA", "King", class = "sf")

		kc_erase <- st_erase(kc, kc_water)

		king_bg <- geo_join(kc_erase,
							nt,
							by = c("GEOID"))

	# ==========================================================================
