# ==========================================================================
# Create neighborhood typology for entire country
# ==========================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("timathomas/neighborhood")
pacman::p_load(sf, tigris, tidycensus, tidyverse)
options(tigris_use_cache = TRUE)

# ==========================================================================
# Create and save data
# ==========================================================================

us_states <- 
  states(cb = TRUE) %>% 
  st_set_geometry(NULL) %>% 
  filter(!STUSPS %in% c("AS", "GU", "MP", "VI")) %>% 
  pull(STUSPS) %>% 
  sort()

us_tracts <-
  map_df(us_states, function(states){
      ntdf(state = "CA", year = 2019) %>% st_crs()
      mutate(state = states, year = 2019)
    })

saveRDS(us_tracts, "~/git/neighborhood/data/us_nt_tracts.rds")
write_csv(us_tracts, "~/git/neighborhood/data/us_nt_tracts.csv.bz2")