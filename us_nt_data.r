# ==========================================================================
# Create neighborhood typology for entire country
# ==========================================================================

if (!require("librarian")) install.packages("librarian")
librarian::shelf(evictionresearch/neighborhood, sf, tigris, tidycensus, tidyverse)
options(tigris_use_cache = TRUE)

# ==========================================================================
# Create and save data
# ==========================================================================

year = 2022

us_states <- 
  states(cb = TRUE) %>% 
  st_set_geometry(NULL) %>% 
  filter(!STUSPS %in% c("AS", "GU", "MP", "VI")) %>% 
  pull(STUSPS) %>% 
  sort()

us_tracts <-
  map_df(us_states, function(states){
      ntdf(state = states, year = year) %>%
      mutate(state = states, year = year)
    })

saveRDS(us_tracts, paste0("~/git/evictionresearch/neighborhood/data/us_nt_tracts", year, ".rds"))
write_csv(us_tracts, paste0("~/git/evictionresearch/neighborhood/data/us_nt_tracts", year, ".csv.bz2"))