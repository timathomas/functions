librarian::shelf(tidyverse, tidycensus)

us_counties <- 
  map(unique(fips_codes$state)[1:51], function(state){
    tigris::counties(state = state)}
    )|> bind_rows()

us_pumas <- 
  map(unique(fips_codes$state)[1:51], function(state){
    pumas(state = state)}
    )|> bind_rows()

sf::st_crs(us_counties)
sf::st_crs(us_pumas)

us_co_puma <- 
  st_join(us_counties, us_pumas) %>% 
  st_drop_geometry() %>% 
  select(
    STATEFP:NAMELSAD, 
    PUMA_GEOID = GEOID10, 
    PUMACE10, 
    PUMA_NAME = NAMELSAD10) %>% 
  left_join(
    fips_codes %>% 
      select(state, state_name, STATEFP = state_code) %>% 
      distinct()
    )

qsave(us_co_puma, "~/data/census/us_county_puma_cross.qs")