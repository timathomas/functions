get_co_puma <- 
  function(st, yr){
    librarian::shelf(tidyverse, tidycensus)
    
    us_counties <- 
      # map(unique(fips_codes$state)[1:51], function(state){
      map2(st, yr, function(s, y){
        tigris::counties(state = s, year = y)}
        )|> bind_rows()
    
    us_pumas <- 
      # map(unique(fips_codes$state)[1:51], function(state){
      map2(st, yr, function(s, y){
        pumas(state = s, year = y) %>% 
          mutate(YEAR = y)}
        )|> bind_rows() 
    
    us_co_puma <- 
      st_join(us_counties, us_pumas) %>% 
      select(
        YEAR, 
        STATEFP:GEOID,
        COUNTY = NAME, 
        PUMA_GEOID = GEOID10, 
        PUMACE10, 
        PUMA_NAME = NAMELSAD10) %>% 
      left_join(
        fips_codes %>% 
          select(state, state_name, STATEFP = state_code) %>% 
          distinct()
      ) 
    return(us_co_puma)
    }

# qsave(us_co_puma, "~/data/census/us_county_puma_cross.qs")
# wa <- get_co_puma("WA", 2017)