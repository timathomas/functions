# Joins state census tracts to PUMAs to get county fips codes & names
#
# You can choose multiple states
#
# Note: highly populated counties may have numerous PUMAs 
# (e.g., King County, WA) while low populated areas may have numerous 
# counties in one PUMA (e.g., North Dakota)

get_co_puma <- 
  function(st, yr){
    librarian::shelf(tidyverse, tidycensus)
    
    us_counties <- 
      # map(unique(fips_codes$state)[1:51], function(state){
      map2(st, yr, function(s, y){
        tracts(state = s, year = y) %>% 
          st_centroid()}
        )|> bind_rows()
    
    us_pumas <- 
      # map(unique(fips_codes$state)[1:51], function(state){
      map2(st, yr, function(s, y){
        pumas(state = s, year = y) %>% 
          mutate(YEAR = y)}
        )|> bind_rows() 
    
    us_co_puma <- 
      st_join(us_pumas, us_counties) %>% 
      st_drop_geometry() %>%
      select(PUMACE10, STATEFP, COUNTYFP, NAMELSAD10) %>% 
      left_join(
        fips_codes %>% 
          select(
            STATEFP = state_code, 
            COUNTYFP = county_code, 
            state, 
            state_name, 
            county)
        ) %>% 
      distinct() %>% 
      arrange(county) %>% 
      mutate(county = str_replace(county, " County", ""))
    
    return(us_co_puma)
    }

# qsave(us_co_puma, "~/git/timathomas/functions/data/us_county_puma_cross.qs")
# wa <- get_co_puma("WA", 2017)
# check <- pumas(state = "WA", year = 2017) %>% left_join(wa)
# check %>% filter(grepl("King", county)) %>% select(NAMELSAD10) %>% plot()
# check %>% filter(grepl("Pierce", county)) %>% select(NAMELSAD10) %>% plot()
# check %>% filter(grepl("Whatcom", county)) %>% select(NAMELSAD10) %>% plot()