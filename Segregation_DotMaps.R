# ==========================================================================
# Puget Sound Race Dot Maps
# ==========================================================================


library(tidycensus)
library(tmap)
library(tmaptools)
library(OpenStreetMap)
library(sf)
library(tigris)
library(magick)
library(tidyverse)

library(tidycensus)
library(tidyverse)
library(sf)
options(tigris_use_cache = TRUE)

ctys <- c("King", "Snohomish", "Pierce")

acs_vars <- 
  c(White = "B03002_003E",
    Black = "B03002_004E",
    Asian = "B03002_006E",
    Latinx = "B03002_012E")

king_race <- 
  get_acs(geography = "block group", 
          variables = acs_vars,
          state = "WA", 
          county = "King", 
          geometry = TRUE) %>%
  rename(value = estimate) %>%
  mutate(year = "2013-2017", 
         variable = case_when(variable == "B03002_003" ~ "White",
                             variable == "B03002_004" ~ "Black",
                             variable == "B03002_006" ~ "Asian",
                             variable == "B03002_012" ~ "Latinx")) 

king_dots <- 
  map(c("White", "Black", "Latinx", "Asian"), function(group) { 
    king_race %>%
    filter(variable == group) %>% 
    st_sample(., size = .$value / 100) %>% 
    st_sf() %>%
    mutate(group = group)
}) %>% 
  reduce(rbind)

king_dots_shuffle <- sample_frac(king_dots, size = 1)

options(tigris_class = "sf")
king_roads <- 
  roads("WA", "King") %>%
  filter(RTTYP %in% c("I", "S", "U"))
king_water <- area_water("WA", "King") 
king_boundary <- counties("WA", cb = TRUE)

ggplot() +
  geom_sf(data = king_boundary, color = NA, fill = "white") +
  geom_sf(data = king_dots, aes(color = group, fill = group), size = 0.1) + geom_sf(data = king_water, color = "lightblue", fill = "lightblue") + geom_sf(data = king_roads, color = "grey") +
  coord_sf(crs = 26918, datum = NA) +
  scale_color_brewer(palette = "Set1", guide = FALSE) + scale_fill_brewer(palette = "Set1") +
  labs(title = "The racial geography of King County, WA",
  subtitle = "2010 decennial U.S. Census",
  fill = "",
  caption = "1 dot = approximately 100 people.\nData acquired with
  the R tidycensus and tigris packages.")

plot(king_dots_shuffle, key.pos = 1)

# king_dots <- 
#   king_dots %>% 
#   group_by(group) %>% 
#   summarize()


p90 <- get_decennial(geography = "block group", variables = "P0010001",
                     state = "WA", county = ctys, geometry = TRUE,
                     year = 1990) %>%
  mutate(year = "1990")

p00 <- get_decennial(geography = "block group", variables = "P001001",
                     state = "WA", county = ctys, geometry = TRUE,
                     year = 2000) %>%
  mutate(year = "2000")

p10 <- get_decennial(geography = "block group", variables = "P0010001",
                     state = "WA", county = ctys, geometry = TRUE,
                     year = 2010) %>%
  mutate(year = "2010")

# ==========================================================================
# Dot Maps
# ==========================================================================

dc_dots <- 
  map(c("White", "Black", "Latinx", "Asian"), function(group) { dc_race %>%
filter(variable == group) %>% st_sample(., size = .$value / 100) %>% st_sf() %>%
mutate(group = group)
}) %>% reduce(rbind)

dc_dots <- dc_dots %>% group_by(group) %>% summarize()

dots <- p17 %>%
    st_transform(26914) %>%
    mutate(value = as.integer(value / 20)) %>%
    st_sample(., .$value) %>%
    st_sf()

dfs <- list(p90, p00, p10, p16)

url <- "mapbox://styles/timthomas/cjguazchf00142rmhil6nunaq"

# osm <- ggmap::qmap(bb(p90), source = "stamen", maptype = "toner")

osm <- read_osm(bb(p90), type = url)

walk(dfs, function(x) {

  dots <- x %>%
    st_transform(26914) %>%
    mutate(value = as.integer(value / 200)) %>%
    st_sample(., .$value) %>%
    st_sf()

  b1 <- tm_shape(osm) +
    tm_raster() +
    tm_shape(dots) +
    tm_dots(alpha = 0.4) +
    tm_layout(title = unique(x$year)) +
    tm_credits("1 dot = 200 people. Data source: Census/ACS via the R tidycensus package")

  save_tmap(b1, paste0("img/", unique(x$year), ".jpg"))

})

i90 <- image_read("img/1990.jpg")
i00 <- image_read("img/2000.jpg")
i10 <- image_read("img/2010.jpg")
i16 <- image_read("img/2012-2016.jpg")

image_animate(c(i90, i00, i10, i16), fps = 0.5)