#=================
# INSTALL PACKAGES
# Taken from here https://www.r-bloggers.com/2017/02/how-to-make-a-global-map-in-r-step-by-step/
#=================
pacman::p_load(tidyverse, rvest, magrittr, ggmap, stringr)

#============
# SCRAPE DATA
#============

html.global_talent <- read_html("https://www.insead.edu/news/2017-global-talent-competitiveness-index-davos")

df.global_talent_RAW <- html.global_talent %>%
                          html_nodes("table") %>%
                          extract2(1) %>%
                          html_table()

print(df.global_talent_RAW)

# X1              X2  X3          X4
#  1     Switzerland   6   Australia
#  2       Singapore   7  Luxembourg
#  3  United Kingdom   8     Denmark
#  4   United States   9     Finland
#  5          Sweden  10      Norway

#=============================================
# SPLIT INTO 2 DATA FRAMES
# - the data are split into 4 columns, whereas
#   we want all of the data in columns
#=============================================

df.global_talent_1 <- df.global_talent_RAW %>% select(X1, X2) %>% rename(rank = X1, country = X2)
df.global_talent_2 <- df.global_talent_RAW %>% select(X3, X4) %>% rename(rank = X3, country = X4)

#===========
# RECOMBINE
#===========

df.global_talent <- rbind(df.global_talent_1, df.global_talent_2)


# INSPECT
glimpse(df.global_talent)
print(df.global_talent)

glimpse(df.global_talent)

# Observations: 10
# Variables: 2
# $ rank    <chr> " 1", " 2", " 3", " 4", " 5", " 6", " 7", " 8", " 9", " 10"
# $ country <chr> " Switzerland", " Singapore", " United Kingdom", " United States", " Sw...

#==========================
# STRIP LEADING WHITE SPACE
#==========================

df.global_talent <- df.global_talent %>% mutate(country = str_trim(country)
                                                ,rank = str_trim(rank)
                                                )

# INSPECT
print(df.global_talent)

#   rank         country
#     1     Switzerland
#     2       Singapore
#     3  United Kingdom
#     4   United States
#     5          Sweden
#     6       Australia
#     7      Luxembourg
#     8         Denmark
#     9         Finland
#    10          Norway

#==============
# GET WORLD MAP
#==============

map.world <- map_data("world")

#===========================================
# RECODE NAMES
# - Two names in the 'global talent' data
#   are not the same as the names in the 
#   map
# - We need to re-name these so they match
# - If they don't match, we won't be able to 
#   join the datasets
#===========================================

# INSPECT
as.factor(df.global_talent$country) %>% levels()

# RECODE NAMES
df.global_talent$country <- recode(df.global_talent$country
                                  ,'United States' = 'USA'
                                  ,'United Kingdom' = 'UK'
                                  )

# INSPECT
print(df.global_talent)

#   rank     country
#     1 Switzerland
#     2   Singapore
#     3          UK
#     4         USA
#     5      Sweden
#     6   Australia
#     7  Luxembourg
#     8     Denmark
#     9     Finland
#    10      Norway

#================================
# JOIN
# - join the 'global talent' data 
#   to the world map
#================================

# LEFT JOIN
map.world_joined <- left_join(map.world, df.global_talent, by = c('region' = 'country'))

head(map.world)

#===================================================
# CREATE FLAG
# - in the map, we're going to highlight
#   the countries with high 'talent competitiveness'
# - Here, we'll create a flag that will
#   indicate whether or not we want to 
#   "fill in" a particular country 
#   on the map
#===================================================

map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(rank),F,T))
head(map.world_joined)

#=======================================================
# CREATE POINT LOCATIONS FOR SINGAPORE AND LUXEMBOURG
# - Luxembourg and Singapore are countries with
#   high 'talent competitiveness'
# - But, they are both small on the map, and hard to see
# - We'll create points for each of these countries
#   so they are easier to see on the map
#=======================================================

df.country_points <- data.frame(country = c("Singapore","Luxembourg"),stringsAsFactors = F)
glimpse(df.country_points)

#--------
# GEOCODE
#--------

geocode.country_points <- geocode(df.country_points$country)

df.country_points <- cbind(df.country_points,geocode.country_points)

# INSPECT
print(df.country_points)

#    country        lon       lat
#  Singapore 103.819836  1.352083
# Luxembourg   6.129583 49.815273

#=======
# MAP
#=======

map.world_joined %>% filter(grepl("c", region)) %>% group_by(region) %>% count() %>% data.frame()

countries <- c(
'United Arab Emirates',
'Argentina',
'Australia',
'Azerbaijan',
'Brazil',
'Canada',
'Switzerland',
'Columbia',
'Germany',
'Egypt',
'Indonesia',
'Israel',
'Italy',
'Jamaica',
'South Korea',
'Morocco',
'Monaco',
'Mexico',
'Netherlands',
'New Zealand',
'Poland',
'Portugal',
'Paraguay',
'Rwanda',
'Saudi Arabia',
'Slovakia',
'Thailand',
'Turkey',
'UK',
'USA')

plot_df <- 
	map.world_joined %>% 
	mutate(fill_flg = case_when(region %in% countries ~ TRUE, TRUE ~ FALSE))

missing <- data.frame(countries) %>% filter(!countries %in% plot_df$region)

ggplot() +
  geom_polygon(data = plot_df, aes(x = long, y = lat, group = group, fill = fill_flg)) +
  # geom_point(data = df.country_points, aes(x = lon, y = lat), color = "#e60000") +
  scale_fill_manual(values = c("#CCCCCC","#e60000")) +
  labs(title = 'MILO Development') +
       # ,subtitle = "source: INSEAD, https://www.insead.edu/news/2017-global-talent-competitiveness-index-davos") +
  theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#444444")
        ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 30)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
        )

