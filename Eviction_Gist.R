# ==========================================================================
# Counts of Evictions for 2004 to 2017
# ==========================================================================

# ==========================================================================
# Libraries
# ==========================================================================

	library(tidyverse)

# ==========================================================================
# Data
# ==========================================================================

	# ev <- data.table::fread("R_Drive/Project/Evictions/Data/Output/Parties_2004_2017.csv")

# ==========================================================================
# Tables
# ==========================================================================

	# counts <- ev %>%
	# 			group_by(County, Year) %>%
	# 			select(Number) %>%
	# 			distinct() %>%
	# 			summarize(count = n())

# ==========================================================================
# Plot
# ==========================================================================

# 	ggplot(counts, aes(x = Year, y = count, color = County)) +
# 		geom_line(size = 1) +
# 		theme_minimal() +
# 		theme(legend.position="none",
# 			  axis.text.x = element_text(angle = -45, hjust = 0),
# 				panel.grid.minor.x = element_blank()) +
# 		geom_text(data = counts %>% filter(Year == 2015),
# 				   aes(label = County,
# 				   	   x = Year + 1,
#                         y = count),
# 				   nudge_y = 175,
# 				   nudge_x = -.5,
# 				   fontface = "bold",
# 					check_overlap = TRUE) +
# 		geom_text(data = counts %>% filter(Year %in% c(2005, 2007, 2009, 2011, 2013, 2017)),
# 				   aes(label = count,
# 				   	   x = Year + 1,
#                         y = count),
# 				   nudge_y = 200,
# 				   nudge_x = -.7,
# 				   size = 3,
# 					check_overlap = TRUE) +
# 		labs(title = "Unlawful Detainer Cases by Washington County",
# 				  y = "Count",
# 				  x = "Year") +
# 		scale_x_continuous(breaks=c(2004:2017))

# ggsave(filename = "H_Drive/Academe/Presentations/180927_StateTestimony/WAUDCases.pdf",
# 	   width = 8,
# 	   height = 6)


aff_homes <- function(place){
  ggplot(data = runits %>%
                   filter(NAME == place,
                         !year %in% c(2015,2016),
                         Rent %in% c("$500 to $799",
                                     "Less than $500"
                                     )) %>%
                    group_by(NAME, year) %>%
                    summarise(count = sum(count)),
                    aes(x = year, y = count)) +
  theme_minimal() +
  scale_x_continuous(breaks=c(2000:2018)) +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(size = 1.5, color = "black", se = FALSE) +
  geom_point(size = 2.5, alpha = .5) +
  coord_cartesian(xlim = c(2000:2018)) +
  labs(title = paste0(place, " trends in affordable housing,\nhomelessness, and evictions"),
       subtitle = "Affordable rental units at $800 or less (2017 dollars)",x = "", y = "Affordable Rental Units") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))}

homelessct <- function(place){
  ggplot(data = homeless %>%
                   filter(county == place),
            aes(x = year, y = homeless)) +
  theme_minimal() +
  scale_x_continuous(breaks=c(2000:2018)) +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(color = "red", size = 1.5, se = FALSE) +
  geom_point(color = "red", size = 1.5, alpha = .5) +
  coord_cartesian(xlim = c(2000:2018)) +
  labs(x = "Year",
       y = "Individuals who are homeless",
       subtitle = "Homelessness point in time count") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))
}

evict <- function(place){
  ggplot(data = year_counts %>% filter(County == place), aes(x = Year, y = Count)) +
  theme_minimal() +
  scale_x_continuous(breaks=c(2000:2018)) +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(color = "cornflowerblue", size = 1.5, se = FALSE) +
  geom_point(alpha = .5, color = "cornflowerblue") +
  coord_cartesian(xlim = c(2000:2018)) +
  labs(x = "",
       y = "Eviction Cases",
       subtitle = "Eviction cases from 2004 to 2017",
       caption = "Data source: US Census American Community Survey,\nWashington State Department of Commerce Annual Point in Time Count") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))}

evgraphs <- function(place){
  ggpubr::ggarrange(
  aff_homes(place),
  homelessct(place),
  evict(place),
  ncol=1,
  nrow = 3,
  common.legend = TRUE,
  legend="right")}
