# ==========================================================================
# Source code for 06_slc_analysis.rmd
# Author Tim Thomas and Julia Greenberg
# 2022.05.31
# 2. Story
#     * Show displacement trend maps (mostly already made)
#     * Why these trends
#         * Rent burden
#             * # of county hh that's rent burdened
#             * % rent burdened in
#                 * Rising rent markets
#                 * EDR
#         * ∆ in rent
#             XXX * 2-y axis graph on rent, income to afford, by year
#             * Recreate table on number of affordable homes to each income group
#         * within EDR
#             * Family structure
#             * ∆ rent
#             * rent burden
#             * median hh income
#                XXX * by race
#                 * family structure
#         * Racial dynamics
#           * Redlining
#           * SLC has X% of Utah's Y race/ethnic group
# ==========================================================================

# ==========================================================================
# Packages & options
# ==========================================================================
source("~/git/timathomas/functions/functions.r")
ipak(c("qs", "knitr","kableExtra","data.table","gdata","leaflet.extras","tigris","sf","scales","tidycensus","tidyverse", "plotly"))
options(scipen=10) # avoid scientific notation
opts_chunk$set(fig.width=8, fig.height=8)

cpi <-
data.table::fread(
'year CPI
2000  1.62
2001  1.54
2002  1.52
2003  1.48
2004  1.44
2005  1.40
2006  1.34
2007  1.31
2008  1.31
2009  1.27
2010  1.25
2011  1.21
2012  1.19
2013  1.17
2014  1.15
2015  1.14
2016  1.13
2017  1.12
2018  1.08
2019  1.06
2020  1.05
2021  1.00
2022  1.00')
# ==========================================================================
# Rent burden
# ==========================================================================
# define variable list
acsrb_vars <-
  c(
    'rb_tot' = 'B25070_001', # GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME
    'rb_10.0' = 'B25070_002', # Less than 10.0 percent GROSS RENT as % hh income
    'rb_14.9' = 'B25070_003', # 10.0 to 14.9 percent GROSS RENT as % hh income
    'rb_19.9' = 'B25070_004', # 15.0 to 19.9 percent GROSS RENT as % hh income
    'rb_24.9' = 'B25070_005', # 20.0 to 24.9 percent GROSS RENT as % hh income
    'rb_29.9' = 'B25070_006', # 25.0 to 29.9 percent GROSS RENT as % hh income
    'rb_34.9' = 'B25070_007', # 30.0 to 34.9 percent GROSS RENT as % hh income
    'rb_39.9' = 'B25070_008', # 35.0 to 39.9 percent GROSS RENT as % hh income
    'rb_49.9' = 'B25070_009', # 40.0 to 49.9 percent GROSS RENT as % hh income
    'rb_55' = 'B25070_010', # 50.0 percent or more GROSS RENT as % hh income
    'rb_nc' = 'B25070_011' # Not computed GROSS RENT as % hh income
  )

# create rent burden function for different geographies
rb_fun <- function(year, geo, name = 'Utah'){
  get_acs(year = year,
          geography = geo, 
          variables = acsrb_vars, 
          state = 'UT', 
          cache = TRUE
  ) %>% 
    filter(grepl(name, NAME)) %>% 
    select(-moe) %>% 
    pivot_wider(names_from = variable, values_from = estimate) %>%
    summarize(
      norb = sum(c_across(rb_10.0:rb_29.9), rb_nc),
      rb30 = sum(c_across(rb_34.9:rb_49.9), na.rm = TRUE),
      rb50 = rb_55) %>%
    mutate(year = year) %>%
  pivot_longer(norb:rb50)
}

# create state rent burden df
rb_st <- 
  rbind(
    rb_fun(2009, "state"), 
    rb_fun(2010, "state"), 
    rb_fun(2011, "state"), 
    rb_fun(2012, "state"), 
    rb_fun(2013, "state"), 
    rb_fun(2014, "state"), 
    rb_fun(2015, "state"), 
    rb_fun(2016, "state"), 
    rb_fun(2017, "state"), 
    rb_fun(2018, "state"), 
    rb_fun(2019, "state"), 
    rb_fun(2020, "state")
  )

# create place rent burden df
rb_pl <- 
  rbind(
    rb_fun(2009, "place", name = 'Salt Lake City'),
    rb_fun(2010, "place", name = 'Salt Lake City'),
    rb_fun(2011, "place", name = 'Salt Lake City'),
    rb_fun(2012, "place", name = 'Salt Lake City'),
    rb_fun(2013, "place", name = 'Salt Lake City'),
    rb_fun(2014, "place", name = 'Salt Lake City'),
    rb_fun(2015, "place", name = 'Salt Lake City'),
    rb_fun(2016, "place", name = 'Salt Lake City'),
    rb_fun(2017, "place", name = 'Salt Lake City'),
    rb_fun(2018, "place", name = 'Salt Lake City'),
    rb_fun(2019, "place", name = 'Salt Lake City'),
    rb_fun(2020, "place", name = 'Salt Lake City')
  )

#
# Plot
# --------------------------------------------------------------------------

rb_plot <- function(df, area){
ggplot(df) +
  geom_area(aes_string(x = "year", y = 'value', fill = "name")) +
  # scale_y_continuous(labels = labels) +
  theme_minimal() +
  labs(title = paste0(area, " Household Rent Burden"),
    subtitle = "Percentage of income going to rent (rent burden = 30% or more)",
     x = "Year",
     y = '',
     caption = "Data source: US Census American Community Survey") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set1",
            direction = -1,
            name = "Percent of Income\nGoing to Rent",
            breaks=c("rb0",
                  "rb30",
                "rb50"),
            labels = c("Less than 30%",
                    "30% to 50%",
                  "Over 50%"))
}

ggplotly(rb_plot(rb_pl, "Salt Lake City"))
# rb_plot(rb_st, "Utah State")

# ==========================================================================
# FMR two y plot
# ==========================================================================

fmr <- readRDS('~/git/timathomas/functions/data/hudfmr.rds') wa_hudfmr %>%
filter(State == "UT", year > 2005, grepl("Ogden|Provo|Salt Lake", AreaName)) %>%
mutate(
  msa =
    case_when(
      grepl("Salt Lake", AreaName) == TRUE ~ "Salt Lake",
      grepl("Ogden", AreaName) == TRUE ~ "Ogden",
      grepl("Provo", AreaName) == TRUE ~ "Provo"
      )
  ) %>%
select(year, fmr_avg_cpi, msa) %>%
distinct()

# group_by(year) %>% count() %>% arrange(year) %>% data.frame()
glimpse(fmr)
fmr
wa_hudfmr %>% group_by(countyname, year) %>% count() %>% data.frame()
fmr %>% filter(countyname == "Davis")

fmr_plot <- ggplot() +
      theme_minimal() +
      geom_smooth(data = wa_hudfmr %>% filter(!county %in% c("Snohomish", "Skamania")),
                  aes(
                    x = year,
                    y = fmr_avg_cpi,
                    color = county),
                  se = FALSE) +
      labs(title = "Average fair market rent for all bedroom types (left)\n& income needed to avoid rent burden (right)",
           subtitle = "rent burden = 30% of income to rent",
           y = "Rent",
           x = "Year",
           caption = "Data source: HUD Fair Market Rent data &\nthe Bureau of Labor Statistics Consumer Price Index") +
      coord_cartesian(ylim = c(800,2500)) +
      scale_color_brewer("", palette = "Set2") +
      scale_y_continuous(labels = scales::dollar,
                         sec.axis = sec_axis(~(.*12)/.3, name = "Income needed to afford rent", labels = scales::dollar),
                         # breaks = c(900, 1000, 1100, 1200, 1300, 1400, 1500)
                         ) +
      scale_x_continuous(breaks=c(min(fmr$year):max(fmr$year))) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0),
          panel.grid.minor.x = element_blank())

fmr_plot

pincrease <-
  fmr %>%
  filter(year %in% c(2013, 2022)) %>%
  pivot_wider(names_from = year, values_from = fmr_avg_cpi, names_prefix = 'y_') %>%
  group_by(msa) %>%
  mutate(p_increase = (y_2022 - y_2013)/y_2013)

pincrease
# ==========================================================================
# Income by race
# ==========================================================================

acsmedinc_vars <-
  c('mhhinc' = 'B19013_001',
    # 'mhhinc_wht' = 'B19013A_001',
    'mhhinc_blk' = 'B19013B_001',
    # 'mhhinc_aian' = 'B19013C_001',
    'mhhinc_asi' = 'B19013D_001',
    'mhhinc_nhop' = 'B19013E_001', # pac islander
    # 'mhhinc_oth' = 'B19013F_001',
    # 'mhhinc_two' = 'B19013G_001',
    'mhhinc_whtnl' = 'B19013H_001',
    'mhhinc_lat' = 'B19013I_001')

medinc_r <- function(year, state, county = null, geo = "county"){
  get_acs(
    variables = acsmedinc_vars,
    state = state,
    # county = county,
    geography = geo,
    year = year
    ) %>%
  mutate(year) %>%
  left_join(cpi) %>%
  mutate(estimate = CPI*estimate) %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    ami80 = mhhinc*.8,
    ami50 = mhhinc*.5,
    ami30 = mhhinc*.3
    ) %>%
  data.frame()
}


mir <- map_df(rep(2009:2020), function(year){
  medinc_r(year, 'UT', geo = 'place') %>% filter(grepl("Salt Lake City", NAME))})

mir_plot <- ggplot() +
  geom_smooth(
    data =
      mir %>%
      select(mhhinc, starts_with("ami"), year) %>%
      pivot_longer(mhhinc:ami30) %>%
      mutate(
        ami = factor(name,
          levels = c('mhhinc', 'ami80','ami50','ami30'),
          labels = c("AMI", "80%", "50%", "30%"))),
    aes(x = year, y = value, group = ami), color = "grey80", size = 6, alpha = .2, se = FALSE) +
  geom_smooth(
    data =
      mir %>%
      select(year, mhhinc_blk:mhhinc_lat) %>%
      pivot_longer(mhhinc_blk:mhhinc_lat) %>%
      mutate(
        race = factor(name,
          levels = c('mhhinc_whtnl', 'mhhinc_asi', 'mhhinc_nhop', 'mhhinc_lat', 'mhhinc_blk'),
          labels = c('White', 'Asian', 'Pacific Islander', 'Latinx', 'Black'))),
    ### Consider replacing 2014 acs5 for 1 yr in that range
        aes(x = year, y = value, color = race), size = 1.5, se = FALSE) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar, breaks = seq(15000,75000, 5000),
    sec.axis = sec_axis(~., breaks = seq(15000,75000, 5000), labels = scales::dollar)) +
  labs(title = "Salt Lake City Median Household Income by Race",
    subtitle = "AMI = Area Median Income\n80% AMI = Low income\n50% AMI = Very low income\n30% AMI = Extremely low income",
     caption = "Author: Tim Thomas, UC Berkeley\nData source: Bureau of Labor Statistics Consumer Price Index &\nUS Census American Community Survey",
     y = "2022 dollars",
     x = "Year") +
  scale_color_brewer(
    palette = "Dark2",
    direction = -1,
    name = "Race & Ethnicity") +
  scale_x_continuous(breaks=c(min(fmr$year):max(fmr$year))) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),
          panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
  annotate(geom="text", x=2014, y=53000, label="AMI", color="black") +
  annotate(geom="text", x=2014, y=42500, label="80%", color="black") +
  annotate(geom="text", x=2014, y=26500, label="50%", color="black") +
  annotate(geom="text", x=2014, y=16000, label="30%", color="black")

mir_plot

