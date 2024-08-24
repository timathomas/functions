# ==========================================================================
# afford function tutorial
# Tim Thomas
# timthomas@berkeley.edu
# 2024.02.26
# 
# Here is some sample code to use the `afford` function from the 
# neighborhood package. 
# ==========================================================================

# ==========================================================================
# Load packages
# ==========================================================================
install.package("librarian")
librarian::shelf(tigris, tidyverse, evictionresearch/neighborhood)

# ==========================================================================
# The afford function is a prototype of the neighborhood package. It 
# measures the number of homes in a tract that are affordable (require less 
# than 30% of a houasehold's income) to renters and owners of a certain area 
# median income limit. Below is an example of how to use it. 
# ==========================================================================

#
# The first input you need is the state and county FIPs codes for your study
# region. Let's say you want to look at Alameda County
# --------------------------------------------------------------------------

study_fips <- 
  fips_codes %>% # a dataset of all state and county fips codes in the country
  filter(
    state == "CA", 
    grepl("Alameda", county)
    )

study_fips

#
# Now, let's get the census data from 2020 using the afford function. We will 
# get all the households that make 80% or less than the county median income. 
# --------------------------------------------------------------------------
alameda <- 
  afford(
    state = study_fips$state_code, # you should only do one state at a time
    counties = study_fips$county_code, 
    year = 2020, 
    ami_limit = .8,
    geometry = FALSE # this is the default
    )

glimpse(alameda)

#
# Here's the output
# --------------------------------------------------------------------------
# > glimpse(alameda)
# Rows: 379
# Columns: 17
# $ GEOID              <chr> "06001400100", "06001400200", "06001400300", "06001…
# $ tr_own_accessible  <dbl> 82, 4, 37, 17, 11, 11, 80, 27, 40, 114, 11, 18, 11,…
# $ tr_own_total       <dbl> 1171, 476, 892, 773, 838, 292, 682, 598, 401, 750, …
# $ tr_own_supply      <dbl> 0.070025619, 0.008403361, 0.041479821, 0.021992238,…
# $ tr_own_ratio       <dbl> 0.19090066, 0.02290886, 0.11308040, 0.05995424, 0.0…
# $ tr_own_rate        <dbl> 39.001189, 1.902497, 17.598098, 8.085612, 5.231867,…
# $ tr_rent_accessible <dbl> 26, 116, 874, 431, 447, 214, 671, 567, 406, 704, 10…
# $ tr_rent_total      <dbl> 132, 354, 1517, 944, 814, 409, 1234, 1152, 668, 146…
# $ tr_rent_supply     <dbl> 0.1969697, 0.3276836, 0.5761371, 0.4565678, 0.54914…
# $ tr_rent_ratio      <dbl> 0.5369698, 0.8933162, 1.5706388, 1.2446744, 1.49704…
# $ tr_rent_rate       <dbl> 12.36623, 55.17241, 415.69560, 204.99405, 212.60404…
# $ reg_total_pop      <dbl> 573174, 573174, 573174, 573174, 573174, 573174, 573…
# $ reg_class_pop      <dbl> 210250, 210250, 210250, 210250, 210250, 210250, 210…
# $ ami_limit          <dbl> 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0…
# $ rent_jenks_cat     <fct> Less than 0.1%, Less than 0.1%, 0.2% to 0.8%, 0.2% …
# $ own_jenks_cat      <fct> Less than 0.1%, Less than 0.1%, Less than 0.1%, Les…
# $ popup              <chr> "<b>Tract: 06001400100</b><br>Tract rental units: 1…

#
# The afford function was built for mapping and analysis so it returns 
# a lot of tract information. For your purposes, you just want the percent
# of county households that are below 80% AMI. This is the `reg_class_pop` 
# variable. Here's all you need to do to get that county's proportion below 
# 80% AMI.  
# --------------------------------------------------------------------------

unique(alameda$reg_class_pop)/unique(alameda$reg_total_pop)
# [1] 0.3668171

# So Alameda's population below 80% AMI is 37%. I assume you'll want this 
# for every county in your study area so you will need to write a loop to 
# run this across every county in your study area. 
# 
# Please let me know if you need any help. 
# --------------------------------------------------------------------------