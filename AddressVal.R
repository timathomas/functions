# ==========================================================================
# USPS Address Validation Tool
# ==========================================================================

if(!require(rusps)){
  !requireNamespace("devtools", quietly = TRUE)
  install.packages("devtools")
  devtools::install_github("crazybilly/rusps", force = TRUE)
  require(rusps)
}

if(!require(XML)){
  install.packages("XML")
  require(XML)
}

if(!require(xml2)){
  install.packages("xml2")
  require(xml2)
}

if(!require(data.table)){
  install.packages("data.table")
  require(data.table)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  require(tidyverse)
}

# ==========================================================================
# Code
# ==========================================================================

# username <- '112UNIVE2850' # get this quickly and freely by signing up at https://registration.shippingapis.com/ (not commercial).
# street <- "234  7th avenue "
# street2 <- '333  raspberry rd'
# city   <- 'kirkland'
# st  <- 'wa'

# usps_validate(username = username, street = street, city = city, st = st)

add <- fread("/data/results/addresses_2019-01-13.csv.bz2")

clean_add<-
add %>%
	# slice(1:100) %>%
	replace(is.na(.), "") %>%
	mutate(full_st = paste(house, preDirection, streetName, street, postDirection),
		clean_add = ifelse(city == "", "",
					usps_validate(username = "112UNIVE2850",
								  street = full_st,
								  city = city,
								  st = "wa"))) %>%
	mutate(clean_add = gsub(" Default address:.*", "", clean_add))
