# ==========================================================================
# ARIMA models for eviction
# Resources
# 	* https://rpubs.com/riazakhan94/arima_with_example
# 	* https://otexts.com/fpp2/arima-r.html
# ==========================================================================

# ==========================================================================
# Begin Example
set.seed(250)
timeseries=arima.sim(list(order = c(1,1,2), ma=c(0.32,0.47), ar=0.8), n = 50)+20

plot(timeseries)
elecequip %>% stl(s.window='periodic') %>% seasadj() -> eeadj
autoplot(eeadj)

# End Example
# ==========================================================================

source('~/git/functions/functions.r')
ipak(c('tidyverse', 'lubridate', 'data.table', 'dtplyr', 'qs'))

# ==========================================================================
# Load in data
# ==========================================================================

chi_ev10 <- fread('/Volumes/GoogleDrive/Shared drives/udpdata/raw/evictions/illinois/cook/output/chicago2010_aws_final.csv') %>% lazy_dt() 
chi_ev19 <- fread('/Volumes/GoogleDrive/Shared drives/udpdata/raw/evictions/illinois/cook/output/chicago2019_aws_final.csv') %>% lazy_dt() 

chi_ev <- 
	bind_rows(
		data.frame(chi_ev19) %>% mutate(ReturnToClerkData = as.character(ReturnToClerkData)), 
		data.frame(chi_ev10) %>% mutate(DistrictNum = as.numeric(DistrictNum)))

time <- 
	chi_ev %>% 
	mutate(
		fd_year = year(File_Date), 
		fd_month = month(File_Date), 
		fd_week = str_pad(week(File_Date), 2, pad = '0'), 
		fd_year_week = as.numeric(paste0(fd_year, fd_week))
	) %>% 
	filter(fd_year >= 2010 & fd_year < 2020) %>% 
	group_by(fd_year_week) %>% 
	summarize(filings = n())


	

# ==========================================================================
# ARIMA
# ==========================================================================

ggplot(time) + 
	geom_line(aes(x = fd_year_week, y = filings))
	