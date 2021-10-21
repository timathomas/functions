source("~/git/functions/functions.r")
ipak(c("tidyverse"))

hudfmr2 <- readRDS("~/git/functions/data/hudfmr.rds") %>% 
mutate(GEOID = as.numeric(fips2010))
co <- read_csv("/Users/timthomas/Downloads/all_counties_covered.csv")

left_join(co, hudfmr2) %>% write_csv("~/Downloads/el_co_fmr.csv")

source("/Users/timthomas/git/functions/Income_Gist.R")


hudfmr <- readRDS("~/git/functions/data/hudfmr.rds")

el <- read_csv("/Users/timthomas/Downloads/el_co_fmr.csv")

plots <- 
	map(states, function(state){
		df <- 
			bind_rows(
				hudfmr %>% 
					filter(State == "WA") %>% 
					group_by(year) %>% 
					mutate(
						st_fmr_avg_cpi = median(fmr_avg_cpi), 
						adj_co_name = case_when()
				),
				hudfmr %>% 
					filter(State == "WA") %>% 
					group_by(year) %>% 
					summarize(
						fmr_avg_cpi = mean(fmr_avg_cpi), 
						countyname = " Avg State FMR", 
						metro = 1
					)
				) 

df %>% 
	group_by(year, fmr_avg_cpi) %>% 
	filter(n() >= 2) %>% 
	group_by(year, countyname) %>% count() %>% data.frame()
	glimpse()

		counties <- 
			df %>% 
			filter(year == 2022) %>% 
			mutate(fmr_diff_st_co = 
				case_when(
					countyname == " Avg State FMR" ~ 1,
					fmr_avg - st_fmr_avg_cpi >= 200 ~ 1, 
					TRUE ~ 0)) %>% 
			filter(metro == 1, fmr_diff_st_co == 1) %>% 
			select(countyname) %>% 
			distinct() %>% 
			# arrange(countyname) %>% 
			pull(countyname) 

	    ggplot(data = df %>% filter(countyname %in% counties), aes(x = year, y = fmr_avg_cpi, color = countyname)) +
	      theme_minimal() +
	      geom_smooth(se = FALSE, position=position_jitter(w=0, h=8)) +
	      labs(title = str_c(df$State," fair market rent for all bedroom types (left)\n& income needed to avoid rent burden (right)"),
	           subtitle = "rent burden = 30% of income to rent",
	           y = "Rent",
	           x = "Year",
	           caption = "Author: Tim Thomas\nData source: HUD Fair Market Rent data &\nthe Bureau of Labor Statistics Consumer Price Index") +
	      # geom_text(
	      # 	data = df %>% filter(year == 2015, countyname %in% counties), 
	      # 	aes(label = countyname, 
       #    		x = year, 
       #    		y = fmr_avg_cpi
       #    	), 
       #    	# nudge_y = 20,
       #     #  nudge_x = -.5,
       # 		fontface = "bold",
       # 		# check_overlap = TRUE
	      # ) +
	      # coord_cartesian(ylim = c(700,2500)) +
	      scale_color_brewer("Counties",
	                         palette = "Paired",
	                         breaks=counties,
	                         labels=counties) +
	      scale_y_continuous(labels = scales::dollar,
	                         sec.axis = sec_axis(~(.*12)/.3, name = "Income needed to afford rent", labels = scales::dollar)) +
	      scale_x_continuous(breaks=c(2003:2022)) +
	      theme(axis.text.x = element_text(angle = -45, hjust = 0),
	          panel.grid.minor.x = element_blank())
})

