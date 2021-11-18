pacman::p_load(tidyverse, plotly, leaflet, flexdashboard, tidycensus)

black_rent <- 
	get_acs(
		geography = 'tract', 
		variables = c('mhhinc_blk' = 'B19013B_001'), 
		county = 'Alameda', 
		state = 'CA', 
		cb = TRUE, 
		geometry = TRUE, 
		output = 'wide'
	)

# Plot 1
p<-plot_ly()
for (i in 1:length(dataset1)) {
p<-add_trace(p,name=dataset1[[i]]$Codi.Estació[1],x=dataset1[[i]]$Data,y=dataset1[[i]]$Valor,mode = 'scatter',type="scatter")
}
p

ggplotly(
	black_rent %>% 
	ggplot(aes(mhhinc_blkE)) +
	geom_bar(state = 'identity')
)

m <- 
	leaflet(black_rent) %>%
	# addTiles() %>% 
	addPolygons(
		fillColor = ~estimate,
		label=~scales::dollar(estimate),
		labelOptions = labelOptions(noHide = T, textOnly = TRUE), 
		weight = .2
)

m