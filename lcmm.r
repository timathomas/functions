#####################################
# Testing LCMM Package for 			#
# categorizing neighborhood changes #
# Data: testing with LTDB 			#
#####################################

#########
# Start #
#########

	rm(list=ls()) #reset
	options(scipen=10) # avoid scientific notation

	library(lcmm)
	library(ggplot2)
	library(sf)
	library(stringr)
	library(tidyverse)

################
# Pull in Data #
################
	load("/Users/timthomas/Sync/data/NCDB/Raw/NCDB_1970_2010.RData")

	tracts <- read_sf("/Users/timthomas/Sync/data/census/Shapefiles/2010/US/Tracts/US_tract_2010.gpkg")

	# crime <- read.table("/Volumes/Net/Project/SeattleCrimeProject/Data/Output/CASgeo1-053015")

##############
# Clean Data #
##############
	# To match the NCDB data to the tract shapefiles, we need to
	# pad some of the NCDB GEO2010 cases with 0's to make it
	# 11 characters long. This is the length of the GEOID10 field
	# in the tracts shapefile.
		ncdb$GEO2010 <- str_pad(ncdb$GEO2010, 11, pad='0')

	# Subset the proportion of race by decade
	# Since I'm only looking at White, Black, Asian, and Latino,
	# I added these four groups under the variables of non-hispanic
	# white, black, Asian, and total hispanic. From here on out, white,
	# black, and Asian refer to non-hispanic. I create the total population
	# from these group numbers and then develop proportions from the
	# total of these groups. American Indian and other are ommitted
	# from the analysis.
		race <- ncdb%>%
				group_by(GEO2010) %>%
				select(SHRNHW8N, SHRNHB8N, SHRNHJ8N, SHRHSP8N, SHRNHW9N,SHRNHB9N,SHRNHA9N,SHRHSP9N, SHRNHW0N,SHRNHB0N,SHRNHA0N,SHRHSP0N, SHRNHW1N,SHRNHB1N,SHRNHA1N,SHRHSP1N) %>%
				summarise(tot80=sum(SHRNHW8N,SHRNHB8N,SHRNHJ8N,SHRHSP8N), #1980
							wht80=SHRNHW8N/tot80,
							blk80=SHRNHB8N/tot80,
							asi80=SHRNHJ8N/tot80,
							his80=SHRHSP8N/tot80,
							tot90=sum(SHRNHW9N,SHRNHB9N,SHRNHA9N,SHRHSP9N), #1990
							wht90=SHRNHW9N/tot90,
							blk90=SHRNHB9N/tot90,
							asi90=SHRNHA9N/tot90,
							his90=SHRHSP9N/tot90,
							tot00=sum(SHRNHW0N,SHRNHB0N,SHRNHA0N,SHRHSP0N), #2000
							wht00=SHRNHW0N/tot00,
							blk00=SHRNHB0N/tot00,
							asi00=SHRNHA0N/tot00,
							his00=SHRHSP0N/tot00,
							tot10=sum(SHRNHW1N,SHRNHB1N,SHRNHA1N,SHRHSP1N), #2010
							### MAJOR NOTE: SHRNHW1N is not in the code book from online but
							# IS in the dataset. The values seem realistic but need to confirm
							# if this is the actual variable name for non-hispanic white.
							wht10=SHRNHW1N/tot10,
							blk10=SHRNHB1N/tot10,
							asi10=SHRNHA1N/tot10,
							his00=SHRHSP1N/tot10)
	
	# coordinates(crime) <- c("MIDPT_X","MIDPT_Y")
	# writeOGR(crime, "/Volumes/Net/Project/SeattleCrimeProject/Data/Output/Shapefiles", "crime08_12", driver="ESRI Shapefile")



#############
# Plot data #
#############
	# Order data for plotting
	data <- race %>%


	# Attempt to create a stacked area plot
	wht <- cbind(race$wht80,race$wht90,race$wht00,race$wht10)
	colnames(wht) <- as.character()
	qplot()



########################################################




		r80 <- ncdb%>%
			tot80 <- summarise()
			mutate(tot80=summarise(sum(SHRNHW8N,SHRNHB8N,SHRNHJ8N,SHRHSP8N)))
			# mutate(tot80=sapply(c(SHRNHW8N,SHRNHB8N,SHRNHJ8N,SHRHSP8N),sum))%>%
			select(GEO2010,
				wht80=SHRNHW8N/tot80,
				blk80=SHRNHB8N/tot80,
				asi80=SHRNHJ8N/tot80,
				his80=SHRHSP8N/tot80
				)
glimpse(r80)


							# oth80= # Prop. Other
							# 1990
							wht90=SHRNHW9, 	# Prop. NH White
							blk90=SHRNHB9,	# Prop. NH Black
							asi90=SHRNHA9,	# Prop. NH Asian
							his90=SHRHSP9
			#gather(year,pop,-GEO2010) %>%
			#arrange(GEO2010)

		ggplot(xlong, aes(x = id, y = prop, fill = variable)) + geom_bar(stat = 'identity')


		r70 <- ncdb %>%
				mutate(asi70 = 0)%>% # Asian doesn't exist in 1970 data so I am creating the field with zeros.
				select(GEO2010, wht70=SHRWHT7, blk70=SHRBLK7, asi70, his70=SHRHSP7)%>%
				gather(year, pop, -GEO2010) %>% 	# reshape wide to long
				arrange(GEO2010)










	# Clean data = Race for each year
		cldf10 <- df10 %>% select(JOIN=JOINTID, POP10=pop10, WHT10=nhwht10, BLK10=nhblk10, ASA10=asian10, HIS10=hisp10)
		cldf00 <- df00 %>% select(JOIN=JOINTID, POP00=POP00, WHT00=NHWHT00, BLK00=NHBLK00, ASA00=ASIAN00, HIS00=HISP00)
		cldf00 <- df00 %>% select(JOIN=JOINTID, POP00=POP00, WHT00=NHWHT00, BLK00=NHBLK00, ASA00=ASIAN00, HIS00=HISP00)
		cldf90 <- df90 %>% select(JOIN=JOINTID, POP90=POP90, WHT90=NHWHT90, BLK90=NHBLK90, ASA90=ASIAN90, HIS90=HISP90)
		cldf80 <- df80 %>% select(JOIN=JOINTID, POP80=POP80, WHT80=NHWHT80, BLK80=NHBLK80, ASA80=ASIAN80, HIS80=HISP80)

	# Merge race data
		race <- merge(cldf10,cldf00, by="JOIN")
		race <- merge(race,cldf90, by="JOIN")
		race <- merge(race,cldf80, by="JOIN")



	# Subset Washington (state fips ==53)
		wa <- tracts[tracts$STATEFP10 =="53",]

	# Join LTDB data to WA shp
		wa <- merge(wa,race,by.x="GEOID10", by.y="JOIN")
		glimpse(wa@data)
		data <- wa@data[,-(2:15), drop=F]

#############
# Plot data #
#############

	# Subset race and make wide, long
		black <- data%>%						# data
			select(GEOID10,BLK80,BLK90,BLK00,BLK10) %>% # Select black
			gather(year, pop, -GEOID10) %>% 	# reshape wide to long
			arrange(GEOID10)					# order by JOIN id

		ggplot(data=black, aes(x=year,y=pop, group=1)) +
			# geom_point(alpha = 1/10) + #scale_x_log10() + scale_y_log10()+
			geom_smooth(method="loess")

			# geom_point(aes(size=pop), alpha=1/2)
			geom_line()

		qplot(data=black,pop,year, position=position_jitter(height=0.1), geom="point")
		qplot(year, mean(pop), data=black) +
			stat_smooth(method="loess")
			# geom_point()

	# clean
		df <-
		data %>%
				group_by(GEOID10) %>%
				summarise(race=spread(BLK80))
		gather(data,year, pop, -GEOID10)

#old way
		df2 <-	reshape(df,
			idvar="GEOID10",
			varying = c("BLK80","BLK90","BLK00","BLK10"),
			v.names="pop",
			timevar="year",
			times=c("BLK80","BLK90","BLK00","BLK10"),
			new.row.names=1:5784,
			direction = 'long')

		df2[order(df2$GEOID10),]

			df%>%gather()

#############
# LCMM Play #
#############
	d2 <-
		lcmm(pop~year, random=~year, subject="id", mixture=~year,ng=2,idiag=T,data=black,link="linear")


	ggplot(data=data, aes(x=))

	d1 <- lcmm()

df %>%
	group_by(GEOID10) %>%
	select(BLK80,BLK90,BLK00,BLK10)


stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

t1 <-
gather(stocks, stock, price, -time)
stocks %>% gather(stock, price, -time)

