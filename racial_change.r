# ==========================================================================
# LCMM racial change
# ==========================================================================

if(!require(pacman)) install.packages('pacman')
pacman::p_load_gh("jalvesaq/colorout")
pacman::p_load(tmap, lcmm, tigris, tidycensus, tidyverse)
options(scipen=10, width=Sys.getenv("COLUMNS"), tigris_use_cache = TRUE) # avoid scientific notation

# ==========================================================================
# Pull in data
# ==========================================================================

# LTDB

# ltdb70 <- read_csv("~/data/LTDB/LTDB_Std_All_fullcount/LTDB_Std_1970_fullcount.csv")
ltdb80 <- 
	read_csv("~/data/LTDB/LTDB_Std_All_fullcount/LTDB_Std_1980_fullcount.csv") %>%
	group_by(TRTID10) %>% 
	mutate(
		year = 1980, 
		other80 = POP80 - sum(NHWHT80, NHBLK80, ASIAN80, HISP80, na.rm = TRUE), 
		pwht = NHWHT80/POP80, 
		pblk = NHBLK80/POP80, 
		pasi = ASIAN80/POP80, 
		plat = HISP80/POP80, 
		poth = other80/POP80
		) %>% 
	select(TRTID10, year, pop = POP80, nhwht = NHWHT80, nhblk = NHBLK80, asian = ASIAN80, latinx = HISP80, other = other80, pwht, pblk, pasi, plat, poth) %>% 
	ungroup()

ltdb90 <- 
	read_csv("~/data/LTDB/LTDB_Std_All_fullcount/LTDB_Std_1990_fullcount.csv") %>%
	group_by(TRTID10) %>% 
	mutate(
		year = 1990, 
		other90 = POP90 - sum(NHWHT90, NHBLK90, ASIAN90, HISP90, na.rm = TRUE), 
		pwht = NHWHT90/POP90, 
		pblk = NHBLK90/POP90, 
		pasi = ASIAN90/POP90, 
		plat = HISP90/POP90, 
		poth = other90/POP90
		) %>% 
	select(TRTID10, year, pop = POP90, nhwht = NHWHT90, nhblk = NHBLK90, asian = ASIAN90, latinx = HISP90, other = other90, pwht, pblk, pasi, plat, poth) %>% 
	ungroup()

ltdb00 <- 
	read_csv("~/data/LTDB/LTDB_Std_All_fullcount/LTDB_Std_2000_fullcount.csv") %>%
	group_by(TRTID10) %>% 
	mutate(
		year = 2000, 
		other00 = POP00 - sum(NHWHT00, NHBLK00, ASIAN00, HISP00, na.rm = TRUE), 
		pwht = NHWHT00/POP00, 
		pblk = NHBLK00/POP00, 
		pasi = ASIAN00/POP00, 
		plat = HISP00/POP00, 
		poth = other00/POP00
		) %>% 
	select(TRTID10, year, pop = POP00, nhwht = NHWHT00, nhblk = NHBLK00, asian = ASIAN00, latinx = HISP00, other = other00, pwht, pblk, pasi, plat, poth) %>% 
	ungroup()

ltdb10 <- 
	read_csv("~/data/LTDB/LTDB_Std_All_fullcount/LTDB_Std_2010_fullcount.csv") %>%
	group_by(TRTID10 = tractid) %>% 
	mutate(
		year = 2010, 
		other10 = pop10 - sum(nhwht10, nhblk10, asian10, hisp10, na.rm = TRUE), 
		pwht = nhwht10/pop10, 
		pblk = nhblk10/pop10, 
		pasi = asian10/pop10, 
		plat = hisp10/pop10, 
		poth = other10/pop10
		) %>%
	select(TRTID10, year, pop = pop10, nhwht = nhwht10, nhblk = nhblk10, asian = asian10, latinx = hisp10, other = other10, pwht, pblk, pasi, plat, poth) %>% 
	ungroup()

#
# Download Census Data
# --------------------------------------------------------------------------

# vars <- 
# 	c('pop' = 'B03002_001',
# 	'nhwht' = 'B03002_003',
# 	'nhblk' = 'B03002_004',
# 	'asian' = 'B03002_006',
# 	'latinx' = 'B03002_012')

# acsrace <- 
# 	get_acs(
# 		state = "WA", 
# 		county = "King", 
# 		variables = vars,
# 		geography = "tract", 
# 		output = "wide"
# 		) %>% 
# 	select(-ends_with("M")) %>% 
# 	group_by(GEOID) %>% 
# 	mutate(
# 		pop = popE,
# 		nhwht = nhwhtE,
# 		nhblk = nhblkE,
# 		asian = asianE,
# 		latinx = latinxE,
# 		TRTID10 = as.numeric(GEOID), 
# 		year = 2015,
# 		other = sum(nhwht, nhblk, asian, latinx, na.rm = TRUE), 
# 		pwht = nhwht/pop, 
# 		pblk = nhblk/pop, 
# 		pasi = asian/pop, 
# 		plat = latinx/pop, 
# 		poth = other/pop) %>% 
# 	ungroup() %>% 
# 	select(TRTID10, year, pop, nhwht, nhblk, asian, latinx, other, pwht, pblk, pasi, plat, poth) 

# saveRDS(acsrace, "~/git/eviction_segregation_paper/data/outputs/acs_race_2015.rds")
acsrace <- readRDS("~/git/eviction_segregation_paper/data/outputs/acs_race_2015.rds")

df <- 
	bind_rows(
		acsrace, 
		ltdb10 %>% filter(TRTID10 %in% acsrace$TRTID10), 
		ltdb00 %>% filter(TRTID10 %in% acsrace$TRTID10), 
		ltdb90 %>% filter(TRTID10 %in% acsrace$TRTID10), 
		ltdb80 %>% filter(TRTID10 %in% acsrace$TRTID10)) %>% 
	as.data.frame()

# ==========================================================================
# Change from 1980 to 2015
# ==========================================================================

df15 <- 
	df %>% 
	filter(year == 2015) %>% 
	select(-year) %>% 
	rename_at(vars(pop:poth), function(x) paste0(x, "_2015"))

df80 <- 
	df %>% 
	filter(year == 1980) %>% 
	select(-year) %>% 
	rename_at(vars(pop:poth), function(x) paste0(x, "_1980"))

dfdiff <- 
	left_join(df15, df80) %>% 
	glimpse() %>% 
	mutate(
		pop_dif = pop_2015 - pop_1980,
		nhwht_dif = nhwht_2015 - nhwht_1980,
		nhblk_dif = nhblk_2015 - nhblk_1980,
		asian_dif = asian_2015 - asian_1980,
		latinx_dif = latinx_2015 - latinx_1980,
		other_dif = other_2015 - other_1980,
		pwht_dif = pwht_2015 - pwht_1980,
		pblk_dif = pblk_2015 - pblk_1980,
		pasi_dif = pasi_2015 - pasi_1980,
		plat_dif = plat_2015 - plat_1980,
		poth_dif = poth_2015 - poth_1980
		) 

saveRDS(dfdiff, "~/git/eviction_segregation_paper/data/outputs/racial_change.rds")

# ==========================================================================
# Map
# ==========================================================================

kctracts <- 
	tracts(state = "WA", county = "King", cb = TRUE) %>% 
	left_join(dfdiff %>% mutate(GEOID = as.character(TRTID10)))

tmap_mode("view")

tm_shape(kctracts) + 
	tm_fill(
		c("pwht_dif", "pblk_dif", "pasi_dif", "plat_dif"),
		# n = 6,
		style = "fixed", 
		breaks = c(-.9, -.5, -.25, -.05, .05, .25, .5, .9)
		) + 
	tm_borders(alpha = .5) + 
  	tm_layout(legend.position = c("right", "bottom"))

# ==========================================================================
# LCMM Setup
# ==========================================================================

mlcmm <- 
	function(x){
		multlcmm(
			pwht + pblk + pasi + plat ~ 1 + year, 
			subject = "TRTID10", 
			link = "linear", 
			ng = x, 
			mixture = ~1+year, 
			random = ~1+year, 
			data = df)
	}

t2 <- mlcmm(2); t2$BIC
t3 <- mlcmm(3); t3$BIC
t4 <- mlcmm(4); t4$BIC
t5 <- mlcmm(5); t5$BIC
t6 <- mlcmm(6); t6$BIC
t7 <- mlcmm(7); t7$BIC
t8 <- mlcmm(8); t8$BIC
t9 <- mlcmm(9); t9$BIC
t10 <- mlcmm(10); t10$BIC

summary(t2); postprob(t2); plot(t2,which="linkfunction")
summary(t3); postprob(t3); plot(t3,which="linkfunction")
summary(t4); postprob(t4); plot(t4,which="linkfunction")
summary(t5); postprob(t5); plot(t5,which="linkfunction")
summary(t6); postprob(t6); plot(t6,which="linkfunction")
summary(t7); postprob(t7); plot(t7,which="linkfunction")
summary(t8); postprob(t8); plot(t8,which="linkfunction")
summary(t9); postprob(t9); plot(t9,which="linkfunction")
summary(t10); postprob(t10); plot(t10,which="linkfunction")