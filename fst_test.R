# ==========================================================================
# TESTBED

	librarian::shelf(fst, tidyfst, bit64, tidyverse)
system.time(
	try <- fst("~/data/raw/dataaxle/US_Consumer_5_File_2019.fst")
	)
	try
	# This code pulls all the FAMILYIDs in Delaware
system.time(
	fis <- try %>% select_fst(FAMILYID, STATE) %>% filter_fst(STATE == "CA")
	)
	class(fis)
	fis



	# system.time(
	# 	sliced <- try %>% filter_fst(FAMILYID == 9008)
	# 	)

	class(sliced)

	glimpse(sliced)
	print(sliced)

	librarian::shelf(fst, tidyfst)
	ft <- parse_fst("~/data/raw/dataaxle/US_Consumer_5_File_2019.fst")
	ft
system.time(
	sliced <- ft %>% filter_fst(FAMILYID == 1754224397)
	)


# END TESTBED
# ==========================================================================