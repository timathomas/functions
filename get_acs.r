# ==========================================================================
# Mobility Functions
# ==========================================================================

install.packages("pacman")
pacman::p_load(data.table,tidycensus,tidyverse, update = TRUE)

# ==========================================================================
# Variables
# ==========================================================================

#
# Geographic mobility variables 
# --------------------------------------------------------------------------

mob_inc_vars <- 
    c(
    'tr_mob_tot' = 'B07010_001', # Estimate!!Total
    'tr_mob_tot_noinc' = 'B07010_002', # Estimate!!Total!!No income
    'tr_mob_tot_winc' = 'B07010_003', # Estimate!!Total!!With income
    'tr_mob_tot_9999' = 'B07010_004', # Estimate!!Total!!With income!!$1 to $9 999 or loss
    'tr_mob_tot_14999' = 'B07010_005', # Estimate!!Total!!With income!!$10 000 to $14 999
    'tr_mob_tot_24999' = 'B07010_006', # Estimate!!Total!!With income!!$15 000 to $24 999
    'tr_mob_tot_34999' = 'B07010_007', # Estimate!!Total!!With income!!$25 000 to $34 999
    'tr_mob_tot_49999' = 'B07010_008', # Estimate!!Total!!With income!!$35 000 to $49 999
    'tr_mob_tot_64999' = 'B07010_009', # Estimate!!Total!!With income!!$50 000 to $64 999
    'tr_mob_tot_74999' = 'B07010_010', # Estimate!!Total!!With income!!$65 000 to $74 999
    'tr_mob_tot_75000' = 'B07010_011', # Estimate!!Total!!With income!!$75 000 or more
    'tr_mob_stay_tot' = 'B07010_012', # Estimate!!Total!!Same house 1 year ago
    'tr_mob_stay_noinc' = 'B07010_013', # Estimate!!Total!!Same house 1 year ago!!No income
    'tr_mob_stay_winc' = 'B07010_014', # Estimate!!Total!!Same house 1 year ago!!With income
    'tr_mob_stay_9999' = 'B07010_015', # Estimate!!Total!!Same house 1 year ago!!With income!!$1 to $9 999 or loss
    'tr_mob_stay_14999' = 'B07010_016', # Estimate!!Total!!Same house 1 year ago!!With income!!$10 000 to $14 999
    'tr_mob_stay_24999' = 'B07010_017', # Estimate!!Total!!Same house 1 year ago!!With income!!$15 000 to $24 999
    'tr_mob_stay_34999' = 'B07010_018', # Estimate!!Total!!Same house 1 year ago!!With income!!$25 000 to $34 999
    'tr_mob_stay_49999' = 'B07010_019', # Estimate!!Total!!Same house 1 year ago!!With income!!$35 000 to $49 999
    'tr_mob_stay_64999' = 'B07010_020', # Estimate!!Total!!Same house 1 year ago!!With income!!$50 000 to $64 999
    'tr_mob_stay_74999' = 'B07010_021', # Estimate!!Total!!Same house 1 year ago!!With income!!$65 000 to $74 999
    'tr_mob_stay_75000' = 'B07010_022', # Estimate!!Total!!Same house 1 year ago!!With income!!$75 000 or more
    'tr_mob_smco_tot' = 'B07010_023', # Estimate!!Total!!Moved within same county
    'tr_mob_smco_noinc' = 'B07010_024', # Estimate!!Total!!Moved within same county!!No income
    'tr_mob_smco_winc' = 'B07010_025', # Estimate!!Total!!Moved within same county!!With income
    'tr_mob_smco_9999' = 'B07010_026', # Estimate!!Total!!Moved within same county!!With income!!$1 to $9 999 or loss
    'tr_mob_smco_14999' = 'B07010_027', # Estimate!!Total!!Moved within same county!!With income!!$10 000 to $14 999
    'tr_mob_smco_24999' = 'B07010_028', # Estimate!!Total!!Moved within same county!!With income!!$15 000 to $24 999
    'tr_mob_smco_34999' = 'B07010_029', # Estimate!!Total!!Moved within same county!!With income!!$25 000 to $34 999
    'tr_mob_smco_49999' = 'B07010_030', # Estimate!!Total!!Moved within same county!!With income!!$35 000 to $49 999
    'tr_mob_smco_64999' = 'B07010_031', # Estimate!!Total!!Moved within same county!!With income!!$50 000 to $64 999
    'tr_mob_smco_74999' = 'B07010_032', # Estimate!!Total!!Moved within same county!!With income!!$65 000 to $74 999
    'tr_mob_smco_75000' = 'B07010_033', # Estimate!!Total!!Moved within same county!!With income!!$75 000 or more
    'tr_mob_difcosmst_tot' = 'B07010_034', # Estimate!!Total!!Moved from different county within same state
    'tr_mob_difcosmst_noinc' = 'B07010_035', # Estimate!!Total!!Moved from different county within same state!!No income
    'tr_mob_difcosmst_winc' = 'B07010_036', # Estimate!!Total!!Moved from different county within same state!!With income
    'tr_mob_difcosmst_9999' = 'B07010_037', # Estimate!!Total!!Moved from different county within same state!!With income!!$1 to $9 999 or loss
    'tr_mob_difcosmst_14999' = 'B07010_038', # Estimate!!Total!!Moved from different county within same state!!With income!!$10 000 to $14 999
    'tr_mob_difcosmst_24999' = 'B07010_039', # Estimate!!Total!!Moved from different county within same state!!With income!!$15 000 to $24 999
    'tr_mob_difcosmst_34999' = 'B07010_040', # Estimate!!Total!!Moved from different county within same state!!With income!!$25 000 to $34 999
    'tr_mob_difcosmst_49999' = 'B07010_041', # Estimate!!Total!!Moved from different county within same state!!With income!!$35 000 to $49 999
    'tr_mob_difcosmst_64999' = 'B07010_042', # Estimate!!Total!!Moved from different county within same state!!With income!!$50 000 to $64 999
    'tr_mob_difcosmst_74999' = 'B07010_043', # Estimate!!Total!!Moved from different county within same state!!With income!!$65 000 to $74 999
    'tr_mob_difcosmst_75000' = 'B07010_044', # Estimate!!Total!!Moved from different county within same state!!With income!!$75 000 or more
    'tr_mob_difst_tot' = 'B07010_045', # Estimate!!Total!!Moved from different state
    'tr_mob_difst_noinc' = 'B07010_046', # Estimate!!Total!!Moved from different state!!No income
    'tr_mob_difst_winc' = 'B07010_047', # Estimate!!Total!!Moved from different state!!With income
    'tr_mob_difst_9999' = 'B07010_048', # Estimate!!Total!!Moved from different state!!With income!!$1 to $9 999 or loss
    'tr_mob_difst_14999' = 'B07010_049', # Estimate!!Total!!Moved from different state!!With income!!$10 000 to $14 999
    'tr_mob_difst_24999' = 'B07010_050', # Estimate!!Total!!Moved from different state!!With income!!$15 000 to $24 999
    'tr_mob_difst_34999' = 'B07010_051', # Estimate!!Total!!Moved from different state!!With income!!$25 000 to $34 999
    'tr_mob_difst_49999' = 'B07010_052', # Estimate!!Total!!Moved from different state!!With income!!$35 000 to $49 999
    'tr_mob_difst_64999' = 'B07010_053', # Estimate!!Total!!Moved from different state!!With income!!$50 000 to $64 999
    'tr_mob_difst_74999' = 'B07010_054', # Estimate!!Total!!Moved from different state!!With income!!$65 000 to $74 999
    'tr_mob_difst_75000' = 'B07010_055', # Estimate!!Total!!Moved from different state!!With income!!$75 000 or more
    'tr_mob_abroad_tot' = 'B07010_056', # Estimate!!Total!!Moved from abroad
    'tr_mob_abroad_noinc' = 'B07010_057', # Estimate!!Total!!Moved from abroad!!No income
    'tr_mob_abroad_winc' = 'B07010_058', # Estimate!!Total!!Moved from abroad!!With income
    'tr_mob_abroad_9999' = 'B07010_059', # Estimate!!Total!!Moved from abroad!!With income!!$1 to $9 999 or loss
    'tr_mob_abroad_14999' = 'B07010_060', # Estimate!!Total!!Moved from abroad!!With income!!$10 000 to $14 999
    'tr_mob_abroad_24999' = 'B07010_061', # Estimate!!Total!!Moved from abroad!!With income!!$15 000 to $24 999
    'tr_mob_abroad_34999' = 'B07010_062', # Estimate!!Total!!Moved from abroad!!With income!!$25 000 to $34 999
    'tr_mob_abroad_49999' = 'B07010_063', # Estimate!!Total!!Moved from abroad!!With income!!$35 000 to $49 999
    'tr_mob_abroad_64999' = 'B07010_064', # Estimate!!Total!!Moved from abroad!!With income!!$50 000 to $64 999
    'tr_mob_abroad_74999' = 'B07010_065', # Estimate!!Total!!Moved from abroad!!With income!!$65 000 to $74 999
    'tr_mob_abroad_75000' = 'B07010_066' # Estimate!!Total!!Moved from abroad!!With income!!$75 000 or more
    )

## Income Variables

inc_vars <- c(
    'HHInc_Total' = 'B19001_001', # Total HOUSEHOLD INCOME
    'HHInc_10' = 'B19001_002', # Less than $10,000 HOUSEHOLD INCOME
    'HHInc_15' = 'B19001_003', # $10,000 to $14,999 HOUSEHOLD INCOME
    'HHInc_20' = 'B19001_004', # $15,000 to $19,999 HOUSEHOLD INCOME
    'HHInc_25' = 'B19001_005', # $20,000 to $24,999 HOUSEHOLD INCOME
    'HHInc_30' = 'B19001_006', # $25,000 to $29,999 HOUSEHOLD INCOME
    'HHInc_35' = 'B19001_007', # $30,000 to $34,999 HOUSEHOLD INCOME
    'HHInc_40' = 'B19001_008', # $35,000 to $39,999 HOUSEHOLD INCOME
    'HHInc_45' = 'B19001_009', # $40,000 to $44,999 HOUSEHOLD INCOME
    'HHInc_50' = 'B19001_010', # $45,000 to $49,999 HOUSEHOLD INCOME
    'HHInc_60' = 'B19001_011', # $50,000 to $59,999 HOUSEHOLD INCOME
    'HHInc_75' = 'B19001_012', # $60,000 to $74,999 HOUSEHOLD INCOME
    'HHInc_100' = 'B19001_013', # $75,000 to $99,999 HOUSEHOLD INCOME
    'HHInc_125' = 'B19001_014', # $100,000 to $124,999 HOUSEHOLD INCOME
    'HHInc_150' = 'B19001_015', # $125,000 to $149,999 HOUSEHOLD INCOME
    'HHInc_200' = 'B19001_016', # $150,000 to $199,999 HOUSEHOLD INCOME
    'HHInc_250' = 'B19001_017', # $200,000 or more HOUSEHOLD INCOME, 
    'mhhinc' = 'B19013_001'
)

## Income by Rent burden

ir_var17 <- c(
    'ir_tot_tot' = 'B25074_001',# Estimate!!Total
    'ir_tot_9999' = 'B25074_002', # Estimate!!Total!!Less than $10 000
    'ir_19_9999' = 'B25074_003', # Estimate!!Total!!Less than $10 000!!Less than 20.0 percent
    'ir_249_9999' = 'B25074_004', # Estimate!!Total!!Less than $10 000!!20.0 to 24.9 percent
    'ir_299_9999' = 'B25074_005', # Estimate!!Total!!Less than $10 000!!25.0 to 29.9 percent
    'ir_349_9999' = 'B25074_006', # Estimate!!Total!!Less than $10 000!!30.0 to 34.9 percent
    'ir_399_9999' = 'B25074_007', # Estimate!!Total!!Less than $10 000!!35.0 to 39.9 percent
    'ir_499_9999' = 'B25074_008', # Estimate!!Total!!Less than $10 000!!40.0 to 49.9 percent
    'ir_5plus_9999' = 'B25074_009', # Estimate!!Total!!Less than $10 000!!50.0 percent or more
    'ir_x_9999' = 'B25074_010', # Estimate!!Total!!Less than $10 000!!Not computed
    'ir_tot_19999' = 'B25074_011', # Estimate!!Total!!$10 000 to $19 999
    'ir_19_19999' = 'B25074_012', # Estimate!!Total!!$10 000 to $19 999!!Less than 20.0 percent
    'ir_249_19999' = 'B25074_013', # Estimate!!Total!!$10 000 to $19 999!!20.0 to 24.9 percent
    'ir_299_19999' = 'B25074_014', # Estimate!!Total!!$10 000 to $19 999!!25.0 to 29.9 percent
    'ir_349_19999' = 'B25074_015', # Estimate!!Total!!$10 000 to $19 999!!30.0 to 34.9 percent
    'ir_399_19999' = 'B25074_016', # Estimate!!Total!!$10 000 to $19 999!!35.0 to 39.9 percent
    'ir_499_19999' = 'B25074_017', # Estimate!!Total!!$10 000 to $19 999!!40.0 to 49.9 percent
    'ir_5plus_19999' = 'B25074_018', # Estimate!!Total!!$10 000 to $19 999!!50.0 percent or more
    'ir_x_19999' = 'B25074_019', # Estimate!!Total!!$10 000 to $19 999!!Not computed
    'ir_tot_34999' = 'B25074_020', # Estimate!!Total!!$20 000 to $34 999
    'ir_19_34999' = 'B25074_021', # Estimate!!Total!!$20 000 to $34 999!!Less than 20.0 percent
    'ir_249_34999' = 'B25074_022', # Estimate!!Total!!$20 000 to $34 999!!20.0 to 24.9 percent
    'ir_299_34999' = 'B25074_023', # Estimate!!Total!!$20 000 to $34 999!!25.0 to 29.9 percent
    'ir_349_34999' = 'B25074_024', # Estimate!!Total!!$20 000 to $34 999!!30.0 to 34.9 percent
    'ir_399_34999' = 'B25074_025', # Estimate!!Total!!$20 000 to $34 999!!35.0 to 39.9 percent
    'ir_499_34999' = 'B25074_026', # Estimate!!Total!!$20 000 to $34 999!!40.0 to 49.9 percent
    'ir_5plus_34999' = 'B25074_027', # Estimate!!Total!!$20 000 to $34 999!!50.0 percent or more
    'ir_x_34999' = 'B25074_028', # Estimate!!Total!!$20 000 to $34 999!!Not computed
    'ir_tot_49999' = 'B25074_029', # Estimate!!Total!!$35 000 to $49 999
    'ir_19_49999' = 'B25074_030', # Estimate!!Total!!$35 000 to $49 999!!Less than 20.0 percent
    'ir_249_49999' = 'B25074_031', # Estimate!!Total!!$35 000 to $49 999!!20.0 to 24.9 percent
    'ir_299_49999' = 'B25074_032', # Estimate!!Total!!$35 000 to $49 999!!25.0 to 29.9 percent
    'ir_349_49999' = 'B25074_033', # Estimate!!Total!!$35 000 to $49 999!!30.0 to 34.9 percent
    'ir_399_49999' = 'B25074_034', # Estimate!!Total!!$35 000 to $49 999!!35.0 to 39.9 percent
    'ir_499_49999' = 'B25074_035', # Estimate!!Total!!$35 000 to $49 999!!40.0 to 49.9 percent
    'ir_5plus_49999' = 'B25074_036', # Estimate!!Total!!$35 000 to $49 999!!50.0 percent or more
    'ir_x_49999' = 'B25074_037', # Estimate!!Total!!$35 000 to $49 999!!Not computed
    'ir_tot_74999' = 'B25074_038', # Estimate!!Total!!$50 000 to $74 999
    'ir_19_74999' = 'B25074_039', # Estimate!!Total!!$50 000 to $74 999!!Less than 20.0 percent
    'ir_249_74999' = 'B25074_040', # Estimate!!Total!!$50 000 to $74 999!!20.0 to 24.9 percent
    'ir_299_74999' = 'B25074_041', # Estimate!!Total!!$50 000 to $74 999!!25.0 to 29.9 percent
    'ir_349_74999' = 'B25074_042', # Estimate!!Total!!$50 000 to $74 999!!30.0 to 34.9 percent
    'ir_399_74999' = 'B25074_043', # Estimate!!Total!!$50 000 to $74 999!!35.0 to 39.9 percent
    'ir_499_74999' = 'B25074_044', # Estimate!!Total!!$50 000 to $74 999!!40.0 to 49.9 percent
    'ir_5plus_74999' = 'B25074_045', # Estimate!!Total!!$50 000 to $74 999!!50.0 percent or more
    'ir_x_74999' = 'B25074_046', # Estimate!!Total!!$50 000 to $74 999!!Not computed
    'ir_tot_99999' = 'B25074_047', # Estimate!!Total!!$75 000 to $99 999
    'ir_19_99999' = 'B25074_048', # Estimate!!Total!!$75 000 to $99 999!!Less than 20.0 percent
    'ir_249_99999' = 'B25074_049', # Estimate!!Total!!$75 000 to $99 999!!20.0 to 24.9 percent
    'ir_299_99999' = 'B25074_050', # Estimate!!Total!!$75 000 to $99 999!!25.0 to 29.9 percent
    'ir_349_99999' = 'B25074_051', # Estimate!!Total!!$75 000 to $99 999!!30.0 to 34.9 percent
    'ir_399_99999' = 'B25074_052', # Estimate!!Total!!$75 000 to $99 999!!35.0 to 39.9 percent
    'ir_499_99999' = 'B25074_053', # Estimate!!Total!!$75 000 to $99 999!!40.0 to 49.9 percent
    'ir_5plus_99999' = 'B25074_054', # Estimate!!Total!!$75 000 to $99 999!!50.0 percent or more
    'ir_x_99999' = 'B25074_055', # Estimate!!Total!!$75 000 to $99 999!!Not computed
    'ir_tot_100000' = 'B25074_056', # Estimate!!Total!!$100 000 or more
    'ir_19_100000' = 'B25074_057', # Estimate!!Total!!$100 000 or more!!Less than 20.0 percent
    'ir_249_100000' = 'B25074_058', # Estimate!!Total!!$100 000 or more!!20.0 to 24.9 percent
    'ir_299_100000' = 'B25074_059', # Estimate!!Total!!$100 000 or more!!25.0 to 29.9 percent
    'ir_349_100000' = 'B25074_060', # Estimate!!Total!!$100 000 or more!!30.0 to 34.9 percent
    'ir_399_100000' = 'B25074_061', # Estimate!!Total!!$100 000 or more!!35.0 to 39.9 percent
    'ir_499_100000' = 'B25074_062', # Estimate!!Total!!$100 000 or more!!40.0 to 49.9 percent
    'ir_5plus_100000' = 'B25074_063', # Estimate!!Total!!$100 000 or more!!50.0 percent or more
    'ir_x_100000' = 'B25074_064' # Estimate!!Total!!$100 000 or more!!Not computed
    )    

ir_var12 <- c(
'ir_tot_tot' = 'B25074_001', # Total:
'ir_tot_9999' = 'B25074_002', # Less than $10,000:
    'ir_19_9999' = 'B25074_003', # Less than $10,000:!!Less than 20.0 percent
    'ir_249_9999' = 'B25074_004', # Less than $10,000:!!20.0 to 24.9 percent
    'ir_299_9999' = 'B25074_005', # Less than $10,000:!!25.0 to 29.9 percent
    'ir_349_9999' = 'B25074_006', # Less than $10,000:!!30.0 to 34.9 percent
    'ir_399_9999' = 'B25074_007', # Less than $10,000:!!35.0 percent or more
    'ir_x_9999' = 'B25074_008', # Less than $10,000:!!Not computed
'ir_tot_19999' = 'B25074_009', # $10,000 to $19,999:
    'ir_19_19999' = 'B25074_010', # $10,000 to $19,999:!!Less than 20.0 percent
    'ir_249_19999' = 'B25074_011', # $10,000 to $19,999:!!20.0 to 24.9 percent
    'ir_299_19999' = 'B25074_012', # $10,000 to $19,999:!!25.0 to 29.9 percent
    'ir_349_19999' = 'B25074_013', # $10,000 to $19,999:!!30.0 to 34.9 percent
    'ir_399_19999' = 'B25074_014', # $10,000 to $19,999:!!35.0 percent or more
    'ir_x_19999' = 'B25074_015', # $10,000 to $19,999:!!Not computed
'ir_tot_34999' = 'B25074_016', # $20,000 to $34,999:
    'ir_19_34999' = 'B25074_017', # $20,000 to $34,999:!!Less than 20.0 percent
    'ir_249_34999' = 'B25074_018', # $20,000 to $34,999:!!20.0 to 24.9 percent
    'ir_299_34999' = 'B25074_019', # $20,000 to $34,999:!!25.0 to 29.9 percent
    'ir_349_34999' = 'B25074_020', # $20,000 to $34,999:!!30.0 to 34.9 percent
    'ir_399_34999' = 'B25074_021', # $20,000 to $34,999:!!35.0 percent or more
    'ir_x_34999' = 'B25074_022', # $20,000 to $34,999:!!Not computed
'ir_tot_49999' = 'B25074_023', # $35,000 to $49,999:
    'ir_19_49999' = 'B25074_024', # $35,000 to $49,999:!!Less than 20.0 percent
    'ir_249_49999' = 'B25074_025', # $35,000 to $49,999:!!20.0 to 24.9 percent
    'ir_299_49999' = 'B25074_026', # $35,000 to $49,999:!!25.0 to 29.9 percent
    'ir_349_49999' = 'B25074_027', # $35,000 to $49,999:!!30.0 to 34.9 percent
    'ir_399_49999' = 'B25074_028', # $35,000 to $49,999:!!35.0 percent or more
    'ir_x_49999' = 'B25074_029', # $35,000 to $49,999:!!Not computed
'ir_tot_74999' = 'B25074_030', # $50,000 to $74,999:
    'ir_19_74999' = 'B25074_031', # $50,000 to $74,999:!!Less than 20.0 percent
    'ir_249_74999' = 'B25074_032', # $50,000 to $74,999:!!20.0 to 24.9 percent
    'ir_299_74999' = 'B25074_033', # $50,000 to $74,999:!!25.0 to 29.9 percent
    'ir_349_74999' = 'B25074_034', # $50,000 to $74,999:!!30.0 to 34.9 percent
    'ir_399_74999' = 'B25074_035', # $50,000 to $74,999:!!35.0 percent or more
    'ir_x_74999' = 'B25074_036', # $50,000 to $74,999:!!Not computed
'ir_tot_99999' = 'B25074_037', # $75,000 to $99,999:
    'ir_19_99999' = 'B25074_038', # $75,000 to $99,999:!!Less than 20.0 percent
    'ir_249_99999' = 'B25074_039', # $75,000 to $99,999:!!20.0 to 24.9 percent
    'ir_299_99999' = 'B25074_040', # $75,000 to $99,999:!!25.0 to 29.9 percent
    'ir_349_99999' = 'B25074_041', # $75,000 to $99,999:!!30.0 to 34.9 percent
    'ir_399_99999' = 'B25074_042', # $75,000 to $99,999:!!35.0 percent or more
    'ir_x_99999' = 'B25074_043', # $75,000 to $99,999:!!Not computed
'ir_tot_100000' = 'B25074_044', # $100,000 or more:
    'ir_19_100000' = 'B25074_045', # $100,000 or more:!!Less than 20.0 percent
    'ir_249_100000' = 'B25074_046', # $100,000 or more:!!20.0 to 24.9 percent
    'ir_299_100000' = 'B25074_047', # $100,000 or more:!!25.0 to 29.9 percent
    'ir_349_100000' = 'B25074_048', # $100,000 or more:!!30.0 to 34.9 percent
    'ir_399_100000' = 'B25074_049', # $100,000 or more:!!35.0 percent or more
    'ir_x_100000' = 'B25074_050' # $100,000 or more:!!Not computed
)
## Race variables

race_vars <- c(
    'race_tot' = 'B03002_001',
    'race_White' = 'B03002_003',
    'race_Black' = 'B03002_004',
    'race_Asian' = 'B03002_006',
    'race_Latinx' = 'B03002_012'
)

## Rent & income variables

rent_vars <- c(
    'medrent' = 'B25064_001', 
    'totten' = 'B25003_001', # Total # households
    'totrent' = 'B25003_003', # total # renting households
    'totown' = 'B25003_002',
    'tottenWHT' = 'B25003A_001',
    'totownWHT' = 'B25003A_002',
    'totrentWHT' = 'B25003A_003',
    'tottenBLK' = 'B25003B_001',
    'totownBLK' = 'B25003B_002',
    'totrentBLK' = 'B25003B_003',
    'tottenAIAN' = 'B25003C_001',
    'totownAIAN' = 'B25003C_002',
    'totrentAIAN' = 'B25003C_003',
    'tottenASI' = 'B25003D_001',
    'totownASI' = 'B25003D_002',
    'totrentASI' = 'B25003D_003',
    'tottenNHOP' = 'B25003E_001',
    'totownNHOP' = 'B25003E_002',
    'totrentNHOP' = 'B25003E_003',
    'tottenOTH' = 'B25003F_001',
    'totownOTH' = 'B25003F_002',
    'totrentOTH' = 'B25003F_003',
    'tottenTWO' = 'B25003G_001',
    'totownTWO' = 'B25003G_002',
    'totrentTWO' = 'B25003G_003',
    'tottenWHTNL' = 'B25003H_001',
    'totownWHTNL' = 'B25003H_002',
    'totrentWHTNL' = 'B25003H_003',
    'tottenLAT' = 'B25003I_001',
    'totownLAT' = 'B25003I_002',
    'totrentLAT' = 'B25003I_003',
    'vac_tot' = 'B25004_001',
    'vac_rent' = 'B25004_003',
    'mgrent' = 'B25064_001'
)


# ==========================================================================
# Function
# ==========================================================================

#
# Tidycensus data pull
# --------------------------------------------------------------------------

get_acs_data <- function(
    vars, 
    years, 
    survey = "acs5", 
    geography = "tract",
    state = "WA", 
    county = NULL, 
    geometry = FALSE, 
    cache_table = TRUE,
    output = "tidy", 
    keep_geo_vars = FALSE){
        map_dfr(years, function(year){
            get_acs(
                geography = geography,
                variables = vars,
                year = year,
                state = state,
                county = county,
                geometry = geometry,
                cache_table = cache_table,
                output = output,
                keep_geo_vars = keep_geo_vars, 
                survey = survey
                ) %>% 
            mutate(year = year, 
                   survey = survey)
        })
}
