# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Jan 23, 2025
# Last updated: Jan 24, 2025

library(tidyverse)
library(dplyr)

####################################################################################################################
## PARAMETERIZATION: Edit these prior to running, customized for the specific NEP site/region: (with default values)
####################################################################################################################
# For Gross-Range Test:
ph_user_min = 6
ph_user_max = 9
temp_user_min = -1
temp_user_max = 35
sal_user_min = 0
sal_user_max = 35
pco2_user_min = 100
pco2_user_max = 2500
do_user_min = 5
do_user_max = 20
# sensor min/max's
ph_sensor_min = 0
ph_sensor_max = 14
temp_sensor_min = -10
temp_sensor_max = 45
sal_sensor_min = -1
sal_sensor_max = 50
pco2_sensor_min = 0
pco2_sensor_max = 3500
do_sensor_min = 0
do_sensor_max = 25
# For Rate-of-Change Test:
num_sd_for_rate_of_change = 3 
time_window = 24*60*60  # (default = 24-hours in seconds)
# for Spike Test:
spike_low_threshold = 1.5
spike_high_threshold = 3
# For Flatline Test:
num_flatline_sus = 2
num_flatline_fail = 3
# For Attenuated Signal Test:


qaqc_nep = function(df, columns_to_qa) {
# Applies QARTOD testing across a single data-frame, assuming all data within the data-frame corresponds to a single NEP
#. Assumed column names:
#... site.code - the code signature of that specific site within the NEP
#... datetime.utc - the date & time format used for time-sensitive testing
#... 
#### Flags:
#1 = Pass
#2 = Suspect
#3 = Fail
#4 = Initial / no flag
#5 = Not Evaluated
  
  # Create columns and assign '5' to all  
  df$na.test <- rep(5,)
  df$test.GrossRange <- rep(5,)
  df$test.Spike <- rep(5,)
  df$test.RateChange <- rep(5,)
  df$test.Flatline <- rep(5,)
  df$test.AttenuatedSig <- rep(5,)
  df$test.Clim <- rep(5,)
  
  # split data by site.code:
  site_list = df |> 
    group_split(site.code)
  # initialize an empty list to store results
  results_list = list()
  
  # Apply QA testing to each site:
  for (i in seq_along(site_list)) {
    site_data = site_list[[i]]
    site_code = unique(site_data$site.code)
    cat('Processing site:', site_code,'\n') # print current site being processed (optional)
    
    # ____________________________ INSERT QA SCRIPTS BELOW _____________________
    ####      GROSS RANGE TEST      ####
    # Gross range test: pH (SUSEPCT = flag 2 | FAIL = flag 3)
    if ('ph' %in% columns_to_qa) {
      site_data$test.GrossRange[site_data$ph < ph_user_min | site_data$ph > ph_user_max] = 2     # SUSPECT
      site_data$test.GrossRange[site_data$ph < ph_sensor_min | site_data$ph > ph_sensor_max] = 3 # FAIL
    }
    # Gross range test: Temperature (SUSEPCT = flag 2 | FAIL = flag 3)
    if ('temp.c' %in% columns_to_qa) {
      site_data$test.GrossRange[site_data$temp.c < temp_user_min | site_data$temp.c > temp_user_max] = 2     # SUSPECT
      site_data$test.GrossRange[site_data$temp.c < temp_sensor_min | site_data$temp.c > temp_sensor_max] = 3 # FAIL
    }
    # Gross range test: Salinity (SUSEPCT = flag 2 | FAIL = flag 3)
    if ('sal.ppt' %in% columns_to_qa) {
      site_data$test.GrossRange[site_data$sal.ppt < sal_user_min | site_data$sal.ppt > sal_user_max] = 2     # SUSPECT
      site_data$test.GrossRange[site_data$sal.ppt < sal_sensor_min | site_data$sal.ppt > sal_sensor_max] = 3 # FAIL
    }
    # Gross range test: pCO2(SUSEPCT = flag 2 | FAIL = flag 3)
    if ('co2.ppm' %in% columns_to_qa) {
      site_data$test.GrossRange[site_data$co2.ppm < co2_user_min | site_data$co2.ppm > co2_user_max] = 2     # SUSPECT
      site_data$test.GrossRange[site_data$co2.ppm < co2_sensor_min | site_data$co2.ppm > co2_sensor_max] = 3 # FAIL
    }
    # Gross range test: DO (SUSEPCT = flag 2 | FAIL = flag 3)
    if ('do.mgl' %in% columns_to_qa) {
      site_data$test.GrossRange[site_data$do.mgl < do_user_min | site_data$do.mgl > do_user_max] = 2     # SUSPECT
      site_data$test.GrossRange[site_data$do.mgl < do_sensor_min | site_data$do.mgl > do_sensor_max] = 3 # FAIL
    }
    # __________________________________
    ####        SPIKE TEST          ####
    # _____________________________ END QA SCRIPTS _____________________________
    # Store the processed data for this site
    results_list[[site_code]] = site_data
  }
  # Combine all results back into a single data frame
  final_results = bind_rows(results_list, .id = 'site.code')
  return(final_results)
}

cols_to_qa = c('ph','temp.c','sal.ppt','do.mgl')
df3 = qaqc_nep(df2, cols_to_qa) # test run

#### Testing ground: ####

site_list = df2 |> 
  group_split(site.code)
