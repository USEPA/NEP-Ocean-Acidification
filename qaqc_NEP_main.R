# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Jan 23, 2025
# Last updated: Jan 27, 2025

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
spike_low_ph = 0.3
spike_high_ph = 0.5
spike_low_temp = 3
spike_high_temp = 5
spike_low_sal = 2
spike_high_sal = 4
spike_low_do = 0.3
spike_high_do = 0.5
spike_low_co2 = 200
spike_high_co2 = 400
# For Flatline Test:
num_flatline_sus = 2
num_flatline_fail = 3
# For Attenuated Signal Test:

# function to prioritize flags over others
prioritize_values = function(row) {
  priority = c(3, 2, 1, 4, 5)
  for (val in priority) {
    if (val %in% row) {
      return(val)
    }
  }
  return(NA)
}


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
  # Create test columns for each measurement
  if ('ph' %in% columns_to_qa) {
    df$test.Spike_ph = rep(5,)
  }
  if ('temp.c' %in% columns_to_qa) {
    df$test.Spike_temp = rep(5,)
  }
  if ('sal.ppt' %in% columns_to_qa) {
    df$test.Spike_sal = rep(5,)
  }
  if ('do.mgl' %in% columns_to_qa) {
    df$test.Spike_do = rep(5,)
  }
  if ('co2.ppm' %in% columns_to_qa) {
    df$test.Spike_co2 = rep(5,)
  }
  
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
    
    # ____________________________ QA SCRIPTS BELOW _____________________
    ####      GROSS RANGE TEST      ####
    # PASS = 1 | SUSPECT = 2 | FAIL = 3 #
    # Gross range test: pH 
    if ('ph' %in% columns_to_qa) {
      site_data$test.GrossRange[site_data$ph >= ph_user_min | site_data$ph <= ph_user_max] = 1   # PASS
      site_data$test.GrossRange[site_data$ph < ph_user_min | site_data$ph > ph_user_max] = 2     # SUSPECT
      site_data$test.GrossRange[site_data$ph < ph_sensor_min | site_data$ph > ph_sensor_max] = 3 # FAIL
    }
    # Gross range test: Temperature 
    if ('temp.c' %in% columns_to_qa) {
      site_data$test.GrossRange[site_data$temp.c >= temp_user_min | site_data$temp.c <= temp_user_max] = 1   # PASS
      site_data$test.GrossRange[site_data$temp.c < temp_user_min | site_data$temp.c > temp_user_max] = 2     # SUSPECT
      site_data$test.GrossRange[site_data$temp.c < temp_sensor_min | site_data$temp.c > temp_sensor_max] = 3 # FAIL
    }
    # Gross range test: Salinity 
    if ('sal.ppt' %in% columns_to_qa) {
      site_data$test.GrossRange[site_data$sal.ppt >= sal_user_min | site_data$sal.ppt <= sal_user_max] = 1   # PASS
      site_data$test.GrossRange[site_data$sal.ppt < sal_user_min | site_data$sal.ppt > sal_user_max] = 2     # SUSPECT
      site_data$test.GrossRange[site_data$sal.ppt < sal_sensor_min | site_data$sal.ppt > sal_sensor_max] = 3 # FAIL
    }
    # Gross range test: pCO2
    if ('co2.ppm' %in% columns_to_qa) {
      site_data$test.GrossRange[site_data$co2.ppm >= co2_user_min | site_data$co2.ppm <= co2_user_max] = 1   # PASS
      site_data$test.GrossRange[site_data$co2.ppm < co2_user_min | site_data$co2.ppm > co2_user_max] = 2     # SUSPECT
      site_data$test.GrossRange[site_data$co2.ppm < co2_sensor_min | site_data$co2.ppm > co2_sensor_max] = 3 # FAIL
    }
    # Gross range test: DO 
    if ('do.mgl' %in% columns_to_qa) {
      site_data$test.GrossRange[site_data$do.mgl >= do_user_min | site_data$do.mgl <= do_user_max] = 1   # PASS
      site_data$test.GrossRange[site_data$do.mgl < do_user_min | site_data$do.mgl > do_user_max] = 2     # SUSPECT
      site_data$test.GrossRange[site_data$do.mgl < do_sensor_min | site_data$do.mgl > do_sensor_max] = 3 # FAIL
    }
    # __________________________________
    ####        SPIKE TEST          ####
    # PASS = 1 | SUSPECT = 2 | FAIL = 3 #
    for (m in 2:(nrow(site_data)-1)) {  # exclude the first and last row of dataset
      current_row = site_data[m,]     # m'th row of the data
      previous_row = site_data[m-1,]  # row prior to m'th row
      next_row = site_data[m+1,]      # row after m'th row
      # print(paste('Current index:',m))
      # if (m > 1) {
      #   print('Previous Row exists:')
      #   print(data[i-1],)
      # } else {
      #   print ('No previous row')
      # }
      if ('ph' %in% columns_to_qa) {
      #   print(length(current_row$ph))
      #   print(length(previous_row$ph))
      #   print(length(next_row$ph))
        # if (!is.na(site_data$ph[m]) & !is.na(site_data$ph[m-1]) & !is.na(site_data$ph[m+1])){
        if (!is.na(current_row$ph) & !is.na(previous_row$ph) & !is.na(next_row$ph)) { # check that 3-row sample is all non-NA
          spike_ref = (previous_row$ph + next_row$ph)/2
          spike = abs(current_row$ph - spike_ref)
          if (spike >= spike_high_ph) {
            site_data$test.Spike_ph[m] = 3
          } else if (spike >= spike_low_ph) {
            site_data$test.Spike_ph[m] = 2
          } else site_data$test.Spike_ph[m] = 1
        }
      } 
      if ('temp.c' %in% columns_to_qa) {
        if (!is.na(current_row$temp.c) & !is.na(previous_row$temp.c) & !is.na(next_row$temp.c)) { # check that 3-row sample is all non-NA
          spike_ref = (previous_row$temp.c + next_row$temp.c)/2
          spike = abs(current_row$temp.c - spike_ref)
          if (spike >= spike_high_temp) {
            site_data$test.Spike_temp[m] = 3
          } else if (spike >= spike_low_temp) {
            site_data$test.Spike_temp[m] = 2
          } else site_data$test.Spike_temp[m] = 1
        }
      }  
      if ('sal.ppt' %in% columns_to_qa) {
        if (!is.na(current_row$sal.ppt) & !is.na(previous_row$sal.ppt) & !is.na(next_row$sal.ppt)) { # check that 3-row sample is all non-NA
          spike_ref = (previous_row$sal.ppt + next_row$sal.ppt)/2
          spike = abs(current_row$sal.ppt - spike_ref)
          if (spike >= spike_high_sal) {
            site_data$test.Spike_sal[m] = 3
          } else if (spike >= spike_low_sal) {
            site_data$test.Spike_sal[m] = 2
          } else site_data$test.Spike_sal[m] = 1
        }
      }  
      if ('do.mgl' %in% columns_to_qa) {
        if (!is.na(current_row$do.mgl) & !is.na(previous_row$do.mgl) & !is.na(next_row$do.mgl)) { # check that 3-row sample is all non-NA
          spike_ref = (previous_row$do.mgl + next_row$do.mgl)/2
          spike = abs(current_row$do.mgl - spike_ref)
          if (spike >= spike_high_do) {
            site_data$test.Spike_do[m] = 3
          } else if (spike >= spike_low_do) {
            site_data$test.Spike_do[m] = 2
          } else site_data$test.Spike_do[m] = 1
        }
      }  
      if ('co2.ppm' %in% columns_to_qa) {
        if (!is.na(current_row$co2.ppm) & !is.na(previous_row$co2.ppm) & !is.na(next_row$co2.ppm)) { # check that 3-row sample is all non-NA
          spike_ref = (previous_row$co2.ppm + next_row$co2.ppm)/2
          spike = abs(current_row$co2.ppm - spike_ref)
          if (spike >= spike_high_co2) {
            site_data$test.Spike_co2[m] = 3
          } else if (spike >= spike_low_co2) {
            site_data$test.Spike_co2[m] = 2
          } else site_data$test.Spike_co2[m] = 1
        }
      }
    }
    ##################################################################################################################################
    # make the overall test.Spike column equal to the highest flag for all 'Spike' columns for each row (flag priority: 3 > 2 > 1 > 5)
    site_data$test.Spike = apply(site_data[, grep('^test\\.Spike', names(site_data),value=TRUE)],1,prioritize_values)
    # _____________________________ END QA SCRIPTS _____________________________
    # Store the processed data for this site
    results_list[[site_code]] = site_data
  }
  
  # Combine all results back into a single data frame
  final_results = bind_rows(results_list, .id = 'site.code')
  return(final_results)
}

cols_to_qa = c('ph','temp.c','sal.ppt','do.mgl')
df3 = qaqc_nep(df2, cols_to_qa)

#### Testing ground: ####
site_list = df2 |> 
  group_split(site.code)
