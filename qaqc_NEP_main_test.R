# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Jan 23, 2025
# Last updated: Jan 28, 2025

library(tidyverse)
library(dplyr)
library(slider)


##### PARAMETERIZATION: Edit these prior to running, customized for the specific NEP site/region: (with default values) ####
# For Gross-Range Test:
ph_user_min = 6
ph_user_max = 9
temp_user_min = -1
temp_user_max = 35
sal_user_min = 0
sal_user_max = 35
co2_user_min = 100
co2_user_max = 2500
do_user_min = 5
do_user_max = 20
# sensor min/max's
ph_sensor_min = 0
ph_sensor_max = 14
temp_sensor_min = -10
temp_sensor_max = 45
sal_sensor_min = -1
sal_sensor_max = 50
co2_sensor_min = 0
co2_sensor_max = 3500
do_sensor_min = 0
do_sensor_max = 25
# Gross-Range thresholds
user_thresholds = list(
  ph = list(min=ph_user_min, max=ph_user_max),
  temp.c = list(min=temp_user_min, max=temp_user_max),
  sal.ppt = list(min=sal_user_min, max=sal_user_max),
  do.mgl = list(min=do_user_min, max=do_user_max),
  co2.ppm = list(min=co2_user_min, max=co2_user_max)
)
sensor_thresholds = list(
  ph = list(min=ph_sensor_min, max=ph_sensor_max),
  temp.c = list(min=temp_sensor_min, max=temp_sensor_max),
  sal.ppt = list(min=sal_sensor_min, max=sal_sensor_max),
  do.mgl = list(min=do_sensor_min, max=do_sensor_max),
  co2.ppm = list(min=co2_sensor_min, max=co2_sensor_max)
)

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

spike_thresholds = list(
  ph = list(low=spike_low_ph, high=spike_high_ph),
  temp.c = list(low=spike_low_temp, high=spike_high_temp),
  sal.ppt = list(low=spike_low_sal, high=spike_high_sal),
  do.mgl = list(low=spike_low_do, high=spike_high_do),
  co2.ppm = list(low=spike_low_co2, high=spike_high_co2)
)


# For Rate-of-Change Test:
num_sd_for_rate_of_change = 3 
time_window = 24*60*60  # (default = 24-hours in seconds)
min_num_pts_rate_of_change = 3

# For Flatline Test:
num_flatline_sus = 2
num_flatline_fail = 3
# For Attenuated Signal Test:
#

##### Creating Separate Functions for each Test: ####
# GROSS RANGE TEST ####
gross_range_test = function(site_data, vars_to_test, user_thresholds, sensor_thresholds) {
  # Initialize test columns with 0 (test not ran)
  data = site_data |> 
    mutate(across(all_of(vars_to_test), ~  0, .names = 'test.GrossRange_{.col}')) # 0 = test not ran
  # Apply test logic
  data = data |> 
    mutate(across(all_of(vars_to_test), ~case_when(
      .x < sensor_thresholds[[cur_column()]]$min | .x > sensor_thresholds[[cur_column()]]$max ~ 3, # FAIL
      .x < user_thresholds[[cur_column()]]$min | .x > user_thresholds[[cur_column()]]$max ~ 2, # SUSPECT
      TRUE ~ 1 # PASS
    ), .names = 'test.GrossRange_{.col}')) # fill test.GrossRange_var column with test results
  # Create overall test.GrossRange column
  data = data |>
    mutate(test.GrossRange = do.call(pmax, c(select(data, starts_with('test.GrossRange_')), na.rm=TRUE)))
    # mutate(test.GrossRange = prioritize_values_vectorized(site_data,'test.GrossRange_'))
  return(data)
}
#
# SPIKE TEST ####
spike_test = function(site_data, vars_to_test, spike_thresholds) {
  # initialize test columns with 0 (test not ran)
  data = site_data |> 
    mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.Spike_{.col}'))
  # Apply test logic
  data = data |> 
    mutate(across(all_of(vars_to_test), ~ case_when(
      is.na(.x) | is.na(lag(.x)) ~ 0.5, # Insufficient data
      abs(.x - lag(.x)) > spike_thresholds[[cur_column()]]$high ~ 3, # FAIL
      abs(.x - lag(.x)) > spike_thresholds[[cur_column()]]$low ~ 2, # SUSPECT
      TRUE ~ 1 # PASS
    ), .names = 'test.Spike_{.col}'))
  # Create overall test.Spike column
  data = data |> 
    mutate(test.Spike = do.call(pmax, c(select(data, starts_with('test.Spike_')), na.rm=TRUE)))
    # mutate(test.Spike = prioritize_values_vectorized(site_data, 'test.Spike_'))
  return(data)
}
#
# FLATLINE TEST ####
flatline_test = function(site_data, vars_to_test) {
  SUS_NUM = 3
  FAIL_NUM = 5
  data = site_data |> 
    mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.Flatline_{.col}'))
  # Apply test logic
  data = data |> 
    mutate(across(all_of(vars_to_test), ~ {
      rle_vals = rle(.x) # get run-length encoding for the variable
      run_lengths = rep(rle_vals$lengths, rle_vals$lengths) # expand lengths to match row positions
      case_when(
        row_number() < FAIL_NUM ~ 0.5, # Insufficient Data
        !is.na(.x) & run_lengths >= FAIL_NUM ~ 3, # FAIL
        !is.na(.x) & run_lengths >= SUS_NUM ~ 2, # SUSEPCT
        TRUE ~ 1                                # PASS
      )
    }, .names = 'test.Flatline_{.col}'))
  # create overall test.Flatline column
  data = data |> 
    mutate(test.Flatline = do.call(pmax, c(select(data, starts_with('test.Flatline_')), na.rm=TRUE)))
  return(data)
}
### RUNNING ALL TESTS WITHIN THIS LOOP:####
site_list = df2 |> group_split(site.code) # split data into a list of dataframes for each site.code
results_list = list()
for (i in seq_along(site_list)) {
  site_data = site_list[[i]]
  site_code = unique(site_data$site.code)
  cat('Processing site:',site_code,'\n')
  # Run QA tests
  site_data = gross_range_test(site_data, cols_to_qa, user_thresholds, sensor_thresholds)
  site_data = spike_test(site_data, cols_to_qa, spike_thresholds)
  site_data = flatline_test(site_data, cols_to_qa)
  
  # add each site_data to results_list
  results_list[[i]] = site_data
}
df2_qa = bind_rows(results_list)


### NEW QAQC Function (calls individual test functions)####
qaqc_nep = function(data, columns_to_qa, user_thresholds, sensor_thresholds, spike_thresholds) {
# METADATA: ####
# Applies QARTOD testing across a single data-frame, assuming all data within the data-frame corresponds to a single NEP
# Assumed column names:
#    site.code - the code signature of that specific site within the NEP
#    datetime.utc - the date & time format used for time-sensitive testing
#    ph - pH on the Total Scale
#    temp.c - temperature in Celsius
#    sal.ppt - salinity in PSU (or parts-per-thousand)
#    do.mgl - dissolved oxygen in milligrams/liter
#    co2.ppm - dissolved CO2 in parts-per-million
  
# Flags:
#    0 = Test not yet performed (default)
#    0.5 = Test not performed (insufficient data)
#    1 = Pass
#    2 = Suspect
#    3 = Fail
# ___________________________________________________
#####
  site_list = data |> 
    group_split(site.code)
  results_list = list()
  for (i in seq_along(site_list)) {
    site_data = site_list[[i]]
    site_code = unique(site_data$site_code)
    cat('Processing site:',site_code,'\n')
    # Run QA tests:
    site_data = gross_range_test(site_data, columns_to_qa, user_thresholds, sensor_thresholds)
    site_data = spike_test(site_data, columns_to_qa, spike_thresholds)
    # site_data = flatline_test(site_data, columns_to_qa)
    # site_data = rate_change_test()
    # site_data = attenuated_signal_test()
    # site_data = climatology_test()
    results_list[[i]] = site_data
  }
  return(bind_rows(results_list))
}


### OLD QAQC code (all in one) ####
qaqc_nep_old = function(df, columns_to_qa) {
# Applies QARTOD testing across a single data-frame, assuming all data within the data-frame corresponds to a single NEP
#. Assumed column names:
#... site.code - the code signature of that specific site within the NEP
#... datetime.utc - the date & time format used for time-sensitive testing
#... 
#### Flags:
#1 = Pass
#2 = Suspect
#3 = Fail
#4 = Test not performed for specific reason
#5 = Not yet evaluated
  
  # Ensure that datetime.utc column is a 'POSIXct' type:
  # df = df |> 
  #   mutate(datetime.utc = as.POSIXct(datetime.utc))

  start_time = Sys.time() 
  progress_interval = 10000
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
    df$test.RateChange_ph = rep(5,)
  }
  if ('temp.c' %in% columns_to_qa) {
    df$test.Spike_temp = rep(5,)
    df$test.RateChange_temp = rep(5,)
  }
  if ('sal.ppt' %in% columns_to_qa) {
    df$test.Spike_sal = rep(5,)
    df$test.RateChange_sal = rep(5,)
  }
  if ('do.mgl' %in% columns_to_qa) {
    df$test.Spike_do = rep(5,)
    df$test.RateChange_do = rep(5,)
  }
  if ('co2.ppm' %in% columns_to_qa) {
    df$test.Spike_co2 = rep(5,)
    df$test.RateChange_co2 = rep(5,)
  }
  
  # split data by site.code:
  site_list = df |> group_split(site.code)
  results_list = list() # initialize an empty list to store results before compiling
  
  
  # Apply QA testing to each site:
  for (i in seq_along(site_list)) {
    site_data = site_list[[i]]              # extract data from 1 site
    site_code = unique(site_data$site.code) # extract unique site code
    cat('Processing site:', site_code,'\n') # print current site being processed (optional)
    # # # ! Potentially re-write QA code to be written separately here:
    # site_data = qa_function(site_data, vars_to_test)
    # results_list[[i]] = site_data # Store the results
    # }
    # final_results = bind_rows(results_list) # combine all into single data frame
    
# ____________________________ QA SCRIPTS BELOW _____________________
# ####      GROSS RANGE TEST      ####
#     # PASS = 1 | SUSPECT = 2 | FAIL = 3 #
#     # Gross range test: pH
#     if ('ph' %in% columns_to_qa) {
#       site_data$test.GrossRange[site_data$ph >= ph_user_min | site_data$ph <= ph_user_max] = 1   # PASS
#       site_data$test.GrossRange[site_data$ph < ph_user_min | site_data$ph > ph_user_max] = 2     # SUSPECT
#       site_data$test.GrossRange[site_data$ph < ph_sensor_min | site_data$ph > ph_sensor_max] = 3 # FAIL
#     }
#     # Gross range test: Temperature
#     if ('temp.c' %in% columns_to_qa) {
#       site_data$test.GrossRange[site_data$temp.c >= temp_user_min | site_data$temp.c <= temp_user_max] = 1   # PASS
#       site_data$test.GrossRange[site_data$temp.c < temp_user_min | site_data$temp.c > temp_user_max] = 2     # SUSPECT
#       site_data$test.GrossRange[site_data$temp.c < temp_sensor_min | site_data$temp.c > temp_sensor_max] = 3 # FAIL
#     }
#     # Gross range test: Salinity
#     if ('sal.ppt' %in% columns_to_qa) {
#       site_data$test.GrossRange[site_data$sal.ppt >= sal_user_min | site_data$sal.ppt <= sal_user_max] = 1   # PASS
#       site_data$test.GrossRange[site_data$sal.ppt < sal_user_min | site_data$sal.ppt > sal_user_max] = 2     # SUSPECT
#       site_data$test.GrossRange[site_data$sal.ppt < sal_sensor_min | site_data$sal.ppt > sal_sensor_max] = 3 # FAIL
#     }
#     # Gross range test: pCO2
#     if ('co2.ppm' %in% columns_to_qa) {
#       site_data$test.GrossRange[site_data$co2.ppm >= co2_user_min | site_data$co2.ppm <= co2_user_max] = 1   # PASS
#       site_data$test.GrossRange[site_data$co2.ppm < co2_user_min | site_data$co2.ppm > co2_user_max] = 2     # SUSPECT
#       site_data$test.GrossRange[site_data$co2.ppm < co2_sensor_min | site_data$co2.ppm > co2_sensor_max] = 3 # FAIL
#     }
#     # Gross range test: DO
#     if ('do.mgl' %in% columns_to_qa) {
#       site_data$test.GrossRange[site_data$do.mgl >= do_user_min | site_data$do.mgl <= do_user_max] = 1   # PASS
#       site_data$test.GrossRange[site_data$do.mgl < do_user_min | site_data$do.mgl > do_user_max] = 2     # SUSPECT
#       site_data$test.GrossRange[site_data$do.mgl < do_sensor_min | site_data$do.mgl > do_sensor_max] = 3 # FAIL
#     }
#     # __________________________________
# ####        SPIKE TEST          ####
    # PASS = 1 | SUSPECT = 2 | FAIL = 3 #
    # for (m in 2:(nrow(site_data)-1)) {  # exclude the first and last row of dataset
    #   current_row = site_data[m,]     # m'th row of the data
    #   previous_row = site_data[m-1,]  # row prior to m'th row
    #   next_row = site_data[m+1,]      # row after m'th row
    #   # print(paste('Current index:',m))
    #   # if (m > 1) {
    #   #   print('Previous Row exists:')
    #   #   print(data[i-1],)
    #   # } else {
    #   #   print ('No previous row')
    #   # }
    #   if ('ph' %in% columns_to_qa) {
    #   #   print(length(current_row$ph))
    #   #   print(length(previous_row$ph))
    #   #   print(length(next_row$ph))
    #     # if (!is.na(site_data$ph[m]) & !is.na(site_data$ph[m-1]) & !is.na(site_data$ph[m+1])){
    #     if (!is.na(current_row$ph) & !is.na(previous_row$ph) & !is.na(next_row$ph)) { # check that 3-row sample is all non-NA
    #       spike_ref = (previous_row$ph + next_row$ph)/2
    #       spike = abs(current_row$ph - spike_ref)
    #       if (spike >= spike_high_ph) {
    #         site_data$test.Spike_ph[m] = 3
    #       } else if (spike >= spike_low_ph) {
    #         site_data$test.Spike_ph[m] = 2
    #       } else site_data$test.Spike_ph[m] = 1
    #     }
    #   }
    #   if ('temp.c' %in% columns_to_qa) {
    #     if (!is.na(current_row$temp.c) & !is.na(previous_row$temp.c) & !is.na(next_row$temp.c)) { # check that 3-row sample is all non-NA
    #       spike_ref = (previous_row$temp.c + next_row$temp.c)/2
    #       spike = abs(current_row$temp.c - spike_ref)
    #       if (spike >= spike_high_temp) {
    #         site_data$test.Spike_temp[m] = 3
    #       } else if (spike >= spike_low_temp) {
    #         site_data$test.Spike_temp[m] = 2
    #       } else site_data$test.Spike_temp[m] = 1
    #     }
    #   }
    #   if ('sal.ppt' %in% columns_to_qa) {
    #     if (!is.na(current_row$sal.ppt) & !is.na(previous_row$sal.ppt) & !is.na(next_row$sal.ppt)) { # check that 3-row sample is all non-NA
    #       spike_ref = (previous_row$sal.ppt + next_row$sal.ppt)/2
    #       spike = abs(current_row$sal.ppt - spike_ref)
    #       if (spike >= spike_high_sal) {
    #         site_data$test.Spike_sal[m] = 3
    #       } else if (spike >= spike_low_sal) {
    #         site_data$test.Spike_sal[m] = 2
    #       } else site_data$test.Spike_sal[m] = 1
    #     }
    #   }
    #   if ('do.mgl' %in% columns_to_qa) {
    #     if (!is.na(current_row$do.mgl) & !is.na(previous_row$do.mgl) & !is.na(next_row$do.mgl)) { # check that 3-row sample is all non-NA
    #       spike_ref = (previous_row$do.mgl + next_row$do.mgl)/2
    #       spike = abs(current_row$do.mgl - spike_ref)
    #       if (spike >= spike_high_do) {
    #         site_data$test.Spike_do[m] = 3
    #       } else if (spike >= spike_low_do) {
    #         site_data$test.Spike_do[m] = 2
    #       } else site_data$test.Spike_do[m] = 1
    #     }
    #   }
    #   if ('co2.ppm' %in% columns_to_qa) {
    #     if (!is.na(current_row$co2.ppm) & !is.na(previous_row$co2.ppm) & !is.na(next_row$co2.ppm)) { # check that 3-row sample is all non-NA
    #       spike_ref = (previous_row$co2.ppm + next_row$co2.ppm)/2
    #       spike = abs(current_row$co2.ppm - spike_ref)
    #       if (spike >= spike_high_co2) {
    #         site_data$test.Spike_co2[m] = 3
    #       } else if (spike >= spike_low_co2) {
    #         site_data$test.Spike_co2[m] = 2
    #       } else site_data$test.Spike_co2[m] = 1
    #     }
    #   }
    # }
    # ________________________________________________________________________________
####     RATE OF CHANGE TEST    #### 
    # PASS = 1 | SUSPECT = 2 | INSUFFICIENT DATA = 4 # (no fail possibility)
    #### Rate of Change attempt 3: efficient version of attempt 2, pre-calculating rolling-SD outside of row-loop
    # compute rolling SDs for all variables in parallel
    rolling_sd_list = map(columns_to_qa, function(var) calc_rolling_sd(site_data,var,time_window))
    View(rolling_sd_list)
    # merge all rolling SDs into site_data
    for (j in seq_along(columns_to_qa)) {
      site_data = left_join(site_data, rolling_sd_list[[j]], by='datetime.utc')
    }
    # compute Rate of Change Test
    site_data = site_data |> 
      arrange(datetime.utc) |> 
      mutate(across(
        all_of(columns_to_qa),
        ~case_when(
          is.na(.x) | is.na(lag(.x)) ~NA_real_, # skip if current or previous value is NA
          is.na(get(paste0('rolling_sd_',cur_column()))) ~ 4, # insufficient data
          abs(.x - lag(.x)) > num_sd_for_rate_of_change * get(paste0('rolling_sd_',cur_column()))~2, # Suspect
          TRUE ~ 1 # Pass
        ),
        .names = 'Test.RateChange_{.col}'
      )) |> 
      ungroup()
    
    #### Rate of Change attempt 2: cleaner code, but very slow (~2 rows per second)
    # for (var in columns_to_qa) { # loop through all variables to test (ph, temp.c, sal.ppt, do.mgl, co2.ppm)
    #   if (var %in% colnames(site_data)) { # check that each var is a column in the dataset
    #     test_column_var = paste0('test.RateChange_',var) # create column name corresponding to each variable
    #     print(paste('QAing var:',var))
    #     for (n in 2:nrow(site_data)) {
    #       print(n)
    #       if (is.na(site_data[n, var]) || is.na(site_data[n-1, var])) {  # skip rows with NA values in current or previous row
    #         next # skip to next iteration
    #       }
    #       print(head(site_data$datetime.utc))
    #       print('test1')
    #       # Define rolling time window
    #       window_start = site_data[n,'datetime.utc']-time_window
    #       print('test2')
    #       window_end = site_data[n,'datetime.utc']
    #       print('test3')
    #       # Filter data to time window and remove NA values for variable to be tested on
    #       window_data = site_data |> 
    #         filter(.data$datetime.utc >= window_start & .data$datetime.utc <= window_end) |>  # define window_data to calculate SD on
    #         filter(!is.na(.data[[var]]))                                          # remove NAs from window_data
    #       if (nrow(window_data > 3)) {
    #         sd_var = sd(window_data[[var]], na.rm=TRUE) 
    #       } else {
    #           site_data[n,test_column_var] = 4 # insufficient data to perform test
    #           cat(sprintf('Row %d: Insufficient data in 24-hr window. Skipping. \n',n))
    #           next
    #       }
    #       # Perform rate of change test
    #       if (!is.na(sd_var) && !is.na(site_data[n, var]) && !is.na(site_data[n-1, var])) {
    #         if (abs(site_data[n, var]-site_data[n-1, var]) > num_sd_for_rate_of_change*sd_var) {
    #           site_data[n,test_column_var] = 2 # SUSPECT
    #         } else {
    #           site_data[n,test_column_var] = 1 # PASS
    #         }
    #       }
    #       # Progress Reporting
    #       if (n %% progress_interval == 0) {
    #         elapsed_time = Sys.time() - start_time
    #         est_total_time = elapsed_time/(n/nrow(site_data))
    #         remaining_time = est_total_time - elapsed_time
    #         cat(
    #           sprintf(
    #             'Processed d rows (%.2f%%). Elapsed time: %.2fs. Estimated remaining time: %.2fs.\n',
    #             n, 100*n/nrow(site_data), as.numeric(elapsed_time),as.numeric(remaining_time)
    #           )
    #         )
    #       }
    #     }
    #   } else {
    #     warning(paste('Variable',var,'not found in dataset. Skipping.'))
    #   }
    # }

    #### Rate of change attempt 1:  ##
    # for (n in 2:nrow(site_data)) {
    #   print(n)
    #   current_row = site_data[n,]
    #   # create time-window for rate-of-change test to be performed:
    #   window_start = current_row$datetime.utc - time_window # 24 hours prior to timestamp
    #   window_end = current_row$datetime.utc
    #   # create window_data which is the data for each row's test
    #   window_data = site_data %>%
    #     filter(datetime.utc >= window_start & datetime.utc < window_end)
    #   # check if sufficient data points in time window:
    #   if (nrow(window_data) < min_num_pts_rate_of_change) { # if the number of rows in time_window are insufficient
    #     site_data$test.RateChange[n] = 4 # 4 = insufficient data to perform test 
    #     print('Not enough data this row')
    #   } else { # if there is sufficient data, proceed:
    #     if ('ph' %in% columns_to_qa) {
    #       window_data_ph = window_data[!is.na(window_data$ph),] # check to ensure we are only testing non-NA rows
    #       sd_ph = sd(window_data_ph$ph)
    #       if (!is.na(site_data[n,]$ph) & !is.na(site_data[n-1,]$ph)) { # if both n and n-1 are non-NA, then perform the test
    #         if (abs(site_data[n,]$ph - site_data[n-1,]$ph) > num_sd_for_rate_of_change*sd_ph) {
    #           # if |X(n) - X(n-1)| > SD(n)*num_SD --> this row "fails" the test
    #           site_data$test.RateChange_ph[n] = 2
    #         } else {
    #           site_data$test.RateChange_ph[n] = 1
    #         }
    #       } else {
    #         site_data$test.RateChange_ph[n] = 5 # remain as 5 (untested) if there were NA values 
    #       }
    #     }
    #     if ('temp.c' %in% columns_to_qa) {
    #       window_data_temp = window_data[!is.na(window_data$temp.c),] 
    #       sd_temp = sd(window_data_temp$temp.c)
    #       if (!is.na(site_data[n,]$temp.c) & !is.na(site_data[n-1,]$temp.c)) { # if both n and n-1 are non-NA, then perform the test
    #         if (abs(site_data[n,]$temp.c - site_data[n-1,]$temp.c) > num_sd_for_rate_of_change*sd_temp) {
    #           site_data$test.RateChange_temp[n] = 2
    #         } else {
    #           site_data$test.RateChange_temp[n] = 1
    #         }
    #       } else {
    #         site_data$test.RateChange_temp[n] = 5 # NA
    #       }
    #     }
    #     if ('sal.ppt' %in% columns_to_qa) {
    #       window_data_sal = window_data[!is.na(window_data$sal.ppt),] 
    #       sd_sal = sd(window_data_sal$sal.ppt)
    #       if (!is.na(site_data[n,]$sal.ppt) & !is.na(site_data[n-1,]$sal.ppt)) { # if both n and n-1 are non-NA, then perform the test
    #         if (abs(site_data[n,]$sal.ppt - site_data[n-1,]$sal.ppt) > num_sd_for_rate_of_change*sd_sal) {
    #           site_data$test.RateChange_sal[n] = 2
    #         } else {
    #           site_data$test.RateChange_sal[n] = 1
    #         }
    #       } else {
    #         site_data$test.RateChange_sal[n] = 5 # NA
    #       }
    #     }
    #     if ('do.mgl' %in% columns_to_qa) {
    #       window_data_do = window_data[!is.na(window_data$do.mgl),] 
    #       sd_do = sd(window_data_do$do.mgl)
    #       if (!is.na(site_data[n,]$do.mgl) & !is.na(site_data[n-1,]$do.mgl)) { # if both n and n-1 are non-NA, then perform the test
    #         if (abs(site_data[n,]$do.mgl - site_data[n-1,]$do.mgl) > num_sd_for_rate_of_change*sd_do) {
    #           site_data$test.RateChange_do[n] = 2
    #         } else {
    #           site_data$test.RateChange_do[n] = 1
    #         }
    #       } else {
    #         site_data$test.RateChange_do[n] = 5 # NA
    #       }
    #     }
    #     if ('co2.ppm' %in% columns_to_qa) {
    #       window_data_co2 = window_data[!is.na(window_data$co2.ppm),] 
    #       sd_co2 = sd(window_data_co2$co2.ppm)
    #       if (!is.na(site_data[n,]$co2.ppm) & !is.na(site_data[n-1,]$co2.ppm)) { # if both n and n-1 are non-NA, then perform the test
    #         if (abs(site_data[n,]$co2.ppm - site_data[n-1,]$co2.ppm) > num_sd_for_rate_of_change*sd_co2) {
    #           site_data$test.RateChange_co2[n] = 2
    #         } else {
    #           site_data$test.RateChange_co2[n] = 1
    #         }
    #       } else {
    #         site_data$test.RateChange_co2[n] = 5 # NA
    #       }
    #     } 
    #   }
    # if (n > 5000) {
    #   break
    # }
    # }
    
    #####
    ##################################################################################################################################
    # make the overall test.X column equal to the highest flag for all test columns for each row (flag priority: 3 > 2 > 1 > 4 > 5)..
    #.. e.g. have test.Spike show the "highest" flag for all test.Spike_X columns
    site_data$test.Spike = apply(site_data[, grep('^test\\.Spike', names(site_data),value=TRUE)],1,prioritize_values)
    site_data$test.Spike = apply(site_data[, grep('^test\\.RateChange', names(site_data),value=TRUE)],1,prioritize_values)
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

#### FUNCTIONS NO LONGER IN USE ####
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
# vectorized priority function to perform priority faster
prioritize_values_vectorized = function(data, pattern) {
  test_cols = select(data, starts_with(pattern)) # extract test columns matching the pattern (e.g. 'test.GrossRange_')
  # Debug: print first few rows to verify values
  print(head(test_cols))
  
  # if no test columns exist, return all zeros
  if (length(test_cols) == 0 || all(is.na(test_cols))) {
    message('No matching columns found for pattern: ',pattern)
    return(rep(0,nrow(data)))
  }
  
  test_cols = mutate_all(test_cols, as.numeric) # convert all to numeric to avoid factor issues
  test_cols[is.na(test_cols)] = 0 # replace NA values with 0 (so they don't interfere with max calculation)
  
  # Debug: check if test_cols contain expected values
  print(head(test_cols))
  max_values = do.call(pmax, c(test_cols, na.rm=TRUE))
  return(as.numeric(max_values))
}

# function to calculate rolling SD for one variable
calc_rolling_sd = function(data, var, time_window) {
  data |> 
    arrange(datetime.utc) |> # ensure time order 
    mutate(                  # create window_start and window_end
      window_start = datetime.utc - time_window, 
      window_end = datetime.utc
    ) |> 
    group_by(datetime.utc) |> 
    summarise(
      rolling_sd = sd(data[[var]][datetime.utc >= first(window_start) & datetime.utc <= first(window_end)], na.rm = TRUE),
      .groups = 'drop'
    ) |> 
    rename(!!paste0('window_sd_',var) := rolling_sd)
}

# newer, faster rolling SD calculation function:
calc_rolling_sd = function(data, vars_to_test, time_window = 24*60*60) {
  start_time = Sys.time()
  setDT(data)
  data = data[order(site.code, datetime.utc)]
  colnames(data)
  # create an empty data.table to store rolling SDs
  rolling_sd_data = data[, .(site.code,datetime.utc)]
  colnames(rolling_sd_data)
  # define window start for each row
  data[, window_start := datetime.utc-time_window, by=site.code]
  # set keys for fast lookup
  setkey(data, site.code, datetime.utc)
  setkey(rolling_sd_data, site.code, datetime.utc)
  # compute rolling SD for each variable, ensuring separate calculations per site
  for (var in vars_to_test) {
    new_col = paste0('rolling_sd_',var)
    if (var %in% names(data)) {
      # self-join per site to find rows in the rolling window
      rolling_sd_data[, (new_col) := data[.SD,
                                          on = .(site.code, datetime.utc), 
                                          .SD[datetime.utc >= window_start & datetime.utc <= i.datetime.utc,
                                              .(sd_value = sd(get(var), na.rm=TRUE))],
                                          by = .EACHI]$sd_value]
    } else {
      warning(paste('Variable',var,'not found in dataset. Skipping.'))
    }
  }
  print(Sys.time()-start_time)
  return(rolling_sd_data)
}

rolling_sd_list = calc_rolling_sd(df2, cols_to_qa, time_window)
