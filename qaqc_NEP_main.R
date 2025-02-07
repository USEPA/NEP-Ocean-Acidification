# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Jan 23, 2025
# Last updated: Feb 6, 2025

library(tidyverse)
library(dplyr)
library(slider)
library(purrr)
library(fuzzyjoin)
library(zoo)

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

# Threshold lists - will need to customize these for each NEP
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
spike_thresholds = list(
  ph = list(low=spike_low_ph, high=spike_high_ph),
  temp.c = list(low=spike_low_temp, high=spike_high_temp),
  sal.ppt = list(low=spike_low_sal, high=spike_high_sal),
  do.mgl = list(low=spike_low_do, high=spike_high_do),
  co2.ppm = list(low=spike_low_co2, high=spike_high_co2)
)

seasonal_thresholds = list(
  ph_min = list(Winter = 7.1, Spring = 7.2, Summer = 7.3, Autumn = 7.2),
  ph_max = list(Winter = 8.0, Spring = 8.2, Summer = 8.3, Autumn = 8.2),
  temp.c_min = list(Winter = 2, Spring = 10, Summer = 15, Autumn = 8),
  temp.c_max = list(Winter = 12, Spring = 20, Summer = 25, Autumn = 18),
  sal.ppt_min = list(Winter = 28, Spring = 29, Summer = 30, Autumn = 29),
  sal.ppt_max = list(Winter = 34, Spring = 35, Summer = 36, Autumn = 34),
  do.mgl_min = list(Winter = 6, Spring = 5.5, Summer = 5, Autumn = 5.5),
  do.mgl_max = list(Winter = 12, Spring = 11, Summer = 10, Autumn = 11),
  co2.ppm_min = list(Winter = 300, Spring = 300, Summer = 300, Autumn = 300),
  co2.ppm_max = list(Winter = 1000, Spring = 1000, Summer = 1000, Autumn = 1000)
)

seasonal_thresholds2 = list(
  ph_min = list(DJF = 7.1, MAM = 7.2, JJA = 7.3, SON = 7.2),
  ph_max = list(DJF = 8.0, MAM = 8.2, JJA = 8.3, SON = 8.2),
  temp.c_min = list(DJF = 2, MAM = 10, JJA = 15, SON = 8),
  temp.c_max = list(DJF = 12, MAM = 20, JJA = 25, SON = 18),
  sal.ppt_min = list(DJF = 28, MAM = 29, JJA = 30, SON = 29),
  sal.ppt_max = list(DJF = 34, MAM = 35, JJA = 36, SON = 34),
  do.mgl_min = list(DJF = 6, MAM = 5.5, JJA = 5, SON = 5.5),
  do.mgl_max = list(DJF = 12, MAM = 11, JJA = 10, SON = 11),
  co2.ppm_min = list(DJF = 300, MAM = 300, JJA = 300, SON = 300),
  co2.ppm_max = list(DJF = 1000, MAM = 1000, JJA = 1000, SON = 1000)
)
seasonal_thresholds_df = bind_rows(
  lapply(names(seasonal_thresholds), function(var) {
    bind_rows(lapply(names(seasonal_thresholds[[var]]), function(season) {
      tibble(
        variable = var,
        season = season,
        min = seasonal_thresholds[[var]][[season]]$min,
        max = seasonal_thresholds[[var]][[season]]$max
      )}))})
)

# For Rate-of-Change Test:
num_sd_for_rate_of_change = 3 
time_window = 24*60*60  # (default = 24-hours in seconds)
min_num_pts_rate_of_change = 3
sample_interval = 15 # minutes

# For Flatline Test:
num_flatline_sus = 2
num_flatline_fail = 3
# For Attenuated Signal Test:
#
# END PARAMETERIZATION #
#_________________________________________________________________________________________

##### Creating Separate Functions for each Test: ####
# GROSS RANGE TEST #
gross_range_test = function(site_data, vars_to_test, user_thresholds, sensor_thresholds) {
  # Initialize test columns with 0 (test not ran)
  data = site_data |> 
    mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.GrossRange_{.col}')) # 0 = test not ran
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
# SPIKE TEST #
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
# FLATLINE TEST #
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
# CLIMATOLOGY TEST #
climatology_test = function(site_data, vars_to_test, seasonal_thresholds) {
  # Debug
  if (!'season2' %in% names(site_data)) {
    stop('Error: there is no season column in the dataset')
  }
  # initialize test columns with 0 
  site_data = site_data |>
    mutate(across(all_of(vars_to_test), ~ 0, .names='test.Climatology_{.col}'))
  # create and fill seasonal min/max columns:
  for (var in vars_to_test) {
    min_col = paste0(var, '_season_min')
    max_col = paste0(var, '_season_max')
    
    site_data[[min_col]] = sapply(site_data$season, function(s) {
      if (!is.null(seasonal_thresholds[[paste0(var, '_min')]][[s]])) {
        return(seasonal_thresholds[[paste0(var, '_min')]][[s]])
      } else {
        return(NA_real_)
      }
    })
    site_data[[max_col]] = sapply(site_data$season, function(s) {
      if (!is.null(seasonal_thresholds[[paste0(var, '_max')]][[s]])) {
        return(seasonal_thresholds[[paste0(var, '_max')]][[s]])
      } else {
        return(NA_real_)
      }
    })
  }
  # Apply test logic:
  data = site_data |> 
    mutate(across(all_of(vars_to_test), ~ {
      min_threshold = get(paste0(cur_column(), '_season_min'))
      max_threshold = get(paste0(cur_column(), '_season_max'))
      case_when(
        is.na(.x) | is.na(season) ~ 0, # test not ran if NAs in data or season
        is.na(min_threshold) | is.na(max_threshold) ~ 0, # test not ran if no thresholds exist
        .x < min_threshold | .x > max_threshold ~ 2, # Suspect if exceed thresholds
        TRUE ~ 1 # Pass
      )
    }, .names = 'test.Climatology_{.col}'))
  # Compute overall test.Climatology as max of all test.Climatology_* columns:
  data = data |> 
    mutate(test.Climatology = do.call(pmax, c(select(data, starts_with('test.Climatology_')), na.rm=TRUE)))
  # Remove _season_min and _season_max columns before returning dataset:
  finished_data = data |> 
    select(-ends_with('_season_min'),-ends_with('_season_max'))
  return(finished_data)
}
# RATE OF CHANGE TEST: 3 functions #
# Function 1: interpolating data (into new data frame) to account for any missing timestamps - empty data is NA
interpolate_data = function(data, vars_to_test, time_interval) {
  # ensure datetime.utc is in POSIXct format
  sorted_data = data |> 
    drop_na(datetime.utc) # remove NA datetime values
  data_length = length(sorted_data$datetime.utc) # length of data
  time_min = sorted_data$datetime.utc[1]
  time_max = sorted_data$datetime.utc[data_length]
  all_times = seq(time_min, time_max, by=(time_interval*60)) # create adjusted time-series
  data_interp = data.frame(list(datetime.utc = all_times)) # put interpolated times into a data.frame
  
  # loop through each var and add it to the interpolated data-frame
  for (var in vars_to_test) {
    testdata = sorted_data |> 
      select(datetime.utc, paste0(var))
    data_interp = data_interp |> 
      left_join(testdata, join_by('datetime.utc'))
  }
  return(data_interp)
}
# Function 2: calculate rolling standard deviation and add to main data frame
calc_rolling_sd = function(data_interp, vars_to_test, time_interval=15, min_non_na = 20) {
  sampling_window = (60/time_interval)*24 # 96 for 15-min data
  for (var in vars_to_test) {
    if (var %in% names(data_interp)) {
      sd_col_name = paste0('sd_',var)
      data_interp = data_interp |> 
        mutate(
          !!sd_col_name := slide_dbl(
            .x = data_interp[[var]],
            .f = ~ifelse(sum(!is.na(.x)) >= min_non_na, sd(.x, na.rm=TRUE), NA_real_),
            .before = sampling_window, # look back this many rows
            .complete = TRUE # only compute once >= sampling_window rows available
          )
        )
    } else {cat('Missing variable from interpolated data: ',var,'\n')}
  }
  return(data_interp)
}
# Function 3: perform rate-of-change test on primary data based on SD values in interpolated data
rate_change_test = function(data, data_interp, vars_to_test) {
  
}

  
### RUNNING ALL TESTS WITHIN THIS LOOP FOR TESTING:####
barnegat_filtered = subset(data_list$Barnegat, sensor.YSI == 1) # filter co2 data out of Barnegat
vars_to_test = c('ph','temp.c','sal.ppt','do.mgl')
time_interval = 15
site_list = barnegat_filtered |> group_split(site.code) # split data into a list of dataframes for each site.code
results_list = list()
for (i in seq_along(site_list)) {
  site_data = site_list[[i]]
  # site_data = site_list[[1]]
  site_code = unique(site_data$site.code)
  cat('Processing site:',site_code,'\n')
  # ensure POSIXct format for datetime.utc
  if (is.character(data$datetime.utc)) {
    site_data$datetime.utc = as.POSIXct(site_data$datetime.utc, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
  }
  # arrange data chronologically
  site_data = site_data |> 
    arrange(datetime.utc)
  # Run QA tests
  # site_data = gross_range_test(site_data, cols_to_qa, user_thresholds, sensor_thresholds)
  # site_data = spike_test(site_data, cols_to_qa, spike_thresholds)
  # site_data = flatline_test(site_data, cols_to_qa)
  # site_data = climatology_test(site_data, cols_to_qa, seasonal_thresholds)
  
  # rate of change:
  site_data_interp = interpolate_data(site_data, vars_to_test, time_interval=15) # interpolate missing timestamps and values per site
  data_interp = calc_rolling_sd(site_data_interp, vars_to_test,time_interval=15, min_non_na = 20)
  # expanded_data = calc_rolling_sd(expanded_data, cols_to_qa, sampling_window)
  # site_data = ratechange_test(site_data, expanded_data, cols_to_qa)

  

  
  # add each site_data to results_list
  results_list[[i]] = site_data
}
df2_qa = bind_rows(results_list)
View(df2_qa)
# _________________________________________________________ #
### NEW QAQC Function (calls individual test functions)####
# _________________________________________________________ #
qaqc_nep = function(data, columns_to_qa, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_interval) {
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
  site_list = data |> 
    group_split(site.code)
  results_list = list()
  for (i in seq_along(site_list)) {
    site_data = site_list[[i]]
    site_code = unique(site_data$site.code)
    cat('Processing site:',site_code,'\n')
    # Run QA tests:
    site_data = gross_range_test(site_data, columns_to_qa, user_thresholds, sensor_thresholds)
    site_data = spike_test(site_data, columns_to_qa, spike_thresholds)
    site_data = flatline_test(site_data, columns_to_qa)
    site_data = climatology_test(site_data, columns_to_qa,seasonal_thresholds)
    site_data_interp = interpolate_data(site_data, cols_to_qa, time_interval=15) # Ratechange 1 - interpolate missing timestamps and values per site
    site_data_interp = calc_rolling_sd(site_data_interp, vars_to_test,time_interval=15, min_non_na = 20) # Ratechange 2- calc rolling SD
    # site_data = rate_change_test() # Ratechange 3 - perform rate of change test
    # site_data = attenuated_signal_test()
    
    results_list[[i]] = site_data
  }
  return(bind_rows(results_list))
}

# Run for each individual NEP:
qa_barnegat = qaqc_nep(data_list$Barnegat, cols_to_qa, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_interval=15)
qa_casco = qaqc_nep() #...



#### ADDITIONAL FUNCTIONS No Longer In Use: ####
# function to determine season:
get_season = function(date) {
  month = month(date)
  if (month %in% c(12, 1, 2)) {
    return('Winter')
  } else if (month %in% c(3,4,5)) {
    return('Spring')
  } else if (month %in% c(6,7,8)) {
    return('Summer')
  } else if (month %in% c(9,10,11)) {
    return('Autumn')
  }
}

get_season2 = function(month) {
  if (month == 12 | month == 1 | month == 2) {
    return('DJF')
  } else if (month == 3 | month == 4 | month == 5) {
    return('MAM')
  } else if (month == 6 | month == 7 | month == 8) {
    return('JJA')
  } else if (month == 9 | month == 10 | month == 11) {
    return('SON')
  }
}

# Function to create season column in dataset (uses 'get_season()' function above):
make_season_column = function(dataset) {
  setDT(dataset)
  dataset[, season := sapply(datetime.utc,get_season)]
  setDF(dataset)
  return(dataset)
}


