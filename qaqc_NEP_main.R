# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Jan 23, 2025
# Last updated: Feb 18, 2025

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#                    INSTRUCTIONS: 
#                    ------------
#  1. Load in necessary packages
#  2. Load in data
#  3. Determine parameterization unique to each NEP 
#      -> note that these must be RE-ENTERED for any subsequent NEPs. 
#       -> Ensure the parameters are correct prior to running the QA script on a different NEP.
#  4. Define necessary functions
#  5. Run QA script 'qaqc_nep()' on NEP dataset
# 
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#### Step 1. Load in necessary packages
library(tidyverse)
library(dplyr)
library(slider)
library(purrr)
library(fuzzyjoin)
library(zoo)

#### Step 2. Load in data
data_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/Finalized Data from NEPs/Continuous'
setwd(data_path)
# this loads in 2 data frames: data_list (all data) and filtered_data_list (data filtered after QA process)
load('NEP_data.Rdata') 
# --> data_list - a list of data frames for each NEP, with harmonized column names
# --> filtered_data_list - data_list but filtered based on flags provided by NEPs or through our QA process here below
# SAVE R image with below line of code:
# save.image('NEP_data.Rdata')

##### Step 3. PARAMETERIZATION: Edit these prior to running, customized for the specific NEP site/region: (with default values) ####
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

# For Rate-of-Change Test:
num_sd_for_rate_of_change = 3 
time_window = 24*60*60  # (default = 24-hours in seconds)
min_num_pts_rate_of_change = 3
sample_interval = 15 # minutes

# For Flatline Test:
num_flatline_sus = 2
num_flatline_fail = 3
# For Attenuated Signal Test:
attenuated_signal_thresholds = list(
  ph = list(min_fail = 0.02, min_sus = 0.05),
  temp.c = list(min_fail = 0.1, min_sus = 0.2),
  sal.ppt = list(min_fail = 0.8, min_sus = 1.3),
  do.mgl = list(min_fail = 0.1, min_sus = 0.3),
  co2.ppm = list(min_fail = 1, min_sus = 2)
)
# END PARAMETERIZATION #
#_________________________________________________________________________________________
# Step 4. Define necessary Functions:  ###################################
# Preliminary functions: ####
# function to determine season:
get_season = function(date) {
  month = month(date)
  if (month %in% c(12, 1, 2)) {
    return('DJF')
  } else if (month %in% c(3,4,5)) {
    return('MAM')
  } else if (month %in% c(6,7,8)) {
    return('JJA')
  } else if (month %in% c(9,10,11)) {
    return('SON')
  }
}
##### QA TEST FUNCTIONS: ####
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
  if (!'season' %in% names(site_data)) {
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
rate_change_test = function(data, data_interp, vars_to_test, num_sd_for_rate_change = 3) {
  # initialize test columns with 0
  data = data |> 
    mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.RateChange_{.col}'))
  for (var in vars_to_test) {
    # dynamically match rolling SD values from data_interp:
    matched_sd_values = data_interp[[paste0('sd_',var)]][match(data$datetime.utc,data_interp$datetime.utc)]
    # apply rate of change test:
    data = data |> 
      mutate(!!paste0('test.RateChange_',var) := case_when(
        is.na(get(var)) | is.na(lag(get(var))) ~ 0.5, # Insufficient data
        is.na(matched_sd_values) ~ 0, # Test not run
        abs(get(var) - lag(get(var))) > num_sd_for_rate_change*matched_sd_values ~ 2, # Suspect
        TRUE ~ 1 # Pass
      ))
  }
  # Create overall test.RateChange column
  data = data |> 
    mutate(test.RateChange = do.call(pmax, c(select(data, starts_with('test.RateChange_')), na.rm=TRUE)))
  return(data)
}
# ATEENUATED SIGNAL TEST #
attenuated_signal_test = function(data, data_interp, vars_to_test, attenuated_signal_thresholds, test_time, time_interval = 15) {
  # define number of rows to assess min and max values across
  num_rows = (test_time * 60 / time_interval)
  # define safe_min and safe_max functions to calculate min and max values across num_rows without creating -Inf 
  safe_max = function(x) if (all(is.na(x))) NA else max(x, na.rm=TRUE)
  safe_min = function(x) if (all(is.na(x))) NA else min(x, na.rm=TRUE)
  # initialize test columns with 0 (test not run)
  data = data |> 
    mutate(across(all_of(vars_to_test), ~ 0, .names = 'test.AttenuatedSignal_{.col}'))
  for (var in vars_to_test) {
    fail_threshold = attenuated_signal_thresholds[[var]]$min_fail
    suspect_threshold = attenuated_signal_thresholds[[var]]$min_sus
    print(paste('Processing variable:',var))
    # print(fail_threshold)
    # print(suspect_threshold)
    # debug: ensure there are valid values
    if (all(is.na(data_interp[[var]]))) {
      warning(paste('All values of',var,'are NA in data_interp. Skipping.'))
      next # skip this variable if all values are NA
    }
    
    # compute rolling max-min difference over test_time window:
    data_interp = data_interp |> 
      mutate(!!paste0(var,'_max') := rollapply(get(var), width=num_rows, FUN=safe_max, fill=NA, align='right'),
             !!paste0(var,'_min') := rollapply(get(var), width=num_rows, FUN=safe_min, fill=NA, align='right'))
    # mutate(!!paste0(var,'_max') := ifelse(
    #         is.infinite(rollapply(get(var), width = num_rows, FUN = max, fill = NA, align = 'right', na.rm=TRUE)),
    #         NA, rollapply(get(var), width = num_rows, FUN = max, fill = NA, align='right', na.rm=TRUE)
    #         ),
    #        !!paste0(var,'_min') := ifelse(
    #          is.infinite(rollapply(get(var), width = num_rows, FUN = min, fill = NA, align = 'right', na.rm=TRUE)),
    #          NA, rollapply(get(var), width = num_rows, FUN = min, fill = NA, align = 'right', na.rm=TRUE)
    #        ))
    # match min and max values from data_interp to data based on datetime.utc
    max_col_values = data_interp[[paste0(var,'_max')]][match(data$datetime.utc, data_interp$datetime.utc)]
    min_col_values = data_interp[[paste0(var,'_min')]][match(data$datetime.utc, data_interp$datetime.utc)]
    # compute attenuated signal test
    data = data |> 
      mutate(!!paste0('test.AttenuatedSignal_',var) := case_when(
        is.na(get(var)) ~ 0, # test not run
        (max_col_values - min_col_values) < fail_threshold ~ 3, # Fail
        (max_col_values - min_col_values) < suspect_threshold ~ 2, # Suspect
        TRUE ~ 1 # Pass
      ))
  }
  # Compute overall test.AttenuatedSignal column
  data = data |> 
    mutate(test.AttenuatedSignal = do.call(pmax, c(select(data, starts_with('test.AttenuatedSignal_')), na.rm=TRUE)))
  return(data)
}


# _________________________________________________________ #
### NEW QAQC Function (calls individual test functions)####
# _________________________________________________________ #
qaqc_nep = function(data, columns_to_qa, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_interval, attenuated_signal_thresholds) {
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
  if (is.character(data$datetime.utc)) {
    data$datetime.utc = as.POSIXct(data$datetime.utc, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
  }
  site_list = data |> 
    group_split(site.code)
  results_list = list()
  for (i in seq_along(site_list)) {
    site_data = site_list[[i]]
    site_code = unique(site_data$site.code)
    cat('Processing site:',site_code,'\n')
    ### Run QA tests ###
    # gross range:
    site_data = gross_range_test(site_data, vars_to_test, user_thresholds, sensor_thresholds)
    # spike:
    site_data = spike_test(site_data, vars_to_test, spike_thresholds)
    # flat line:
    site_data = flatline_test(site_data, vars_to_test)
    # climatology:
    site_data = climatology_test(site_data, vars_to_test, seasonal_thresholds)
    # rate of change:
    site_data_interp = interpolate_data(site_data, vars_to_test, time_interval) # interpolate missing timestamps and values per site
    data_interp = calc_rolling_sd(site_data_interp, vars_to_test,time_interval, min_non_na = 20)
    site_data = rate_change_test(site_data, data_interp, vars_to_test)
    # attenuated signal:
    site_data = attenuated_signal_test(site_data, data_interp, vars_to_test, attenuated_signal_thresholds, 12)
    
    results_list[[i]] = site_data
  }
  return(bind_rows(results_list))
}

#### Step 5. Run for each NEP: ####
# REMEMBER: re-assign parameters (thresholds, time intervals, etc.) when starting a new NEP. 

# Barnegat
barnegat_filtered = subset(data_list$Barnegat, sensor.YSI == 1) # filter co2 data out of Barnegat
vars_to_test = c('ph','temp.c','sal.ppt','do.mgl')
qa_barnegat = qaqc_nep(barnegat_filtered, vars_to_test, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_interval=15, attenuated_signal_thresholds)
# Casco
vars_to_test = c('ph','temp.c','sal.ppt','do.mgl')
qa_casco = qaqc_nep(data_list$Cascobay, vars_to_test, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_interval=60, attenuated_signal_thresholds)
# Long Island Sound
qa_longislandsound = qaqc_nep(data_list$LongIslandSound, vars_to_test, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_interval = 5)
# Pensacola
qa_pensacola = qaqc_nep(data_list$Pensacola, vars_to_test, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_interval=10, attenuated_signal_thresholds)



#### ADDITIONAL FUNCTIONS No Longer In Use: ####
# Function to create season column in dataset (uses 'get_season()' function above):
make_season_column = function(dataset) {
  setDT(dataset)
  dataset[, season := sapply(datetime.utc,get_season)]
  setDF(dataset)
  return(dataset)
}


