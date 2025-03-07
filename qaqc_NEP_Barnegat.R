# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Jan 23, 2025
# Last updated: Mar 5, 2025

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#                    INSTRUCTIONS FOR USER: 
#                    ----------------------
#  1. Define parameters and thresholds unique to BARNEGAT BAY before running 
#  2. Runs qaqc script for BARNEGAT BAY
#  3. Save the results (optional)
# 
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

cat('Processing location: Barnegat Bay...')

##### Step 1. PARAMETERIZATION: Edit these prior to running, customized for the specific NEP site/region: ####

# DATE OF LAST UPDATE: ____ 
# Updated by: ____ 

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

#### Step 2: Running QA script for Barnegat Bay: ####
# filter co2 data out of Barnegat:
barnegat_filtered = subset(data_list$Barnegat, sensor.YSI == 1)
# define variables to be tested:
vars_to_test = c('ph','temp.c','sal.ppt','do.mgl')
# RUN SCRIPT:
qa_barnegat = qaqc_nep(barnegat_filtered, vars_to_test, user_thresholds, sensor_thresholds, spike_thresholds, seasonal_thresholds, time_interval=15, attenuated_signal_thresholds)
#---------

#### Step 3: Saving Options ####
save_all_option = 'n'
dataframe_option = readline(prompt = 'Add QAd Barnegat Data to qa_data_list? (y/n): ')
if (tolower(dataframe_option) %in% c('y','yes')) {
  qa_data_list$Barnegat = qa_barnegat
  cat('QAd Barnegat Data successfully saved to qa_data_list$Barnegat in current R Environment')
  save_all_option = readline(prompt = 'Overwrite previous qa_data_list to O:drive (O:/.../NEP Acidification Impacts and WQS/Data/4. Finalized Data from NEPs/) as .Rdata? (y/n): ')
}

if (tolower(save_all_option) %in% c('y','yes')) {
  save_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/4. Finalized Data from NEPs/qa_data_list.Rdata'
  cat('Saving qa_data_list to:',save_path,'\n')
  save(qa_data_list, file = save_path)
  cat('qa_data_list saved successfully to O:drive')
} 

save_nep_option = readline(prompt = 'Save QAd Barnegat Data on its own to O:drive (O:/.../NEP Acidification Impacts and WQS/Data/4. Finalized Data from NEPs/) as .Rdata? (y/n): ')
if (tolower(save_nep_option) %in% c('y','yes')) {
  save_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/4. Finalized Data from NEPs/qa_barnegat.Rdata'
  cat('Saving qa_barnegat to:',save_path,'\n')
  save(qa_barnegat, file=save_path)
  cat('qa_barnegat saved successfully to O:drive')
} else {
  cat('Skipped.')
}
save_local_option = readline(prompt = 'Save QAd Barnegat Data to current directory? (y/n): ')
if (tolower(save_local_option) %in% c('y','yes')) {
  save_path = getwd()
  cat('Saving Barnegat data locally to current directory \n')
  save(qa_barnegat, file = paste0(getwd(),'/qa_barnegat.Rdata'))
  cat('qa_barnegat saved locally.')
}


# # Manual Entry below (COMMENT OUT): 
# # SAVE R image with below line of code:
# qa_data_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/'
# # O:drive save:
# save(qa_data_list, file=paste0(qa_data_path,'NEP_qa_list.Rdata'))
# # Local save:
# save(qa_data_list, 'qa_data_list.Rdata')



