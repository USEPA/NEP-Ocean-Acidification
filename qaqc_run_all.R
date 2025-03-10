# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Mar 5, 2025
# Last updated: Mar 5, 2025
# -------------------------------------------------------------------------------
# From this script, the user may run the entire QA-QC process for each NEP included here which has a file within the same directory
#
# ! LOOK HERE!!!!!! To run the QA process, read all commented sections
# ! 
# ! PRIOR TO RUNNING: The user must ensure that:
# ! 1. All required files are downloaded and saved into the same local folder 
# !   - Required files: this file (qaqc_run_all.R), qaqc_NEP_main.R, and every qaqc_NEP_xxx.R
# ! 2. Have setwd(local_R_path) correctly entered below to said folder (lines 39-40)
# ! 3. Each qaqc_NEP_xxx.R file:
# !   3a. Has had QA thresholds entered for that NEP (they may be dummy/old values)
# !   3b. Has the DATE and NAME OF LAST UPDATE filled in for future users
# ! (Note: User will be prompted on saving preferences prior to QA script running)
# !
# ---------------------------------------------------
# This script:
# 1. Loads in data defined by the below data_path
# 2. Prompts user for output save preferences
# 3. Runs the following scripts:
# > qaqc_NEP_main.R 
#     a. Loads in necessary packages
#     b. Loads in data to be QA'd
#     c. Defines necessary functions to be used
# > qaqc_NEP_Barnegat/Casco/Pensacola.R
#     a. Enters user-defined thresholds for each NEP
#     b. Runs QA on specific NEPs 
# --------------------------------------------------

cat('Beginning script... \n Loading data from O:drive...\n')
# #### Load in data (data_list) a list of data frames for each NEP, with harmonized column names
data_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/4. Finalized Data from NEPs/'
load(paste0(data_path,'data_list.Rdata'))
#
# Set local path to location of folder with saved scripts (including this one)
local_R_path = 'C:/Users/amandovi/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/R'
setwd(local_R_path)
# # # # # # 

# Prompts - determine saving locally / O:drive
save_Odrive_option = readline(prompt='Save QAd NEP Data for each NEP to O:drive (O:/.../NEP Acidification Impacts and WQS/Data/4. Finalized Data from NEPs/) as .Rdata? (y/n): ')
save_local_option = readline(prompt='Save QAd NEP Data for each NEP locally (to where you setwd() to in lines 39-40)? (y/n): ')

# Begin QA Process:
cat('Starting QA Process... Loading main QA .R script... \n')
start_time = Sys.time()
source('qaqc_NEP_main.R')

#Barnegat:
source('qaqc_NEP_Barnegat.R')
cat('Barnegat Bay QA process complete. \n')

#Casco:
source('qaqc_NEP_Casco.R')
cat('Casco Bay QA process complete. \n')

#Pensacola:
source('qaqc_NEP_Pensacola.R')
cat('Pensacola Bay QA process complete. \n')

end_time = Sys.time()
time_taken = end_time - start_time
cat('*~*~* All QA Processes completed! ^_^ *~*~* \n Completion time:',round(time_taken,1),'min.')
