# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Mar 5, 2025
# Last updated: Mar 5, 2025
# -------------------------------------------------------------------------------
# From this script, the user may run the entire QA-QC process for each NEP included here which has a file within the same directory
#
# LOOK HERE!!!!!! 
# 
# PRIOR TO RUNNING: The user must ensure that each qaqc_NEP_xxxx.R file:
# 1. Has had QA thresholds entered for that NEP (they may be dummy/old values)
# 2. Has the DATE and NAME OF LAST UPDATE filled in for future users


# ---------------------------------------------------
# This script:
# 1. Loads in data defined by the below data_path

# 2. runs the following scripts:
# > qaqc_NEP_main.R 
#     a. Loads in necessary packages
#     b. Loads in data to be QA'd
#     c. Defines necessary functions to be used
# > qaqc_NEP_Barnegat/Casco/Pensacola.R
#     a. Enters user-defined thresholds for each NEP
#     b. Runs QA on specific NEPs 
#     c. Prompts user for saving options

#### Step 2. Load in data
data_path = 'O:/PRIV/CPHEA/PESD/NEW/EPA/PCEB/Acidification Monitoring/NEP Acidification Impacts and WQS/Data/data_list.Rdata'
load(data_path)

# loads a list (data_list) of data frames for each NEP, with harmonized column names

# Begin QA Process:
cat('Starting QA Process... Loading main QA .R script... \n')

source('qaqc_NEP_main.R')

source('qaqc_NEP_Barnegat.R')
cat('Barnegat Bay QA process complete. \n')

source('qaqc_NEP_Casco.R')
cat('Casco Bay QA process complete. \n')

source('qaqc_NEP_Pensacola.R')
cat('Pensacola Bay QA process complete. \n')

cat('All QA Processes completed! ^_^ ')
