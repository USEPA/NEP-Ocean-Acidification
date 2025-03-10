# README

The purpose of this repository is to store shared R code for working with data from National Estuary Program (NEP) monitoring sites to: 
I. QA/QC-ing raw data for analysis
II. Performing analysis on the data
III. Creating visualizations of data for communication and publication of results

**I. QA/QC-ing raw data for analysis**, steps:
1. This requires the following files to be downloaded to your local machine:
 - qaqc_NEP_main.R
 - qaqc_run_all.R
 - qaqc_NEP_Barnegat.R
 - qaqc_NEP_Casco.R
 - qaqc_NEP_Pensacola.R
 - (and any corresponding additional NEP files to QA, but as of 3/6/2025, these are the only 3)
2. Then, the user must ensure that the thresholds for each NEP file are correct. (3/6/25 - they have not yet been adjusted)
3. Once R is opened, the user should open the **'qaqc_run_all.R'** script and customize row 39:
   - this is where the user sets their working directory to the local filepath where the R scripts were downloaded (NOT where the data is on the O:drive)
4. Finally, the user can execute all of the scripts by running the following line of code in the R console: **source('qaqc_run_all.R')**
   - note: this is different than running the entire script via ctrl-shift-enter. This allows the readline prompts to pull the user's saving preferences. 


File naming conventions:

The **PREFIX** of a file dictates the file's category:
- **qaqc_**: scripts containing R code for performing QA/QC and filtering of data
- **calc_**: scripts containing R code for running analysis on the data. This includes performing carbonate calculations, calculating uncertainties, or other statistical calculations.
- **plot_**: scripts containing R code for plotting visualizations of data
- **test_**: scripts written  for testing, practice, or reference material

