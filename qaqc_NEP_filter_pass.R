# Andrew Mandovi
# ORISE EPA - Office of Research and Development, Pacific Coastal Ecology Branch, Newport, OR
# Originally created: Apr 10, 2025
# Last updated: Apr 18, 2025

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#                    This R script performs the following: 
#                    -------------------------------------
#  1. Performs filters on the NEP data set 
#      - based on each NEP's specific QA/QC process, if any
#  2. Creates an output file containing the filtered dataset: pass_data_list.Rdata 
# 
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# create initial 'pass_data_list' copy from 'data_list'
pass_data_list = data_list

## Create cutoff date to filter data_list with, for 2015-present
cutoff_date = as.POSIXct('2014-12-31 23:59:59',format='%Y-%m-%d %H:%M:%S', tz='UTC')

#####  NEP Filters:  ####
# Barnegat
barnegat = qa_barnegat |> 
  arrange(datetime.utc) |> 
  select(-starts_with('test.Climatology')) 

# re-make 'flags' column with test.Climatology columns removed:
# !!! REMOVE IF CLIMATOLOGY TEST IMPLEMENTED
barnegat = barnegat |> 
  mutate(flags = do.call(pmax, c(select(barnegat, starts_with('test.')), na.rm=TRUE)))   

barnegat = barnegat |> 
  mutate(ph_flag = do.call(pmax, c(select(barnegat, ends_with('_ph')),na.rm=TRUE)),
         do_flag = do.call(pmax, c(select(barnegat, ends_with('_do.mgl')),na.rm=TRUE)),
         temp_flag = do.call(pmax,c(select(barnegat, ends_with('_temp.c')),na.rm=TRUE)),
         sal_flag = do.call(pmax,c(select(barnegat, ends_with('_sal.ppt')),na.rm=TRUE))
  ) 
pass_data_list$Barnegat = barnegat |>
  filter(datetime.utc > cutoff_date & flags == 1)
qa_data_list$Barnegat = barnegat 

# Casco
casco = qa_casco  |> 
  arrange(datetime.utc) |> 
  select(-starts_with('test.Climatology')) 

# re-make 'flags' column with test.Climatology columns removed:      
# !!! REMOVE IF CLIMATOLOGY TEST IMPLEMENTED
casco = casco |> 
  mutate(flags = do.call(pmax, c(select(casco, starts_with('test.')), na.rm=TRUE)))   
casco = casco |> 
  mutate(ph_flag = do.call(pmax, c(select(casco, ends_with('_ph')),na.rm=TRUE)),
         do_flag = do.call(pmax, c(select(casco, ends_with('_do.mgl')),na.rm=TRUE)),
         temp_flag = do.call(pmax,c(select(casco, ends_with('_temp.c')),na.rm=TRUE)),
         sal_flag = do.call(pmax,c(select(casco, ends_with('_sal.ppt')),na.rm=TRUE))
  )

pass_data_list$Cascobay = casco |> 
  filter(datetime.utc > cutoff_date & flags == 1)
qa_data_list$Cascobay = casco  

# Coastal Bend
pass_data_list$Coastalbend = data_list$Coastalbend |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date 
         & PH_FLAG == 1 
         & SAL_FLAG == 1
         & co2.ppm.flag == 1
         & TEMP_FLAG == 1
  )

# Delaware Inland Bays
pass_data_list$DelawareInland = data_list$DelawareInland |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date & is.na(ph_flag))

# Indian River Lagoon
pass_data_list$IndianRiverLagoon = data_list$IndianRiverLagoon |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date) |>
  filter(PH_FLAG == 'good' | PH_FLAG == '') |>   
  filter(TEMP_FLAG == 'good' | TEMP_FLAG == '') |> 
  filter(SAL_FLAG == 'good' | SAL_FLAG == '') |> 
  filter(DO_FLAG == 'good' | DO_FLAG == '') |> 
  filter(CO2_FLAG == 'good' | CO2_FLAG == '') |> 
  filter(ph.T >= 5.5 & ph.T <= 9.5)

# Long Island Sound
pass_data_list$LongIslandSound = data_list$LongIslandSound |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date) |> 
  filter(year(datetime.utc) != 2021)

# Mobile:
mobile_flags_pass = c('<0>','<0> (CAB)','<0> (CDF)','<0> (CND)','<0> (CRE)','<0> (CWE)','<0> [GIT] (CND)','<0> [GIT] (CRE)','<0> [GIT] (CWE)', '<0> (CDA)')
flag_columns = c('F_Temp','F_Sal','F_DO_mgl','F_Depth','F_pH')
df = data_list$Mobile |> 
  mutate(flag_all = 9) 
df[, flag_all := fifelse(
  rowSums(sapply(.SD, function(x) x %in% mobile_flags_pass)) == length(flag_columns),
  1,0),
  .SDcols = flag_columns]
# pass_data_list$Mobile = df
pass_data_list$Mobile = df |> 
  arrange(datetime.utc) |> 
  filter(flag_all == 1 & datetime.utc > cutoff_date)

# Morro Bay:
pass_data_list$Morro = data_list$Morro |> 
  arrange(datetime.utc) |> 
  filter(site.code == 'BM1') |> 
  filter(datetime.utc > cutoff_date & PH_ext_QC == 1 & DO_QC == 1 & PRES_QC == 1 & SAL_QC == 1 & TEMP_QC == 1)

# Narragansett Bay
pass_data_list$Narrgansett = data_list$Narrgansett |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date)

# New York-New Jersey Harbor
pass_data_list$NYNJH = data_list$NYNJH |> 
  arrange(datetime.utc) |> 
  filter(flag.ph == 'A' | flag.ph == 'C') |> 
  filter(flag.do.mgl == 'A' | flag.do.mgl == 'C') |> 
  filter(flag.sal.ppt == 'A' | flag.sal.ppt == 'C') |> 
  filter(flag.temp.c == 'A' | flag.temp.c == 'C') |> 
  filter(sal.ppt < 40) |> 
  filter(datetime.utc > cutoff_date)

# Pensacola
pensacola = qa_pensacola |> 
  arrange(datetime.utc) |> 
  select(-starts_with('test.Climatology'))

# re-make 'flags' column with test.Climatology columns removed:      
# !!! REMOVE IF CLIMATOLOGY TEST IMPLEMENTED
pensacola = pensacola |> 
  mutate(flags = do.call(pmax, c(select(pensacola, starts_with('test.')), na.rm=TRUE)))   

pensacola = pensacola |> 
  mutate(ph_flag = do.call(pmax, c(select(pensacola, ends_with('_ph')),na.rm=TRUE)),
         do_flag = do.call(pmax, c(select(pensacola, ends_with('_do.mgl')),na.rm=TRUE)),
         temp_flag = do.call(pmax,c(select(pensacola, ends_with('_temp.c')),na.rm=TRUE)),
         sal_flag = do.call(pmax,c(select(pensacola, ends_with('_sal.ppt')),na.rm=TRUE))
  ) 

pass_data_list$Pensacola = pensacola |> 
  filter(datetime.utc > cutoff_date & flags == 1)
qa_data_list$Pensacola = pensacola 

# Puget Sound
data_list$PugetSound = data_list$PugetSound |> 
  arrange(datetime.utc) |> 
  mutate(ph.T = ph.T_lueker)
pass_data_list$PugetSound = data_list$PugetSound |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date)

# San Francisco Bay
pass_data_list$SanFrancisco = data_list$SanFrancisco |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date
         & ph.tot.qc == 1
         & do.mgl.qc == 1 
         & PRES_QC == 1
         & sal.ppt.qc == 1
         & temp.c.qc == 1)

# Tampa Bay
pass_data_list$Tampa = data_list$Tampa |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date) |> 
  filter(QF_pHT == 1 | QF_pHT == 15) |> 
  filter(QF_SALINITY == 1 | QF_SALINITY == 15) |> 
  filter(QF_CO2 == 1 | QF_CO2 == 15) |> 
  filter(QF_OXYGEN == 1 | QF_OXYGEN == 15) |> 
  filter(temp.c.qc == 1 | temp.c.qc == 15)

# Tillamook Bay
pass_data_list$Tillamook = data_list$Tillamook |> 
  arrange(datetime.utc) |> 
  filter(datetime.utc > cutoff_date) |> 
  filter(flags_seafet == 1 | is.na(flags_seafet)) |> 
  filter(flags_seaphox == 1 | is.na(flags_seaphox)) |> 
  filter(flags_ysi == 1 | is.na(flags_ysi)) |> 
  filter(flags_samico2 == 1 | is.na(flags_samico2))


