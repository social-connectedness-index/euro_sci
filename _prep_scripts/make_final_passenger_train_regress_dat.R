# Purpose: Assemble the final regression data for the passenger train regressions
# Inputs: _raw_data/SCI_Nuts2_Nuts2.csv
#         _intermediate_data/passenger_train_dat/eurostat_pass_dat_EITHER.csv
#         _intermediate_data/geo_distance_dat.csv
#         _intermediate_data/rail_travel_time.csv
#         _intermediate_data/drive_travel_time.csv
# Outputs: _intermediate_data/final_regress_dat/passenger_rail_regress_dat.dta
# Date: 2020-10-29

library(tidyverse)
library(haven)

###################
# 1. Read-in data #
###################

sci_dat <- read_csv("_raw_data/SCI_Nuts2_Nuts2.csv") %>% 
  mutate(log_sci = log(sci)) %>% 
  filter(user_loc != fr_loc)

# see: _prep_scripts/generate_passenger_train_travel.R
passenger_train_dat_in <- read_csv("_intermediate_data/passenger_train_dat/eurostat_pass_dat_EITHER.csv")

# get most recent non-NA
passenger_train_dat <- passenger_train_dat_in %>% 
  mutate(pass_rail_most_recent = case_when(
    !is.na(pass_rail_2015) ~ pass_rail_2015,
    !is.na(pass_rail_2010) ~ pass_rail_2010,
    !is.na(pass_rail_2005) ~ pass_rail_2005
  )) %>% 
  select(user_loc, fr_loc, sci, pass_rail_2015, pass_rail_2010, pass_rail_2005, pass_rail_most_recent)

# see: _prep_scripts/generate_distances.R
distance_dat <- read_csv("_intermediate_data/geo_distance_dat.csv") %>%
  mutate(log_distance = log(distance))

# see: _prep_scripts/generate_travel_times.R
rail_time_dat <- read_csv("_intermediate_data/rail_travel_time.csv") %>% 
  mutate(log_rail_time = log(rail_time))

# see: _prep_scripts/generate_travel_times.R
drive_time_dat <- read_csv("_intermediate_data/drive_travel_time.csv") %>% 
  mutate(log_drive_time = log(drive_time))


###########################################
# 2. Combined and output to dta for Stata #
###########################################

final_dat <- sci_dat %>% 
  left_join(passenger_train_dat) %>% 
  left_join(distance_dat) %>% 
  left_join(rename(rail_time_dat, user_loc=origin, fr_loc=dest)) %>% 
  left_join(drive_time_dat) %>% 
  # Add country pair fixed effects
  mutate(user_country = substr(user_loc, 1, 2),
         fr_country = substr(fr_loc, 1, 2)) %>% 
  mutate(ctry_pair = paste(user_country,fr_country,sep="-"))

write_dta(final_dat, "_intermediate_data/final_regress_dat/passenger_rail_regress_dat.dta")