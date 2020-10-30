# Purpose: Read-in election data from the Brookings Paper:
#           "The European trust crisis and the rise of populism"
# Inputs: _raw_data/brookings_paper/voting_ESS_NUTS2.dta
# Outputs: _intermediate_data/brookings_election_dat.csv
# Date: 2020-10-29

# Report and data at: https://www.brookings.edu/bpea-articles/the-european-trust-crisis-and-the-rise-of-populism/

library(tidyverse)
library(haven)

################################
# 1. Read-in and prep the data #
################################

brookings_in <- read_dta("_raw_data/brookings_paper/voting_ESS_NUTS2.dta", encoding='latin1') %>% 
  # This filters out the full country data
  filter(nv == 0) %>%
  # What they are calling FR[1-5] appear to be the islands FRY[1-5] - we delete as in our other analyses
  filter(!nuts %in% c("FR1", "FR2", "FR3", "FR4", "FR5"))

# Average vote shares for any election past 2010
brookings_avgs <- brookings_in %>% 
  # We look at any election past 2010
  filter(year > 2008) %>% 
  select(nuts, nutsname, year, starts_with("extreme")) %>% 
  select(-ends_with("_new")) %>% 
  gather(vote_type, pct, -nuts, -nutsname, -year) %>% 
  filter(!is.na(pct)) %>% 
  group_by(nuts, nutsname, vote_type) %>% 
  summarise(pct = mean(pct, na.rm=T)) %>% 
  ungroup


############################
# 2. Crosswalk to NUTS2016 #
############################

# see ../_prep_scripts/build_nuts2_crosswalk.R
nuts2_crosswalk <- read_csv("_intermediate_data/crosswalks/NUTS2_update_crosswalk.csv")

brookings_final <- brookings_avgs %>% 
  # Here we change a couple single NUTS1 region country-level dat to their NUTS1 ID
  mutate(nuts = case_when(
    nuts == "CY" ~ "CY0",
    nuts == "DK" ~ "DK0",
    nuts == "EE" ~ "EE0",
    TRUE ~ nuts
  )) %>% 
  # Now we crosswalk the NUTS2 to NUTS2016
  left_join(nuts2_crosswalk, by=c(nuts="code_original")) %>% 
  mutate(pct = if_else(!is.na(code_update), pct*stat_weight, pct)) %>% 
  mutate(nuts = if_else(!is.na(code_update), code_update, nuts)) %>% 
  select(-code_update, -stat_weight) %>%
  # Then group by for any region merges (THERE WERE NO MERGES IN THIS 2013->2016 update)
  group_by(nuts, vote_type) %>% 
  summarise(pct = mean(pct)) %>% 
  ungroup %>% 
  # Finally spread so that each of these are seperate columns
  spread(vote_type, pct)

write_csv(brookings_final, "_intermediate_data/brookings_election_dat.csv")