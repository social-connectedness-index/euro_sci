# Purpose: Make the final regression data for the anti-EU outcomes regressions
# Inputs: _intermediate_data/eurostat_controls.csv;
#         _intermediate_data/share_friends_nuts2_to_country.csv;
#         _intermediate_data/share_friends_nuts1_to_country.csv;
#         _intermediate_data/brookings_election_dat.csv;
#         _intermediate_data/eurobarometer_trust_in_EU.csv
# Outputs: _intermediate_data/final_regress_dat/eurobarometer_regress_dat.dta;
#           _intermediate_data/final_regress_dat/brookings_regress_dat.dta;
# Date: 2020-10-29

library(tidyverse)
library(haven)

#####################################
# 1. Read-in and clean each dataset #
#####################################

# see: _prep_scripts/generate_eurostat_controls.R
eurostat_dat <- read_csv("_intermediate_data/eurostat_controls.csv") %>% 
  mutate(ED0_2 = ED0_2/100, ED3_4 = ED3_4/100, ED5_8 = ED5_8/100) %>% 
  mutate_at(colnames(.)[grepl("Y",colnames(.))], function(x) x/100)

# see: _prep_scripts/generate_industry_shares.R
industry_shares_dat <- read_csv("_intermediate_data/industry_shares.csv") %>% 
  select(-workers, total_workers) %>% 
  spread(nace_r2, industry_share)

# see: _prep_scripts/generate_region_to_country_frnd_shares.R
share_friends_nuts2_in <- read_csv("_intermediate_data/share_friends_nuts2_to_country.csv")
share_friends_nuts1_in <- read_csv("_intermediate_data/share_friends_nuts1_to_country.csv")

# Get the two metrics of interst: share of EU friendships outside own country and number of countries that make up >1% of friendships
share_friends_dat <- bind_rows(share_friends_nuts2_in, share_friends_nuts1_in) %>% 
  mutate(own_country = substr(user_loc,1,2) == fr_country) %>% 
  filter(own_country == T) %>% 
  mutate(share_EU_connections_out_country = 1-share_connections_in_fr_country) %>% 
  select(user_loc, fr_country, share_EU_connections_out_country)

# see: _prep_scripts/generate_election_dat_from_brookings.R
brookings_dat <- read_csv("_intermediate_data/brookings_election_dat.csv")

# see: _prep_scripts/generate_eurobarometer_survey_dat.R
eurobarometer_dat <- read_csv("_intermediate_data/eurobarometer_trust_in_EU.csv")

########################################
# 2. Generate two regression data sets #
########################################

num_cuts <- 10

eurobarometer_regress_dat <- eurobarometer_dat %>% 
  # Add share friends
  left_join(share_friends_dat, by=c("NUTS_ID"="user_loc")) %>%
  # Add eurostat controls
  left_join(eurostat_dat, by=c("NUTS_ID"="geo")) %>% 
  # Add industry share controls
  left_join(industry_shares_dat, by=c("NUTS_ID"="geo")) %>%
  mutate(manufacturing_emp = C,
         construction_emp = `F`,
         # M = Professional, Scientific and Technical Activities
         # N = Administrative and Support Service Activities
         professional_emp = M+N) %>%
  # Add income in thousands
  mutate(income_thous = income/1000) %>% 
  # Filter to NUTS regions for which we have eurostat dat
  filter(!is.na(income), !is.na(unemp_rate), !is.na(manufacturing_emp)) %>% 
  mutate(country_fe = substr(NUTS_ID,1,2)) %>%
  rename(`Share EU Connections Outside Country`=share_EU_connections_out_country, `Share trust in EU`=trust_pct) %>% 
  # Create country fixed effect
  rename(cntry_fe=fr_country)

brookings_regress_dat <- brookings_dat %>% 
  # Add share friends
  left_join(share_friends_dat, by=c("nuts"="user_loc")) %>%
  # Add eurostat controls
  left_join(eurostat_dat, by=c("nuts"="geo")) %>% 
  # Add industry share controls
  left_join(industry_shares_dat, by=c("nuts"="geo")) %>%
  mutate(manufacturing_emp = C,
         construction_emp = `F`,
         # M = Professional, Scientific and Technical Activities
         # N = Administrative and Support Service Activities
         professional_emp = M+N) %>%
  # Add income in thousands
  mutate(income_thous = income/1000) %>% 
  # Filter to NUTS regions for which we have eurostat dat
  filter(!is.na(income), !is.na(unemp_rate), !is.na(manufacturing_emp)) %>% 
  mutate(country_fe = substr(nuts,1,2)) %>%
  rename(`Share EU Connections Outside Country`=share_EU_connections_out_country, `Share vote for Anti-EU party`=extreme_perae) %>% 
  # Create country fixed effect
  rename(cntry_fe=fr_country)


eurobarometer_regress_dat %>% 
  rename(share_EU_connections_out_country=`Share EU Connections Outside Country`, Trust_in_EU=`Share trust in EU`) %>% 
  write_dta("_intermediate_data/final_regress_dat/eurobarometer_regress_dat.dta")

brookings_regress_dat %>% 
  rename(share_EU_connections_out_country=`Share EU Connections Outside Country`, Anti_EU_vote=`Share vote for Anti-EU party`) %>% 
  write_dta("_intermediate_data/final_regress_dat/brookings_regress_dat.dta")

# For appendix, report these stats
cor(eurobarometer_regress_dat$share_other_eu, eurobarometer_regress_dat$`Share trust in EU`)

cor(brookings_regress_dat$share_other_eu, brookings_regress_dat$`Share vote for Anti-EU party`)