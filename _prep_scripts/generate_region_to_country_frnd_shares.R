# Purpose: Calculate, for each NUTS2-country pair and each NUTS1-country pair,
#           the share of friendships from region i to country j.
# Inputs: _raw_data/SCI_Nuts2_Nuts2.csv; _raw_data/eurostat/demo_r_pjanaggr3.tsv
# Outputs: _intermediate_data/share_friends_nuts2_to_country.csv
#           _intermediate_data/share_friends_nuts1_to_country.csv
# Date: 2020-10-29

library(tidyverse)

################################
# 1. Read in and prep the data #
################################

# Read in the SCI data
nuts2_sci <- read_csv("_raw_data/SCI_Nuts2_Nuts2.csv")

# Read in the population data
pop_dat_in <- read.table("_raw_data/eurostat/demo_r_pjanaggr3.tsv", sep = '\t', header = TRUE) %>% 
  separate(unit.sex.age.geo.time, c("unit","sex","age","geo"), ",") %>% 
  filter(sex == "T", age == "TOTAL") %>% 
  select(region=geo, X2017) %>% 
  mutate(pop_2017 = gsub("[^0-9\\.]", "", X2017)) %>%  # Remove comment codes
  mutate(pop_2017 = as.numeric(pop_2017)) %>% 
  select(-X2017)

nuts2_pop <- pop_dat_in %>% 
  filter(str_length(region) == 4) %>% 
  as_tibble()


#######################################################################
# 2. For each NUTS2, get the share of friends from region to country  #
#######################################################################

# Join the population data in
nuts2_sci_pop <- nuts2_sci %>%
  left_join(nuts2_pop, by=c(user_loc="region")) %>% 
  rename(user_pop=pop_2017) %>% 
  left_join(nuts2_pop, by=c(fr_loc="region")) %>% 
  rename(fr_pop=pop_2017)

# Calculate the number of connections and get the country IDs
nuts2_share0 <- nuts2_sci_pop %>% 
  mutate(num_connections = sci*user_pop*fr_pop) %>% 
  mutate(user_country=substr(user_loc,1,2),
         fr_country=substr(fr_loc,1,2))

# Get the share of friends in country
nuts2_share_country <- nuts2_share0 %>% 
  # First just get the number of connections from region to country
  group_by(user_loc, user_country, fr_country) %>% 
  summarise(num_connections_user_loc_fr_country = sum(num_connections)) %>% 
  ungroup %>% 
  # Then get the total number of connections from region
  group_by(user_loc) %>% 
  mutate(total_connections_user_loc = sum(num_connections_user_loc_fr_country)) %>% 
  ungroup %>% 
  # And use that to get the share of connections from region that are to country
  mutate(share_connections_in_fr_country = num_connections_user_loc_fr_country / total_connections_user_loc) %>% 
  select(user_loc, fr_country, share_connections_in_fr_country,
         num_connections_in_fr_country = num_connections_user_loc_fr_country)

write_csv(nuts2_share_country, "_intermediate_data/share_friends_nuts2_to_country.csv")

# Pullout stats in text
share_out_country <- nuts2_share_country %>% 
  filter(substr(user_loc,1,2) == fr_country) %>% 
  mutate(share_out = 1-share_connections_in_fr_country)

# Section 5
median(share_out_country$share_out)
quantile(share_out_country$share_out, .9)
quantile(share_out_country$share_out, .1)

# Appendix
filter(share_out_country, user_loc == "FRE2")$share_out # Picardy
filter(share_out_country, user_loc == "FR10")$share_out # Ile-de-France


#######################################################################
# 3. For each NUTS1, get the share of friends from region to country  #
#######################################################################

# Using the number of connections from each NUTS2, aggregate up to NUTS1
nuts1_share0 <- nuts2_share0 %>% 
  mutate(user_loc = substr(user_loc,1,3),
         fr_loc = substr(fr_loc,1,3)) %>% 
  group_by(user_loc, fr_loc, user_country, fr_country) %>% 
  summarise(num_connections = sum(num_connections)) %>% 
  ungroup

# Get the share of friends in country
nuts1_share_country <- nuts1_share0 %>% 
  # First just get the number of connections from region to country
  group_by(user_loc, user_country, fr_country) %>% 
  summarise(num_connections_user_loc_fr_country = sum(num_connections)) %>% 
  ungroup %>% 
  # Then get the total number of connections from region
  group_by(user_loc) %>% 
  mutate(total_connections_user_loc = sum(num_connections_user_loc_fr_country)) %>% 
  ungroup %>% 
  # And use that to get the share of connections from region that are to country
  mutate(share_connections_in_fr_country = num_connections_user_loc_fr_country / total_connections_user_loc) %>% 
  select(user_loc, fr_country, share_connections_in_fr_country,
         num_connections_in_fr_country = num_connections_user_loc_fr_country)

write_csv(nuts1_share_country, "_intermediate_data/share_friends_nuts1_to_country.csv")