# Purpose: Generate Industry-Shares by NUTS2
#           Here we grab the most recent year the data are
#           available from 2014 to 2017.
#           Also calculate the cosine distance of vectors
#           of industry shares between each region.
# Inputs: _raw_data/eurostat/sbs_r_nuts06_r2.tsv;
# Outputs: _intermediate_data/industry_shares.csv
#           _intermediate_data/industry_similarity.csv
# Date: 2020-10-29

library(tidyverse)
library(lsa)

#############################
# 1. Read-in and clean data #
#############################

dat_in <- read.table("_raw_data/eurostat/sbs_r_nuts06_r2.tsv", sep = '\t', header = TRUE) %>% 
  separate(nace_r2.indic_sb.geo.time, c("nace_r2","indic_sb","geo"), ",")

# There are many different measures - we choose to focus on employment
# So filter to V16110: Persons employed - number
dat_long <- dat_in %>% 
  filter(indic_sb == "V16110",
         str_length(geo) == 4 | str_length(geo) == 3,
         !grepl("ZZ", geo)) %>% 
  select(-indic_sb) %>% 
  # We only keep the highest level nace
  filter(str_length(nace_r2) == 1) %>%
  # Make long
  gather(year, workers, -nace_r2, -geo) %>% 
  # Remove the comment codes and X's from years
  mutate(workers = gsub("[^0-9\\.]", "", workers),
         year = gsub("[^0-9\\.]", "", year)) %>%
  mutate(workers = as.numeric(workers),
         year = as.numeric(year)) %>% 
  as_tibble()


#############################
# 2. Crosswalk to NUTS 2016 #
#############################

# see ../_prep_scripts/build_nuts2_crosswalk.R
nuts2_crosswalk <- read_csv("_intermediate_data/crosswalks/NUTS2_update_crosswalk.csv")

dat_crosswalked <- dat_long %>% 
  left_join(nuts2_crosswalk, by=c(geo="code_original")) %>% 
  mutate(workers = if_else(!is.na(code_update), workers*stat_weight, workers)) %>% 
  mutate(geo = if_else(!is.na(code_update), code_update, geo)) %>% 
  select(-code_update, -stat_weight) %>% 
  # Group by to handle any merges
  group_by(nace_r2, geo, year) %>% 
  # We want to keep NAs as NA, but sum if some are filled
  mutate(all_missing = sum(is.na(workers)) == n()) %>% 
  summarise(workers = if_else(first(all_missing) == F, sum(workers, na.rm=T), NA_real_)) %>% 
  ungroup

############################
# 3. Create final data set #
############################

dat_final <- dat_crosswalked %>% 
  arrange(desc(year)) %>% 
  group_by(geo, nace_r2) %>%
  summarise(workers = first(na.omit(workers))) %>% 
  ungroup %>% 
  # Fill remaining NAs with zeros
  mutate(workers = if_else(is.na(workers), 0, workers)) %>% 
  group_by(geo) %>% 
  mutate(total_workers = sum(workers)) %>% 
  ungroup %>% 
  mutate(industry_share = workers/total_workers)

write_csv(dat_final, "_intermediate_data/industry_shares.csv")

#####################################################
# 4. Create industry share vector cosine similarity #
#####################################################

sim_matrix <- dat_final %>%
  select(-workers, -total_workers) %>% 
  spread(geo, industry_share) %>% 
  select(-nace_r2) %>% 
  as.matrix %>% 
  cosine

industry_sim_final <- as_tibble(sim_matrix) %>% 
  mutate(user_loc = colnames(sim_matrix)) %>% 
  gather(fr_loc, industry_sim, -user_loc)

write_csv(industry_sim_final, "_intermediate_data/industry_similarity.csv")