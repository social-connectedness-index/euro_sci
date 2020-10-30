# Purpose: Assemble the final regression data for the NUTS2-to-NUTS2 SCI regressions.
# Inputs: _raw_data/SCI_Nuts2_Nuts2.csv;
#         _intermediate_data/geo_distance_dat.csv
#         _intermediate_data/historical/country_historical.csv
#         _intermediate_data/eurostat_controls.csv
#         _intermediate_data/countries_border.csv
#         _intermediate_data/ess_controls.csv
#         _intermediate_data/industry_similarity.csv
# Outputs: _intermediate_data/final_regress_dat/nuts2_to_nuts2_regress_dat.csv
#           _output/nuts2_to_nuts2_regress.pdf
# Date: 2020-10-29

library(tidyverse)
library(haven)

###################
# 1. Read in data #
###################

sci_dat <- read_csv("_raw_data/SCI_Nuts2_Nuts2.csv") %>% 
  mutate(log_sci = log(sci)) %>% 
  filter(user_loc != fr_loc)

# see: _prep_scripts/generate_distances
distance_dat <- read_csv("_intermediate_data/geo_distance_dat.csv") %>%
  mutate(log_distance = log(distance))

# see: _prep_scripts/generate_30_year_historicals.R
hist_full_dat <- read_csv("_intermediate_data/historical/country_historical.csv") %>% 
  select(-country)

# see: _prep_scripts/generate_eurostat_controls.R
eurostat_dat <-
  read_csv("_intermediate_data/eurostat_controls.csv") %>% 
  select(-ED3_4, -ED5_8, -starts_with("Y"), -share_other_eu, -share_non_eu, -unemp_rate) %>% 
  rename(unemp_rate = unemp_rate_avg)
  
# see: _prep_scripts/generate_border_dummies.R
country_border_dat <- 
  read_csv("_intermediate_data/countries_border.csv")

# see: _prep_scripts/generate_ess_controls.R
ess_dat <- 
  read_csv("_intermediate_data/ess_controls.csv")

# see: _prep_scripts/generate_industry_shares.R
industry_sim_dat <- 
  read_csv("_intermediate_data/industry_similarity.csv")

############################
# 2. Build regression data #
############################

# We will drop a few regions that do not have complete data
drop_regions <- c(
  "FRY1", "FRY2", "FRY3", "FRY4", "FRY5", # French Islands
  "PT20", "PT30", # Portugese Islands
  "FI20", # Finish Isle - no unemployment data
  "ES64", # Melilla - ALL EXCEPT ESS (a Spanish autonomous city located on Africa)
  "FRM0", # Corsica - ALL EXCEPT ESS
  "ITF2" ) # Molise - ALL EXCEPT ESS (an under-populated area in Italy)

# Get the countries that were in Europe pre-2004 accession
eu_2003 <- c("AT", "BE", "DE", "DK", "EL", "ES", "FI", "FR",
             "IE", "IT", "LU", "NL", "PT", "SE", "UK")

regress_dat <- sci_dat %>% 
  # Drop the regions that we enumerated above
  filter(!user_loc %in% drop_regions, !fr_loc %in% drop_regions) %>%
  # Add the distance data
  left_join(distance_dat) %>% 
  # Add the country border data
  left_join(country_border_dat) %>% 
  # Add whether the country is in eu_2003
  mutate(user_eu_2003 = user_country %in% eu_2003,
         fr_eu_2003 = fr_country %in% eu_2003) %>% 
  # Add country pair fixed effects
  mutate(ctry_pair = paste(user_country,fr_country,sep="-")) %>% 
  # Add the history data
  left_join(hist_full_dat, by=c("user_loc"="NUTS_ID")) %>% 
  rename(user_country_1900 = country_1900, user_country_1930 = country_1930,
         user_country_1960 = country_1960, user_country_1990 = country_1990) %>% 
  left_join(hist_full_dat, by=c("fr_loc"="NUTS_ID")) %>%
  rename(fr_country_1900 = country_1900, fr_country_1930 = country_1930,
         fr_country_1960 = country_1960, fr_country_1990 = country_1990) %>% 
  mutate(country_1900_same = user_country_1900==fr_country_1900,
         country_1930_same = user_country_1930==fr_country_1930,
         country_1960_same = user_country_1960==fr_country_1960,
         country_1990_same = user_country_1990==fr_country_1990,
         country_same = user_country==fr_country) %>% 
  # Historical event dummies - 1900
  mutate(both_austro_hung_empire = user_country_1900 == "AT" & fr_country_1900 == "AT") %>% # 1900 Russia
  mutate(both_de_1900 = user_country_1900 == "DE" & fr_country_1900 == "DE") %>% # 1900 Germany
  mutate(both_ru_1900 = user_country_1900 == "RU" & fr_country_1900 == "RU") %>% # 1900 Russia
  mutate(both_se_no_1900 = user_country_1900 == "SE" & fr_country_1900 == "SE") %>% # 1900 Sweden-Norway
  mutate(both_uk_1900 = user_country_1900 == "UK" & fr_country_1900 == "UK") %>% # Ireland-UK
  # Historical event dummies - 1930
  mutate(both_de_1930 = user_country_1930 == "DE" & fr_country_1930 == "DE") %>% # 1930 Germany
  # Historical event dummies - 1960
  mutate(both_uk_1960 = user_country_1960 == "UK" & fr_country_1960 == "UK") %>% # 1960 UK
  # Historical event dummies - 1960
  mutate(both_west_de = user_country_1990 == "DE" & fr_country_1990 == "DE") %>% # East Germany
  mutate(both_east_de = user_country_1990 == "GC" & fr_country_1990 == "GC") %>% # West Germany
  mutate(both_eastern_bloc =
           (user_country_1960 %in% c("BG", "CS", "GC", "HU", "PL", "RO", "RU") &  # Eastern Bloc
              fr_country_1960 %in% c("BG", "CS", "GC", "HU", "PL", "RO", "RU"))) %>%
  mutate(both_soviet_union = user_country_1990 == "RU" & fr_country_1990 == "RU") %>% 
  # Historical event dummies - 1990
  mutate(both_czechoslovakia = user_country_1990 == "CS" & fr_country_1990 == "CS") %>% # Czechoslovakia
  mutate(both_yugoslavia = user_country_1990 == "YO" & fr_country_1990 == "YO") %>% # Yugoslavia
  # Add the Eurostat controls
  left_join(eurostat_dat, by=c(user_loc="geo")) %>% 
  rename(user_ED0_2=ED0_2, user_median_age=median_age,
         user_income=income, user_unemp_rate=unemp_rate) %>% 
  left_join(eurostat_dat, by=c(fr_loc="geo")) %>% 
  rename(fr_ED0_2=ED0_2, fr_median_age=median_age,
         fr_income=income, fr_unemp_rate=unemp_rate) %>% 
  mutate(diff_share_low_edu = abs(user_ED0_2 - fr_ED0_2),
         diff_median_age = abs(user_median_age - fr_median_age),
         diff_income_thous = abs(user_income - fr_income)/1000,
         diff_unemp_rate = abs(user_unemp_rate - fr_unemp_rate)) %>% 
  # Add in the ESS controls
  left_join(ess_dat, by="user_loc") %>%
  rename(rlgdnm_user_w_none=highest_rlgdnm_w_none,
         rlgdnm_user_wo_none=highest_rlgdnm_wo_none,
         lnghom_user=highest_lnghom) %>%
  left_join(ess_dat, by=c("fr_loc"="user_loc")) %>%
  rename(rlgdnm_fr_w_none=highest_rlgdnm_w_none,
         rlgdnm_fr_wo_none=highest_rlgdnm_wo_none,
         lnghom_fr=highest_lnghom) %>%
  mutate(same_rlgdnm_w_none = rlgdnm_user_w_none == rlgdnm_fr_w_none,
         same_rlgdnm_wo_none = rlgdnm_user_wo_none == rlgdnm_fr_wo_none,
         same_main_language = lnghom_user == lnghom_fr) %>% 
  # Add in the industry similarity data
  left_join(industry_sim_dat, by = c("user_loc", "fr_loc")) %>% 
  # Add country-specific same-country fixed effects
  mutate(both_country=if_else(country_same, user_country, "0")) %>% 
  mutate(both_country=as.factor(both_country)) %>% 
  mutate(both_country=relevel(both_country, ref="0"))

# Limit to only countries that have data
countries_w_dat <- regress_dat[complete.cases(regress_dat),] %>% .$user_country %>% unique %>% sort
regress_dat <- filter(regress_dat, user_country %in% countries_w_dat, fr_country %in% countries_w_dat)

write_csv(regress_dat, "_intermediate_data/final_regress_dat/nuts2_to_nuts2_regress_dat.csv")
write_dta(regress_dat, "_intermediate_data/final_regress_dat/nuts2_to_nuts2_regress_dat.dta")