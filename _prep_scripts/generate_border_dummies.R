# Purpose: Generate dummies for each country that tell whether the two countries
#           border eachother. The final dataset will have rows for each NUTS2 region.
# Inputs: _raw_data/borders/GEODATASOURCE-COUNTRY-BORDERS.csv;
#         _raw_data/SCI_Nuts2_Nuts2.csv;
# Outputs: _intermediate_data/countries_border.csv
# Date: 2020-10-29

library(tidyverse)
library(haven)

# Read in border data
border_dat <- read_csv("_raw_data/borders/GEODATASOURCE-COUNTRY-BORDERS.csv") %>% 
  select(user_country = country_code, fr_country = country_border_code) %>% 
  # Add the islands as fr_countries so they will be caught in the "complete" below
  add_row(user_country = NA, fr_country = "IS") %>% 
  add_row(user_country = NA, fr_country = "MT") %>% 
  add_row(user_country = NA, fr_country = "CY") %>% 
  mutate(country_border = 1) %>% 
  # Get every country
  complete(user_country, fr_country) %>% 
  mutate(country_border = if_else(is.na(country_border), 0, country_border)) %>% 
  mutate(user_country = case_when(
    user_country == "GB" ~ "UK",
    user_country == "GR" ~ "EL",
    TRUE ~ user_country
  )) %>% 
  mutate(fr_country = case_when(
    fr_country == "GB" ~ "UK",
    fr_country == "GR" ~ "EL",
    TRUE ~ fr_country
  ))

# Now we want to limit to only Europe. We do this using the SCI data
sci_dat <- read_csv("_raw_data/SCI_Nuts2_Nuts2.csv") %>% 
  mutate(user_country = substr(user_loc,1,2),
         fr_country = substr(fr_loc,1,2)) %>% 
  select(-sci)

# Combine data
final_dat <- sci_dat %>% 
  left_join(border_dat)

# Write the final data
write_csv(final_dat, "_intermediate_data/countries_border.csv")