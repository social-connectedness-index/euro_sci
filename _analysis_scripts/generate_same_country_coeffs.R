# Purpose: Create bar chart that shows magnitude of same country
#           fixed effects by country.
# Inputs: _intermediate_data/final_regress_dat/nuts2_to_nuts2_regress_dat.csv
# Outputs: _output/same_country_coeffs
# Date: 2020-10-29

library(tidyverse)
library(lfe)
library(broom)
library(dichromat)
library(RColorBrewer)

###################
# 1. Read in data #
###################

# see: make_final_nuts2_to_nuts2_regress_dat.R
regress_dat <- read_csv("_intermediate_data/final_regress_dat/nuts2_to_nuts2_regress_dat.csv") %>% 
  # Add country-specific same-country fixed effects
  mutate(both_country=if_else(country_same, user_country, "0")) %>% 
  mutate(both_country=as.factor(both_country)) %>% 
  mutate(both_country=relevel(both_country, ref="0"))

# Read in population data (for our second model)
country_pop <- read.table("_raw_data/eurostat/demo_r_pjanaggr3.tsv", sep = '\t', header = TRUE) %>% 
  separate(unit.sex.age.geo.time, c("unit","sex","age","geo"), ",") %>% 
  filter(sex == "T", age == "TOTAL") %>% 
  select(region=geo, X2017) %>% 
  mutate(pop_2017 = gsub("[^0-9\\.]", "", X2017)) %>%  # Remove comment codes
  mutate(pop_2017 = as.numeric(pop_2017)) %>% 
  select(-X2017) %>% 
  filter(str_length(region) == 2) %>% 
  as_tibble()


#################################################
# 2. Build the base model and output coeff plot #
#################################################

base_model <- felm(log_sci~log_distance+both_country+country_border+diff_share_low_edu+diff_median_age+diff_income_thous+
                     diff_unemp_rate+same_rlgdnm_wo_none+same_main_language+industry_sim |
                     user_loc+fr_loc | 0 | 0, data=regress_dat)

coeff_dat <- base_model %>% 
  tidy() %>% 
  filter(grepl("both_country", term)) %>% 
  mutate(term=substr(term, str_length(term)-1, str_length(term))) %>% 
  select(term, estimate)

pal <- colorRampPalette(brewer.pal(8,"Set3"))

set.seed(3)
pal_shuffled <- sample(pal(nrow(coeff_dat)), nrow(coeff_dat), replace=F)

ggplot(coeff_dat, aes(x=reorder(term, estimate), y=estimate, fill=term)) +
  geom_bar(stat="identity") +
  labs(x="Country", y="Coefficient") +
  guides(fill = FALSE) +
  theme_bw() +
  scale_fill_manual(values = pal_shuffled)

ggsave("_output/same_country_coeffs/distance_and_demographic_controls.png", last_plot())
