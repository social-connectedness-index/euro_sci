# Purpose: Read-in data from the Eurobarometer on Trust in the EU
# Inputs: _raw_data/eurobarometer/trust_in_eu.csv
#         _raw_data/eurobarometer/trust_in_eu_missing.csv
# Outputs: _intermediate_data/eurobarometer_trust_in_EU.csv
# Date: 2020-10-29

library(tidyverse)

#####################################
##### Read-in Q8 - Trust in EU ######
#####################################

trust_in_eu <- read_csv("_raw_data/eurobarometer/trust_in_eu.csv")

trust_in_eu <- 
  select(trust_in_eu, -ends_with("-1")) %>% # Remove the data from a previous survey
  gather(NUTS_ID, value, -X1) %>% 
  spread(X1, value) %>% 
  select(NUTS_ID, wave = "\\####WAVE####",
         trust_n = "Plutot confiance", trust_pct = "Tend to trust",
         no_trust_n = "Plutot pas confiance", no_trust_pct = "Tend not to trust",
         NA_n = "NSP/SR (NE PAS LIRE)", NA_pct = "DK/NA (DO NOT READ OUT)",
         total = "TOTAL")

trust_in_eu_pct <- select(trust_in_eu, NUTS_ID, trust_pct)

# Some of the data in the excel sheet downloaded from EU Open Data Portal are missing.
# So, in addition, we manually add the data from the pdf data appendix, available here:
# https://ec.europa.eu/commfrontoffice/publicopinion/index.cfm/survey/getsurveydetail/instruments/flash/surveyky/2219
nrow(trust_in_eu_pct) # Should be 204, but it is 173
trust_in_eu.missing <- read_csv("_raw_data/eurobarometer/trust_in_eu_missing.csv")
trust_in_eu.full <- bind_rows(trust_in_eu_pct, trust_in_eu.missing)
nrow(trust_in_eu.full) # Now with missing rows added to get us to 204

# Quite a bit of the data are at NUTS1 level instead of NUTS2
trust_in_eu.full <- trust_in_eu.full %>% 
  mutate(nuts1_level = str_length(NUTS_ID) == 3)

write_csv(trust_in_eu.full, "_intermediate_data/eurobarometer_trust_in_EU.csv")
