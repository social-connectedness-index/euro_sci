# Purpose: This builds a NUTS1 crosswalk from 2010 -> 2016
#           it comes from data avaliable at https://ec.europa.eu/eurostat/web/nuts/history.
#           This crosswalk assumes the availability of NUTS2 data to make the crosswalk.
# Inputs: NONE
# Outputs: _intermediate_data/crosswalks/NUTS1_update_crosswalk_w_NUTS2_groupings.csv
# Date: 2020-10-29

# NOTE!! When two regions are merged, this will make spererate rows
# one needs to "group by" after joining in this data.

library(tidyverse)

# Update EL 10 -> 13
el_crosswalk_10 <- tibble(
  code_original=c("EL1","EL2"),
  code_update=c("EL5","EL6"),
  stat_weight=c(1,1))

# Update FR 10 -> 13
fr_crosswalk_10 <- tibble(
  code_original=c("FR9"),
  code_update=c("FRA"),
  stat_weight=c(1))

# Update FR 13 -> 16
fr_crosswalk_13 <- tibble(
  code_original=c("FR24","FR26","FR43","FR23","FR25","FR22","FR30",
                  "FR21","FR41","FR42","FR51","FR52","FR53","FR61","FR63",
                  "FR62","FR81","FR7","FR82","FR83","FRA","FR9", # FR9 captures the 10 -> 16 case as well
                  "FR2","FR3","FR4","FR5","FR6","FR7","FR8"),
  code_update=c("FRB","FRC","FRC","FRD","FRD","FRE","FRE",
                "FRF","FRF","FRF","FRG","FRH","FRI","FRI","FRI",
                "FRJ","FRJ","FRK","FRL","FRM","FRY","FRY",
                "DISC","DISC","DISC","DISC","DISC","DISC","DISC"),
  stat_weight=c(rep(1,22), rep(-1,7)))

# Update PL 13 -> 16
pl_crosswalk_16 <- tibble(
  code_original=c("PL1","PL3","PL11","PL33","PL31","PL32","PL34","PL12"),
  code_update=c("DISC","DISC","PL7","PL7","PL8","PL8","PL8","PL9"),
  stat_weight=c(rep(-1,2), rep(1,6)))

# Grab all the crosswalks (based on name being _crosswalk_)
all_crosswalk <- bind_rows(lapply(ls()[grepl("_crosswalk_", ls())], get))

# Write the final csv
write_csv(all_crosswalk, "_intermediate_data/crosswalks/NUTS1_update_crosswalk_w_NUTS2_groupings.csv")