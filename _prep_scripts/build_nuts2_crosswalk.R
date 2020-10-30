# Purpose: This builds a NUTS2 crosswalk from 2003 -> 2016
#           it comes from data avaliable at https://ec.europa.eu/eurostat/web/nuts/history
# Inputs: _raw_data/eurostat/demo_r_pjanaggr3.tsv;
# Outputs: _intermediate_data/crosswalks/NUTS2_update_crosswalk.csv
# Date: 2020-10-29

# NOTE!! When two regions are merged, this will make spererate rows
# one needs to "group by" after joining in this data.

library(tidyverse)

# Read-in the 2015 population data for weighting splits data
nuts2_pop <- read.table("_raw_data/eurostat/demo_r_pjanaggr3.tsv", sep = '\t', header = TRUE) %>% 
  separate(unit.sex.age.geo.time, c("unit","sex","age","geo"), ",") %>% 
  filter(sex == "T", age == "TOTAL") %>%
  # Albania is missing 2015 data but not 2016 data - we backfill using 2016
  mutate(X2015 = if_else(geo %in% c("AL01", "AL02", "AL03"),
                         as.character(X2016),
                         as.character(X2015))) %>% 
  select(region=geo, X2015) %>% 
  mutate(pop_2015 = gsub("[^0-9\\.]", "", X2015)) %>%  # Remove comment codes
  mutate(pop_2015 = as.numeric(pop_2015)) %>% 
  select(-X2015) %>% 
  filter(str_length(region) == 4) %>% 
  as_tibble()

# Update BG 03 -> 16
bg_crosswalk_03 <- tibble(
  code_original=c("BG11","BG12","BG21"),
  code_update=c("BG31","BG32","BG41"),
  stat_weight=c(1,1,1))

# Update DE 03 -> 16
de_crosswalk_03 <- tibble(
    code_original=c("DEE1","DEE2","DEE3"),
    code_update=c("DEE0","DEE0","DEE0"),
    stat_weight=c(1,1,1))

# Update DK 03 -> 16
dk_pop <- sum(filter(nuts2_pop, grepl("DK", region))$pop_2015)

dk_crosswalk_03 <- tibble(
  code_original=c("DK00","DK00","DK00","DK00","DK00"),
  code_update=c("DK01","DK02","DK03","DK04","DK05"),
  stat_weight=c(
    filter(nuts2_pop, region == "DK01")$pop_2015 / dk_pop,
    filter(nuts2_pop, region == "DK02")$pop_2015 / dk_pop,
    filter(nuts2_pop, region == "DK03")$pop_2015 / dk_pop,
    filter(nuts2_pop, region == "DK04")$pop_2015 / dk_pop,
    filter(nuts2_pop, region == "DK05")$pop_2015 / dk_pop))

# Update RO 03 -> 16
ro_crosswalk_03 <- tibble(
  code_original=c("RO06","RO07","RO01","RO02","RO03","RO08","RO04","RO05"),
  code_update=c("RO11","RO12","RO21","RO22","RO31","RO32","RO41","RO42"),
  stat_weight=c(1,1,1,1,1,1,1,1)
)

# Update SE 03 -> 16
se_crosswalk_03 <- tibble(
  code_original=c("SE01","SE02","SE09","SE04","SE0A","SE06","SE07","SE08"),
  code_update=c("SE11","SE12","SE21","SE22","SE23","SE31","SE32","SE33"),
  stat_weight=c(1,1,1,1,1,1,1,1)
)

# Update SI (pick up both 03 -> 16 and 10 -> 16)
si_crosswalk_03_10 <- tibble(
  code_original=c("SI00","SI00","SI01","SI02"),
  code_update=c("SI03","SI04","SI03","SI04"),
  stat_weight=c(
    filter(nuts2_pop, region == "SI03")$pop_2015 / (filter(nuts2_pop, region == "SI03")$pop_2015+filter(nuts2_pop, region == "SI04")$pop_2015),
    filter(nuts2_pop, region == "SI04")$pop_2015 / (filter(nuts2_pop, region == "SI03")$pop_2015+filter(nuts2_pop, region == "SI04")$pop_2015),
    1,1
  ))

# Update UK 03 -> 16
uk_crosswalk_03 <- tibble(
  code_original=c("UKM1","UKM4"),
  code_update=c("UKM5","UKM6"),
  stat_weight=c(1,1))

# Update DE 06 -> 16
de_crosswalk_06 <- tibble(
  code_original=c("DED1","DED3","DE41","DE42"),
  code_update=c("DED4","DED5","DE40","DE40"),
  stat_weight=c(1,1,1,1))

# Update GR/EL (pick up both 03 -> 16 and 10 -> 16)
gr_crosswalk_03_10 <- tibble(
  code_original=c("GR11","GR12","GR13","GR14","GR21","GR22",
                  "GR23","GR24","GR25","GR30","GR41","GR42","GR43",
                  "EL11", "EL12", "EL13", "EL21", "EL14","EL22",
                  "EL23","EL24","EL25"),
  code_update=c("EL51","EL52","EL53","EL54","EL61","EL62",
                "EL63","EL64","EL65","EL30","EL41","EL42","EL43",
                "EL51", "EL52", "EL53", "EL54", "EL61", "EL62",
                "EL63", "EL64", "EL65"),
  stat_weight=rep(1,22))

# Update IT 06 -> 16
it_crosswalk_06 <- tibble(
  code_original=c("ITD1","ITD2","ITD3","ITD4","ITE4","ITE1","ITE2","ITD5","ITE3"),
  code_update=c("ITH1","ITH2","ITH3","ITH4","ITI4","ITI1","ITI2","ITH5","ITI3"),
  stat_weight=c(1,1,1,1,1,1,1,1,1)
)

# Update FI 06 -> 16
fi_crosswalk_06 <- tibble(
  code_original=c("FI18","FI18","FI13","FI1A"),
  code_update=c("FI1B","FI1C","FI1D","FI1D"),
  stat_weight=c(
    filter(nuts2_pop, region == "FI1B")$pop_2015 / (filter(nuts2_pop, region == "FI1B")$pop_2015+filter(nuts2_pop, region == "FI1C")$pop_2015),
    filter(nuts2_pop, region == "FI1C")$pop_2015 / (filter(nuts2_pop, region == "FI1B")$pop_2015+filter(nuts2_pop, region == "FI1C")$pop_2015),
    1,1
  )
)

# Update UK 06 -> 16
uk_crosswalk_06 <- tibble(
  code_original=c("UKD2","UKD5"),
  code_update=c("UKD6","UKD7"),
  stat_weight=c(1,1))

# Update FR (pick up both 10 -> 16 and 13 -> 16)
fr_crosswalk_10_13 <- tibble(
  code_original=c("FR91","FR92","FR93","FR94",
                  "FR24","FR26","FR43","FR25","FR23","FR30",
                  "FR22","FR42","FR21","FR41","FR51","FR52",
                  "FR61","FR63","FR53","FR81","FR62","FR72",
                  "FR71","FR82","FR83","FRA1","FRA2","FRA3",
                  "FRA4","FRA5"),
  code_update=c("FRY1","FRY2","FRY3","FRY4",
                "FRB0","FRC1","FRC2","FRD1","FRD2","FRE1",
                "FRE2","FRF1","FRF2","FRF3","FRG0","FRH0",
                "FRI1","FRI2","FRI3","FRJ1","FRJ2","FRK1",
                "FRK2","FRL0","FRM0","FRY1","FRY2","FRY3",
                "FRY4","FRY5"),
  stat_weight=rep(1,30))

# Update HR 10 -> 16
# This is not recorded in historical crosswalks because HR wasn't in EU before 2013.
# The mapping was done by looking at the names of regions
hr_crosswalk_10 <- tibble(
  code_original=c("HR01","HR02"),
  code_update=c("HR04","HR04"),
  stat_weight=c(1,1)
)

# Update UK 10 -> 16
uki3_of_uki1 <- filter(nuts2_pop, region == "UKI3")$pop_2015 / (filter(nuts2_pop, region == "UKI3")$pop_2015 + 
                                                                  filter(nuts2_pop, region == "UKI4")$pop_2015)
uki4_of_uki1 <- 1 - uki3_of_uki1
uki5_of_uki2 <- filter(nuts2_pop, region == "UKI5")$pop_2015 / (filter(nuts2_pop, region == "UKI5")$pop_2015 + 
                                                                  filter(nuts2_pop, region == "UKI6")$pop_2015 +
                                                                  filter(nuts2_pop, region == "UKI7")$pop_2015)
uki6_of_uki2 <- filter(nuts2_pop, region == "UKI6")$pop_2015 / (filter(nuts2_pop, region == "UKI5")$pop_2015 + 
                                                                  filter(nuts2_pop, region == "UKI6")$pop_2015 +
                                                                  filter(nuts2_pop, region == "UKI7")$pop_2015)
uki7_of_uki2 <- 1 - (uki5_of_uki2+uki6_of_uki2)

uk_crosswalk_10 <- tibble(
  code_original=c("UKI1","UKI1","UKI2","UKI2","UKI2"),
  code_update=c("UKI3","UKI4","UKI5","UKI6","UKI7"),
  stat_weight=c(uki3_of_uki1,uki4_of_uki1,uki5_of_uki2,uki6_of_uki2,uki7_of_uki2))


# Update IE 13 -> 16
ie_crosswalk_13 <- tibble(
  code_original=c("IE01","IE02","IE02"),
  code_update=c("IE04","IE05","IE06"),
  stat_weight=c(
    1,
    filter(nuts2_pop, region == "IE05")$pop_2015 / (filter(nuts2_pop, region == "IE05")$pop_2015+filter(nuts2_pop, region == "IE06")$pop_2015),
    filter(nuts2_pop, region == "IE06")$pop_2015 / (filter(nuts2_pop, region == "IE05")$pop_2015+filter(nuts2_pop, region == "IE06")$pop_2015)
  )
)

# Upadte LT 13 -> 16
lt_crosswalk_13 <- tibble(
  code_original=c("LT00","LT00"),
  code_update=c("LT01","LT02"),
  stat_weight=c(
    filter(nuts2_pop, region == "LT01")$pop_2015 / (filter(nuts2_pop, region == "LT01")$pop_2015+filter(nuts2_pop, region == "LT02")$pop_2015),
    filter(nuts2_pop, region == "LT02")$pop_2015 / (filter(nuts2_pop, region == "LT01")$pop_2015+filter(nuts2_pop, region == "LT02")$pop_2015)
  )
)

# Update HU 13 -> 16
hu_crosswalk_13 <- tibble(
  code_original=c("HU10","HU10"),
  code_update=c("HU11","HU12"),
  stat_weight=c(
    filter(nuts2_pop, region == "HU11")$pop_2015 / (filter(nuts2_pop, region == "HU11")$pop_2015+filter(nuts2_pop, region == "HU12")$pop_2015),
    filter(nuts2_pop, region == "HU12")$pop_2015 / (filter(nuts2_pop, region == "HU11")$pop_2015+filter(nuts2_pop, region == "HU12")$pop_2015)
  )
)

# Update PL 13 -> 16
pl_crosswalk_13 <- tibble(
  code_original=c("PL11","PL33","PL31","PL32","PL34","PL12","PL12"),
  code_update=c("PL71","PL72","PL81","PL82","PL84","PL91","PL92"),
  stat_weight=c(
    1,1,1,1,1,
    filter(nuts2_pop, region == "PL91")$pop_2015 / (filter(nuts2_pop, region == "PL91")$pop_2015+filter(nuts2_pop, region == "PL92")$pop_2015),
    filter(nuts2_pop, region == "PL92")$pop_2015 / (filter(nuts2_pop, region == "PL91")$pop_2015+filter(nuts2_pop, region == "PL92")$pop_2015)
  )
)

# Update UK 13 -> 16
uk_crosswalk_13 <- tibble(
  code_original=c("UKM2","UKM3","UKM3"),
  code_update=c("UKM7","UKM8","UKM9"),
  stat_weight=c(
    1,
    filter(nuts2_pop, region == "UKM8")$pop_2015 / (filter(nuts2_pop, region == "UKM8")$pop_2015+filter(nuts2_pop, region == "UKM9")$pop_2015),
    filter(nuts2_pop, region == "UKM9")$pop_2015 / (filter(nuts2_pop, region == "UKM8")$pop_2015+filter(nuts2_pop, region == "UKM9")$pop_2015)
  )
)

# Grab all the crosswalks (based on name being _crosswalk_)
all_crosswalk <- bind_rows(lapply(ls()[grepl("_crosswalk_", ls())], get))

# Write the final csv
write_csv(all_crosswalk, "_intermediate_data/crosswalks/NUTS2_update_crosswalk.csv")