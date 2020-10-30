# Purpose: Read in various regression controls from Eurostat data sources
#           We must do this for both NUTS2 and NUTS1 level, as many of our
#           political outcomes are at NUTS1.
# Inputs: _raw_data/eurostat/nama_10r_2hhinc.tsv;
#         _raw_data/eurostat/edat_lfse_04.tsv;
#         _raw_data/eurostat/demo_r_pjanind2.tsv;
#         _raw_data/eurostat/cens_11cobe_r2.tsv;
#         _raw_data/eurostat/lfst_r_lfu3rt.tsv;
#         _raw_data/eurostat/demo_r_pjanaggr3.tsv
# Outputs: _intermediate_data/eurostat_controls.csv
# Date: 2020-10-29

library(tidyverse)

#############
# 1. Income #
#############

# The income data come from: https://ec.europa.eu/eurostat/web/products-datasets/-/nama_10r_2hhinc
# UNIT = Euro per inhabitant
# ITEM = Balance of primary incomes/National income, net
# TIME = 2016
income_dat <- read.table("_raw_data/eurostat/nama_10r_2hhinc.tsv", sep = '\t', header = TRUE) %>% 
  separate(unit.direct.na_item.geo.time, c("unit","direct","na_item","geo"), ",") %>% 
  filter(unit == "EUR_HAB", direct == "BAL", na_item == "B5N") %>% 
  select(geo, X2016) %>% 
  mutate(income = gsub("[^0-9\\.]", "", X2016)) %>%  # Remove comment codes
  mutate(income = as.numeric(income)) %>% 
  select(-X2016) %>%
  # Estonia only has one NUTS1 and NUTS2 region, but the data are only with country code
  add_row(geo="EE0", income=filter(., geo== "EE")$income) %>% 
  add_row(geo="EE00", income=filter(., geo== "EE")$income) %>% 
  # Malta is missing, but because it has only a single NUTS2 region we can use a similar stat
  # that is only recorded at the national level.
  # See http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=ilc_di04&lang=en.
  # We use their 2016 mean equivalised net income in Euros
  add_row(geo="MT00", income=15505) %>% 
  filter(str_length(geo) == 3 | str_length(geo) == 4) %>% 
  as_tibble()

################
# 2. Education #
################

# The education data come from: https://ec.europa.eu/eurostat/en/web/products-datasets/-/EDAT_LFSE_04
# UNIT = Percentage
# AGE = From 25 to 64 years
# TIME = 2018
education_dat <- read.table("_raw_data/eurostat/edat_lfse_04.tsv", sep = '\t', header = TRUE) %>% 
  separate(sex.isced11.age.unit.geo.time, c("sex","isced11","age","unit", "geo"), ",") %>% 
  filter(sex == "T") %>% 
  select(geo, isced11, X2018) %>% 
  mutate(pct = gsub("[^0-9\\.]", "", X2018)) %>%  # Remove comment codes
  mutate(pct = as.numeric(pct)) %>% 
  select(-X2018) %>% 
  spread(isced11, pct) %>% 
  select(geo, ED0_2=`ED0-2`, ED3_4, ED5_8=`ED5-8`) %>% 
  filter(str_length(geo) == 3 | str_length(geo) == 4) %>% 
  as_tibble()


##########
# 3. Age #
##########

# The age data come from: https://ec.europa.eu/eurostat/web/products-datasets/product?code=demo_r_pjanind2
# TIME = 2017
age_dat_in <- read.table("_raw_data/eurostat/demo_r_pjanind2.tsv", sep = '\t', header = TRUE) %>% 
  separate(indic_de.unit.geo.time, c("indic_de","unit","geo"), ",") %>% 
  filter(str_length(geo) == 3 | str_length(geo) == 4) %>% 
  as_tibble()

age_dat_median <- age_dat_in %>% 
  filter(indic_de == "MEDAGEPOP") %>% 
  select(geo, median_age=X2018) %>% 
  mutate(median_age = gsub("[^0-9\\.]", "", median_age)) %>%  # Remove comment codes
  mutate(median_age = as.numeric(median_age))

age_dat_bkts <- age_dat_in %>% 
  # There are a few difference choices for buckets, we choose medium granularity
  filter(indic_de %in% c(
    "PC_Y0_4", "PC_Y5_9", "PC_Y10_14",
    "PC_Y15_19", "PC_Y20_24", "PC_Y25_29",
    "PC_Y30_34", "PC_Y35_39", "PC_Y40_44",
    "PC_Y45_49", 'PC_Y50_54', "PC_Y55_59",
    "PC_Y60_64", "PC_Y65_69", "PC_Y70_74",
    "PC_Y75_79", "PC_Y80_MAX")) %>% 
  mutate(age_bkt = case_when(
    indic_de == "PC_Y0_4" | indic_de == "PC_Y5_9" ~ "Y0_9",
    indic_de == "PC_Y10_14" | indic_de == "PC_Y15_19" ~ "Y10_19",
    indic_de == "PC_Y20_24" | indic_de == "PC_Y25_29" ~ "Y20_29",
    indic_de == "PC_Y30_34" | indic_de == "PC_Y35_39" ~ "Y30_39",
    indic_de == "PC_Y40_44" | indic_de == "PC_Y45_49" ~ "Y40_49",
    indic_de == "PC_Y50_54" | indic_de == "PC_Y55_59" ~ "Y50_59",
    indic_de == "PC_Y60_64" | indic_de == "PC_Y65_69" ~ "Y60_69",
    indic_de == "PC_Y70_74" | indic_de == "PC_Y75_79" | indic_de == "PC_Y80_MAX" ~ "Y70_MAX"
  )) %>% 
  select(geo, age_bkt, pct=X2017) %>% 
  mutate(pct = gsub("[^0-9\\.]", "", pct)) %>%  # Remove comment codes
  mutate(pct = as.numeric(pct)) %>% 
  group_by(geo, age_bkt) %>% 
  summarise(pct = sum(pct)) %>% 
  ungroup %>% 
  spread(age_bkt, pct)


#######################
# 4. Country of birth #
#######################

# The country of birth data come from: https://ec.europa.eu/eurostat/web/products-datasets/-/cens_11cobe_r2
# UNIT = Number
# TIME = 2011
nationality_dat_in <- read.table("_raw_data/eurostat/cens_11cobe_r2.tsv", sep = '\t', header = TRUE) %>% 
  separate(age.sex.c_birth.isced97.unit.time.geo, c("age", "sex", "c_birth", "isced97", "unit", "time"), ",") %>% 
  filter(age == "TOTAL", sex == "T", isced97 == "TOTAL",
         c_birth != "UNK", c_birth != "OTH") %>% 
  select(-age, -sex, -isced97, -unit, -time) %>% 
  gather(key=geo, value=persons, -c_birth) %>% 
  mutate(persons = gsub("[^0-9\\.]", "", persons)) %>%  # Remove comment codes
  mutate(persons = as.numeric(persons)) %>%
  spread(key=c_birth, value=persons) %>% 
  as_tibble() %>% 
  # There looks like there are zeroes that shouldn't be there in IS0, HR03, HR04. We fix these manually by subtracting
  # the statistics that is reported from the total foreign country born statistic that is always reported. For all other
  # regions, this difference is never more than 10 persons away from the other reported statistic.
  mutate(EU_OTH = if_else(geo=="IS0", FOR - NEU, EU_OTH),
         NEU = if_else(grepl("HR", geo), FOR-EU_OTH, NEU)) %>% 
  gather(key=birth_country, value=persons, -geo)

## Crosswalk the NUTS2 nationality data ##
# see ../_prep_scripts/build_nuts2_crosswalk.R
nuts2_crosswalk <- read_csv("_intermediate_data/crosswalks/NUTS2_update_crosswalk.csv")

nationality_dat.nuts2 <- nationality_dat_in %>% 
  filter(str_length(geo) == 4) %>% 
  left_join(nuts2_crosswalk, by=c(geo="code_original")) %>% 
  mutate(persons = if_else(!is.na(code_update), persons*stat_weight, persons)) %>% 
  mutate(geo = if_else(!is.na(code_update), code_update, geo)) %>% 
  select(-code_update, -stat_weight) %>%
  # Then group by for any region merges
  group_by(geo, birth_country) %>% 
  summarise(persons = sum(persons)) %>% 
  ungroup %>% 
  # Finally spread so that each of these are seperate columns
  spread(birth_country, persons)

## Crosswalk the NUTS1 nationality data ##
# see ../_prep_scripts/build_nuts1_crosswalk_w_nuts2_groupings
nuts1_crosswalk <- read_csv("_intermediate_data/crosswalks/NUTS1_update_crosswalk_w_NUTS2_groupings.csv")

# First, get the nuts2 population data
nuts2_pop <- read.table("_raw_data/eurostat/demo_r_pjanaggr3.tsv", sep = '\t', header = TRUE) %>% 
  separate(unit.sex.age.geo.time, c("unit","sex","age","geo"), ",") %>% 
  filter(sex == "T", age == "TOTAL") %>% 
  select(geo, X2017) %>% 
  filter(str_length(geo) == 4) %>% 
  mutate(pop_2017 = gsub("[^0-9\\.]", "", X2017)) %>%  # Remove comment codes
  mutate(pop_2017 = as.numeric(pop_2017)) %>% 
  select(-X2017) %>% 
  as_tibble()

# Then, get the updated regions
nationality_dat.nuts1.updated <- nationality_dat_in %>% 
  filter(str_length(geo) == 4 | str_length(geo) == 3) %>% # Here we need both the NUTS1 and NUTS2 data 
  left_join(nuts2_pop, by="geo") %>% # And the NUTS2 population data to weight merges
  mutate(geo_old = geo) %>% 
  left_join(nuts1_crosswalk, by=c("geo"="code_original")) %>% 
  mutate(geo = if_else(!is.na(code_update), code_update, geo)) %>% 
  # filter to only the updated regions
  filter(!is.na(code_update), code_update != "DISC") %>% 
  mutate(pop_2017 = if_else(is.na(pop_2017), 1, pop_2017)) %>%  # when there are nuts1 to nuts1 switches we don't have pop
  # group the nuts2 regions together
  group_by(geo, birth_country) %>%
  summarise(persons=sum(persons)) %>% 
  ungroup

# Finally, bind the updated regions to the other NUTS1 regions
nationality_dat.nuts1 <- nationality_dat_in %>% 
  filter(!geo %in% nationality_dat.nuts1.updated$geo,
         str_length(geo)==3) %>% 
  bind_rows(nationality_dat.nuts1.updated) %>% 
  spread(birth_country, persons)

## Then bind the nuts2 and nuts1 data together
nationality_dat <- bind_rows(
  nationality_dat.nuts2,
  nationality_dat.nuts1) %>% 
  mutate(share_other_eu = EU_OTH/TOTAL,
         share_non_eu = NEU/TOTAL) %>% 
  select(geo, share_other_eu, share_non_eu)


###################
# 5. Unemployment #
###################

# The unemployment rate data come from: https://ec.europa.eu/eurostat/web/products-datasets/-/lfst_r_lfu3rt
# UNIT  = Percentage
# AGE = 15 to 74 years
# TIME = 2018
unemployment_dat <- read.table("_raw_data/eurostat/lfst_r_lfu3rt.tsv", sep = '\t', header = TRUE) %>% 
  separate(unit.age.sex.geo.time, c("unit", "age", "sex", "geo"), ",") %>% 
  filter(age == "Y15-74", sex == "T") %>%
  # FRM/FRM0 is the only region that does not have 2018 dat - fill with 2017
  mutate(X2018 = as.character(X2018),
         X2016 = as.character(X2016)) %>% 
  mutate(X2018 = if_else(geo %in% c("FRM", "FRM0"), X2016, X2018)) %>% 
  select(geo, unemp_rate=X2018) %>% 
  mutate(unemp_rate = gsub("[^0-9\\.]", "", unemp_rate)) %>% # Remove comment codes
  mutate(unemp_rate = as.numeric(unemp_rate))

# Or: instead we use average unemployment from 2009 to 2018 (THIS IS WHAT WE USE IN THE END)
unemployment_dat_avg <- read.table("_raw_data/eurostat/lfst_r_lfu3rt.tsv", sep = '\t', header = TRUE) %>% 
  separate(unit.age.sex.geo.time, c("unit", "age", "sex", "geo"), ",") %>% 
  filter(age == "Y15-74", sex == "T") %>%
  gather(yr, unemp_rate, -unit, -age, -sex, -geo) %>% 
  mutate(yr=as.numeric(substr(yr, 2, str_length(yr)))) %>% 
  mutate(unemp_rate = gsub("[^0-9\\.]", "", unemp_rate)) %>% # Remove comment codes
  mutate(unemp_rate = as.numeric(unemp_rate)) %>% 
  select(geo, yr, unemp_rate) %>% 
  filter(yr >= 2009) %>% 
  group_by(geo) %>% 
  summarise(unemp_rate_avg = mean(unemp_rate, na.rm=T)) %>% 
  ungroup %>% 
  as_tibble()

unemployment_dat <- left_join(unemployment_dat,
                              unemployment_dat_avg)


##############################
# 6. Merge together the data #
##############################

full_dat <- income_dat %>% 
  full_join(education_dat, by="geo") %>% 
  full_join(age_dat_bkts, by="geo") %>% 
  full_join(age_dat_median, by="geo") %>% 
  full_join(nationality_dat, by="geo") %>% 
  full_join(unemployment_dat, by="geo")

write_csv(full_dat, "_intermediate_data/eurostat_controls.csv")