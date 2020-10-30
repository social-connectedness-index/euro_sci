# Purpose: Read-in data from the European Social Survey on language and religion.
# Inputs: _raw_data/ess [the Stata files in each subsequent folder]
#         _raw_data/SCI_Nuts2_Nuts2.csv
# Outputs: _intermediate_data/ess_controls.csv
# Date: 2020-10-29

# NOTE 1: While the ESS provides data at the NUTS2 level for most countries,
# it provides only NUTS1 level data for Germany and the UK. For these countries,
# we map that NUTS1 value to each NUTS2 region.

# NOTE 2: While the ESS round 8 includes most countries, some countries were not included
# in ESS 8, but included in earlier survey iterations. The country-level availability
# of data is provided here: https://www.europeansocialsurvey.org/data/multilevel/guide/essreg.html.
# Below we list country data pulled from earlier surveys.
# ESS 7: DK
# ESS 6: AL, BG, CY, SK
# ESS 5: HR, GR(EL)
# ESS 4: LV, RO

# NOTE 3: As recommended by the ESS, we use the post-stratification weights. See:
# https://www.europeansocialsurvey.org/docs/methodology/ESS_weighting_data_1.pdf.

library(tidyverse)
library(haven)

################################
# 1. Read in and prep the data #
################################

dat_in_8 <- read_dta("_raw_data/ess/ess8/ESS8e02_1.dta") %>% select(cntry, region, essround, rlgblg, rlgdnm, lnghom1, pspwght)
dat_in_7 <- read_dta("_raw_data/ess/ess7/ESS7e02_2.dta") %>% select(cntry, region, essround, rlgblg, rlgdnm, lnghom1, pspwght)
dat_in_6 <- read_dta("_raw_data/ess/ess6/ESS6e02_4.dta") %>% select(cntry, region, essround, rlgblg, rlgdnm, lnghom1, pspwght)
dat_in_5 <- read_dta("_raw_data/ess/ess5/ESS5e03_4.dta") %>% select(cntry, region, essround, rlgblg, rlgdnm, lnghom1, pspwght)
dat_in_4 <- read_dta("_raw_data/ess/ess4/ESS4e04_5.dta") %>% select(cntry, starts_with("regio"), essround, rlgblg, rlgdnm, lnghoma, pspwght)

# The regions are coded differently in the ESS4.
# We only want Romania and Latvia, so we manually work with those.
dat_in_4 <- dat_in_4 %>% 
  filter(cntry %in% c("RO","LV")) %>% 
  mutate(temp = case_when(
    cntry == "LV" ~ "LV00",
    cntry == "RO" ~ paste0("RO", regionro)
  )) %>% 
  select(-starts_with("regio")) %>% 
  rename(region=temp, lnghom1=lnghoma)

# This will remove the labels from the haven objects - we are okay with that
dat_in <- dat_in_8 %>% 
  bind_rows(filter(dat_in_7, cntry %in% c("DK"))) %>% 
  bind_rows(filter(dat_in_6, cntry %in% c("AL", "BG", "CY", "SK"))) %>% 
  bind_rows(filter(dat_in_5, cntry %in% c("HR", "GR"))) %>% 
  bind_rows(filter(dat_in_4, cntry %in% c("LV", "RO")))


#########################
# 2. Crosswalk the data #
#########################

# While ESS8 uses the updated NUTS 2016 regions,
# the older surveys use the older NUTS regions. For some of the
# data from the older surveys that we use, we need
# to update the NUTS regions.

dat_clean <- dat_in %>% 
  select(region, essround, rlgblg, rlgdnm, lnghom1, pspwght) %>% 
  # For this Hungarian crosswalk, we can match perfectly
  # because we have NUTS3 data
  mutate(region_new=case_when(
    region == "HU101" ~ "HU11",
    region == "HU102" ~ "HU12",
    TRUE ~ region
  )) %>% 
  # Now for anything else in NUTS3, aggregate up to NUTS2
  mutate(region_new=substr(region_new,1,4)) %>%
  # For the other NUTS changes, we either crosswalk to new if possible or aggregate up to NUTS1 if not
  mutate(region_new=case_when(
    # ESS 8
    region_new == "IE01" ~ "IE0",
    region_new == "IE02" ~ "IE0",
    region_new == "LT00" ~ "LT0",
    region_new == "PL11" ~ "PL71",
    region_new == "PL33" ~ "PL72",
    region_new == "PL31" ~ "PL81",
    region_new == "PL32" ~ "PL82",
    region_new == "PL34" ~ "PL84",
    region_new == "PL12" ~ "PL9",
    region_new == "SI01" ~ "SI03",
    region_new == "SI02" ~ "SI04",
    # ESS 5
    region_new == "HR01" ~ "HR04",
    region_new == "HR02" ~ "HR04",
    region_new == "GR11" ~ "EL51",
    region_new == "GR12" ~ "EL52",
    region_new == "GR13" ~ "EL53",
    region_new == "GR14" ~ "EL61",
    region_new == "GR21" ~ "EL54",
    region_new == "GR22" ~ "EL62",
    region_new == "GR23" ~ "EL63",
    region_new == "GR24" ~ "EL64",
    region_new == "GR25" ~ "EL65",
    region_new == "GR30" ~ "EL30",
    region_new == "GR41" ~ "EL41",
    region_new == "GR42" ~ "EL42",
    region_new == "GR43" ~ "EL43",
    TRUE ~ region_new
  )) %>% 
  select(-region) %>% 
  filter(region_new != "9999")


##############################
# 3. Clean the religion data #
##############################

dat_religion <- dat_clean %>% 
  # For individuals who answered that they are not religious, we code them as religious denomination 0.
  mutate(rlgdnm_new=if_else(rlgblg==2,"0",as.character(rlgdnm))) %>% 
  # Some individuals appear to have refused the religion questions - we drop them here.
  filter(!is.na(rlgdnm_new)) %>% 
  select(-rlgblg, -rlgdnm) %>% 
  group_by(region_new, rlgdnm_new) %>% 
  summarise(n=n(), weight=sum(pspwght)) %>% 
  group_by(region_new) %>% 
  mutate(total_weight=sum(weight), total_n=sum(n)) %>% 
  ungroup %>% 
  mutate(share_rlgdnm = weight/total_weight)

# Calculate a measure of most common religion including NONE as an option
most_common_religion_w_none <- dat_religion %>% 
  group_by(region_new) %>% 
  filter(share_rlgdnm == max(share_rlgdnm)) %>% 
  ungroup %>% 
  select(region_new, highest_rlgdnm_w_none=rlgdnm_new)

# Calculate a measure of most common religion excluding NONE as an option
most_common_religion_wo_none <- dat_religion %>%
  filter(rlgdnm_new != 0) %>% 
  group_by(region_new) %>% 
  filter(share_rlgdnm == max(share_rlgdnm)) %>% 
  ungroup %>% 
  select(region_new, highest_rlgdnm_wo_none=rlgdnm_new)

most_common_religion <- 
  full_join(most_common_religion_w_none,
            most_common_religion_wo_none)


##############################
# 4. Clean the language data #
##############################

# A key to the language data is available at:
# https://www.europeansocialsurvey.org/docs/round8/survey/ESS8_appendix_a7_e01_0.pdf

dat_language <- dat_clean %>% 
  group_by(region_new, lnghom1) %>% 
  summarise(n=n(), weight=sum(pspwght)) %>% 
  group_by(region_new) %>% 
  mutate(total_weight=sum(weight), total_n=sum(n)) %>% 
  ungroup %>% 
  mutate(share_lnghom1 = weight/total_weight)

most_common_language <- dat_language %>% 
  group_by(region_new) %>% 
  filter(share_lnghom1 == max(share_lnghom1)) %>% 
  ungroup %>% 
  select(region_new, highest_lnghom1=lnghom1)


############################################
# 5. Combine and create the final data set #
############################################

# Here, as described above in NOTE 1, we are mapping from NUTS1 to
# NUTS2 for countries where we only have data at the NUTS2 level.
regions <-
  read_csv("_raw_data/SCI_Nuts2_Nuts2.csv") %>% 
  distinct(user_loc) %>% 
  mutate(user_loc_nuts1 = substr(user_loc,1,3))

final_dat <- regions %>% 
  # Add religion data
  left_join(most_common_religion, by=c(user_loc="region_new")) %>%
  rename(highest_rlgdnm_w_none_nuts2=highest_rlgdnm_w_none, 
         highest_rlgdnm_wo_none_nuts2=highest_rlgdnm_wo_none) %>% 
  left_join(most_common_religion, by=c(user_loc_nuts1="region_new")) %>% 
  rename(highest_rlgdnm_w_none_nuts1=highest_rlgdnm_w_none, 
         highest_rlgdnm_wo_none_nuts1=highest_rlgdnm_wo_none) %>% 
  # Match religion data at lowest level possible
  mutate(highest_rlgdnm_w_none=case_when(
    !is.na(highest_rlgdnm_w_none_nuts2) ~ highest_rlgdnm_w_none_nuts2,
    !is.na(highest_rlgdnm_w_none_nuts1) ~ highest_rlgdnm_w_none_nuts1
  )) %>% 
  mutate(highest_rlgdnm_wo_none=case_when(
    !is.na(highest_rlgdnm_wo_none_nuts2) ~ highest_rlgdnm_wo_none_nuts2,
    !is.na(highest_rlgdnm_wo_none_nuts1) ~ highest_rlgdnm_wo_none_nuts1
  )) %>% 
  # Add language data
  left_join(most_common_language, by=c(user_loc="region_new")) %>% 
  rename(highest_lnghom_nuts2=highest_lnghom1) %>% 
  left_join(most_common_language, by=c(user_loc_nuts1="region_new")) %>% 
  rename(highest_lnghom_nuts1=highest_lnghom1) %>% 
  # Match religion data at lowest level possible
  mutate(highest_lnghom=case_when(
    !is.na(highest_lnghom_nuts2) ~ highest_lnghom_nuts2,
    !is.na(highest_lnghom_nuts1) ~ highest_lnghom_nuts1
  )) %>% 
  # Remove old columns
  select(-ends_with("nuts2"), -ends_with("nuts1"), -user_loc_nuts1)

# We add Malta which has only a single NUTS2 region
# Malta has two languages - Maltese and English. This
# Eurobarometer survey shows that 97% say Maltese is their mother tongue.
# more commonly spoken than English.
# file:///C:/Users/drussel/Downloads/ebs_386_anx_en.pdf
# Malta's state religion is Catholicism and over 90% of Maltese
# identified as Catholic.
final_dat <- final_dat %>% 
  mutate(highest_rlgdnm_w_none = if_else(user_loc == "MT00", as.character(1), highest_rlgdnm_w_none),
         highest_rlgdnm_wo_none = if_else(user_loc == "MT00", as.character(1), highest_rlgdnm_wo_none),
         highest_lnghom = if_else(user_loc == "MT00", "MLT", highest_lnghom))

write_csv(final_dat, "_intermediate_data/ess_controls.csv")