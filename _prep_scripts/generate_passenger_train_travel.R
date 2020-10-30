# Purpose: Generate passenger travel between NUTS2 regions.
#           In general, we have learned (from correspondence with Eurostat)
#           that missing values can indicate EITHER zero or missing. However,
#           the data are only missing at the country reporting level.
#           Countries report international and domestic data seperately,
#           so data missing entirely at the country-domestic / country-international
#           level are treated as missing and other missings are treated as 0.
# Inputs: _raw_data/eurostat/sbs_r_nuts06_r2.tsv;
# Outputs: _intermediate_data/passenger_train_dat/eurostat_pass_dat_EITHER.csv
# Date: 2020-10-29

library(tidyverse)

#######################################################################
# 1. Read in train data and deal with comment code / Read in SCI data #
#######################################################################

# Read in SCI data (used to rectangularize)
sci_dat <- read_csv("_raw_data/SCI_Nuts2_Nuts2.csv")

# Read in raw train data
train_dat_clean0 <- 
  read.table(file = "_raw_data/eurostat/tran_r_rapa.tsv", sep = '\t', header = TRUE) %>% 
  separate(unit.c_unload.c_load.geo.time, c("unit","c_unload","c_load","geo"), ",")

# Add a comment code for when the data is deemed confidential
train_dat_clean1 <- train_dat_clean0 %>% 
  mutate(X2015_cmnt = if_else(X2015 == ": c", "c", ""),
         X2010_cmnt = if_else(X2010 == ": c", "c", ""),
         X2005_cmnt = if_else(X2005 == ": c", "c", ""),
         X2015 = if_else(grepl(":", X2015), NA_character_, as.character(X2015)),
         X2010 = if_else(grepl(":", X2010), NA_character_, as.character(X2010)),
         X2005 = if_else(grepl(":", X2005), NA_character_, as.character(X2005))) %>% 
  mutate(X2015 = as.numeric(X2015), X2010 = as.numeric(X2010), X2005 = as.numeric(X2005)) %>%
  select(-unit) %>% # Everything in passengers already
  as_tibble()


##########################################################
# 2. Hardcode the names of single-NUTS2 region countries #
##########################################################

# Make necessary updates for single NUTS2-region countries
train_dat_clean02 <- train_dat_clean1 %>% 
  mutate(c_unload = case_when(
    c_unload %in% c("LU", "LUXX", "LUZZ") ~ "LU00",
    c_unload %in% c("LV", "LVXX", "LVZZ") ~ "LV00",
    c_unload %in% c("EE", "EEXX", "EEZZ") ~ "EE00",
    TRUE ~ c_unload
  )) %>% 
  mutate(c_load = case_when(
    c_load %in% c("LU", "LUXX", "LUZZ") ~ "LU00",
    c_load %in% c("LV", "LVXX", "LVZZ") ~ "LV00",
    c_load %in% c("EE", "EEXX", "EEZZ") ~ "EE00",
    TRUE ~ c_load
  )) %>%
  mutate(c_unload = case_when(
    c_unload == "DK" & is.na(X2015) & is.na(X2010) ~ "DK00", # DK was 1 NUTS2 region in 2005
    c_unload == "SI" & is.na(X2015) & is.na(X2010) ~ "SI00", # SI was 1 NUTS2 region in 2005
    c_unload == "LT" ~ "LT00", # LT was 1 NUTS2 region until 2016
    TRUE ~ c_unload
  )) %>% 
  mutate(c_load = case_when(
    c_load == "DK" & is.na(X2015) & is.na(X2010) ~ "DK00", # DK was 1 NUTS2 region in 2005
    c_load == "SI" & is.na(X2015) & is.na(X2010) ~ "SI00", # SI was 1 NUTS2 region in 2005
    c_load == "LT" ~ "LT00", # LT was 1 NUTS2 region until 2016
    TRUE ~ c_load
  )) %>% 
  # It looks like in at at least one case LU and LUXX was reported. Looking at trade flows the other
  # way, it appears that these should be aggregated. However, we want to leave missings when both missing and
  # sum when one is non-missing. To do so, we use the non_missing_[XXXX] flag.
  mutate(non_missing_2015 = !is.na(X2015),
         non_missing_2010 = !is.na(X2010),
         non_missing_2005 = !is.na(X2005)) %>% 
  group_by(c_unload, c_load, geo) %>% 
  summarise(X2015 = sum(X2015, na.rm=T),
            X2010 = sum(X2010, na.rm=T),
            X2005 = sum(X2005, na.rm=T),
            X2015_cmnt = max(X2015_cmnt),
            X2010_cmnt = max(X2010_cmnt),
            X2005_cmnt = max(X2005_cmnt),
            non_missing_2015 = sum(non_missing_2015),
            non_missing_2010 = sum(non_missing_2010),
            non_missing_2005 = sum(non_missing_2005)) %>% 
  ungroup %>% 
  mutate(X2015 = if_else(non_missing_2015 == 0, NA_real_, X2015),
         X2010 = if_else(non_missing_2010 == 0, NA_real_, X2010),
         X2005 = if_else(non_missing_2005 == 0, NA_real_, X2005))

# There were also some single NUTS2-region countries in the past - update these.
train_dat_clean2 <- train_dat_clean02 %>% 
  mutate(c_unload = case_when(
    c_unload == "DK" ~ "DK00",
    c_unload == "SI" ~ "SI00",
    c_unload == "LT" ~ "LT00",
    TRUE ~ c_unload
  )) %>% 
  mutate(c_load = case_when(
    c_load == "DK" ~ "DK00",
    c_load == "SI" ~ "SI00",
    c_load == "LT" ~ "LT00",
    TRUE ~ c_load
  ))


#############################
# 3. Get only NUTS2 Regions #
#############################

train_dat_clean03 <- train_dat_clean2 %>% 
  mutate(unload_chars = nchar(c_unload),
         load_chars = nchar(c_load)) %>% 
  # This takes out non-EU countries which are reported at the aggregate level.
  # It also takes out 2005 reporting from Bulgaria, Croatia, and Romania
  # as they were reported as aggregate countries before they joined the EU
  # (Croatia did report disaggregated data in 2010, even though they officially joined in 2013).
  filter(unload_chars == 4, load_chars == 4) %>%
  select(-unload_chars, -load_chars)

pass_unknown <- train_dat_clean03 %>% 
  filter(grepl("XX|ZZ", c_unload)) %>% 
  filter(grepl("XX|ZZ", c_load)) %>% 
  gather(year, passengers, -c_unload, -c_load, -geo) %>% 
  .$passengers %>% 
  as.numeric() %>% 
  sum(., na.rm=T)
  

pass_known <- train_dat_clean03 %>% 
  filter(!grepl("XX|ZZ", c_unload)) %>% 
  filter(!grepl("XX|ZZ", c_load)) %>% 
    gather(year, passengers, -c_unload, -c_load, -geo) %>% 
    .$passengers %>% 
    as.numeric() %>% 
    sum(., na.rm=T)

# Pullout stat in Appendix
pass_unknown/(pass_unknown+pass_known)

# For now filter out data with XX or ZZ
train_dat_clean3 <- train_dat_clean03 %>% 
  filter(!grepl("XX|ZZ", c_unload)) %>% 
  filter(!grepl("XX|ZZ", c_load))


#################################################
# 4. Get only countries reporting on themselves #
#################################################

train_dat_clean4 <- train_dat_clean3 %>% 
  mutate(country_unload = substr(c_unload,1,2),
         country_load = substr(c_load,1,2)) %>% 
  filter(country_unload == geo | country_load == geo)

# Pullout statistic
filter(train_dat_clean4, !is.na(X2015), !is.na(X2010), !is.na(X2005)) %>% nrow()

####################################################################
# 5. Grab the countries that repot data at the NUTS2 level by year #
####################################################################

# First get overall
country_ns <- 
  train_dat_clean4 %>% 
  group_by(geo) %>%
  summarise(
    means_2015 = mean(X2015, na.rm=T),
    non_na_2015 = sum(!is.na(X2015)),
    means_2010 = mean(X2010, na.rm=T),
    non_na_2010 = sum(!is.na(X2010)),
    means_2005 = mean(X2005, na.rm=T),
    non_na_2005 = sum(!is.na(X2005))
  )

write_csv(
  select(country_ns, geo, non_na_2015, non_na_2010, non_na_2005),
  "_intermediate_data/passenger_train_dat/eurostat_passenger_data_ns.csv")

countries_2015 <- filter(country_ns, non_na_2015 != 0)$geo
countries_2010 <- filter(country_ns, non_na_2010 != 0)$geo
countries_2005 <- filter(country_ns, non_na_2005 != 0)$geo

# Then separate by national/international reporting
country_ns_int_split <- train_dat_clean4 %>% 
  mutate(domestic = country_load == country_unload) %>%
  group_by(geo, domestic) %>% 
  summarise(
    means_2015 = mean(X2015, na.rm=T),
    non_na_2015 = sum(!is.na(X2015)),
    means_2010 = mean(X2010, na.rm=T),
    non_na_2010 = sum(!is.na(X2010)),
    means_2005 = mean(X2005, na.rm=T),
    non_na_2005 = sum(!is.na(X2005))
  ) %>% 
  ungroup %>% 
  complete(domestic, geo) %>% 
  mutate(non_na_2015 = if_else(is.na(non_na_2015), as.integer(0), non_na_2015),
         non_na_2010 = if_else(is.na(non_na_2010), as.integer(0), non_na_2010),
         non_na_2005 = if_else(is.na(non_na_2005), as.integer(0), non_na_2005))

write_csv(
  select(country_ns_int_split, geo, domestic, non_na_2015, non_na_2010, non_na_2005),
  "_intermediate_data/passenger_train_dat/eurostat_passenger_data_ns_int_split.csv")

# Get all countries for rectangularization
all_countries <- data.frame(
  geo = sort(unique(substr(sci_dat$user_loc,1,2))))

country_ns_int_split %>% 
  mutate(non_na_2015 = if_else(non_na_2015 != 0, as.integer(1), non_na_2015)) %>% 
  mutate(non_na_2010 = if_else(non_na_2010 != 0, as.integer(1), non_na_2010)) %>% 
  mutate(non_na_2005 = if_else(non_na_2005 != 0, as.integer(1), non_na_2005)) %>% 
  select(-means_2015, -means_2010, -means_2005) %>%
  gather(yr, has_dat, -geo, -domestic) %>% 
  mutate(key = case_when(
    domestic & yr == "non_na_2015" ~ "Domestic2015",
    !domestic & yr == "non_na_2015" ~ "International2015",
    domestic & yr == "non_na_2010" ~ "Domestic2010",
    !domestic & yr == "non_na_2010" ~ "International2010",
    domestic & yr == "non_na_2005" ~ "Domestic2005",
    !domestic & yr == "non_na_2005" ~ "International2005"
  )) %>% 
  spread(key, has_dat) %>% 
  group_by(geo) %>% 
  summarise(Domestic2015 = max(Domestic2015, na.rm=T), International2015 = max(International2015, na.rm=T),
            Domestic2010 = max(Domestic2010, na.rm=T), International2010 = max(International2010, na.rm=T),
            Domestic2005 = max(Domestic2005, na.rm=T), International2005 = max(International2005, na.rm=T)) %>% 
  ungroup %>% 
  # Fill data set with missings for other countries
  right_join(all_countries) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  write_csv("_intermediate_data/passenger_train_dat/appendix_table dat.csv")

countries_2015.domestic <- filter(country_ns_int_split, domestic == T, non_na_2015 != 0)$geo
countries_2010.domestic <- filter(country_ns_int_split, domestic == T, non_na_2010 != 0)$geo
countries_2005.domestic <- filter(country_ns_int_split, domestic == T, non_na_2005 != 0)$geo

setdiff(countries_2015, countries_2015.domestic)
setdiff(countries_2010, countries_2010.domestic)
setdiff(countries_2005, countries_2005.domestic)

countries_2015.international <- filter(country_ns_int_split, domestic == F, non_na_2015 != 0)$geo
countries_2010.international <- filter(country_ns_int_split, domestic == F, non_na_2010 != 0)$geo
countries_2005.international <- filter(country_ns_int_split, domestic == F, non_na_2005 != 0)$geo

setdiff(countries_2015, countries_2015.international)
setdiff(countries_2010, countries_2010.international)
setdiff(countries_2005, countries_2005.international)


##############################################################
# 6. When multiple countries report, average between the two #
##############################################################

train_dat_clean6 <- train_dat_clean4 %>% 
  group_by(c_unload, c_load) %>% 
  summarise(pass_rail_2015 = mean(X2015, na.rm=T),
            pass_rail_2010 = mean(X2010, na.rm=T),
            pass_rail_2005 = mean(X2005, na.rm=T),
            # The max here will take "c" rather than "" if they are both there
            cmnt_2015 = max(X2015_cmnt),
            cmnt_2010 = max(X2010_cmnt),
            cmnt_2005 = max(X2005_cmnt)) %>% 
  ungroup %>% 
  # But if we have a non-zero entry in one field, we let ourselves use it
  mutate(cmnt_2015 = if_else(pass_rail_2015>0,"",cmnt_2015))


########################
# 7. Crosswalk to 2016 #
########################

all_crosswalk <- read_csv("_intermediate_data/crosswalks/NUTS2_update_crosswalk.csv")

train_dat_clean6.1 <- train_dat_clean6 %>% 
  # Update the c_unload side
  left_join(all_crosswalk, by=c(c_unload="code_original")) %>% 
  mutate(pass_rail_2015 = if_else(!is.na(code_update), pass_rail_2015*stat_weight, pass_rail_2015),
         pass_rail_2010 = if_else(!is.na(code_update), pass_rail_2010*stat_weight, pass_rail_2010),
         pass_rail_2005 = if_else(!is.na(code_update), pass_rail_2005*stat_weight, pass_rail_2005)) %>% 
  mutate(c_unload = if_else(!is.na(code_update), code_update, c_unload)) %>%
  select(-code_update, -stat_weight) %>% 
  # Update the c_load side
  left_join(all_crosswalk, by=c(c_load="code_original")) %>% 
  mutate(pass_rail_2015 = if_else(!is.na(code_update), pass_rail_2015*stat_weight, pass_rail_2015),
         pass_rail_2010 = if_else(!is.na(code_update), pass_rail_2010*stat_weight, pass_rail_2010),
         pass_rail_2005 = if_else(!is.na(code_update), pass_rail_2005*stat_weight, pass_rail_2005)) %>% 
  mutate(c_load = if_else(!is.na(code_update), code_update, c_load)) %>% 
  select(-code_update, -stat_weight)

# Finally, we group by to handle the merges. We must do this seperately
# for the group by sums (the tons of trade) and the group by maxes (the comments).

# Get the sums for the tonnage columns
train_dat_clean7_cols1 <- 
  train_dat_clean6.1 %>% 
  select(c("c_unload","c_load","pass_rail_2015", "pass_rail_2010", "pass_rail_2005")) %>% 
  group_by(c_unload, c_load) %>% 
  summarise_all(sum, na.rm=T) %>% 
  ungroup

# Get the maxes for the comment columns
train_dat_clean7_cols2 <- 
  train_dat_clean6.1 %>% 
  select(c("c_unload","c_load","cmnt_2015","cmnt_2010","cmnt_2005")) %>% 
  group_by(c_unload, c_load) %>% 
  summarise_all(max, na.rm=T) %>% 
  ungroup

# Bind the columns
train_dat_clean7 <- 
  inner_join(
    train_dat_clean7_cols1,
    train_dat_clean7_cols2,
  )


#######################################################################
# 8 Join in SCI data and rectangularize by [year] / [domestic vs int] #
#######################################################################

# Either country reports
train_dat_clean8.1 <- sci_dat %>% 
  left_join(train_dat_clean7,
            by=c(user_loc="c_load", fr_loc="c_unload")) %>% 
  mutate(user_country = substr(user_loc,1,2),
         fr_country = substr(fr_loc,1,2)) %>% 
  mutate(domestic = user_country == fr_country) %>% 
  
  ### Update 2015 ###
  mutate(pass_rail_2015 = case_when(
    
    ## If it is NA right now and there is a comment code, leave it NA
    ((is.na(pass_rail_2015) | pass_rail_2015 == 0) & cmnt_2015 == "c") ~ NA_real_,
    
    ## If it is NA right now, but one of the countries reports, make it zero ##
    # Domestic
    (domestic == T & 
       user_country %in% countries_2015.domestic & is.na(pass_rail_2015)) ~ 0,
    # International
    (domestic == F &
       (user_country %in% countries_2015.international | fr_country %in% countries_2015.international) &
       is.na(pass_rail_2015)) ~ 0,
    
    ## If there is a number right now, but neither of the countries report, make it NA ##
    # Domestic
    (domestic == T &
       (!(user_country %in% countries_2015.domestic))) ~ NA_real_,
    # International
    (domestic == F &
       (!(user_country %in% countries_2015.international) & !(fr_country %in% countries_2015.international))) ~ NA_real_,
    
    ## Otherwise do not change it ##
    TRUE ~ pass_rail_2015)) %>% 
  
  ### Update 2010 ###
  mutate(pass_rail_2010 = case_when(
    
    ## If it is NA right now and there is a comment code, leave it NA
    ((is.na(pass_rail_2010) | pass_rail_2010 == 0) & cmnt_2010 == "c") ~ NA_real_,
    
    ## If it is NA right now, but one of the countries reports, make it zero ##
    # Domestic
    (domestic == T & 
       user_country %in% countries_2010.domestic & is.na(pass_rail_2010)) ~ 0,
    # International
    (domestic == F &
       (user_country %in% countries_2010.international | fr_country %in% countries_2010.international) &
       is.na(pass_rail_2010)) ~ 0,
    
    ## If there is a number right now, but neither of the countries report, make it NA ##
    # Domestic
    (domestic == T &
       (!(user_country %in% countries_2010.domestic))) ~ NA_real_,
    # International
    (domestic == F &
       (!(user_country %in% countries_2010.international) & !(fr_country %in% countries_2010.international))) ~ NA_real_,
    
    ## Otherwise do not change it ##
    TRUE ~ pass_rail_2010)) %>% 
  
  ### Update 2005 ###
  mutate(pass_rail_2005 = case_when(
    
    ## If it is NA right now and there is a comment code, leave it NA
    ((is.na(pass_rail_2005) | pass_rail_2005 == 0) & cmnt_2005 == "c") ~ NA_real_,
    
    ## If it is NA right now, but one of the countries reports, make it zero ##
    # Domestic
    (domestic == T & 
       user_country %in% countries_2005.domestic & is.na(pass_rail_2005)) ~ 0,
    # International
    (domestic == F &
       (user_country %in% countries_2005.international | fr_country %in% countries_2005.international) &
       is.na(pass_rail_2005)) ~ 0,
    
    ## If there is a number right now, but neither of the countries report, make it NA ##
    # Domestic
    (domestic == T &
       (!(user_country %in% countries_2005.domestic))) ~ NA_real_,
    # International
    (domestic == F &
       (!(user_country %in% countries_2005.international) & !(fr_country %in% countries_2005.international))) ~ NA_real_,
    
    ## Otherwise do not change it ##
    TRUE ~ pass_rail_2005))


# Replace NaNs with NA
train_dat_clean8.1[is.na(train_dat_clean8.1)] <- NA
# Replace NA comments with blanks
train_dat_clean8.1 <- train_dat_clean8.1 %>% 
  mutate(cmnt_2015 = if_else(is.na(cmnt_2015), "", cmnt_2015),
         cmnt_2010 = if_else(is.na(cmnt_2010), "", cmnt_2010),
         cmnt_2005 = if_else(is.na(cmnt_2005), "", cmnt_2005))

test <- train_dat_clean8.1 %>% 
  group_by(user_country, fr_country) %>% 
  summarise_at(c("pass_rail_2015", "pass_rail_2010", "pass_rail_2005"), mean, na.rm=T)

write_csv(train_dat_clean8.1, "_intermediate_data/passenger_train_dat/eurostat_pass_dat_EITHER.csv")

