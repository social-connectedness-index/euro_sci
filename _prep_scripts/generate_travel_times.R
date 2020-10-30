# Purpose: Get two sets of travel times between NUTS2 regions
#           First, a measure of train travel times that were constructed
#           for the ETIS project. We use the NUTS3 travel times
#           and population weight up to NUTS2. 
#           Second, a measure of car travel times between geographic
#           centers of NUTS2 that we construct using the Open Source Routing Machine.
# Inputs: _raw_data/etis_2010_observed/p_imp_rail.csv;
#         _raw_data/etis_2010_observed/_EZ2006_3.csv;
#         _raw_data/ETIS_region_to_nuts_key.csv;
#         _raw_data/etis_2010_observed/s_pop_ez_3.csv;
#         _raw_data/eu_map
# Outputs: _intermediate_data/rail_travel_time.csv;
#           _intermediate_data/drive_travel_time.csv
# Date: 2020-10-29

library(tidyverse)
library(osrm)
library(sp)
library(sf)

######################################
# 1.Read the data (incl. crosswalks) #
######################################

rail_dat <- read_csv("_raw_data/etis_2010_observed/p_imp_rail.csv") %>% 
  mutate(p_imp_rail_frequency = if_else(p_imp_rail_frequency==-1,0,p_imp_rail_frequency))

nuts3_to_nuts2 <- read_csv("_raw_data/etis_2010_observed/_EZ2006_3.csv") %>% 
  select(nuts3_id=ID, nuts2_id=EZ2006_2_ID)

# The ETIS uses a different code for the NUTS2 regions
# We manually constructed a crosswalk to the NUTS2 that is used elsewhere
etis_to_nuts_key <- read_csv("_raw_data/ETIS_region_to_nuts_key.csv")


#################################################################
# 2. Group NUTS3 -> NUTS2 using pop weighted travel times of NUTS3 #
#################################################################

nuts3_pop <- read_csv("_raw_data/etis_2010_observed/s_pop_ez_3.csv") %>% 
  select(nuts3_id=EZ2006_3_ID, pop=REG_D3AVG_T)

rail_dat_nuts2 <- rail_dat %>%
  # Add in the population data
  left_join(nuts3_pop, by=c("ORIGINZONE_3_ID"="nuts3_id")) %>% 
  rename(origin_pop = pop) %>% 
  left_join(nuts3_pop, by=c("DEST_ZONE_3_ID"="nuts3_id")) %>% 
  rename(dest_pop = pop) %>% 
  left_join(nuts3_to_nuts2, by=c(ORIGINZONE_3_ID="nuts3_id")) %>% 
  rename(origin_nuts2_id=nuts2_id) %>% 
  left_join(nuts3_to_nuts2, by=c(DEST_ZONE_3_ID="nuts3_id")) %>% 
  rename(dest_nuts2_id=nuts2_id) %>% 
  group_by(origin_nuts2_id, dest_nuts2_id) %>% 
  summarise(rail_time = weighted.mean(p_imp_rail_time, dest_pop*origin_pop)) %>% 
  ungroup


####################################
# 3. Merge in the etis to nuts key #
####################################

final_rail_dat <- rail_dat_nuts2 %>% 
  left_join(etis_to_nuts_key, by=c("origin_nuts2_id"="ID")) %>% 
  select(-Name, -COUNTRY_ID) %>% 
  rename(origin=region_id) %>% 
  left_join(etis_to_nuts_key, by=c("dest_nuts2_id"="ID")) %>% 
  select(-Name, -COUNTRY_ID) %>% 
  rename(dest=region_id)


##################################
# 4. Crosswalk from 2010 -> 2016 #
##################################

# This crosswalk is simple - we map any region in the old nuts to any region in the new nuts.
# This can include more than one entry for the same old region (if the old region split).
# This can also include more than one entry for the same new region (if the old region merged with another).
# Then we take the average over any entry that is mapped to the same new region.

all_crosswalk <- read_csv("_intermediate_data/crosswalks/NUTS2_update_crosswalk.csv")

updated_rail_dat0 <- final_rail_dat %>% 
  # Update the origin side
  left_join(all_crosswalk, by=c(origin="code_original")) %>% 
  mutate(origin = if_else(!is.na(code_update), code_update, origin)) %>%
  select(-code_update, -stat_weight) %>% 
  # Update the dest side
  left_join(all_crosswalk, by=c(dest="code_original")) %>% 
  mutate(dest = if_else(!is.na(code_update), code_update, dest)) %>% 
  select(-code_update, -stat_weight)

# Finally, we group by to handle any merges
updated_rail_dat <- 
  updated_rail_dat0 %>% 
  group_by(origin, dest) %>% 
  summarise(rail_time=mean(rail_time)) %>% 
  ungroup

write_csv(updated_rail_dat, "_intermediate_data/rail_travel_time.csv")


########################################
# 5. Now use OSRM to get driving times #
########################################

# Get the shape files
nuts2_shapes <- st_read("_raw_data/eu_map/NUTS_RG_60M_2016_4326.shp",
                        layer="NUTS_RG_60M_2016_4326") %>% 
  filter(LEVL_CODE == 2) %>% 
  as_Spatial

# Get the NUTS2 centroids
# See ?coordinates() for more info on centroid calculation
nuts2_centroids <- tibble( 
  nuts2 = nuts2_shapes$NUTS_ID,
  centroid_x = coordinates(nuts2_shapes)[,1],
  centroid_y = coordinates(nuts2_shapes)[,2]) %>% 
  arrange(nuts2)

# Drop some islands that will cause an OSRM error
drop_regions <- c(
  "FRY1", "FRY2", "FRY3", "FRY4", "FRY5", # French Islands
  "PT20", "PT30", # Portugese Islands
  "CY00", # Cyprus
  "IS00") # Iceland

nuts2_centroids <- nuts2_centroids %>% 
  filter(!nuts2 %in% drop_regions)

# The public server doesn't allow us to do over 10,000
# We have just over 110,000 so we break it into four pieces (16 tables then)
pull_pieces <- list(
  c(1,100),
  c(101,200),
  c(201,300),
  c(301,nrow(nuts2_centroids))
)

nuts2_drive_time_pieces <- list()

# THIS CALLS THE DEMO SERVER AND TAKES A FEW SECONDS
counter <- 1
for(i in 1:length(pull_pieces)){
  for(j in 1:length(pull_pieces)){
    print(counter)
    nuts2_drive_time <- osrmTable(src=nuts2_centroids[pull_pieces[[i]][1]:pull_pieces[[i]][2],],
                                  dst=nuts2_centroids[pull_pieces[[j]][1]:pull_pieces[[j]][2],])
    nuts2_drive_time_pieces[[counter]] <- nuts2_drive_time
    counter <- counter + 1
    Sys.sleep(1) # We force a sleep for 1 second between calls (otherwise OSRM will kick us out)
  }
}

# Make the first part of the long dat
long_dat <- nuts2_drive_time_pieces[[1]]$durations %>%
  as.data.frame() %>% 
  mutate(user_loc = row.names(.)) %>% 
  as_tibble() %>% 
  gather(key="fr_loc", value="drive_time", -user_loc)

# Add the remaining 15 pieces to the long dat
for(i in 2:length(nuts2_drive_time_pieces)){
  long_dat_new <- nuts2_drive_time_pieces[[i]]$durations %>%
    as.data.frame() %>% 
    mutate(user_loc = row.names(.)) %>% 
    as_tibble() %>% 
    gather(key="fr_loc", value="drive_time", -user_loc)
  
  long_dat <- bind_rows(long_dat, long_dat_new)
}

write_csv(long_dat, "_intermediate_data/drive_travel_time.csv")