# Purpose: Produce distances between NUTS2 regions. These distances are
#           first calculated at the NUTS3 level then population weighted
#           up to the NUTS2, providing a semi-population-weighted distance.
# Inputs: _raw_data/eu_map;
# Outputs: _intermediate_data/geo_distance_dat.csv
# Date: 2020-10-29

library(tidyverse)
library(sp)
library(sf)

# Get the shape files
nuts3_shapes <- st_read("_raw_data/eu_map/NUTS_RG_60M_2016_4326.shp",
                        layer="NUTS_RG_60M_2016_4326") %>% 
  filter(LEVL_CODE == 3)

# Get the populations which we will use to weight the distances
nuts3_pop <- read.table("_raw_data/eurostat/demo_r_pjanaggr3.tsv", sep = '\t', header = TRUE) %>% 
  separate(unit.sex.age.geo.time, c("unit","sex","age","geo"), ",") %>% 
  filter(sex == "T", age == "TOTAL") %>% 
  select(geo, X2017) %>% 
  filter(str_length(geo) == 5) %>% 
  mutate(pop_2017 = gsub("[^0-9\\.]", "", X2017)) %>%  # Remove comment codes
  mutate(pop_2017 = as.numeric(pop_2017)) %>% 
  select(-X2017) %>% 
  # For some reason, the shape files have only one NUTS3 in NO06 and the pop data have two
  # It won't matter at NUTS2, so we aggregate the NUTS3 together
  mutate(geo = if_else(geo %in% c("NO061", "NO062"), "NO060", geo)) %>% 
  group_by(geo) %>% summarise(pop_2017 = sum(pop_2017)) %>% ungroup %>% 
  as_tibble()

# Get the NUTS3 centroids
# See ?coordinates() for more info on centroid calculation
nuts3_shapes <- as_Spatial(nuts3_shapes) # Function is not available in sf, so we convert back to SpatialPolygonsDataFrame
nuts3_centroids <- tibble( 
  nuts3 = nuts3_shapes$NUTS_ID,
  centroid_x = coordinates(nuts3_shapes)[,1],
  centroid_y = coordinates(nuts3_shapes)[,2])

# Trasnform the centroids into a geometry set
all_centroids <- 
  nuts3_centroids[,2:3] %>% 
  as.data.frame %>% 
  st_as_sf(coords = c(1,2)) %>% 
  st_geometry()

# Add the CRS type
st_crs(all_centroids) <- nuts3_shapes@proj4string

# Now calculate all the distances (takes a few moments to run)
all_distances <- st_distance(all_centroids, all_centroids)

# Convert the matrix to table and meters to KM
nuts3_dist_table <- all_distances %>% 
  as_tibble() %>%
  rename_all(function(x) nuts3_shapes$NUTS_ID) %>% # rename the rows 
  mutate(from_nuts3 = nuts3_shapes$NUTS_ID) %>%
  gather(key=to_nuts3, value=dist, -from_nuts3) %>% 
  mutate(dist = as.numeric(dist)) %>% 
  mutate(dist = dist/1000) %>% # convert to KM
  arrange(to_nuts3, from_nuts3)

# Add in the populations and corresponding NUTS2 Regions
nuts3_dist_table2 <- nuts3_dist_table %>% 
  # Join the from and to on the population
  inner_join(nuts3_pop, by=c(from_nuts3="geo")) %>% 
  rename(from_pop = pop_2017) %>% 
  inner_join(nuts3_pop, by=c(to_nuts3="geo")) %>% 
  rename(to_pop = pop_2017) %>% 
  # Get the nuts2 from the nuts3 by substr
  mutate(from_nuts2 = substr(from_nuts3,1,4)) %>% 
  mutate(to_nuts2 = substr(to_nuts3,1,4))

# Finally, group by nuts2 and take the weighted average distance
nuts2_dist <- nuts3_dist_table2 %>% 
  group_by(from_nuts2, to_nuts2) %>% 
  summarise(distance = weighted.mean(dist, from_pop*to_pop)) %>% 
  ungroup %>% 
  mutate(distance = if_else(from_nuts2 == to_nuts2, 0, distance)) %>% 
  rename(user_loc=from_nuts2, fr_loc=to_nuts2)

write_csv(nuts2_dist, "_intermediate_data/geo_distance_dat.csv")
