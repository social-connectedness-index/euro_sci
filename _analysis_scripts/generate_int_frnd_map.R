# Purpose: Generate a map of each region's share of friends that are in a
#           different country.
# Inputs: _raw_data/eu_map; _intermediate_data/share_friends_nuts2_to_country.csv
# Outputs: _output/share_friends_outside_country.png
# Date: 2020-10-29

library(tidyverse)
library(randomcoloR)
library(ggmap)
library(sf)
library(htmlwidgets)
library(leaflet)

###########################################
# 1. Setup the original data and map data #
###########################################

# Read-in shapes
nuts_shapes <- st_read("_raw_data/eu_map/NUTS_RG_20M_2016_4326.shp",
                       layer="NUTS_RG_20M_2016_4326")

# Keep NUTS2 only
nuts2_shapes <- filter(nuts_shapes,
                       LEVL_CODE == 2)

# Drop the French Islands
nuts2_shapes <- filter(nuts2_shapes, !grepl("FRY", NUTS_ID))

# Make country border data
country_shapes <- nuts_shapes %>% 
  filter(LEVL_CODE == 0)

# Read in the friendship shares - filter out French Islands
frnd_dat <- read_csv("_intermediate_data/share_friends_nuts2_to_country.csv") %>% 
  filter(!grepl("FRY", user_loc),
         substr(user_loc,1,2) == fr_country)

###########################
# 2. Combine data and map #
###########################

# Combine data
map_dat <- frnd_dat %>% 
  left_join(nuts2_shapes, by=c("user_loc"="NUTS_ID")) %>% 
  st_as_sf %>% 
  # Create buckets
  mutate(share_different_country = case_when(
    1 - share_connections_in_fr_country > .3 ~ ">30%",
    1 - share_connections_in_fr_country > .15 ~ "15-30%",
    1 - share_connections_in_fr_country > .1 ~ "10-15%",
    1 - share_connections_in_fr_country > .075 ~ "7.5-10%",
    1 - share_connections_in_fr_country > .05 ~ "5-7.5%",
    1 - share_connections_in_fr_country > 0 ~ "<5%")) %>% 
  mutate(share_different_country = factor(share_different_country,
                                          levels=c(">30%", "15-30%", "10-15%", "7.5-10%",
                                                   "5-7.5%", "2.5-5%", "<5%")))

# Make final plot
ggplot(data=map_dat) +
  geom_sf(aes(fill=share_different_country), size=0.3, col="#606060") +
  geom_sf(data=country_shapes, fill=NA, color="black", size=0.7) +
  theme_void() +
  scale_fill_brewer(palette = "PuOr", direction = -1) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55)) +
  labs(fill = "Share of European \n connections in \n different country")

ggsave("_output/share_friends_outside_country.png", last_plot(), width=8, height=5.75, units="in", dpi=500)