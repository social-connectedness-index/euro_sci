# Purpose: Classify modern NUTS2 regions by the country they overlapped with most (often they were entirely within)
#           in the years 1900, 1930, 1960, and 1990.
# Inputs: _raw_data/eu_map;
#         _raw_data/eu_map_historical/01 Europe Main;
#         _raw_data/eu_map_historical/princeton_country_num_crosswalk.csv;
# Outputs: _intermediate_data/historical/country_historical.csv;
#           _intermediate_data/historical/1900_map.png;
#           _intermediate_data/historical/1900_map_of_modern_nuts2.png;
#           _intermediate_data/historical/1900_country_of_modern_nuts2.csv";
#           _intermediate_data/historical/1930_map.png;
#           _intermediate_data/historical/1930_map_of_modern_nuts2.png;
#           _intermediate_data/historical/1930_country_of_modern_nuts2.csv";
#           _intermediate_data/historical/1960_map.png;
#           _intermediate_data/historical/1960_map_of_modern_nuts2.png;
#           _intermediate_data/historical/1960_country_of_modern_nuts2.csv";
#           _intermediate_data/historical/1990_map.png;
#           _intermediate_data/historical/1990_map_of_modern_nuts2.png;
#           _intermediate_data/historical/1990_country_of_modern_nuts2.csv";
#           _output/historical_maps_1990_1960.jpg;
#           _output/historical_maps_1930_1900.jpg;
#           _output/historical_maps_present_day.jpg
# Date: 2020-10-29

library(tidyverse)
library(rgdal)
library(sp)
library(sf)
library(rmapshaper)
library(randomcoloR)
library(grid)
library(gridExtra)


###########################################
# 1. Setup the original data and map data #
###########################################

# See country key at: https://opr.princeton.edu/archive/pefp/mastlist.aspx
country_key <- read_csv("_raw_data/eu_map_historical/princeton_country_num_crosswalk.csv") %>% 
  mutate(number=as.character(number))

# Data files from: https://censusmosaic.demog.berkeley.edu/data/historical-gis-files
country_shapes_1900 <- st_read("_raw_data/eu_map_historical/01 Europe Main/Europe_1900_v.1.1.shp",
                               layer="Europe_1900_v.1.1")

country_shapes_1930 <- st_read("_raw_data/eu_map_historical/01 Europe Main/Europe_1930_v.1.0.shp",
                               layer="Europe_1930_v.1.0")

country_shapes_1960 <- st_read("_raw_data/eu_map_historical/01 Europe Main/Europe_1960_v.1.0.shp",
                               layer="Europe_1960_v.1.0")

country_shapes_1990 <- st_read("_raw_data/eu_map_historical/01 Europe Main/Europe_1990_v.1.0.shp",
                               layer="Europe_1990_v.1.0")

# Read-in persent shapes
nuts_shapes <- st_read("_raw_data/eu_map/NUTS_RG_20M_2016_4326.shp",
                       layer="NUTS_RG_20M_2016_4326")

# Transform the NUTS shapes so that we are using the same projection
nuts_shapes_clean <- st_transform(nuts_shapes, st_crs(country_shapes_1900))

# Keep NUTS2 only
nuts_shapes_clean<- filter(nuts_shapes_clean,
                       LEVL_CODE == 2)

# Drop the French Islands
nuts_shapes_clean <- filter(nuts_shapes_clean, !grepl("FRY", NUTS_ID))

####################
# 2. The year 1900 #
####################

# Clean the old map
country_shapes_1900_cl <- country_shapes_1900 %>% 
  st_simplify(dTolerance=2000) %>%
  mutate(COUNTRY = as.character(COUNTRY)) %>% 
  mutate(COUNTRY = case_when(
    # Group in Scotland and Ireland with England (1900 is pre-Irish home rule)
    COUNTRY == "170" | COUNTRY == "100" ~ "50",
    # Combine the various Austria and Hungary
    COUNTRY == "240" ~ "10",
    # Change three series code endings from 1->0 to match crosswalk
    COUNTRY == "181" ~ "180",
    COUNTRY == "31" ~ "30",
    COUNTRY == "91" ~ "90",
    # Classify the Ottoman Emipre
    COUNTRY == "0" & COMMENTS == "Osman Empire" ~ "OE",
    # Group Surrey with UK
    COUNTRY == "0" & NAME == "SURREY" ~ "50",
    # Malta was a UK colony until 1964
    COUNTRY == "0" & NAME == "MALTA" ~ "UK",
    # Poland and Finland were within Russian Empire
    COUNTRY == "140" | COUNTRY == "60" ~ "RU",
    # Kingdom of Swedend and Norway was united
    COUNTRY == "130" | COUNTRY == "190" ~ "SE",
    # Iceland did not become sovereign state until 1918
    COUNTRY == "270" ~ "DK",
    TRUE ~ COUNTRY
  )) %>% 
  filter(COUNTRY != "0") %>% 
  left_join(country_key, by=c("COUNTRY"="number")) %>% 
  mutate(COUNTRY = if_else(!is.na(country), country, COUNTRY))

countries <- unique(country_shapes_1900_cl$COUNTRY)
palette <- distinctColorPalette(length(countries))

# Generate the old map
country_shapes_1900_cl %>%
  ggplot() + geom_sf(aes(fill=COUNTRY)) +
  scale_fill_manual(values=palette)

ggsave("_intermediate_data/historical/1900_map.png", last_plot(), width=11, height=8.5, units = "in")

# Get the final data you will output
nuts_shapes_clean_1900 <- mutate(nuts_shapes_clean,
                                 max_overlap = 0,
                                 max_overlap_country = NA_character_)

countries <- unique(country_shapes_1900_cl$COUNTRY)
print("Year = 1900")
for(i in 1:length(countries)){
  print(paste("Country", i, "of", length(countries)))
  # Get the historical country
  curr_country <- filter(country_shapes_1900_cl, COUNTRY==countries[i]) %>% st_union
  for(j in 1:nrow(nuts_shapes_clean_1900)){
    # Get the modern region
    curr_region <- nuts_shapes_clean_1900[j,]
    # Determine the share of the modern region in the historical country
    share_overlap <- as.numeric(st_area(st_intersection(curr_region, curr_country))/st_area(curr_region))
    if(is_empty(share_overlap)){next}
    else if(nuts_shapes_clean_1900[j,]$max_overlap >= share_overlap){next}
    else{
      nuts_shapes_clean_1900[j,]$max_overlap <- share_overlap
      nuts_shapes_clean_1900[j,]$max_overlap_country <- countries[i]
    }
  }
}

final_dat_1900 <- nuts_shapes_clean_1900 %>% 
  # A few manual updates
  mutate(max_overlap_country = case_when(
    # Cyprus was part of the UK until 1900
    NUTS_ID == "CY00" ~ "UK",
    # Malta was part of the UK until 1964
    NUTS_ID == "MT00" ~ "UK",
    # Lichenstein has always been its own country
    NUTS_ID == "LI00" ~ "LI",
    # Flevoland is in the Netherlands, but is missed on old maps because it was re-claimed from the Sea.
    # Here we manually put it in the Netherlands
    NUTS_ID == "NL23" ~ "NL",
    # Turkey has been unified since before 1900 (as part of Ottoman Empire here)
    grepl("TR", NUTS_ID) ~ "OE",
    # Melilla and the Canaries have been under Spanish rule since before 1900
    NUTS_ID == "ES64" | NUTS_ID == "ES70" ~ "ES",
    # Azores and Madeira have been under Portugese rule since before 1900
    NUTS_ID == "PT20" | NUTS_ID == "PT30" ~ "PT",
    TRUE ~ max_overlap_country
  ))


# Final dat 1900
final_dat_1900 %>% 
  as.data.frame() %>% 
  select(NUTS_ID, max_overlap_country) %>%
  write_csv("_intermediate_data/historical/1900_country_of_modern_nuts2.csv")

### Map for appendix ###
set.seed(11)
palette <- distinctColorPalette(length(unique(final_dat_1900$max_overlap_country)))

# Change a couple historical names
final_dat_1900 <- final_dat_1900 %>% 
  mutate(max_overlap_country = case_when(
    max_overlap_country == "AT" ~ "AT-HU",
    max_overlap_country == "SE" ~ "SE-NO",
    max_overlap_country == "YO" ~ "RS",
    TRUE ~ max_overlap_country
))

temp <- palette[6]; palette[6] <- palette[17]; palette[17] <- temp 

# Make the final map
map_final_1900 <- final_dat_1900 %>% 
  # Re-project data to match other maps
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
  ggplot() +
  geom_sf(dat=final_dat_1900 , aes(fill=max_overlap_country)) +
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  scale_fill_manual(values=palette) +
  labs(fill = "Country") +
  theme_void() +
  theme(legend.position = c(0.9, 0.55))

ggsave("_intermediate_data/historical/1900_map_of_modern_nuts2.png", last_plot(), width=11, height=8.5, units = "in")


####################
# 3. The year 1930 #
####################

# Get the final data you will output
nuts_shapes_clean_1930 <- mutate(nuts_shapes_clean,
                                 max_overlap = 0,
                                 max_overlap_country = NA_character_)

# Clean the old map
country_shapes_1930_cl <- country_shapes_1930 %>% 
  st_as_sf %>%
  st_simplify(dTolerance=2000) %>%
  mutate(COUNTRY = as.character(COUNTRY)) %>% 
  mutate(COUNTRY = case_when(
    # Split off Northern Ireland and group Northern Ireland Scotland and England/Wales together into UK
    # Northern Ireland counties come from: https://en.wikipedia.org/wiki/Counties_of_Northern_Ireland
    COUNTRY == "170" ~ "50",
    COUNTRY == "100" & grepl("ANTRIM|ARMAGH|DOWN|FERMANAGH|TYRONE|LONDONDER", NAME) ~ "50",
    # Change two series code endings from 1->0 to match crosswalk
    COUNTRY == "31" ~ "30", 
    COUNTRY == "181" ~ "180",
    # Estonia, Latvia, and Lithuania were independent between the wars
    NAME == "Estonia" ~ "EE",
    NAME == "Latvia" ~ "LV",
    NAME == "LITHUANIA" ~ "LT",
    TRUE ~ COUNTRY
  )) %>% 
  filter(COUNTRY != 0) %>% 
  left_join(country_key, by=c("COUNTRY"="number")) %>% 
  mutate(COUNTRY = if_else(!is.na(country), country, COUNTRY))

countries <- unique(country_shapes_1930_cl$COUNTRY)
palette <- distinctColorPalette(length(countries))

# Test the old map
country_shapes_1930_cl %>%
  ggplot() + geom_sf(aes(fill=COUNTRY)) +
  scale_fill_manual(values=palette)

ggsave("_intermediate_data/historical/1930_map.png", last_plot(), width=11, height=8.5, units = "in")
print("Year = 1930")
for(i in 1:length(countries)){
  print(paste("Country", i, "of", length(countries)))
  # Get the historical country
  curr_country <- filter(country_shapes_1930_cl, COUNTRY==countries[i]) %>% st_union
  for(j in 1:nrow(nuts_shapes_clean_1930)){
    # Get the modern region
    curr_region <- nuts_shapes_clean_1930[j,]
    # Determine the share of the modern region in the historical country
    share_overlap <- as.numeric(st_area(st_intersection(curr_region, curr_country))/st_area(curr_region))
    if(is_empty(share_overlap)){next}
    else if(nuts_shapes_clean_1930[j,]$max_overlap >= share_overlap){next}
    else{
      nuts_shapes_clean_1930[j,]$max_overlap <- share_overlap
      nuts_shapes_clean_1930[j,]$max_overlap_country <- countries[i]
    }
  }
}

final_dat_1930 <- nuts_shapes_clean_1930 %>% 
  # A few manual updates
  mutate(max_overlap_country = case_when(
    # Cyprus was part of the UK until 1960
    NUTS_ID == "CY00" ~ "UK",
    # Malta was part of the UK until 1964
    NUTS_ID == "MT00" ~ "UK",
    # Lichenstein has always been its own country
    NUTS_ID == "LI00" ~ "LI",
    # Flevoland is in the Netherlands, but is missed on old maps because it was re-claimed from the Sea.
    # Here we manually put it in the Netherlands
    NUTS_ID == "NL23" ~ "NL",
    # Turkey has been unified since before 1900
    grepl("TR", NUTS_ID) ~ "TR",
    # Melilla and the Canaries have been under Spanish rule since before 1900
    NUTS_ID == "ES64" | NUTS_ID == "ES70" ~ "ES",
    # Azores and Madeira have been under Portugese rule since before 1900
    NUTS_ID == "PT20" | NUTS_ID == "PT30" ~ "PT",
    TRUE ~ max_overlap_country
  ))

# Final dat 1930
final_dat_1930 %>% 
  as.data.frame() %>% 
  select(NUTS_ID, max_overlap_country) %>%
  write_csv("_intermediate_data/historical/1930_country_of_modern_nuts2.csv")

### Map for appendix ###
set.seed(1)
palette <- distinctColorPalette(length(unique(final_dat_1930$max_overlap_country)))

temp <- palette[6]; palette[6] <- palette[11]; palette[11] <- temp
temp <- palette[29]; palette[29] <- palette[19]; palette[19] <- temp

# Make the final map
map_final_1930 <- final_dat_1930 %>% 
  # Re-project data to match other maps
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
  ggplot() +
  geom_sf(dat=final_dat_1930 , aes(fill=max_overlap_country)) +
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  scale_fill_manual(values=palette) +
  labs(fill = "Country") +
  theme_void() +
  theme(legend.position = c(0.9, 0.55))
ggsave("_intermediate_data/historical/1930_map_of_modern_nuts2.png", last_plot(), width=11, height=8.5, units = "in")


####################
# 4. The year 1960 #
####################

# Get the final data you will output
nuts_shapes_clean_1960 <- mutate(nuts_shapes_clean,
                                 max_overlap = 0,
                                 max_overlap_country = NA_character_)


# Clean the old map
country_shapes_1960_cl <- country_shapes_1960 %>% 
  st_as_sf %>%
  st_simplify(dTolerance=2000) %>%
  mutate(COUNTRY = as.character(COUNTRY)) %>% 
  mutate(COUNTRY = case_when(
    # Split off Northern Ireland and group Northern Ireland Scotland and England/Wales together into UK
    # Northern Ireland counties come from: https://en.wikipedia.org/wiki/Counties_of_Northern_Ireland
    COUNTRY == "170" ~ "50",
    COUNTRY == "100" & NAME %in% c("ANTRIM", "ARMAGH COUNTY", "DOWN", "FERMANAGH COUNTY",
                                   "TYRONE COUNTY", "LONDONDERRY COUNTY") ~ "50",
    # Change four series code endings from 1->0 to match crosswalk
    COUNTRY == "181" ~ "180",
    COUNTRY == "231" ~ "230",
    COUNTRY == "31" ~ "30",
    COUNTRY == "51" ~ "50",
    # East/West Germany were originally combined in the old maps
    # These are EG admin divisions found at: https://en.wikipedia.org/wiki/Portal:East_Germany/Region
    COUNTRY == "80" & NAME %in% c("EAST BERLIN", "COTTBUS", "DRESDEN", "ERFURT", 	"FRANKFURT ODER",
                                  "GERA", "HALLE", "KARL-MARX-STADT", "LEIPZIG", "MAGDEBURG", "NEUBRANDENBURG",
                                  "POTSDAM", "ROSTOCK", "SCHWERIN", "SUHL") ~ "EG",
    TRUE ~ COUNTRY
  )) %>% 
  filter(COUNTRY != 0) %>% 
  left_join(country_key, by=c("COUNTRY"="number")) %>% 
  mutate(COUNTRY = if_else(!is.na(country), country, COUNTRY))

countries <- unique(country_shapes_1960_cl$COUNTRY)
palette <- distinctColorPalette(length(countries))

# Test the old map
country_shapes_1960_cl %>%
  ggplot() + geom_sf(aes(fill=COUNTRY)) +
  scale_fill_manual(values=palette)

ggsave("_intermediate_data/historical/1960_map.png", last_plot(), width=11, height=8.5, units = "in")
print("Year = 1960")
for(i in 1:length(countries)){
  print(paste("Country", i, "of", length(countries)))
  # Get the historical country
  curr_country <- filter(country_shapes_1960_cl, COUNTRY==countries[i]) %>% st_union
  for(j in 1:nrow(nuts_shapes_clean_1960)){
    # Get the modern region
    curr_region <- nuts_shapes_clean_1960[j,]
    # Determine the share of the modern region in the historical country
    share_overlap <- as.numeric(st_area(st_intersection(curr_region, curr_country))/st_area(curr_region))
    if(is_empty(share_overlap)){next}
    else if(nuts_shapes_clean_1960[j,]$max_overlap >= share_overlap){next}
    else{
      nuts_shapes_clean_1960[j,]$max_overlap <- share_overlap
      nuts_shapes_clean_1960[j,]$max_overlap_country <- countries[i]
    }
  }
}

final_dat_1960 <- nuts_shapes_clean_1960 %>% 
  # A few manual updates
  mutate(max_overlap_country = case_when(
    # Cyprus is part of the UK until 1960
    NUTS_ID == "CY00" ~ "UK",
    # Malta was part of the UK until 1964
    NUTS_ID == "MT00" ~ "UK",
    # Lichenstein has always been its own country
    NUTS_ID == "LI00" ~ "LI",
    # Flevoland is in the Netherlands, but is missed on old maps because it was re-claimed from the Sea.
    # Here we manually put it in the Netherlands
    NUTS_ID == "NL23" ~ "NL",
    # Turkey has been unified since before 1900
    grepl("TR", NUTS_ID) ~ "TR",
    # Melilla and the Canaries have been under Spanish rule since before 1900
    NUTS_ID == "ES64" | NUTS_ID == "ES70" ~ "ES",
    # Azores and Madeira have been under Portugese rule since before 1900
    NUTS_ID == "PT20" | NUTS_ID == "PT30" ~ "PT",
    TRUE ~ max_overlap_country
  ))

# Final dat 1960
final_dat_1960 %>% 
  as.data.frame() %>% 
  select(NUTS_ID, max_overlap_country) %>%
  write_csv("_intermediate_data/historical/1960_country_of_modern_nuts2.csv")

### Map for appendix ###
# Change a couple historical names
final_dat_1960 <- final_dat_1960 %>%
  mutate(max_overlap_country = case_when(
    max_overlap_country == "RU" ~ "USSR",
    max_overlap_country == "EG" ~ "GDR",
    TRUE ~ max_overlap_country
  ))

set.seed(1)
palette <- distinctColorPalette(length(unique(final_dat_1960$max_overlap_country)))

# Make the final map
map_final_1960 <- final_dat_1960 %>% 
  # Re-project data to match other maps
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
  ggplot() +
  geom_sf(dat=final_dat_1960 , aes(fill=max_overlap_country)) +
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  scale_fill_manual(values=palette) +
  labs(fill = "Country") +
  theme_void() +
  theme(legend.position = c(0.9, 0.55))

ggsave("_intermediate_data/historical/1960_map_of_modern_nuts2.png", last_plot(), width=11, height=8.5, units = "in")


####################
# 5. The year 1990 #
####################

# Get the final data you will output
nuts_shapes_clean_1990 <- mutate(nuts_shapes_clean,
                                 max_overlap = 0,
                                 max_overlap_country = NA_character_)

# Clean the old map
country_shapes_1990_cl <- country_shapes_1990 %>% 
  st_as_sf %>%
  st_simplify(dTolerance=2000) %>%
  mutate(COUNTRY = as.character(COUNTRY)) %>% 
  mutate(COUNTRY= case_when(
    # Greek country re-classification
    COUNTRY == "GR" ~ "EL",
    # # German unification happened in 1990 - we chose to unite it for this analysis
    # COUNTRY == "GC" ~ "DE",
    # Ireland, Portugal, and Sweden are coded differently
    COUNTRY == "EI" ~ "IE",
    COUNTRY == "PO" ~ "PT",
    COUNTRY == "SW" ~ "SE",
    # Lithuania, Estonia, and Latvia were part of USSR until 1991. As we did in 1960, call them Russia to capture unity
    COUNTRY == "LT" | COUNTRY == "EE" | COUNTRY == "LV" ~ "RU",
    TRUE ~ COUNTRY)) %>% 
  filter(COUNTRY != 0)

countries <- unique(country_shapes_1990_cl$COUNTRY)
palette <- distinctColorPalette(length(countries))

# Test the old map
country_shapes_1990_cl %>%
  ggplot() + geom_sf(aes(fill=COUNTRY)) +
  scale_fill_manual(values=palette)

ggsave("_intermediate_data/historical/1990_map.png", last_plot(), width=11, height=8.5, units = "in")
print("Year = 1990")
for(i in 1:length(countries)){
  print(paste("Country", i, "of", length(countries)))
  # Get the historical country
  curr_country <- filter(country_shapes_1990_cl, COUNTRY==countries[i]) %>% st_union
  for(j in 1:nrow(nuts_shapes_clean_1990)){
    # Get the modern region
    curr_region <- nuts_shapes_clean_1990[j,]
    # Determine the share of the modern region in the historical country
    share_overlap <- as.numeric(st_area(st_intersection(curr_region, curr_country))/st_area(curr_region))
    if(is_empty(share_overlap)){next}
    else if(nuts_shapes_clean_1990[j,]$max_overlap > share_overlap){next}
    else{
      nuts_shapes_clean_1990[j,]$max_overlap <- share_overlap
      nuts_shapes_clean_1990[j,]$max_overlap_country <- countries[i]
    }
  }
}

final_dat_1990 <- nuts_shapes_clean_1990 %>% 
  # A few manual updates
  mutate(max_overlap_country = case_when(
    # Cyprus is independent in 1960
    NUTS_ID == "CY00" ~ "CY",
    # Flevoland is in the Netherlands, but is missed on old maps because it was re-claimed from the Sea.
    # Here we manually put it in the Netherlands
    NUTS_ID == "NL23" ~ "NL",
    # Turkey has been unified since before 1900 (as part of Ottoman Empire here)
    grepl("TR", NUTS_ID) ~ "TR",
    # Melilla and the Canaries have been under Spanish rule since before 1900
    NUTS_ID == "ES64" | NUTS_ID == "ES70" ~ "ES",
    # Azores and Madeira have been under Portugese rule since before 1900
    NUTS_ID == "PT20" | NUTS_ID == "PT30" ~ "PT",
    TRUE ~ max_overlap_country
  ))

# Final dat 1990
final_dat_1990 %>% 
  as.data.frame() %>% 
  select(NUTS_ID, max_overlap_country) %>%
  write_csv("_intermediate_data/historical/1990_country_of_modern_nuts2.csv")

### Map for appendix ###
# Change a couple historical names
final_dat_1990 <- final_dat_1990 %>%
  mutate(max_overlap_country = case_when(
    max_overlap_country == "RU" ~ "USSR",
    max_overlap_country == "GC" ~ "GDR",
    TRUE ~ max_overlap_country
  ))

set.seed(1)
palette <- distinctColorPalette(length(unique(final_dat_1990$max_overlap_country)))

# Make the final map
map_final_1990 <- final_dat_1990 %>% 
  # Re-project data to match other maps
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
  ggplot() +
  geom_sf(dat=final_dat_1990 , aes(fill=max_overlap_country)) +
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  scale_fill_manual(values=palette) +
  labs(fill = "Country") +
  theme_void() +
  theme(legend.position = c(0.9, 0.55))

ggsave("_intermediate_data/historical/1990_map_of_modern_nuts2.png", last_plot(), width=11, height=8.5, units = "in")


###########################
# 6. Combine the datasets #
###########################

country_historical <- 
  read_csv("_intermediate_data/historical/1900_country_of_modern_nuts2.csv") %>% 
  rename(country_1900=max_overlap_country) %>% 
  left_join(read_csv("_intermediate_data/historical/1930_country_of_modern_nuts2.csv")) %>% 
  rename(country_1930=max_overlap_country) %>% 
  left_join(read_csv("_intermediate_data/historical/1960_country_of_modern_nuts2.csv")) %>% 
  rename(country_1960=max_overlap_country) %>% 
  left_join(read_csv("_intermediate_data/historical/1990_country_of_modern_nuts2.csv")) %>% 
  rename(country_1990=max_overlap_country) %>% 
  mutate(country = substr(NUTS_ID, 1, 2))

write_csv(country_historical, "_intermediate_data/historical/country_historical.csv")

# The code below can be used to inspect country changes

# filter(country_historical, country_1900 != country_1930) %>% select(NUTS_ID, country_1900, country_1930) %>% View()
# filter(country_historical, country_1930 != country_1960) %>% select(NUTS_ID, country_1930, country_1960) %>% View()
# filter(country_historical, country_1960 != country_1990) %>% select(NUTS_ID, country_1960, country_1990) %>% View()
# filter(country_historical, country_1990 != country) %>% select(NUTS_ID, country_1990, country) %>% View()



########################################################
# 7. Make the cleaned up versions of maps for appendix #
########################################################

modern_map_dat <- st_read("_raw_data/eu_map/NUTS_RG_20M_2016_4326.shp", layer="NUTS_RG_20M_2016_4326") %>% 
  filter(LEVL_CODE == 2) %>% 
  filter(!grepl("FRY", NUTS_ID)) %>% 
  mutate(country = substr(NUTS_ID, 1, 2)) %>% 
  st_as_sf()

countries_1900 <- unique(final_dat_1900$max_overlap_country)
countries_1930 <- unique(final_dat_1930$max_overlap_country)
countries_1960 <- unique(final_dat_1960$max_overlap_country)
countries_1990 <- unique(final_dat_1990$max_overlap_country)
countries_modern <- unique(modern_map_dat$country)

all_countries <- union(countries_1900,
                       union(countries_1930,
                            union(countries_1960,
                                  union(countries_1990,
                                        countries_modern)))) %>% sort

set.seed(10)
palette <- distinctColorPalette(length(all_countries))

all_country_palette <- bind_cols(
  country = all_countries,
  color = palette) %>% 
  mutate(color = case_when(
    country == "ES" ~ "#DAC19F",
    country == "BG" ~ "#858860",
    country == "CH" ~ "#E3EAB5",
    country == "SE" ~ "#61AAB5",
    country == "EL" ~ "#347aeb", 
    country == "TR" ~ "#EBAA9D",
    country == "RO" ~ "#9741E8",
    country == "MK" ~ "#e1fc56",
    country == "CZ" ~ "#7B8CD7",
    country == "UK" ~ "#B7EC9D",
    country == "IE" ~ "#ff6666",
    country == "IS" ~ "#9CE93A",
    country == "DK" ~ "#A15E95",
    country == "RS" ~ "#abb7f5",
    country == "YO" ~ "#A8DC5A",
    country == "ME" ~ "#63EAC2",
    country == "AL" ~ "#E64B8C",
    country == "OE" ~ "#EBAA9D",
    country == "SE-NO" ~ "#E9C54B",
    country == "AT-HU" ~ "#B08487",
    country == "USSR" ~ "#ADE6C1",
    TRUE ~ color
  ))

# Modern map
modern_map_pal <- all_country_palette %>% 
  filter(country %in% modern_map_dat$country) %>% 
  .$color

map_modern <- 
  ggplot() +
    geom_sf(dat=modern_map_dat, aes(fill=country)) +
    coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
    scale_fill_manual(values=modern_map_pal) +
    labs(fill = "Country") +
    theme_void() +
    theme(legend.position = c(0.9, 0.55)) +
    guides(fill=guide_legend(ncol=3))

# 1900 Map
map_pal_1900 <- all_country_palette %>% 
  filter(country %in% final_dat_1900$max_overlap_country) %>% 
  .$color

map_1900 <- 
  final_dat_1900 %>% 
  # Re-project data to match other maps
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
  ggplot(.) +
    geom_sf(dat=final_dat_1900, aes(fill=max_overlap_country)) +
    coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
    scale_fill_manual(values=map_pal_1900) +
    labs(fill = "Country") +
    theme_void() +
    theme(legend.position = c(0.9, 0.55),       
          plot.title = element_text(size=12, family="serif")) +
    guides(fill=guide_legend(ncol=2)) +
    labs(title="B: 1900 Countries of Modern NUTS2")

# 1930 Map
map_pal_1930 <- all_country_palette %>% 
  filter(country %in% final_dat_1930$max_overlap_country) %>% 
  .$color

map_1930 <-
  final_dat_1930 %>% 
  # Re-project data to match other maps
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
  ggplot(.) +
    geom_sf(dat=final_dat_1930, aes(fill=max_overlap_country)) +
    coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
    scale_fill_manual(values=map_pal_1930) +
    labs(fill = "Country") +
    theme_void() +
    theme(legend.position = c(0.9, 0.55),       
        plot.title = element_text(size=12, family="serif")) +
    guides(fill=guide_legend(ncol=3)) +
    labs(title="A: 1930 Countries of Modern NUTS2")

# 1960 Map
map_pal_1960 <- all_country_palette %>% 
  filter(country %in% final_dat_1960$max_overlap_country) %>% 
  .$color

map_1960 <-
  final_dat_1960 %>% 
  # Re-project data to match other maps
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
  ggplot(.) +
    geom_sf(dat=final_dat_1960, aes(fill=max_overlap_country)) +
    coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
    scale_fill_manual(values=map_pal_1960) +
    labs(fill = "Country") +
    theme_void() +
    theme(legend.position = c(0.9, 0.55),       
          plot.title = element_text(size=12, family="serif")) +
    guides(fill=guide_legend(ncol=3)) +
    labs(title="B: 1960 Countries of Modern NUTS2")

# 1990 Map
map_pal_1990 <- all_country_palette %>% 
  filter(country %in% final_dat_1990$max_overlap_country) %>% 
  .$color

map_1990 <- 
  final_dat_1990 %>% 
    # Re-project data to match other maps
    st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
    ggplot(.) +
    geom_sf(dat=final_dat_1990, aes(fill=max_overlap_country)) +
    coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
    scale_fill_manual(values=map_pal_1990) +
    labs(fill = "Country") +
    theme_void() +
    theme(legend.position = c(0.9, 0.55),       
          plot.title = element_text(size=12, family="serif")) +
    guides(fill=guide_legend(ncol=3)) +
    labs(title="A: 1990 Countries of Modern NUTS2")


grid.arrange(map_1990, map_1960, nrow = 2) %>% 
  ggsave(paste0("_output/historical_maps_1990_1960.jpg"), .,
         width=7.75, height=10.25, units="in", dpi=320)

grid.arrange(map_1930, map_1900, nrow = 2) %>% 
  ggsave(paste0("_output/historical_maps_1930_1900.jpg"), .,
         width=7.75, height=10.25, units="in", dpi=320)

ggsave("_output/historical_maps_present_day.jpg", map_modern,
       width=7.75, height=5, units="in", dpi=320)