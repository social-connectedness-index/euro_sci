# Purpose: This file creates a NUTS2016 map in _raw_data/eu_map
# and the SCI_Nuts2_Nuts2.csv file in _raw_data. Both pull
# directly from online sources.

# Date: 2020-10-30

require(tidyverse)

##################################
## 1. Generate NUTS2 Shapefiles ##
##################################

# Download the European shapefiles from the Eurostat website.
# See the relevant liscence there: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
download.file(
  "https://gisco-services.ec.europa.eu/distribution/v2/nuts/shp/NUTS_RG_20M_2016_4326.shp.zip",
  "/tmp/nuts.zip"
)

# Store this first zipped file in a temp directory
dir.create("/tmp/nuts/", showWarnings = FALSE)

# Then unzip in directory you want to use
unzip("/tmp/nuts.zip", exdir="_raw_data/eu_map")

################################
## 2. Generate NUTS2 SCI data ##
################################

# Download the SCI data and make NUTS2_NUTS2 data
sci_dat_in <- 
  read_tsv("https://data.humdata.org/dataset/e9988552-74e4-4ff4-943f-c782ac8bca87/resource/7570bcc3-a208-49c4-8821-17f8df93c0e2/download/gadm1_nuts2_gadm1_nuts2_aug2020.tsv") %>% 
  rename(sci_dat_in, sci=scaled_sci)

nuts2_regions <- 
  read_csv("https://data.humdata.org/dataset/e9988552-74e4-4ff4-943f-c782ac8bca87/resource/800c2b30-e347-4e82-a04e-53c6d17def7b/download/gadm1_nuts2_levels.csv") %>% 
  filter(level == "nuts2") %>%
  .$key

sci_dat_out <- sci_dat_in %>% 
  filter(user_loc %in% nuts2_regions, fr_loc %in% nuts2_regions)

write_csv(sci_dat_out, "_raw_data/SCI_Nuts2_Nuts2.csv")


