# Purpose: This driver file is a simple interface to run
# all of the R files in _analysis_scripts

# Date: 2020-10-29

require(tidyverse)
require(randomcoloR)
require(ggmap)
require(sf)
require(htmlwidgets)
require(leaflet)
require(grid)
require(gridExtra)
require(lfe)
require(broom)
require(dichromat)
require(RColorBrewer)

# Produce cluster maps and data using Euro SCI
source("_analysis_scripts/generate_cluster_maps.R")
rm(list=ls())

# Produce `Heatmaps` for each NUTS2 Region
# CAUTION: This produces a ton of maps (two for each NUTS2 region), and will take time to run
source("_analysis_scripts/generate_heatmaps.R")
rm(list=ls())

# Generate a map of each region's share of friends that are in a different country.
source("_analysis_scripts/generate_int_frnd_map.R")
rm(list=ls())

# Create bar chart that shows magnitude of same country fixed effects by country
source("_analysis_scripts/generate_same_country_coeffs.R")
rm(list=ls())