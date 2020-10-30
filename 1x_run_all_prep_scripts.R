# Purpose: This driver file is a simple interface to run
# all of the prep files in _prep_scripts.

# Date: 2020-10-29

require(tidyverse)
require(rgdal)
require(sp)
require(sf)
require(rmapshaper)
require(randomcoloR)
require(grid)
require(gridExtra)
require(haven)
require(lsa)
require(osrm)

##############################################################################
#### A. Build initial NUTS historical crosswalks and NUTS2-NUTS2 SCI data ####
##############################################################################

# This builds a NUTS2 crosswalk from 2003 -> 2016.
# It comes from data avaliable at https://ec.europa.eu/eurostat/web/nuts/history
source("_prep_scripts/build_nuts2_crosswalk.R")
rm(list=ls())

# This builds a NUTS1 crosswalk from 2010 -> 2016.
# It comes from data avaliable at https://ec.europa.eu/eurostat/web/nuts/history.
# NOTE: This crosswalk assumes the availability of NUTS2 data to make the crosswalk.
source("_prep_scripts/build_nuts1_crosswalk_w_nuts2_groupings.R")
rm(list=ls())

# This filters the gadm-based SCI data to NUTS2-NUTS2 pairs only
source("_prep_scripts/build_nuts2_nuts2_sci_data.R")
rm(list=ls())


#################################################################################
#### B. Generate a number of cleaned datasets reading directly from raw data ####
#################################################################################

# Classify modern NUTS2 regions by the country they overlapped with most (often they were entirely within)
# in the years 1900, 1930, 1960, and 1990.
# CAUTION: This script does fairly expensive geographic overlap calculations and may take a long time to run.
source("_prep_scripts/generate_30_year_historicals.R")
rm(list=ls())

# Generate dummies for each country that tell whether the two countries
# border eachother. The final dataset will have rows for each NUTS2 region.
source("_prep_scripts/generate_border_dummies.R")
rm(list=ls())

# Produce distances between NUTS2 regions. These distances are
# first calculated at the NUTS3 level then population weighted
# up to the NUTS2, providing a semi-population-weighted distance.
source("_prep_scripts/generate_distances.R")
rm(list=ls())

# Read-in election data from the Brookings Paper:
# "The European trust crisis and the rise of populism"
source("_prep_scripts/generate_election_dat_from_brookings.R")
rm(list=ls())

# Read-in data from the European Social Survey on language and religion.
# CAUTION: These ESS files are fairly large, and this script may take some time to run.
source("_prep_scripts/generate_ess_controls.R")
rm(list=ls())

# Read-in data from the Eurobarometer on Trust in the EU
source("_prep_scripts/generate_eurobarometer_survey_dat.R")
rm(list=ls())

# Read in various regression controls from Eurostat data sources
# We must do this for both NUTS2 and NUTS1 level, as many of our
# political outcomes are at NUTS1.
source("_prep_scripts/generate_eurostat_controls.R")
rm(list=ls())

# Generate Industry-Shares by NUTS2.
# Here we grab the most recent year the data are available from 2014 to 2017.
# We also calculate the cosine distance of vectors of industry shares between each region.
source("_prep_scripts/generate_industry_shares.R")
rm(list=ls())


# Generate passenger travel between NUTS2 regions.
# In general, we have learned (from correspondence with Eurostat)
# that missing values can indicate EITHER zero or missing. However,
# the data are only missing at the country reporting level.
# Countries report international and domestic data seperately,
# so data missing entirely at the country-domestic / country-international
# level are treated as missing and other missings are treated as 0.
source("_prep_scripts/generate_passenger_train_travel.R")
rm(list=ls())

# Calculate, for each NUTS2-country pair and each NUTS1-country pair,
# the share of friendships from region i to country j.
source("_prep_scripts/generate_region_to_country_frnd_shares.R")
rm(list=ls())

# Get two sets of travel times between NUTS2 regions
# First, a measure of train travel times that were constructed
# for the ETIS project. We use the NUTS3 travel times
# and population weight up to NUTS2. 
# Second, a measure of car travel times between geographic
# centers of NUTS2 that we construct using the Open Source Routing Machine.
# CAUTION: The OSRM travel calculations can be computationally expensive to run.
source("_prep_scripts/generate_travel_times.R")
rm(list=ls())


###################################################################
#### C. Make final analysis datasets reading from cleaned data ####
###################################################################

# Make the final regression data for the anti-EU outcomes regressions
source("_prep_scripts/make_final_anti_eu_outcomes.R")
rm(list=ls())

# Make the final regression data for the NUTS2-to-NUTS2 SCI regressions
source("_prep_scripts/make_final_nuts2_to_nuts2_regress_dat.R")
rm(list=ls())

# Make the final regression data for the passenger train regressions
source("_prep_scripts/make_final_passenger_train_regress_dat.R")
rm(list=ls())