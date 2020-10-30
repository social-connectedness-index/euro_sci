# Purpose: Produce `Heatmaps` for each NUTS2 Region
# Inputs: _raw_data/eu_map; _raw_data/SCI_Nuts2_Nuts2.csv
# Outputs: _output/sci_heatmaps_regional_scaling;
#          _output/sci_heatmaps_overall_scaling.csv
# Date: 2020-10-29

library(tidyverse)
library(sf)
library(gridExtra)

###################################
# 1. Read/clean NUTS shapes & SCI #
###################################

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

# Read in the SCI data
sci_dat <- read_csv("_raw_data/SCI_Nuts2_Nuts2.csv")

# Get all the regions
regions <- unique(sci_dat$user_loc)


##########################################################
# 2. Generate heatmaps using the overall 20th percentile #
##########################################################

# The measures are scaled up from the 20th percentile overall
x1 <- quantile(sci_dat$sci, .2)
x2 <- x1 * 2
x3 <- x1 * 3
x5 <- x1 * 5
x10 <- x1 * 10
x25 <- x1 * 25
x100 <- x1 * 100

# Create clean buckets for these levels
sci_dat_bkts <- sci_dat %>% 
  mutate(sci_bkt = case_when(
    sci < x1 ~ "< 1x (Overall 20th percentile)",
    sci < x2 ~ "1-2x",
    sci < x3 ~ "2-3x",
    sci < x5 ~ "3-5x",
    sci < x10 ~ "5-10x",
    sci < x25 ~ "10-25x",
    sci < x100 ~ "25-100x",
    TRUE ~ ">= 100x")) %>% 
  mutate(sci_bkt = factor(sci_bkt, levels=c("< 1x (Overall 20th percentile)", "1-2x", "2-3x", "3-5x",
                                            "5-10x", "10-25x", "25-100x", ">= 100x")))

for(i in 1:length(regions)){
  
  # Get the data for the ith region
  dat <- filter(sci_dat_bkts, user_loc == regions[i])
  
  # Merge with shape files
  dat_map <- 
    inner_join(dat,
               nuts2_shapes,
               by=c("fr_loc"="NUTS_ID")) %>% 
    st_as_sf
  
  # Get the region you are in
  curr_region_outline <- dat_map %>% 
    filter(fr_loc == regions[i])
  
  # Plot the data
  ggplot(dat_map) +
    geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
    geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
    geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.4) +
    labs(fill = "SCI") +
    theme_void() +
    scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
    coord_sf(xlim = c(-35, 60), ylim = c(34.25, 71), expand = FALSE) +
    theme(legend.position = c(0.9, 0.55))
    
  ggsave(paste0("_output/sci_heatmaps_overall_scaling/sci_", regions[i], ".jpg"),
         last_plot(), width=12, height=5.5, units="in", dpi=320)
}


######################################
# 3. Make specific results for paper #
######################################

library(grid)
library(gridExtra)

########## Figure 1 ###########

region1 <- "BE22"
region2 <- "BE35"

dat <- filter(sci_dat_bkts, user_loc == region1)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region1)

# Plot the data
map1 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.4) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="A: Limburg, Belgium (BE22)")



dat <- filter(sci_dat_bkts, user_loc == region2)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region2)

# Plot the data
map2 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.4) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="B: Namur, Belgium (BE35)")

grid.arrange(map1, map2, nrow = 2) %>% 
  ggsave(paste0("_output/belgium_heatmaps.jpg"), .,
         width=7.75, height=10.25, units="in", dpi=320)


########## Figure 2 ############

region1 <- "RO41"
region2 <- "TR83"

dat <- filter(sci_dat_bkts, user_loc == region1)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region1)

# Plot the data
map1 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.4) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="A: South-West Oltenia, Romania (RO41)")



dat <- filter(sci_dat_bkts, user_loc == region2)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region2)

# Plot the data
map2 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.4) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="B: Samsun Subregion, Turkey (TR83)")

grid.arrange(map1, map2, nrow = 2) %>% 
  ggsave(paste0("_output/migration_heatmaps.jpg"), .,
         width=7.75, height=10.25, units="in", dpi=320)


########## Figure A1 ############

region1 <- "HR04"

dat <- filter(sci_dat_bkts, user_loc == region1)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region1)

# Plot the data
map1 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.4) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="")

ggsave(paste0("_output/croatia_heatmap.jpg"), map1,
       width=7.75, height=5, units="in", dpi=320)


########## Figure A2 ############

region1 <- "CH01"
region2 <- "CH06"

dat <- filter(sci_dat_bkts, user_loc == region1)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region1)

# Plot the data
map1 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.4) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="A: Lake Geneva Region, Switzerland (CH01)")


dat <- filter(sci_dat_bkts, user_loc == region2)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region2)

# Plot the data
map2 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.4) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="B: Central Switzerland, Switzerland (CH06)")

grid.arrange(map1, map2, nrow = 2) %>% 
  ggsave(paste0("_output/swiss_heatmaps.jpg"), .,
         width=7.75, height=10.25, units="in", dpi=320)


########## Figure A3 ############

region1 <- "TR32"
region2 <- "TRC2"

dat <- filter(sci_dat_bkts, user_loc == region1)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region1)

# Plot the data
map1 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.4) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="A: Aydin Subregion, Turkey (TR23)")


dat <- filter(sci_dat_bkts, user_loc == region2)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region2)

# Plot the data
map2 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.4) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="B: Sanliurfa Subregion, Turkey (TRC2)")

grid.arrange(map1, map2, nrow = 2) %>% 
  ggsave(paste0("_output/turkey_heatmaps.jpg"), .,
         width=7.75, height=10.25, units="in", dpi=320)

########## Figure A4 ############

region1 <- "FRE2"
region2 <- "FR10"

dat <- filter(sci_dat_bkts, user_loc == region1)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region1)

# Plot the data
map1 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.3) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="A: Picardy, France (FRE2)")


dat <- filter(sci_dat_bkts, user_loc == region2)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region2)

# Plot the data
map2 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.3) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="B: Ile-De-France, France (FR10)")

grid.arrange(map1, map2, nrow = 2) %>% 
  ggsave(paste0("_output/french_heatmaps.jpg"), .,
         width=7.75, height=10.25, units="in", dpi=320)

########## Figure A5 ############

region1 <- "CZ01"
region2 <- "CZ02"

dat <- filter(sci_dat_bkts, user_loc == region1)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region1)

# Plot the data
map1 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0.6) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="A: Prague, Czech Republic (CZ01)")


dat <- filter(sci_dat_bkts, user_loc == region2)

# Merge with shape files
dat_map <- 
  inner_join(dat,
             nuts2_shapes,
             by=c("fr_loc"="NUTS_ID")) %>% 
  st_as_sf

# Get the region you are in
curr_region_outline <- dat_map %>% 
  filter(fr_loc == region2)

# Plot the data
map2 <- ggplot(dat_map) +
  geom_sf(aes(fill = sci_bkt), colour="#ADADAD") +
  geom_sf(data=country_shapes, fill="transparent", colour="#444444") +
  geom_sf(data=curr_region_outline, fill="#A00000", colour="#A00000", size=0) +
  labs(fill = "SCI") +
  theme_void() +
  scale_fill_brewer(palette = "GnBu", drop=FALSE) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55),
        plot.title = element_text(size=12, family="serif")) +
  labs(title="B: Central Bohemia, Czech Republic (CZ02)")

grid.arrange(map1, map2, nrow = 2) %>% 
  ggsave(paste0("_output/czech_heatmaps.jpg"), .,
         width=7.75, height=10.25, units="in", dpi=320)