# Purpose: Produce cluster maps and data using Euro SCI
# Inputs: _raw_data/eu_map; _raw_data/SCI_Nuts2_Nuts2.csv
# Outputs: _output/cluster_results/cluster_groups.csv;
#           _output/cluster_results/20_clusters.png;
#           _output/cluster_results/50_clusters.png;
#           _output/cluster_results/20_clusters_black_and_white.png;
#           _output/cluster_results/50_clusters_black_and_white.png
#           _output/cluster_results/cluster_interactive.html;
# Date: 2020-10-29

library(tidyverse)
library(randomcoloR)
library(ggmap)
library(sf)
library(htmlwidgets)
library(leaflet)
library(grid)
library(gridExtra)

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

# Read in the SCI data - filter out French Islands
sci_dat <- read_csv("_raw_data/SCI_Nuts2_Nuts2.csv") %>% 
  filter(!grepl("FRY", user_loc),
         !grepl("FRY", fr_loc))

# Get an original distance table
all_regional_dist <- sci_dat %>% 
  filter(user_loc != fr_loc) %>% 
  mutate(dist = 1/sci)

# Make it a region matrix
dist_matrix <- all_regional_dist %>% 
  select(-sci) %>% 
  spread(fr_loc, dist) %>% 
  as.data.frame()
rownames(dist_matrix) <- dist_matrix$user_loc
dist_matrix <- select(dist_matrix, -user_loc)
dist_matrix <- as.dist(dist_matrix)

# Run the cluster
set.seed(1)
clusters_out <- hclust(dist_matrix, "ave")

# Save the clusters to a tibble with columns as the clusters
clusters_dat <- data.frame(clusters1=cutree(clusters_out, k=1)) %>% 
  rownames_to_column(var = "region")
for(i in 2:length(clusters_out$labels)){
  curr_clusters <- data.frame(cutree(clusters_out, k=i))
  names(curr_clusters) <- c(paste0("clusters",i))
  clusters_dat <- bind_cols(clusters_dat, curr_clusters)
}
clusters_dat <- as_tibble(clusters_dat)

# To re-run without re-doing the clustering, start here
write_csv(clusters_dat, "_output/cluster_results/cluster_groups.csv")
clusters_dat <- read_csv("_output/cluster_results/cluster_groups.csv")


#############################
# 2. Create the static maps #
#############################

## 20 Clusters ##
map_dat_20 <- select(clusters_dat, region, group=clusters20) %>% 
  mutate(group = as.factor(group)) %>% 
  left_join(nuts2_shapes, by=c("region"="NUTS_ID")) %>% 
  st_as_sf

## Color map
set.seed(1)
color_pal_20 <- distinctColorPalette(20)
color_pal_20 <- sample(color_pal_20, 20, replace=F)

color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "EE")$group)] <- "#9D63C2"
color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "LT")$group)] <- "#9EE1AC"
color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "PL")$group)] <- "#F8C1D0"
color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "DE")$group)] <- "#E56868"
color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "IT")$group)] <- "#6DEE73"
color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "FR")$group)] <- "#6DE1EE"
color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "BE")$group)] <- "#E1F8C9"
color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "IE")$group)] <- "#6C83E6"
color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "IS")$group)] <- "#D282FF"
color_pal_20[unique(filter(map_dat_20, region == "UKI7")$group)] <- "#fce758"
color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "CZ")$group)] <- "#559868"
color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "BG")$group)] <- "#ed592f"
color_pal_20[unique(filter(map_dat_20, CNTR_CODE == "NO")$group)] <- "#f7a848"

ggplot(data=map_dat_20) +
  geom_sf(aes(fill=group), size=0.3, col="#606060") +
  scale_fill_manual(values = color_pal_20) +
  geom_sf(data=country_shapes, fill=NA, color="black", size=0.7) +
  coord_sf(xlim = c(-24, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme_nothing()

ggsave("_output/cluster_results/20_clusters.png", last_plot(), width=12, height=7, units="in", dpi=1500)


## 50 Clusters ##
map_dat_50 <- select(clusters_dat, region, group=clusters50) %>% 
  mutate(group = as.factor(group)) %>% 
  left_join(nuts2_shapes, by=c("region"="NUTS_ID")) %>% 
  st_as_sf

## Color map
set.seed(1)
color_pal_50 <- distinctColorPalette(50)

color_pal_50[unique(filter(map_dat_50, CNTR_CODE == "IE")$group)] <- "#F5EBB8"
color_pal_50[unique(filter(map_dat_50, region == "UKM6")$group)] <- "#E94F53"
color_pal_50[unique(filter(map_dat_50, region == "UKD1")$group)] <- "#559868"
color_pal_50[unique(filter(map_dat_50, region == "UKH1")$group)] <- "#e4eff0"
color_pal_50[unique(filter(map_dat_50, region == "UKG1")$group)] <- "#D245C6"
color_pal_50[unique(filter(map_dat_50, region == "UKI7")$group)] <- "#f7ee34"
color_pal_50[unique(filter(map_dat_50, CNTR_CODE == "SE")$group)] <- "#E7C0FA"
color_pal_50[unique(filter(map_dat_50, region == "FI1D")$group)] <- "#E9FDFF"
color_pal_50[unique(filter(map_dat_50, region == "EE00")$group)] <- "#7FFFF0"
color_pal_50[unique(filter(map_dat_50, region == "LT02")$group)] <- "#917AFC"
color_pal_50[unique(filter(map_dat_50, region == "PL41")$group)] <- "#EC6E5B"
color_pal_50[unique(filter(map_dat_50, region == "CZ07")$group)] <- "#FDFB97"
color_pal_50[unique(filter(map_dat_50, region == "AT12")$group)] <- "#D0AB42"
color_pal_50[unique(filter(map_dat_50, region == "DE80")$group)] <- "#A2B676"
color_pal_50[unique(filter(map_dat_50, region == "DK04")$group)] <- "#83BE2C"
color_pal_50[unique(filter(map_dat_50, region == "DE21")$group)] <- "#41D8F1"
color_pal_50[unique(filter(map_dat_50, region == "DEA5")$group)] <- "#B26F2D"
color_pal_50[unique(filter(map_dat_50, region == "DEF0")$group)] <- "#DF72C3"
color_pal_50[unique(filter(map_dat_50, region == "DE71")$group)] <- "#B969E6"
color_pal_50[unique(filter(map_dat_50, region == "FRE2")$group)] <- "#79B6F5"
color_pal_50[unique(filter(map_dat_50, region == "ITI3")$group)] <- "#AA6EE3"
color_pal_50[unique(filter(map_dat_50, region == "CH02")$group)] <- "#5A60E1"
color_pal_50[unique(filter(map_dat_50, region == "FRF3")$group)] <- "#F2F1F1"
color_pal_50[unique(filter(map_dat_50, region == "FRC1")$group)] <- "#DFC811"
color_pal_50[unique(filter(map_dat_50, region == "NL21")$group)] <- "#6DFBC5"
color_pal_50[unique(filter(map_dat_50, region == "FRK1")$group)] <- "#B8FA91"
color_pal_50[unique(filter(map_dat_50, region == "FRG0")$group)] <- "#7556DB"
color_pal_50[unique(filter(map_dat_50, region == "FR10")$group)] <- "#A73014"
color_pal_50[unique(filter(map_dat_50, region == "ES51")$group)] <- "#F7A3A3"
color_pal_50[unique(filter(map_dat_50, region == "AL02")$group)] <- "#F1DCF1"
color_pal_50[unique(filter(map_dat_50, region == "RO21")$group)] <- "#43FB48"
color_pal_50[unique(filter(map_dat_50, region == "HU33")$group)] <- "#EC99F5"
color_pal_50[unique(filter(map_dat_50, region == "ITG1")$group)] <- "#A0F7C1"
color_pal_50[unique(filter(map_dat_50, region == "NO05")$group)] <- "#f7a848"

ggplot(data=map_dat_50) +
  geom_sf(aes(fill=group), size=0.3, col="#606060") +
  scale_fill_manual(values = color_pal_50) +
  geom_sf(data=country_shapes, fill=NA, color="black", size=0.7) +
  coord_sf(xlim = c(-40, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme_nothing()

ggsave("_output/cluster_results/50_clusters.png", last_plot(), width=12, height=7, units="in", dpi=1500)

#############################
# 3. Create the dynamic map #
#############################

map_list <- list()
color_pal <- list()
cluster_pal <- list()
labels <- list()
color <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
for(i in 1:100){
  # Generate the current map
  curr_map <- clusters_dat[,c(1, i+1)] 
  colnames(curr_map) <- c("region", "group")
  curr_map <- curr_map %>% 
    mutate(group = as.factor(group)) %>% 
    left_join(nuts2_shapes, by=c("region"="NUTS_ID")) %>% 
    st_as_sf
  map_list[[i]] <- curr_map
  set.seed(10)
  # Generate the color palette
  color_pal[[i]] <- sample(color, i)
  cluster_pal[[i]] <- colorFactor(color_pal[[i]], map_list[[i]]$group)
  labels[[i]] <- sprintf(
    "<strong>Name:</strong>%s<br/>
    <strong>NUTS ID:</strong>%s<br/>
    <strong>Group:</strong>%d<br/>",
    map_list[[i]]$NUTS_NAME,
    map_list[[i]]$region,
    map_list[[i]]$group
  ) %>% lapply(htmltools::HTML)
}

map <- leaflet() %>% 
  addProviderTiles(
    "Esri.WorldStreetMap",
    options = leafletOptions()               
  ) %>% 
  addPolygons(
    data=country_shapes,
    weight = 3,
    opacity = 1,
    fillOpacity = 0,
    color = "black",
    group = "Country"
  )

for(i in seq(5, 100, by=5)){
  map <- map %>% 
    addPolygons(
      data = map_list[[i]],
      weight = 1.7,
      opacity = 0.5,
      fillOpacity = 0.7,
      color = "gray",
      fillColor = ~cluster_pal[[i]](group),
      highlight = highlightOptions(
        weight = 10,
        opacity = 1,
        color = "#666",
        bringToFront = TRUE
      ),
      group = paste(i, "Clusters"),
      label = labels[[i]],
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
}

map <- map %>% 
  addLayersControl(
    baseGroups = paste(seq(5, 100, by=5), "Clusters"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  setView(lat=57, lng=10, zoom=4)

# Saving with saveWidget outside of the current directory can lead to issues
setwd("_output/cluster_results")
saveWidget(map, "cluster_interactive.html")
setwd("../..")

#######################################
# 4. Create stacked figures for paper #
#######################################

map20_full <- ggplot(data=map_dat_20) +
  geom_sf(aes(fill=group), size=0.3, col="#606060") +
  scale_fill_manual(values = color_pal_20) +
  geom_sf(data=country_shapes, fill=NA, color="black", size=0.7) +
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(plot.title = element_text(size=12, family="serif")) +
  theme_nothing()

map20_cutout <- ggplot(data=filter(map_dat_20, CNTR_CODE == "UK")) +
  geom_sf(aes(fill=group), size=0.3, col="#606060") +
  scale_fill_manual(values = c("#6C83E6", "#fce758")) +
  coord_sf(xlim = c(-1.1, 1.7), ylim = c(50.9, 52), expand = TRUE) +
  theme_void() +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=9)) +
  labs(title="Greater London")

map1 <- grid.arrange(map20_cutout, map20_full, widths = c(1,5),
                     top = textGrob("A: 20 Units", x=0, hjust=-0.1,
                                    gp=gpar(fontfamily="serif", fontsize=12)))

map50_full <- ggplot(data=map_dat_50) +
  geom_sf(aes(fill=group), size=0.3, col="#606060") +
  scale_fill_manual(values = color_pal_50) +
  geom_sf(data=country_shapes, fill=NA, color="black", size=0.7) +
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(plot.title = element_text(size=12, family="serif")) +
  theme_nothing()

map50_cutout <- ggplot(data=filter(map_dat_50)) +
  geom_sf(aes(fill=group), size=0.3, col="#606060") +
  scale_fill_manual(values = color_pal_50) +
  coord_sf(xlim = c(-1.1, 1.7), ylim = c(50.9, 52), expand = TRUE) +
  theme_void() +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size=9)) +
  labs(title="Greater London")

map2 <- grid.arrange(map50_cutout, map50_full, widths = c(1,5),
                     top = textGrob("B: 50 Units", x=0, hjust=-0.1,
                                    gp=gpar(fontfamily="serif", fontsize=12)))

grid.arrange(map1, map2, nrow = 2) %>% 
  ggsave(paste0("_output/cluster_maps_w_pullout.jpg"), .,
         width=7.75, height=10.25, units="in", dpi=320)