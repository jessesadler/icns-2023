# Women's movements #

library(tibble)
library(dplyr)
library(readr)
library(sf)
library(terra)
library(tidyterra)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(glue)


# Map data ----------------------------------------------------------------
set_crs <- st_crs(3034)

rivers <- st_read(
  "data/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp") |>
  st_transform(crs = set_crs)
lakes <-  st_read("data/ne_10m_lakes/ne_10m_lakes.shp") |>
  st_transform(crs = set_crs)
oceans <- st_read("data/ne_10m_ocean/ne_10m_ocean.shp") |>
  st_transform(crs = set_crs)

# Elevation data
lc_raster <- rast("data/lowcountries.tif")
nw_raster <- rast("data/northwest.tif")

# Geocoded data
locations <- read_csv("data/locations.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = set_crs) %>%
  mutate(lng = st_coordinates(.)[ , 1],
         lat = st_coordinates(.)[ , 2])


# Movements in 1584 -------------------------------------------------------
hex <- c(scales::hue_pal()(1), rep("black", 4))

locations_1584 <- tibble(
  location = c("Antwerp", "Dordrecht", "Haarlem", "Delft", "Cologne"),
  type = c("home", rep("place", 4))) |>
  left_join(locations, by = "location") |>
  st_as_sf()

# Base map
lc_movements <- ggplot() +
  geom_spatraster(data = lc_raster, alpha = 0.6) +
  scale_fill_whitebox_c(palette = "atlas", direction = 1) +
  geom_sf(data = oceans, color = NA, fill = "lightblue") +
  geom_sf(data = rivers, color = "lightblue", size = 0.2) +
  geom_sf(data = lakes, color = NA, fill = "lightblue") +
  geom_sf(data = locations_1584, size = 4, aes(color = type)) +
  scale_color_manual(values = c(scales::hue_pal()(1), "#000000")) +
  geom_text_repel(data = locations_1584,
                  aes(x = lng, y = lat, label = location),
                  size = 4, seed = 242) +
  coord_sf(xlim = c(3450000, 3850000),
           ylim = c(2650000, 3000000),
           expand = FALSE, datum = NA) +
  theme_void() +
  theme(legend.position = "none")

title_1584 <- glue(
  "**Movements of women in
  <span style='color:{scales::hue_pal()(1)};'>1584</span>
  as Farnese approached Antwerp**")

lc_movements +
labs(title = title_1584) +
theme(plot.title = element_markdown(size = 18, margin = margin(t = 5, b = 10)))

ggsave("plots/women-1584.png", width = 8, height = 10)

# Movements in 1585 -------------------------------------------------------
title_1585 <- glue(
  "**Movements of women in
  <span style='color:{scales::hue_pal()(1)};'>1585</span>
  after the fall of Antwerp**")

lc_movements +
  labs(title = title_1585) +
  theme(plot.title = element_markdown(size = 18, margin = margin(t = 5, b = 10)))

ggsave("plots/women-1585.png", width = 8, height = 10)


# Movements after fall of Antwerp -----------------------------------------
locations_1585 <- tibble(
  location = c("Antwerp", "Haarlem", "Delft", "Bremen", "Cologne",
               "Geertruidenberg", "Enkhuizen"),
  type = c("home", rep("place", 6))) |>
  left_join(locations, by = "location") |>
  st_as_sf()

title_1585_vdm <- glue(
  "**Movements of women in
  <span style='color:{scales::hue_pal()(1)};'>1585</span>
  after the fall of Antwerp**")

ggplot() +
  geom_spatraster(data = nw_raster, alpha = 0.6) +
  scale_fill_whitebox_c(palette = "atlas", direction = 1) +
  geom_sf(data = oceans, color = NA, fill = "lightblue") +
  geom_sf(data = rivers, color = "lightblue", size = 0.2) +
  geom_sf(data = lakes, color = NA, fill = "lightblue") +
  geom_sf(data = locations_1585, size = 4, aes(color = type)) +
  scale_color_manual(values = c(scales::hue_pal()(1), "#000000")) +
  geom_text_repel(data = locations_1585,
                  aes(x = lng, y = lat, label = location),
                  size = 4) +
  coord_sf(xlim = c(3450000, 4000000),
           ylim = c(2650000, 3000000),
           expand = FALSE, datum = NA) +
  labs(title = title_1585) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 18, margin = margin(t = 5, b = 10)))

ggsave("plots/women-1585-vdm.png", width = 8, height = 10)
