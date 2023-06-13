## Reconquest plots ##

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

# Geocoded data
locations <- read_csv("data/locations.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = set_crs) %>%
  mutate(lng = st_coordinates(.)[ , 1],
         lat = st_coordinates(.)[ , 2])


# Reconquest data ---------------------------------------------------------
reconquest <- tribble(
  ~location,    ~date,
  "Kortrijk",   1580,
  "Breda",      1580,
  "Oudenaarde", 1582,
  "Lier",       1582,
  "Hulst",      1583,
  "Zutphen",    1583,
  "Bruges",     1584,
  "Ghent",      1584,
  "Brussels",   1585,
  "Mechelen",   1585,
  "Antwerp",    1585,
  "Nijmegen",   1585
)

reconquest <- reconquest |>
  left_join(locations, by = "location") |>
  st_as_sf()


# Plots -------------------------------------------------------------------
hex <- scales::brewer_pal(palette = "Set1")(length(unique(reconquest$date)))

# 1580 --------------------------------------------------------------------
title_1580 <- glue("
      **Major cities conquered by Farnese in
      <span style='color:{hex[[1]]};'>1580</span>**</span>")

reconquest |>
  filter(date == 1580) |>
  ggplot() +
  geom_spatraster(data = lc_raster, alpha = 0.6) +
  scale_fill_whitebox_c(palette = "atlas", direction = 1) +
  geom_sf(data = oceans, color = NA, fill = "lightblue") +
  geom_sf(data = rivers, color = "lightblue", size = 0.2) +
  geom_sf(data = lakes, color = gray(0.8), fill = "lightblue") +
  geom_sf(color = hex[[1]], size = 4) +
  geom_text_repel(aes(x = lng, y = lat, label = location),
                  size = 4, seed = 242) +
  coord_sf(xlim = c(3450000, 3800000),
           ylim = c(2600000, 3000000),
           expand = FALSE, datum = NA) +
  labs(title = title_1580) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 20, margin = margin(t = 5, b = 5)))

ggsave("plots/reconquest-1580.png", width = 8, height = 10)

# 1582 --------------------------------------------------------------------
title_1582 <- glue("
      **Major cities conquered by Farnese in
      <span style='color:{hex[[2]]};'>1582</span>**</span>")

reconquest |>
  filter(date <= 1582) |>
  ggplot() +
  geom_spatraster(data = lc_raster, alpha = 0.6) +
  scale_fill_whitebox_c(palette = "atlas", direction = 1) +
  geom_sf(data = oceans, color = NA, fill = "lightblue") +
  geom_sf(data = rivers, color = "lightblue", size = 0.2) +
  geom_sf(data = lakes, color = gray(0.8), fill = "lightblue") +
  geom_sf(aes(color = factor(date)), size = 4) +
  scale_color_brewer(palette = "Set1") +
  geom_text_repel(aes(x = lng, y = lat, label = location),
                  size = 4, seed = 242) +
  coord_sf(xlim = c(3450000, 3800000),
           ylim = c(2600000, 3000000),
           expand = FALSE, datum = NA) +
  labs(title = title_1582) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 20, margin = margin(t = 5, b = 5)))

ggsave("plots/reconquest-1582.png", width = 8, height = 10)

# 1583 --------------------------------------------------------------------
title_1583 <- glue("
      **Major cities conquered by Farnese in
      <span style='color:{hex[[3]]};'>1583</span>**</span>")

reconquest |>
  filter(date <= 1583) |>
  ggplot() +
  geom_spatraster(data = lc_raster, alpha = 0.6) +
  scale_fill_whitebox_c(palette = "atlas", direction = 1) +
  geom_sf(data = oceans, color = NA, fill = "lightblue") +
  geom_sf(data = rivers, color = "lightblue", size = 0.2) +
  geom_sf(data = lakes, color = gray(0.8), fill = "lightblue") +
  geom_sf(aes(color = factor(date)), size = 4) +
  scale_color_brewer(palette = "Set1") +
  geom_text_repel(aes(x = lng, y = lat, label = location),
                  size = 4, seed = 242) +
  coord_sf(xlim = c(3450000, 3800000),
           ylim = c(2600000, 3000000),
           expand = FALSE, datum = NA) +
  labs(title = title_1583) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 20, margin = margin(t = 5, b = 5)))

ggsave("plots/reconquest-1583.png", width = 8, height = 10)

# 1584 --------------------------------------------------------------------
title_1584 <- glue("
      **Major cities conquered by Farnese in
      <span style='color:{hex[[4]]};'>1584</span>**</span>")

reconquest |>
  filter(date <= 1584) |>
  ggplot() +
  geom_spatraster(data = lc_raster, alpha = 0.6) +
  scale_fill_whitebox_c(palette = "atlas", direction = 1) +
  geom_sf(data = oceans, color = NA, fill = "lightblue") +
  geom_sf(data = rivers, color = "lightblue", size = 0.2) +
  geom_sf(data = lakes, color = gray(0.8), fill = "lightblue") +
  geom_sf(aes(color = factor(date)), size = 4) +
  scale_color_brewer(palette = "Set1") +
  geom_text_repel(aes(x = lng, y = lat, label = location),
                  size = 4, seed = 242) +
  coord_sf(xlim = c(3450000, 3800000),
           ylim = c(2600000, 3000000),
           expand = FALSE, datum = NA) +
  labs(title = title_1584) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 20, margin = margin(t = 5, b = 5)))

ggsave("plots/reconquest-1584.png", width = 8, height = 10)

# 1585 --------------------------------------------------------------------
title_1585 <- glue("
      **Major cities conquered by Farnese in
      <span style='color:{hex[[5]]};'>1585</span>**</span>")

reconquest |>
  ggplot() +
  geom_spatraster(data = lc_raster, alpha = 0.6) +
  scale_fill_whitebox_c(palette = "atlas", direction = 1) +
  geom_sf(data = oceans, color = NA, fill = "lightblue") +
  geom_sf(data = rivers, color = "lightblue", size = 0.2) +
  geom_sf(data = lakes, color = gray(0.8), fill = "lightblue") +
  geom_sf(aes(color = factor(date)), size = 4) +
  scale_color_brewer(palette = "Set1") +
  geom_text_repel(aes(x = lng, y = lat, label = location),
                  size = 4, seed = 242) +
  coord_sf(xlim = c(3450000, 3800000),
           ylim = c(2600000, 3000000),
           expand = FALSE, datum = NA) +
  labs(title = title_1585) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 20, margin = margin(t = 5, b = 5)),)

ggsave("plots/reconquest-1585.png", width = 8, height = 10)
