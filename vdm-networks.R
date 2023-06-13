## VdM Networks ##

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
euro_raster <- rast("data/euro.tif")
nw_raster <- rast("data/northwest.tif")

# Geocoded data
locations <- read_csv("data/locations.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = set_crs) %>%
  mutate(lng = st_coordinates(.)[ , 1],
         lat = st_coordinates(.)[ , 2])

# DF and VdM trade networks -----------------------------------------------
home <- tibble(
  location = "Antwerp",
  type = "Home"
)
residence <- tibble(
  location = c("Bremen", "Cologne", "Frankfurt", "Stade",
               "Utrecht", "Leiden", "Dordrecht", "Haarlem"),
  type = "Residence"
)
trade <- tibble(
  location = c("London", "Venice", "Verona", "Naples", "Amsterdam",
               "Middelburg", "Hamburg", "Strasbourg", "Augsburg", "Nuremberg",
               "Seville", "Lisbon", "Kortrijk", "Valenciennes"),
  type = "Trade"
)

network <- bind_rows(home, residence, trade) |>
  left_join(locations, by = "location") |>
  st_as_sf()


# Styled title
hex <- scales::hue_pal()(length(unique(network$type)))

styled_title <- glue("**Network of the Van der Meulen and Della Faille families**")
styled_subtitle <- glue(
  "Their home in <span style='color:{hex[[1]]};'>Antwerp</span>,
  the <span style='color:{hex[[2]]};'>places they resided</span>,
  and important locations in their
  <span style='color:{hex[[3]]};'>trade networks</span>"
)

ggplot() +
  geom_spatraster(data = euro_raster, alpha = 0.6) +
  scale_fill_whitebox_c(palette = "atlas", direction = 1) +
  geom_sf(data = oceans, color = NA, fill = "lightblue") +
  geom_sf(data = rivers, color = "lightblue", size = 0.1) +
  geom_sf(data = lakes, color = NA, fill = "lightblue") +
  geom_sf(data = network, aes(color = type), size = 2) +
  geom_text_repel(data = network,
                  aes(x = lng, y = lat, label = location),
                  size = 4) +
  coord_sf(xlim = c(2100000, 4600000),
           ylim = c(1050000, 3400000),
           expand = FALSE, datum = NA) +
  labs(title = styled_title,
       subtitle = styled_subtitle) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 18),
        plot.subtitle = element_markdown(size = 14, margin = margin(t = 5, b = 10)))

ggsave("plots/family-networks.png", width = 12, height = 8)


# VdM 1586 ----------------------------------------------------------------
hex <- scales::hue_pal()(3)

locations_vdm <- tibble(
  location = c("Antwerp", "London", "Middelburg", "Haarlem", "Hamburg",
               "Stade", "Bremen", "Cologne", "Frankfurt", "Strasbourg"),
  type = c("home", rep("place", 5), rep("residence", 2), rep("fair", 2))
  ) |>
  left_join(locations, by = "location") |>
  st_as_sf()

styled_subtitle <- glue(
  "Places of <span style='color:{hex[[2]]};'>residence</span> and
  <span style='color:{hex[[3]]};'>fairs</span> where they traded"
)

ggplot() +
  geom_spatraster(data = nw_raster, alpha = 0.6) +
  scale_fill_whitebox_c(palette = "atlas", direction = 1) +
  geom_sf(data = oceans, color = NA, fill = "lightblue") +
  geom_sf(data = rivers, color = "lightblue", size = 0.2) +
  geom_sf(data = lakes, color = NA, fill = "lightblue") +
  geom_sf(data = locations_vdm, aes(color = type), size = 4) +
  scale_color_manual(
    values = c(home = hex[[1]], place = "#000000",
               residence = hex[[2]], fair = hex[[3]])) +
  geom_text_repel(data = locations_vdm,
                  aes(x = lng, y = lat, label = location),
                  size = 4, seed = 242) +
  coord_sf(xlim = c(3250000, 4100000),
           ylim = c(2300000, 3100000),
           expand = FALSE, datum = NA) +
  labs(title = "**Van der Meulen network after the fall of Antwerp**",
       subtitle = styled_subtitle) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 20),
        plot.subtitle = element_markdown(size = 18, margin = margin(t = 5, b = 10)))

ggsave("plots/vdms-1586.png", width = 12, height = 8)


# VdM 1588 ----------------------------------------------------------------

locations_vdm <- locations_vdm |>
  mutate(type = c("home", rep("place", 4), rep("residence", 3), rep("fair", 2)))

ggplot() +
  geom_spatraster(data = nw_raster, alpha = 0.6) +
  scale_fill_whitebox_c(palette = "atlas", direction = 1) +
  geom_sf(data = oceans, color = NA, fill = "lightblue") +
  geom_sf(data = rivers, color = "lightblue", size = 0.2) +
  geom_sf(data = lakes, color = NA, fill = "lightblue") +
  geom_sf(data = locations_vdm, aes(color = type), size = 4) +
  scale_color_manual(
    values = c(home = hex[[1]], place = "#000000",
               residence = hex[[2]], fair = hex[[3]])) +
  geom_text_repel(data = locations_vdm,
                  aes(x = lng, y = lat, label = location),
                  size = 4, seed = 242) +
  coord_sf(xlim = c(3250000, 4100000),
           ylim = c(2300000, 3100000),
           expand = FALSE, datum = NA) +
  labs(title = "**Anna moves to Stade in 1588**",
       subtitle = styled_subtitle) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 20),
        plot.subtitle = element_markdown(size = 18, margin = margin(t = 5, b = 10)))

ggsave("plots/vdms-1588.png", width = 12, height = 8)


# VdM network in 1609 -----------------------------------------------------

locations_1609 <- tibble(
  location = c("Antwerp", "Middelburg", "Haarlem",
               "Leiden", "Bremen", "Utrecht", "Frankfurt"),
  type = c("home", rep("place", 2), rep("residence", 4))) |>
  left_join(locations, by = "location") |>
  st_as_sf()

styled_title <- glue("**Van der Meulen network in
                     <span style='color:{hex[[1]]};'>1609</span>**")
styled_subtitle <- glue(
  "Places of <span style='color:{hex[[2]]};'>residence</span>
  at the start of the Twelve-Year Truce")

ggplot() +
  geom_spatraster(data = nw_raster, alpha = 0.6) +
  scale_fill_whitebox_c(palette = "atlas", direction = 1) +
  geom_sf(data = oceans, color = NA, fill = "lightblue") +
  geom_sf(data = rivers, color = "lightblue", size = 0.2) +
  geom_sf(data = lakes, color = NA, fill = "lightblue") +
  geom_sf(data = locations_1609, aes(color = type), size = 4) +
  scale_color_manual(values = c(hex[[1]], "#000000", hex[[2]])) +
  geom_text_repel(data = locations_1609,
                  aes(x = lng, y = lat, label = location),
                  size = 4) +
  coord_sf(xlim = c(3450000, 4000000),
           ylim = c(2550000, 3000000),
           expand = FALSE, datum = NA) +
  labs(title = styled_title,
       subtitle = styled_subtitle) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 20),
        plot.subtitle = element_markdown(size = 18, margin = margin(t = 5, b = 10)))

ggsave("plots/vdms-1609.png", width = 12, height = 8)
