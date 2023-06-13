# Geographic data #

library(opencage)
library(sf)
library(terra)
library(elevatr)
library(readr)

# Geocoding ---------------------------------------------------------------

locations <- c(
  "Antwerp", "Bremen", "Cologne", "Frankfurt", "Stade", "Utrecht", "Leiden",
  "Dordrecht", "Haarlem", "London", "Venice", "Verona", "Naples", "Amsterdam",
  "Middelburg", "Hamburg", "Strasbourg", "Augsburg", "Nuremberg", "Seville",
  "Lisbon", "Kortrijk", "Valenciennes", "Breda", "Oudenaarde", "Lier", "Hulst",
  "Zutphen", "Bruges", "Ghent", "Brussels", "Mechelen", "Nijmegen", "Delft",
  "Geertruidenberg", "Enkhuizen"
)

locations <- oc_forward_df(placename = locations,
                           bounds = oc_bbox(-11, 34, 24, 58)) |>
  select(location = placename, lat = oc_lat, lng = oc_lng)

write_csv(locations, "data/locations.csv")

# Europe base map ---------------------------------------------------------

euro_bbox <- st_bbox(c(xmin = -11, xmax = 24, ymax = 58, ymin = 34),
                     crs = st_crs(4326)) |>
  st_as_sfc()

euro_raster <- get_elev_raster(euro_bbox, z = 6)
euro_raster <- euro_raster |>
  rast() |>
  project("EPSG:3034")

# Remove negative elevation
min_val <- min(values(euro_raster), na.rm = TRUE)
euro_raster <- euro_raster |>
  classify(matrix(c(min_val, 0 , 0), ncol = 3))

writeRaster(euro_raster, "data/euro.tif")

# Northwest Europe base map -----------------------------------------------

nw_bbox <- st_bbox(c(xmin = -2, xmax = 12, ymax = 55, ymin = 49),
                   crs = st_crs(4326)) |>
  st_as_sfc()

nw_raster <- get_elev_raster(nw_bbox, z = 7)
nw_raster <- nw_raster |>
  rast() |>
  project("EPSG:3034")

# Remove negative elevation and high peaks outside the used area
min_val <- min(values(nw_raster), na.rm = TRUE)
max_val <- max(values(nw_raster), na.rm = TRUE)

nw_raster <- nw_raster |>
  classify(matrix(c(min_val, 0 , 0), ncol = 3))

writeRaster(nw_raster, "data/northwest.tif")

# Low Countries base map --------------------------------------------------

lc_bbox <- st_bbox(c(xmin = 0, xmax = 8, ymax = 54, ymin = 49),
                   crs = st_crs(4326)) |>
  st_as_sfc()

lc_raster <- get_elev_raster(lc_bbox, z = 8)

lc_raster <- lc_raster |>
  rast() |>
  project("EPSG:3034")

# Remove negative elevation
min_val <- min(values(lc_raster), na.rm = TRUE)
lc_raster <- lc_raster |>
  classify(matrix(c(min_val, 0 , 0), ncol = 3))

writeRaster(lc_raster, "data/lowcountries.tif")
