#### example polygon application
library(sf)
library(dplyr)
library(tidyr)

source("./get_nlcd_data.R")
source("./nlcd_definitions.R")

raw_data <- readr::read_csv("test/my_address_file_geocoded.csv")
raw_data$.row <- seq_len(nrow(raw_data))

d <-
  raw_data %>%
  select(.row, lat, lon) %>%
  na.omit() %>%
  nest(.rows = c(.row)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# project to 5072 for buffering in meters
d <- d %>%
  st_transform(5072) %>%
  st_buffer(dist = 400)

# reproject points into NLCD projection for overlay
d <- d %>%
  sf::st_transform(crs = raster::crs(r_nlcd_empty))

#### function to get all data for a single polygon

# this assumes that the query polygon is already in the CRS of the raster!
get_nlcd_percentages <- function(row) {
  
  query_poly <- d[row, ]
  
  nlcd_cells <- raster::cellFromPolygon(r_nlcd_empty, as(query_poly, "Spatial"))[[1]]
  
  nlcd_data <-
    purrr::map_dfr(nlcd_cells, get_nlcd_data) %>%
    mutate(.row = 1:nrow(.))
  
  nlcd_data <-
    nlcd_data %>%
    pivot_longer(cols = -.row, names_to = c("product", "year"), names_sep = "_") %>%
    pivot_wider(names_from = product, values_from = value) %>%
    left_join(nlcd_legend, by = c("nlcd" = "value")) %>%
    select(-nlcd) %>%
    left_join(imperviousness_legend, by = c("imperviousdescriptor" = "value")) %>%
    select(-imperviousdescriptor)
  
  road_type_percentage <- function(road_type_vector, road_type) {
    fraction_roads <- sum(road_type_vector == road_type) / length(road_type_vector)
    round(fraction_roads * 100, 0)
  }
  
  nlcd_data %>%
    select(-.row) %>%
    group_by(year) %>%
    summarize(
      impervious = round(mean(impervious), 0),
      green = round(100 * sum(green) / length(green), 0),
      primary_urban = road_type_percentage(road_type, "primary_urban"),
      primary_rural = road_type_percentage(road_type, "primary_rural"),
      secondary_urban = road_type_percentage(road_type, "secondary_urban"),
      secondary_rural = road_type_percentage(road_type, "secondary_rural"),
      tertiary_urban = road_type_percentage(road_type, "tertiary_urban"),
      tertiary_rural = road_type_percentage(road_type, "tertiary_rural"),
      thinned_urban = road_type_percentage(road_type, "thinned_urban"),
      thinned_rural = road_type_percentage(road_type, "thinned_rural"),
      nonroad_urban = road_type_percentage(road_type, "nonroad_urban"),
      nonroad_rural = road_type_percentage(road_type, "nonroad_rural"),
      energyprod_urban = road_type_percentage(road_type, "energyprod_urban"),
      energyprod_rural = road_type_percentage(road_type, "energyprod_rural"),
      nonimpervious = road_type_percentage(road_type, "non-impervious")
    )
}

## get_nlcd_percentages(d$geometry[[1]])

## TODO applying over geometry is not efficient b/c mappp converts it to a list, right?
## TODO it would better to apply over a geoid/h3/.row identifier instead (as in st_pm_hex repo)
d <- d %>%
  mutate(nlcd_data = mappp::mappp(1:nrow(d), get_nlcd_percentages))

# merge back on .row after unnesting .rows into .row
d <- d %>%
  st_drop_geometry() %>%
  unnest(cols = c(.rows))

out <- left_join(raw_data, d, by = ".row") %>% select(-.row)

# save here as nested data or flatten and save as CSV:

# either unnest everything for all years
out <- unnest(out, cols = c(nlcd_data))

# or process and select desired years
# ...

readr::write_csv(out, "test/my_address_file_geocoded_400m_buffer_nlcd.csv")