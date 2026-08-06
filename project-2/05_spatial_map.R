# ============================================================
# 05 - Spatial visualization: station map of NRW
# Shows annual mean temperature (color) and elevation (size) per station
#
# Requires live internet access:
# - geodata::gadm()   downloads the federal-state geometry (GADM)
# - osmdata::opq()    queries the Ruhr river course via the Overpass API
# ============================================================

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(geodata)
library(osmdata)
library(ggrepel)

df_weather <- read_csv("data/weather_clean.csv", show_col_types = FALSE)

# ---- Station data: last year with complete data for all 6 stations ----
last_complete_year <- df_weather %>%
  group_by(year) %>%
  filter(n() == 6, all(!is.na(annual))) %>%
  ungroup() %>%
  summarise(max(year)) %>%
  pull()

station_data <- tibble::tribble(
  ~station,        ~lon,  ~lat,   ~elevation,
  "Duisburg",      6.76,  51.43,  31,
  "Essen",         7.01,  51.45,  150,
  "Dortmund",      7.46,  51.51,  120,
  "Arnsberg",      8.08,  51.40,  218,
  "Brilon",        8.57,  51.39,  472,
  "Kahler Asten",  8.49,  51.18,  839
) %>%
  left_join(
    df_weather %>% filter(year == last_complete_year) %>% select(station, annual_mean = annual),
    by = "station"
  )

station_data_sf <- st_as_sf(station_data, coords = c("lon", "lat"), crs = 4326)

# ---- Federal-state geometry (NRW) ----
germany_states <- gadm(country = "DEU", level = 1, path = tempdir())
germany_states_sf <- st_as_sf(germany_states)
nrw <- germany_states_sf %>% filter(NAME_1 == "Nordrhein-Westfalen")

# ---- Ruhr river course ----
q <- opq(bbox = c(6.65, 50.95, 8.75, 51.75), timeout = 120) %>%
  add_osm_feature(key = "waterway", value = c("river", "stream", "canal")) %>%
  add_osm_feature(key = "name", value = "Ruhr")

osm_rivers <- osmdata_sf(q)
ruhr <- osm_rivers$osm_lines %>% filter(grepl("Ruhr", name, ignore.case = TRUE))

# ---- Map ----
map_plot <- ggplot() +
  geom_sf(data = nrw, fill = "grey95", color = "grey60") +
  geom_sf(data = ruhr, color = "#5DA5DA", linewidth = 1) +
  geom_sf(data = station_data_sf, aes(color = annual_mean, size = elevation)) +
  geom_text_repel(
    data = station_data_sf,
    aes(x = st_coordinates(station_data_sf)[, 1],
        y = st_coordinates(station_data_sf)[, 2],
        label = station),
    size = 4.5,
    point.padding = 0.3,
    box.padding = 0.5,
    nudge_y = 0.2,
    nudge_x = -0.1,
    min.segment.length = 0
  ) +
  coord_sf(xlim = c(5.8, 9.4), ylim = c(50.3, 52.5)) +
  scale_size_area(max_size = 12) +
  scale_color_viridis_c() +
  labs(
    color = "Annual mean temperature [°C]",
    size = "Elevation [m above sea level]"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 11)
  )

map_plot

ggsave("plots/station_map.png", map_plot, width = 8, height = 6, dpi = 150, bg = "white")
