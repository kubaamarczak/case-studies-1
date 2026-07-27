# ============================================================
# 01 - Datenimport & Aufbereitung
# Quelle: European Climate Assessment & Dataset (ECA&D)
#         https://www.ecad.eu/download/millennium/millennium.php
# 6 Stationen im Ruhrgebiet / Sauerland
# ============================================================

library(dplyr)
library(readr)

stations <- c(
  duisburg     = "data/indexTG_004030.txt",
  essen        = "data/indexTG_004074.txt",
  dortmund     = "data/indexTG_004021.txt",
  arnsberg     = "data/indexTG_004172.txt",
  brilon       = "data/indexTG_004897.txt",
  kahler_asten = "data/indexTG_000812.txt"
)

col_names <- c(
  "souid", "year", "annual", "winter_half_year", "summer_half_year",
  "winter_djf", "spring_mam", "summer_jja", "autumn_son",
  "january", "february", "march", "april", "may", "june",
  "july", "august", "september", "october", "november", "december"
)

# Höhenlage jeder Station in m ü. NHN (für die Analyse des adiabatischen Effekts)
station_labels <- c("Duisburg", "Essen", "Dortmund", "Arnsberg", "Brilon", "Kahler Asten")
elevation_m <- setNames(c(31, 150, 120, 218, 472, 839), station_labels)

#' Liest eine ECA&D-Stationsdatei ein und bringt sie in ein einheitliches Format.
#'
#' Die Rohdaten liegen als 1/100 °C vor und kennzeichnen fehlende Werte mit -9999.99.
read_station <- function(path) {
  read.table(path, skip = 32, header = FALSE, col.names = col_names) %>%
    mutate(across(-c(souid, year), ~ na_if(as.double(.x) / 100, -9999.99)))
}

df_weather <- lapply(stations, read_station) %>%
  bind_rows(.id = "station") %>%
  mutate(
    station = factor(station, levels = names(stations), labels = station_labels),
    elevation = elevation_m[as.character(station)]
  )

dir.create("data", showWarnings = FALSE)
write_csv(df_weather, "data/weather_clean.csv")

cat("Aufbereitete Daten gespeichert unter data/weather_clean.csv\n")
cat("Stationen:", paste(levels(df_weather$station), collapse = ", "), "\n")
cat("Zeitraum:", min(df_weather$year), "-", max(df_weather$year), "\n")

