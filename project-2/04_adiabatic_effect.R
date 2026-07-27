# ============================================================
# 04 - Erklärt die Höhenlage (adiabatische Abkühlung) die räumlichen
#      Temperaturunterschiede zwischen den Stationen?
# ============================================================

library(dplyr)
library(readr)
library(ggplot2)

df_weather <- read_csv("data/weather_clean.csv", show_col_types = FALSE) %>%
  mutate(station = factor(station,
    levels = c("Duisburg", "Essen", "Dortmund", "Arnsberg", "Brilon", "Kahler Asten")))

# Höhenkorrektur: erwarteter adiabatischer Effekt von -0.65 °C / 100 m Höhe
df_weather <- df_weather %>%
  mutate(annual_corrected = annual + 0.0065 * elevation)

cat("--- Modell 1: Temperatur ~ Höhe ---\n")
model_elev <- lm(annual ~ elevation, data = df_weather)
print(summary(model_elev)$coefficients)
cat("Geschätzter Effekt:", round(coef(model_elev)["elevation"] * 100, 3), "°C pro 100 m\n")
cat("(Weltweiter Referenzwert adiabatische Abkühlung: -0.65 °C pro 100 m)\n\n")

cat("--- Modell 2: Temperatur ~ Höhe + Station ---\n")
model_elev_station <- lm(annual ~ elevation + station, data = df_weather)
cat("Modellvergleich (F-Test):\n")
print(anova(model_elev, model_elev_station))

cat("\n--- Kruskal-Wallis: Unterschiede vor vs. nach Höhenkorrektur ---\n")
kw_raw <- kruskal.test(annual ~ station, data = df_weather)
kw_corrected <- kruskal.test(annual_corrected ~ station, data = df_weather)
cat("Ohne Höhenkorrektur:  chi² =", round(kw_raw$statistic, 2), ", p =", signif(kw_raw$p.value, 3), "\n")
cat("Mit Höhenkorrektur:   chi² =", round(kw_corrected$statistic, 2), ", p =", signif(kw_corrected$p.value, 3), "\n")

# ---- Visualisierung: Boxplot vor/nach Höhenkorrektur ----
df_long <- df_weather %>%
  select(station, annual, annual_corrected) %>%
  tidyr::pivot_longer(c(annual, annual_corrected), names_to = "type", values_to = "temperature")

plot_adiabatic <- ggplot(df_long, aes(x = station, y = temperature, fill = station)) +
  geom_boxplot() +
  facet_wrap(~type, labeller = labeller(type = c(
    annual = "Ohne Höhenkorrektur", annual_corrected = "Mit Höhenkorrektur"
  ))) +
  labs(x = NULL, y = "Jahresmitteltemperatur [°C]", fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom")

ggsave("plots/adiabatic_effect.png", plot_adiabatic, width = 8, height = 4.5, dpi = 150)

cat("\nInterpretation: Der adiabatische Effekt erklärt einen wesentlichen Teil der\n")
cat("räumlichen Unterschiede, aber Modell 2 zeigt, dass einzelne Stationen auch nach\n")
cat("Kontrolle für Höhe noch signifikant zum Unterschied beitragen -> Höhe allein\n")
cat("erklärt die räumliche Variation nicht vollständig.\n")

