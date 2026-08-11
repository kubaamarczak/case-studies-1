# ============================================================
# 04 - Does elevation (adiabatic cooling) explain the spatial
#      temperature differences between the stations?
# ============================================================

library(dplyr)
library(readr)
library(ggplot2)

df_weather <- read_csv("data/weather_clean.csv", show_col_types = FALSE) %>%
  mutate(station = factor(station,
    levels = c("Duisburg", "Essen", "Dortmund", "Arnsberg", "Brilon", "Kahler Asten")
  ))

# Elevation correction: expected adiabatic effect of -0.65 °C / 100 m of elevation
df_weather <- df_weather %>%
  mutate(annual_corrected = annual + 0.0065 * elevation)

cat("--- Model 1: temperature ~ elevation ---\n")
model_elev <- lm(annual ~ elevation, data = df_weather)
print(summary(model_elev)$coefficients)
cat("Estimated effect:", round(coef(model_elev)["elevation"] * 100, 3), "°C per 100 m\n")
cat("(Global reference value for adiabatic cooling: -0.65 °C per 100 m)\n\n")

cat("--- Model 2: temperature ~ elevation + station ---\n")
model_elev_station <- lm(annual ~ elevation + station, data = df_weather)
cat("Model comparison (F-test):\n")
print(anova(model_elev, model_elev_station))

cat("\n--- Kruskal-Wallis: differences before vs. after elevation correction ---\n")
kw_raw <- kruskal.test(annual ~ station, data = df_weather)
kw_corrected <- kruskal.test(annual_corrected ~ station, data = df_weather)
cat("Without elevation correction: chi² =", round(kw_raw$statistic, 2), 
    ", p =", signif(kw_raw$p.value, 3), "\n")
cat("With elevation correction:    chi² =", round(kw_corrected$statistic, 2), 
    ", p =", signif(kw_corrected$p.value, 3), "\n")

# ---- Visualization: boxplot before/after elevation correction ----
df_long <- df_weather %>%
  select(station, annual, annual_corrected) %>%
  tidyr::pivot_longer(c(annual, annual_corrected), names_to = "type", values_to = "temperature")

plot_adiabatic <- ggplot(df_long, aes(x = station, y = temperature, fill = station)) +
  geom_boxplot() +
  facet_wrap(~type, labeller = labeller(type = c(
    annual = "Without elevation correction", annual_corrected = "With elevation correction"
  ))) +
  labs(x = NULL, y = "Annual mean temperature [°C]", fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom")

ggsave("plots/adiabatic_effect.png", plot_adiabatic, 
       width = 8, height = 4.5, dpi = 150, bg = "white")

cat("\nInterpretation: The adiabatic effect explains a substantial part of the\n")
cat("spatial differences, but Model 2 shows that individual stations still\n")
cat("contribute significantly to the difference even after controlling for\n")
cat("elevation -> elevation alone does not fully explain the spatial variation.\n")
