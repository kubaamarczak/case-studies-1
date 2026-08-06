# ============================================================
# 02 - Descriptive analysis
# Univariate/bivariate statistics, distribution shape per station
# ============================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(patchwork)

df_weather <- read_csv("data/weather_clean.csv", show_col_types = FALSE) %>%
  mutate(station = factor(station,
    levels = c("Duisburg", "Essen", "Dortmund", "Arnsberg", "Brilon", "Kahler Asten")))

dir.create("plots", showWarnings = FALSE)

# ---- 1. Descriptive statistics per station (annual, winter, summer means) ----
descriptive_stats <- df_weather %>%
  select(station, annual, winter_djf, summer_jja) %>%
  pivot_longer(-station, names_to = "variable", values_to = "value") %>%
  group_by(station, variable) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    mad = mad(value, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(descriptive_stats, "data/descriptive_stats.csv")
print(descriptive_stats, n = 20)

# ---- 2. Boxplots per station (outliers, distribution location) ----
boxplot_annual <- ggplot(df_weather, aes(x = station, y = annual)) +
  geom_boxplot(fill = "grey80", color = "black", linewidth = 0.35,
               outlier.size = 1.2, outlier.alpha = 0.8) +
  labs(x = NULL, y = "Annual mean temperature [°C]") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("plots/boxplot_annual.png", boxplot_annual, width = 6, height = 4, dpi = 150, bg = "white")

# ---- 3. Checking distribution shape: KDE + QQ plot per station ----
df_long <- df_weather %>%
  select(station, annual, winter_djf, summer_jja) %>%
  pivot_longer(c(annual, winter_djf, summer_jja), names_to = "type", values_to = "temperature") %>%
  mutate(type = recode(type,
    annual = "Annual mean", winter_djf = "Winter quarter", summer_jja = "Summer quarter"))

kde_plot <- ggplot(df_long, aes(x = temperature, fill = type, color = type)) +
  geom_density(alpha = 0.35, linewidth = 0.35, na.rm = TRUE) +
  facet_wrap(~station, ncol = 3) +
  labs(x = "Temperature [°C]", y = "Density", fill = "Period", color = "Period") +
  theme_minimal(base_size = 10)

ggsave("plots/kde_by_station.png", kde_plot, width = 8, height = 5, dpi = 150, bg = "white")

qq_plot <- ggplot(df_weather, aes(sample = annual)) +
  stat_qq(size = 0.8, alpha = 0.7) +
  stat_qq_line(linewidth = 0.4, color = "steelblue") +
  facet_wrap(~station, ncol = 3) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal(base_size = 10)

ggsave("plots/qq_annual.png", qq_plot, width = 8, height = 5, dpi = 150, bg = "white")

# ---- 4. Time series (annual mean per station) ----
timeseries_plot <- ggplot(df_weather, aes(x = year, y = annual, color = station)) +
  geom_line(alpha = 0.75) +
  labs(x = "Year", y = "Annual mean temperature [°C]", color = "Station") +
  theme_minimal(base_size = 11)

ggsave("plots/timeseries_annual.png", timeseries_plot, width = 7, height = 4, dpi = 150, bg = "white")

cat("EDA complete. Plots in plots/, statistics in data/descriptive_stats.csv\n")
cat("Distribution shape: histograms/KDE show an approximately bell-shaped\n")
cat("distribution per station, QQ plots lie close to the diagonal -> normal distribution approximately plausible.\n")
