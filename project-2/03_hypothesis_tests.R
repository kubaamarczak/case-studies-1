# ============================================================
# 03 - Hypothesentests
# (a) Zeitliche Analyse: Hat sich das Klima in Essen erwärmt?
# (b) Räumliche Analyse: Unterscheiden sich die 6 Standorte?
# (c) Diskussion multiples Testen
# ============================================================

library(dplyr)
library(readr)

df_weather <- read_csv("data/weather_clean.csv", show_col_types = FALSE) %>%
  mutate(station = factor(station,
    levels = c("Duisburg", "Essen", "Dortmund", "Arnsberg", "Brilon", "Kahler Asten")))

# ------------------------------------------------------------
# (a) Zeitlicher Vergleich am Beispiel Essen
# Vergleichszeiträume: 1950-1980 (Beginn Industrialisierung/Wirtschaftswunder)
# vs. 1990-2020 (moderne Zeit)
# ------------------------------------------------------------
essen_early <- df_weather %>% filter(station == "Essen", between(year, 1950, 1980)) %>% pull(annual)
essen_late  <- df_weather %>% filter(station == "Essen", between(year, 1990, 2020)) %>% pull(annual)

cat("\n--- (a) Zeitlicher Vergleich: Essen 1950-1980 vs. 1990-2020 ---\n")
cat("Mittelwert 1950-1980:", round(mean(essen_early, na.rm = TRUE), 2), "°C\n")
cat("Mittelwert 1990-2020:", round(mean(essen_late, na.rm = TRUE), 2), "°C\n")

# Voraussetzung prüfen: Varianzhomogenität
f_test <- var.test(essen_early, essen_late, alternative = "two.sided")
cat("F-Test auf Varianzhomogenität: p =", signif(f_test$p.value, 3), "\n")

# -> je nach Ergebnis Welch-t-Test (keine Varianzhomogenität vorausgesetzt)
# H0: mu_early >= mu_late  vs.  H1: mu_early < mu_late (einseitig: Erwärmung)
welch_test <- t.test(essen_early, essen_late, alternative = "less")
cat("Welch t-Test (H1: Erwärmung): p =", signif(welch_test$p.value, 3), "\n")

# Effektstärke: Cohen's d (gepoolte SD, da hier als Ergänzung zum Welch-Test betrachtet)
cohens_d <- function(x, y) {
  nx <- sum(!is.na(x)); ny <- sum(!is.na(y))
  pooled_sd <- sqrt(((nx - 1) * var(x, na.rm = TRUE) + (ny - 1) * var(y, na.rm = TRUE)) / (nx + ny - 2))
  (mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) / pooled_sd
}
d <- cohens_d(essen_early, essen_late)
cat("Cohen's d:", round(d, 2), "\n")

# ------------------------------------------------------------
# (b) Räumlicher Vergleich der 6 Standorte
# Zeitraum 1950-1995: einzige Periode mit vollständigen Daten an allen Stationen
# ------------------------------------------------------------
df_spatial <- df_weather %>%
  filter(between(year, 1950, 1995)) %>%
  group_by(year) %>%
  filter(n() == 6, all(!is.na(annual))) %>%
  ungroup()

cat("\n--- (b) Räumlicher Vergleich: 6 Standorte, 1950-1995 ---\n")
cat("n Jahre mit vollständigen Daten an allen Stationen:", n_distinct(df_spatial$year), "\n")

# Kruskal-Wallis als robuste Alternative zur ANOVA (Gruppenvergleich, keine strikte
# Normalverteilungsannahme nötig)
kw_test <- kruskal.test(annual ~ station, data = df_spatial)
cat("Kruskal-Wallis-Test: chi² =", round(kw_test$statistic, 2),
    ", df =", kw_test$parameter, ", p =", signif(kw_test$p.value, 3), "\n")

# Effektstärke: Epsilon-Quadrat
n_obs <- sum(!is.na(df_spatial$annual))
k_groups <- n_distinct(df_spatial$station)
epsilon_sq <- (kw_test$statistic - k_groups + 1) / (n_obs - k_groups)
cat("Epsilon-Quadrat (Effektstärke):", round(epsilon_sq, 3), "\n")

# Paarweiser Wilcoxon-Test mit Holm-Korrektur (welche Stationen unterscheiden sich konkret?)
pairwise_result <- pairwise.wilcox.test(df_spatial$annual, df_spatial$station, p.adjust.method = "holm")
cat("\nPaarweiser Wilcoxon-Test (Holm-adjustierte p-Werte):\n")
print(round(pairwise_result$p.value, 4))

# ------------------------------------------------------------
# (c) Multiples Testen
# ------------------------------------------------------------
cat("\n--- (c) Multiples Testen ---\n")
cat("Der paarweise Wilcoxon-Test (15 Vergleiche für 6 Stationen) adjustiert die\n")
cat("p-Werte bereits mit der Holm-Methode, um die Inflation des Fehlers 1. Art zu\n")
cat("kontrollieren. Tests zu unterschiedlichen Hypothesen (zeitlich vs. räumlich)\n")
cat("werden hingegen unabhängig betrachtet, da es sich um separate Fragestellungen handelt.\n")

# Ergebnisse für README/Bericht sichern
results <- list(
  essen_mean_early = mean(essen_early, na.rm = TRUE),
  essen_mean_late = mean(essen_late, na.rm = TRUE),
  welch_p = welch_test$p.value,
  cohens_d = d,
  kruskal_p = kw_test$p.value,
  epsilon_sq = epsilon_sq
)
saveRDS(results, "data/hypothesis_test_results.rds")
