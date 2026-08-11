# ============================================================
# 03 - Hypothesis tests
# (a) Temporal analysis: Has the climate in Essen warmed?
# (b) Spatial analysis: Do the 6 locations differ?
# (c) Discussion of multiple testing
# ============================================================

library(dplyr)
library(readr)

df_weather <- read_csv("data/weather_clean.csv", show_col_types = FALSE) %>%
  mutate(station = factor(station,
    levels = c("Duisburg", "Essen", "Dortmund", "Arnsberg", "Brilon", "Kahler Asten")
  ))

# ------------------------------------------------------------
# (a) Temporal comparison using Essen as an example
# Comparison periods: 1950-1980 (start of industrialization/economic miracle)
# vs. 1990-2020 (modern era)
# ------------------------------------------------------------
essen_early <- df_weather %>%
  filter(station == "Essen", between(year, 1950, 1980)) %>%
  pull(annual)
essen_late <- df_weather %>%
  filter(station == "Essen", between(year, 1990, 2020)) %>%
  pull(annual)

cat("\n--- (a) Temporal comparison: Essen 1950-1980 vs. 1990-2020 ---\n")
cat("Mean 1950-1980:", round(mean(essen_early, na.rm = TRUE), 2), "°C\n")
cat("Mean 1990-2020:", round(mean(essen_late, na.rm = TRUE), 2), "°C\n")

# Check assumption: homogeneity of variance
f_test <- var.test(essen_early, essen_late, alternative = "two.sided")
cat("F-test for homogeneity of variance: p =", signif(f_test$p.value, 3), "\n")

# -> depending on the result, Welch's t-test (does not assume equal variances)
# H0: mu_early >= mu_late  vs.  H1: mu_early < mu_late (one-sided: warming)
welch_test <- t.test(essen_early, essen_late, alternative = "less")
cat("Welch's t-test (H1: warming): p =", signif(welch_test$p.value, 3), "\n")

# Effect size: Cohen's d (pooled SD, used here as a supplement to the Welch test)
cohens_d <- function(x, y) {
  nx <- sum(!is.na(x))
  ny <- sum(!is.na(y))
  pooled_sd <- sqrt(((nx - 1) * var(x, na.rm = TRUE) + (ny - 1) * 
                       var(y, na.rm = TRUE)) / (nx + ny - 2))
  (mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) / pooled_sd
}
d <- cohens_d(essen_early, essen_late)
cat("Cohen's d:", round(d, 2), "\n")

# ------------------------------------------------------------
# (b) Spatial comparison of the 6 locations
# Period 1950-1995: the only period with complete data at all stations
# ------------------------------------------------------------
df_spatial <- df_weather %>%
  filter(between(year, 1950, 1995)) %>%
  group_by(year) %>%
  filter(n() == 6, all(!is.na(annual))) %>%
  ungroup()

cat("\n--- (b) Spatial comparison: 6 locations, 1950-1995 ---\n")
cat("n years with complete data at all stations:", n_distinct(df_spatial$year), "\n")

# Kruskal-Wallis as a robust alternative to ANOVA (group comparison, no strict
# normality assumption required)
kw_test <- kruskal.test(annual ~ station, data = df_spatial)
cat(
  "Kruskal-Wallis test: chi² =", round(kw_test$statistic, 2),
  ", df =", kw_test$parameter, ", p =", signif(kw_test$p.value, 3), "\n"
)

# Effect size: epsilon-squared
n_obs <- sum(!is.na(df_spatial$annual))
k_groups <- n_distinct(df_spatial$station)
epsilon_sq <- (kw_test$statistic - k_groups + 1) / (n_obs - k_groups)
cat("Epsilon-squared (effect size):", round(epsilon_sq, 3), "\n")

# Pairwise Wilcoxon test with Holm correction 
# (which stations differ specifically?)
pairwise_result <- pairwise.wilcox.test(df_spatial$annual, df_spatial$station, 
                                        p.adjust.method = "holm")
cat("\nPairwise Wilcoxon test (Holm-adjusted p-values):\n")
print(round(pairwise_result$p.value, 4))

# ------------------------------------------------------------
# (c) Multiple testing
# ------------------------------------------------------------
cat("\n--- (c) Multiple testing ---\n")
cat("The pairwise Wilcoxon test (15 comparisons for 6 stations) already adjusts the\n")
cat("p-values using the Holm method to control the inflation of the type I error.\n")
cat("Tests for different hypotheses (temporal vs. spatial), on the other hand,\n")
cat("are considered independently, since these are separate research questions.\n")

# Save results for README/report
results <- list(
  essen_mean_early = mean(essen_early, na.rm = TRUE),
  essen_mean_late = mean(essen_late, na.rm = TRUE),
  welch_p = welch_test$p.value,
  cohens_d = d,
  kruskal_p = kw_test$p.value,
  epsilon_sq = epsilon_sq
)
saveRDS(results, "data/hypothesis_test_results.rds")
