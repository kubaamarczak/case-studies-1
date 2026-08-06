# ============================================================
# Classification model explaining mobility choices
# – Exploratory data analysis for Case Studies I
#
# 02_eda.R
# Requires: 01_data_cleaning.R must be run first
# ============================================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

col_main <- "#1a6faf"
col_alt  <- "#C0392B"

## -----------------------------------------------------------------------------

## 2. Variable screening and descriptive relationships with the target variable

## Read in the cleaned dataset from step 1
## csv does not preserve factor/ordered classes, so the types are restored
## here

dat <- read_csv("thema5_cleaned.csv", show_col_types = FALSE) %>%
  mutate(
    w6sgq4   = factor(w6sgq4, levels = c("walking", "bicycle", "public_transport",
                                         "gasoline_car", "electric_car")),
    cntry    = factor(cntry),
    gndr     = factor(gndr),
    mnactic  = factor(mnactic),
    eisced   = ordered(eisced, levels = sort(unique(eisced))),
    hinctnta = ordered(hinctnta, levels = sort(unique(hinctnta))),
    netusoft = ordered(netusoft, levels = sort(unique(netusoft))),
    hincfel  = ordered(hincfel, levels = sort(unique(hincfel)))
  )

## Variable groups according to the level of measurement from step 1

vars_nominal  <- c("cntry", "gndr", "mnactic")
vars_ordinal  <- c("eisced", "netusoft", "hincfel", "w6sgq11", "w6sgq22", "hinctnta")
vars_metric   <- c("w6age", "w6sgq12", "w6sgq5", "w6wq2", "w6wq8")

## Helper function for Cramer's V
## serves as an effect size for the association between two categorical
## variables. V close to 0 means no association, V close to 1 a strong
## association

cramers_v <- function(x, y) {
  tab <- table(x, y)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  n   <- sum(tab)
  k   <- min(nrow(tab), ncol(tab))
  sqrt(as.numeric(chi$statistic) / (n * (k - 1)))
}

## Categorical and ordinal predictors against the target variable
## The chi-squared test checks whether the distribution of the predictor
## differs across the five mode-of-transport classes; Cramer's V quantifies
## the strength
## H0: Predictor and mode-of-transport choice are independent
## H1: Predictor and mode-of-transport choice are not independent

cat_results <- lapply(c(vars_nominal, vars_ordinal), function(v) {
  tab  <- table(dat[[v]], dat$w6sgq4)
  test <- suppressWarnings(chisq.test(tab, correct = FALSE))
  data.frame(
    variable  = v,
    type      = if (v %in% vars_nominal) "nominal" else "ordinal",
    statistic = round(as.numeric(test$statistic), 1),
    df        = test$parameter,
    p_value   = signif(test$p.value, 3),
    cramers_v = round(cramers_v(dat[[v]], dat$w6sgq4), 3)
  )
}) %>%
  bind_rows() %>%
  arrange(desc(cramers_v))

print(cat_results)

## Metric and quasi-metric predictors against the target variable
## The Kruskal-Wallis test checks whether the five mode-of-transport groups
## differ in location; epsilon-squared is reported as the effect measure
## H0: The distributions/medians do not differ between the groups
## H1: The distributions/medians differ between the groups

eps_squared <- function(h, n, k) {
  (h - k + 1) / (n - k)
}

metric_results <- lapply(vars_metric, function(v) {
  kw <- kruskal.test(dat[[v]], dat$w6sgq4)
  n  <- sum(!is.na(dat[[v]]))
  k  <- n_distinct(dat$w6sgq4)
  data.frame(
    variable    = v,
    type        = "metric / quasi-metric",
    statistic   = round(as.numeric(kw$statistic), 1),
    df          = kw$parameter,
    p_value     = signif(kw$p.value, 3),
    eps_squared = round(eps_squared(as.numeric(kw$statistic), n, k), 3)
  )
}) %>%
  bind_rows() %>%
  arrange(desc(eps_squared))

print(metric_results)

## Combined ranking of all 14 covariates
## Cramer's V and epsilon-squared are both bounded effect sizes on a
## similar 0-to-1 scale, so they are combined here for an overall overview,
## not as a single test

screening_table <- bind_rows(
  cat_results %>% transmute(variable, type, effect_size = cramers_v, p_value),
  metric_results %>% transmute(variable, type, effect_size = eps_squared, p_value)
) %>%
  arrange(desc(effect_size))

print(screening_table)

## Descriptive plots

plot_stacked <- function(var) {
  ggplot(dat, aes(x = .data[[var]], fill = w6sgq4)) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("#1a6faf", "#5aa9d6", "#a4c8e1",
                                 "#C0392B", "#e08a7c")) +
    labs(x = var, y = "Share", fill = "Mode of transport") +
    scale_x_discrete(
      limits = levels(var)
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_stacked <- function(var) {

  lev <- dat %>%
    count(.data[[var]], w6sgq4) %>%
    group_by(.data[[var]]) %>%
    mutate(prop = n / sum(n)) %>%
    filter(w6sgq4 %in% c("walking","bicycle","public_transport")) %>%
    summarise(score = sum(prop), .groups = "drop") %>%
    arrange(desc(score)) %>%
    pull(.data[[var]])

  ggplot(dat,
         aes(x = factor(.data[[var]], levels = lev),
             fill = w6sgq4)) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("#1a6faf", "#5aa9d6", "#a4c8e1",
                                 "#C0392B", "#e08a7c"),
                      labels = c(
                        walking = "Walking",
                        bicycle = "Bicycle",
                        public_transport = "Public transport",
                        gasoline_car = "Gasoline/\ndiesel car",
                        electric_car = "Electric/\nhybrid car"

                      )) +
    labs(x = "Country", y = "Share", fill = "Mode of transport") +
    theme_minimal() +
    theme(
          text = element_text(size = 9),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          axis.text = element_text(size = 8),
          panel.grid = element_blank(),
          legend.key.height = unit(0.7, "cm"),
          legend.spacing.y = unit(0.2, "cm"))
}

p_cntry    <- plot_stacked("cntry")
p_eisced   <- plot_stacked("eisced")
p_mnactic  <- plot_stacked("mnactic")
p_hinctnta <- plot_stacked("hinctnta")

p_cntry

#library(tikzDevice)
#tikz("stackedcountry.tex", width = 4.5, height = 2)
#p_cntry
#dev.off()

p_eisced
p_mnactic
p_hinctnta


plot_box <- function(var) {
  ggplot(dat, aes(x = w6sgq4, y = .data[[var]], fill = w6sgq4)) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_manual(values = rep(col_main, 5)) +
    labs(x = NULL, y = var) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

p_age   <- plot_box("w6age")
p_sgq12 <- plot_box("w6sgq12")
p_sgq5  <- plot_box("w6sgq5")
p_wq2   <- plot_box("w6wq2")
p_wq8   <- plot_box("w6wq8")

p_metric_panel <- (p_age | p_sgq12 | p_sgq5) / (p_wq2 | p_wq8 | plot_spacer())

p_metric_panel

## Multicollinearity screening among the metric/quasi-metric predictors
## Spearman correlation is used, since the four Likert items are ordinal
## in nature. Strong pairwise correlations would be relevant for the
## assumption checks in step 4.

cor_metric <- dat %>%
  select(all_of(vars_metric)) %>%
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  round(2)

print(cor_metric)

## All pairwise correlations among the metric/quasi-metric predictors are
## below 0.4. No pair shows a strong association, so multicollinearity is
## not a concern for now.
