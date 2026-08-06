# ============================================================
# Classification model explaining mobility choices
# – Data preparation for Case Studies I
# Source: https://ess.sikt.no/en/study/dad96456-2ab4-42e3-8272-166bf5749bf9/
#
# 01_data_cleaning.R
# ============================================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

col_main <- "#1a6faf"
col_alt <- "#C0392B"

## 1. Data import, level of measurement, cleaning, target distribution

## Overview
## X:         Index
## w6sgq4:    Main mode of transport on a typical day (NA code: 9)
## cntry:     Country
## w6age:     Age (NA code: 999)
## gndr:      Gender (NA code: 9)
## eisced:    Highest level of education, ES - ISCED (NA code: 99)
## mnactic:   Main activity, last 7 days. All respondents. Post coded
##            (NA code: 99)
## hinctnta:  Household's total net income, all sources (NA code: 99)
## netusoft:  Internet use, how often (NA code: 9)
## hincfel:   Feeling about household's income nowadays (NA code: 9)
## w6sgq11:   How worried about climate change (NA code: 9)
## w6sgq12:   To what extent feel personal responsibility to reduce climate
##            change (NA code: 99)
## w6sgq5:    Satisfaction with public transport infrastructure in local area
##            (NA code: 99)
## w6wq2:     Feel people in local area help one another (NA code: 99)
## w6wq8:     Feel safe and secure in your life (NA code: 99)
## w6sgq22:   Concern that [country] government's climate change policies might
##            increase transportation costs (NA code: 9)

## Import data

raw <- read.csv("/Users/jakubmarczak/Downloads/case-studies-1/project-5/data/CRON3w6e01_selection.csv")

raw <- raw |>
  dplyr::select(-1)

## Missing-value codes per variable, according to the codebook
## Each entry lists the codes that the codebook flags as refusal, "don't
## know", or "no answer"
## eisced additionally contains code 55 "other", which does not fit on the
## ordinal scale

missing_codes <- list(
  w6age    = c(999),
  gndr     = c(9),
  eisced   = c(55, 77, 88, 99),
  mnactic  = c(66, 77, 88, 99),
  netusoft = c(7, 8, 9),
  hincfel  = c(7, 8, 9),
  w6sgq11  = c(9),
  w6sgq12  = c(99),
  w6sgq5   = c(99),
  w6wq2    = c(99),
  w6wq8    = c(99),
  w6sgq22  = c(9)
)

## Cleaning of the target variable
## Code 6 "no regular/daily commute" and code 7 "other" for w6sgq4 are
## excluded, since these respondents either made no mode-of-transport
## choice or chose an unspecific category. Code 9 "no answer" is treated
## as standard missing. This reduces the target variable to a clean
## k = 5 class problem.

n_raw <- nrow(raw)

dat <- raw %>%
  filter(w6sgq4 %in% 1:5)

n_after_target <- nrow(dat)

## n raw = 9585, n after filtering the target variable = 9001
## 584 respondents excluded due to "no regular commute", "other", or
## "no answer" on the target variable

dat <- dat %>%
  mutate(hinctnta = if_else(hinctnta %in% c(77, 88, 99), NA_integer_, hinctnta))

dat <- dat %>%
  mutate(
    w6age    = na_if(w6age, 999),
    gndr     = if_else(gndr %in% missing_codes$gndr, NA_integer_, gndr),
    eisced   = if_else(eisced %in% missing_codes$eisced, NA_integer_, eisced),
    mnactic  = if_else(mnactic %in% missing_codes$mnactic, NA_integer_, mnactic),
    netusoft = if_else(netusoft %in% missing_codes$netusoft, NA_integer_, netusoft),
    hincfel  = if_else(hincfel %in% missing_codes$hincfel, NA_integer_, hincfel),
    w6sgq11  = if_else(w6sgq11 %in% missing_codes$w6sgq11, NA_integer_, w6sgq11),
    w6sgq12  = if_else(w6sgq12 %in% missing_codes$w6sgq12, NA_integer_, w6sgq12),
    w6sgq5   = if_else(w6sgq5 %in% missing_codes$w6sgq5, NA_integer_, w6sgq5),
    w6wq2    = if_else(w6wq2 %in% missing_codes$w6wq2, NA_integer_, w6wq2),
    w6wq8    = if_else(w6wq8 %in% missing_codes$w6wq8, NA_integer_, w6wq8),
    w6sgq22  = if_else(w6sgq22 %in% missing_codes$w6sgq22, NA_integer_, w6sgq22)
  )

miss_table <- dat %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(100 * n_missing / nrow(dat), 1)) %>%
  arrange(desc(n_missing))

print(miss_table)


dat_final <- dat %>%
  tidyr::drop_na()

n_final <- nrow(dat_final)

## n after target-variable filter = 9001, final n after complete-case analysis = 7330

## Set levels of measurement
dat_final <- dat_final %>%
  mutate(
    w6sgq4 = factor(w6sgq4,
      levels = 1:5,
      labels = c(
        "walking", "bicycle", "public_transport",
        "gasoline_car", "electric_car"
      )
    ),
    cntry = factor(cntry),
    gndr = factor(gndr, levels = c(1, 2), labels = c("male", "female")),
    mnactic = factor(mnactic,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        "paid_work", "education", "unemployed_looking",
        "unemployed_not_looking", "sick_disabled", "retired",
        "community_military", "housework", "other"
      )
    ),
    eisced = ordered(eisced, levels = 1:7),
    hinctnta = ordered(
      hinctnta,
      levels = 1:10,
      labels = paste0("decile_", 1:10)
    ),
    netusoft = ordered(netusoft, levels = 1:5),
    hincfel = ordered(hincfel, levels = 1:4)
  )

## Target distribution after cleaning

target_dist <- dat_final %>%
  count(w6sgq4) %>%
  mutate(pct = round(100 * n / sum(n), 1))

print(target_dist)

## The cleaned target variable is strongly imbalanced. gasoline_car accounts
## for roughly half the sample (49.7 %), while bicycle and public_transport
## are the smallest classes (8.5 % and 12.2 % respectively).

p_target <- ggplot(target_dist, aes(x = w6sgq4, y = n)) +
  geom_col(fill = col_main) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, size = 3.5) +
  labs(x = "Main mode of transport", y = "Number of respondents") +
  theme_minimal() +
  theme(
    text = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    panel.grid = element_blank(),
    axis.line.y = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 8))
  )

p_target

## Save the cleaned dataset for the subsequent tasks

write_csv(dat_final, "thema5_cleaned.csv")
