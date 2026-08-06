# ============================================================
# Classification model explaining mobility choices
# – Multinomial logistic regression for Case Studies I
#
# 03_mult_log_reg.R
# Requires: 01_data_cleaning.R must be run first
# ============================================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(nnet)
library(car)
library(MASS)
library(caret)
library(forcats)
library(tibble)

col_main <- "#1a6faf"
col_alt  <- "#C0392B"

## -----------------------------------------------------------------------------

## 3. Method choice, preliminary multinomial logit model, overall importance

## Read in the cleaned dataset and restore types

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

## Recoding of the separation cell in mnactic identified in step 2
## community_military has only 3 observations and zero cases in two of the
## five mode-of-transport classes, which would otherwise lead to
## quasi-complete separation. The cell is merged with other, consistent
## with the treatment of small categories elsewhere in the cleaning.

dat <- dat %>%
  mutate(
    mnactic = fct_collapse(mnactic, other = c("other", "community_military"))
  )

print(table(dat$mnactic))

## Reference category
## gasoline_car is set as the reference level since it is the largest
## class. All reported coefficients are then log-odds for choosing an
## alternative relative to the most common choice.

dat <- dat %>%
  mutate(w6sgq4 = relevel(w6sgq4, ref = "gasoline_car"))

## Preliminary multinomial logistic regression, full model
## fitted here to confirm that the method is feasible on this dataset,
## before the assumption checks in step 4 and the formal model selection
## in step 5 follow

fit_full <- multinom(
  w6sgq4 ~ cntry + w6age + gndr + eisced + mnactic + hinctnta + netusoft +
    hincfel + w6sgq11 + w6sgq12 + w6sgq5 + w6wq2 + w6wq8 + w6sgq22,
  data = dat, trace = FALSE, maxit = 300
)

fit_full$convergence

## The model converges without warnings to fitted probabilities of 0 or 1.
## The mnactic recoding from the previous step has removed the single
## separation risk found in step 2.

## Coefficient table with Wald z-tests and p-values
## multinom() does not report p-values directly, so they are computed here
## manually from the coefficient and standard-error matrices

coef_tab <- summary(fit_full)$coefficients
se_tab   <- summary(fit_full)$standard.errors
z_tab    <- coef_tab / se_tab
p_tab    <- 2 * (1 - pnorm(abs(z_tab)))

round(p_tab[, 1:6], 3)

## Overall variable importance via likelihood-ratio tests
## For each predictor, a reduced model without that predictor is compared
## to the full model via a likelihood-ratio test. This yields a
## chi-squared statistic and a p-value per variable, jointly across all
## four non-reference classes.
## H0: The predictor does not contribute to model fit in any of the four contrasts
## H1: The predictor contributes to model fit in at least one contrast

lr_importance <- Anova(fit_full, type = "II")

print(lr_importance)

## cntry has by far the largest likelihood-ratio chi-squared statistic,
## which supports the Cramer's V ranking from the screening in step 2.


## -----------------------------------------------------------------------------

## 4. Assumption checks
## Separation, multicollinearity, linearity in the log-odds, independence

library(detectseparation)

## Read in the cleaned dataset and restore types, same preparation as in
## step 3

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
  ) %>%
  mutate(
    mnactic = forcats::fct_collapse(mnactic, other = c("other", "community_military")),
    w6sgq4  = relevel(w6sgq4, ref = "gasoline_car")
  )

rhs <- "cntry + w6age + gndr + eisced + mnactic + hinctnta + netusoft +
        hincfel + w6sgq11 + w6sgq12 + w6sgq5 + w6wq2 + w6wq8 + w6sgq22"

## Separation
non_ref_classes <- setdiff(levels(dat$w6sgq4), "gasoline_car")

separation_check <- lapply(non_ref_classes, function(cls) {
  sub <- dat %>% filter(w6sgq4 %in% c(cls, "gasoline_car")) %>%
    mutate(y = as.numeric(w6sgq4 == cls))
  fit_sep <- glm(as.formula(paste("y ~", rhs)), data = sub, family = binomial(),
                 method = "detect_separation")
  fit_sep
})
names(separation_check) <- paste(non_ref_classes, "vs gasoline_car")

for (nm in names(separation_check)) {
  cat("\n---", nm, "---\n")
  print(separation_check[[nm]])
}

## No complete separation

## Multicollinearity
fit_vif <- glm(as.formula(paste("I(w6sgq4 == 'walking') ~", rhs)),
               data = dat, family = binomial())

vif_tab <- vif(fit_vif)

print(vif_tab)

## Linearity in the log-odds
vars_metric <- c("w6age", "w6sgq12", "w6sgq5", "w6wq2", "w6wq8")

empirical_logit <- function(var, n_bins = 6) {

  n_distinct_vals <- n_distinct(dat[[var]])

  if (n_distinct_vals <= 12) {
    ## coarse Likert scales (0 to 10 or 0 to 6) have too few distinct
    ## values for quantile binning, so every observed value is instead
    ## used as its own bin
    binned <- dat %>%
      mutate(bin = factor(.data[[var]]))
  } else {
    binned <- dat %>%
      mutate(bin = cut_number(.data[[var]], n = n_bins))
  }

  bin_means <- binned %>%
    group_by(bin) %>%
    summarise(bin_mean = mean(.data[[var]]), .groups = "drop")

  binned %>%
    count(bin, w6sgq4) %>%
    left_join(bin_means, by = "bin") %>%
    dplyr::select(-bin) %>%
    pivot_wider(names_from = w6sgq4, values_from = n, values_fill = 0) %>%
    mutate(
      walking          = log((walking + 0.5) / (gasoline_car + 0.5)),
      bicycle          = log((bicycle + 0.5) / (gasoline_car + 0.5)),
      public_transport = log((public_transport + 0.5) / (gasoline_car + 0.5)),
      electric_car     = log((electric_car + 0.5) / (gasoline_car + 0.5))
    ) %>%
    dplyr::select(bin_mean, walking, bicycle, public_transport, electric_car) %>%
    pivot_longer(-bin_mean, names_to = "class", values_to = "log_odds") %>%
    mutate(
      variable = var,
      class = dplyr::recode(
        class,
        walking = "Walking",
        bicycle = "Bicycle",
        public_transport = "Public transport",
        electric_car = "Electric/\nhybrid car"
      )
    )
}

logit_data <- lapply(vars_metric, empirical_logit) %>%
  bind_rows()

p_linearity <- ggplot(logit_data, aes(x = bin_mean, y = log_odds, colour = class)) +
  geom_point() +
  geom_line() +
  facet_wrap(
    ~ variable,
    scales = "free_x",
    nrow = 2,
    labeller = as_labeller(c(
      w6age   = "Age",
      w6sgq12 = "Personal \nresponsibility",
      w6sgq5  = "Satisfaction \nwith public transport",
      w6wq2   = "Cohesion \nin local area",
      w6wq8   = "Feeling of safety"
    ))
  ) +
  scale_colour_manual(values = c("#5aa9d6", "#a4c8e1", "#C0392B", "#1a6faf")) +
  labs(x = "Predictor value (bin mean)",
       y = "Empirical log-odds relative to gasoline/diesel car",
       colour = "Mode of transport") +
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

p_linearity

#library(tikzDevice)
#tikz("linearityinlogits.tex", width = 6, height = 3.5)
#p_linearity
#dev.off()


## it may be worth considering a quadratic age term

## Independence of observations
## Each row corresponds to a different CRONOS panel respondent; there is no
## repeated measurement or clustering of respondents in this wave, so the
## independence assumption is satisfied by the survey design.


## -----------------------------------------------------------------------------

## Appendix to step 4: linear versus quadratic age
## The empirical logit plot above showed a u-shaped pattern for w6age in
## the public_transport contrast, not a straight line. Here two full
## models are compared, one with w6age linear, one with an additional
## quadratic term, to check whether the nonlinearity is worth keeping in
## the model carried forward in step 5.

dat <- dat %>%
  mutate(w6age_c = w6age - mean(w6age))

## Model with w6age linear

fit_linear <- multinom(
  w6sgq4 ~ cntry + w6age + gndr + eisced + mnactic + hinctnta + netusoft +
    hincfel + w6sgq11 + w6sgq12 + w6sgq5 + w6wq2 + w6wq8 + w6sgq22,
  data = dat, trace = FALSE, maxit = 300
)

## Model with w6age linear and quadratic
## the raw quadratic term is centred beforehand so that the linear and
## quadratic parts are not strongly correlated with each other, which
## would otherwise inflate their standard errors without changing the fit
## itself

fit_quad <- multinom(
  w6sgq4 ~ cntry + w6age_c + I(w6age_c^2) + gndr + eisced + mnactic + hinctnta +
    netusoft + hincfel + w6sgq11 + w6sgq12 + w6sgq5 + w6wq2 + w6wq8 + w6sgq22,
  data = dat, trace = FALSE, maxit = 300
)

## Likelihood-ratio test of the two nested models
## H0: The quadratic term does not contribute to model fit
## H1: The quadratic term contributes to model fit

lr_stat <- -2 * (as.numeric(logLik(fit_linear)) - as.numeric(logLik(fit_quad)))
df_diff <- attr(logLik(fit_quad), "df") - attr(logLik(fit_linear), "df")
lr_p    <- pchisq(lr_stat, df = df_diff, lower.tail = FALSE)

data.frame(
  model  = c("Age linear", "Age quadratic"),
  AIC    = c(AIC(fit_linear), AIC(fit_quad)),
  logLik = c(as.numeric(logLik(fit_linear)), as.numeric(logLik(fit_quad))),
  df     = c(attr(logLik(fit_linear), "df"), attr(logLik(fit_quad), "df"))
)

cat("\nLikelihood-ratio test, age linear vs. quadratic\n")
cat("LR chi-squared =", round(lr_stat, 2), ", df =", df_diff,
    ", p-value =", signif(lr_p, 3), "\n")
## p < 0.001 => reject H0: There is statistically significant evidence that
## the quadratic age term improves model fit.

## Coefficients of the quadratic age term, walking and public_transport
## contrasts

round(summary(fit_quad)$coefficients[c("walking", "public_transport"),
                                     c("w6age_c", "I(w6age_c^2)")], 4)

## The quadratic specification improves the fit, AIC 19460.29 versus
## 19494.14 for the linear model, LR chi-squared = 41.85, df = 4, p < 0.001.
## Both the walking and public_transport coefficients of the quadratic term
## are positive, which confirms the u-shape seen in the empirical logit
## plot. Age is therefore used in all subsequent models as a centred
## quadratic term, w6age_c + I(w6age_c^2).


## -----------------------------------------------------------------------------

## 5. Automatic model selection

## Read in the cleaned dataset and restore types, same preparation as in
## steps 3 and 4

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
  ) %>%
  mutate(
    mnactic = forcats::fct_collapse(mnactic, other = c("other", "community_military")),
    w6sgq4  = relevel(w6sgq4, ref = "gasoline_car"),
    w6age_c = w6age - mean(w6age)
  )

## Starting model
fit_start <- multinom(
  w6sgq4 ~ cntry + w6age_c + I(w6age_c^2) + gndr + eisced + mnactic + hinctnta +
    netusoft + hincfel + w6sgq11 + w6sgq12 + w6sgq5 + w6wq2 + w6wq8 + w6sgq22,
  data = dat, trace = FALSE, maxit = 300
)

## Stepwise selection by AIC
scope_lower <- ~ 1
scope_upper <- ~ cntry + w6age_c + I(w6age_c^2) + gndr + eisced + mnactic +
  hinctnta + netusoft + hincfel + w6sgq11 + w6sgq12 + w6sgq5 + w6wq2 + w6wq8 + w6sgq22

fit_step <- stepAIC(
  fit_start,
  scope = list(lower = scope_lower, upper = scope_upper),
  direction = "both",
  trace = TRUE
)

## Summary of the selected model
summary(fit_step)

cat("\nAIC full model       =", AIC(fit_start), "\n")
cat("AIC selected model   =", AIC(fit_step), "\n")

## Variables removed during selection
dropped_vars <- setdiff(
  attr(terms(fit_start), "term.labels"),
  attr(terms(fit_step), "term.labels")
)

cat("\nvariables removed during stepwise selection:\n")
print(dropped_vars)

## The stepwise selection removes netusoft and eisced, AIC improves from
## 17653.06 to 17636.98.

## Likelihood-ratio test of the selected model against the null model
## H0: All coefficients (except the intercepts) are zero
## H1: At least one coefficient is nonzero

fit_null <- multinom(w6sgq4 ~ 1, data = dat, trace = FALSE)

lr_stat_overall <- -2 * (as.numeric(logLik(fit_null)) - as.numeric(logLik(fit_step)))
df_overall      <- attr(logLik(fit_step), "df") - attr(logLik(fit_null), "df")
lr_p_overall    <- pchisq(lr_stat_overall, df = df_overall, lower.tail = FALSE)

cat("\nLikelihood-ratio test, selected model vs. null model\n")
cat("LR chi-squared =", round(lr_stat_overall, 2), ", df =", df_overall,
    ", p-value =", signif(lr_p_overall, 3), "\n")

## p < 0.001 => reject H0: The selected model is highly significant
## relative to the null model, consistent with the strength of cntry and
## hinctnta in the screening from step 2.


## -----------------------------------------------------------------------------

## 6. Train-test split and model evaluation

## Read in the cleaned dataset and restore types, same preparation as in
## steps 3 to 5

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
  ) %>%
  mutate(
    mnactic = forcats::fct_collapse(mnactic, other = c("other", "community_military")),
    w6sgq4  = relevel(w6sgq4, ref = "gasoline_car"),
    w6age_c = w6age - mean(w6age)
  )

## Train-test split
set.seed(2026)

train_idx <- createDataPartition(dat$w6sgq4, p = 0.75, list = FALSE)

dat_train <- dat[train_idx, ]
dat_test  <- dat[-train_idx, ]

cat("Training observations:", nrow(dat_train), "\n")
cat("Test observations:    ", nrow(dat_test), "\n")

cat("\nClass shares in the training set\n")
print(round(100 * prop.table(table(dat_train$w6sgq4)), 1))
cat("\nClass shares in the test set\n")
print(round(100 * prop.table(table(dat_test$w6sgq4)), 1))

## Refit the model selected in step 5 on the training data only
## netusoft and eisced were removed during stepwise selection and remain
## excluded here

fit_train <- multinom(
  w6sgq4 ~ cntry + w6age_c + I(w6age_c^2) + gndr + mnactic + hinctnta +
    hincfel + w6sgq11 + w6sgq12 + w6sgq5 + w6wq2 + w6wq8 + w6sgq22,
  data = dat_train, trace = FALSE, maxit = 300
)

## Predictions on the held-out test set

pred_test <- predict(fit_train, newdata = dat_test)

## Confusion matrix and model fit measures
## confusionMatrix() provides overall accuracy with a confidence interval,
## the no-information rate (accuracy from always predicting the majority
## class), Kappa, and class-wise sensitivity, specificity, and balanced
## accuracy

## ⚠️⚠️ IN THE REPORT -----------------------------------------------------------
##------------------------------------------------------------------------------

cm <- confusionMatrix(pred_test, dat_test$w6sgq4)

print(cm)

## ⚠️⚠️-------------------------------------------------------------------------
##------------------------------------------------------------------------------

## Likelihood-ratio test of the training model against a null model fitted
## on the same training data
## H0: All coefficients (except the intercepts) are zero
## H1: At least one coefficient is nonzero

fit_null_train <- multinom(w6sgq4 ~ 1, data = dat_train, trace = FALSE)

lr_stat <- -2 * (as.numeric(logLik(fit_null_train)) - as.numeric(logLik(fit_train)))
df_lr   <- attr(logLik(fit_train), "df") - attr(logLik(fit_null_train), "df")
lr_p    <- pchisq(lr_stat, df = df_lr, lower.tail = FALSE)

cat("\nLikelihood-ratio test, training model vs. null model\n")
cat("LR chi-squared =", round(lr_stat, 2), ", df =", df_lr,
    ", p-value =", signif(lr_p, 3), "\n")

## p < 0.001 => reject H0: The training model remains highly significant
## relative to its null model, consistent with the result on the full
## sample in step 5.


## -----------------------------------------------------------------------------

## 7. Class imbalance

## Read in the cleaned dataset and restore types, same preparation as in
## steps 3 to 6

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
  ) %>%
  mutate(
    mnactic = forcats::fct_collapse(mnactic, other = c("other", "community_military")),
    w6sgq4  = relevel(w6sgq4, ref = "gasoline_car"),
    w6age_c = w6age - mean(w6age)
  )

## Class imbalance check
## Step 1 already showed gasoline_car accounting for roughly half the
## sample; this is confirmed here directly on the target variable before
## any split

cat("Class shares in the full cleaned sample\n")
print(round(100 * prop.table(table(dat$w6sgq4)), 1))

## Same train-test split as in step 6, for direct comparability

set.seed(2026)

train_idx <- createDataPartition(dat$w6sgq4, p = 0.75, list = FALSE)

dat_train <- dat[train_idx, ]
dat_test  <- dat[-train_idx, ]

## the original, non-upsampled model from step 6 is refitted here as well
## so that this script is self-contained and the before/after comparison
## below does not depend on objects from step 6

fit_orig <- multinom(
  w6sgq4 ~ cntry + w6age_c + I(w6age_c^2) + gndr + mnactic + hinctnta +
    hincfel + w6sgq11 + w6sgq12 + w6sgq5 + w6wq2 + w6wq8 + w6sgq22,
  data = dat_train, trace = FALSE, maxit = 300
)

pred_orig <- predict(fit_orig, newdata = dat_test)
cm_orig   <- confusionMatrix(pred_orig, dat_test$w6sgq4)

## Upsampling the training data
## upSample() samples with replacement from every class except the
## largest, until all classes reach the size of the largest class.
## This is applied only to the training data; the test set remains
## untouched so that model fit continues to be measured against the
## original, real class distribution.

set.seed(2026)

predictor_vars <- c("cntry", "w6age_c", "gndr", "mnactic", "hinctnta",
                    "hincfel", "w6sgq11", "w6sgq12", "w6sgq5", "w6wq2", "w6wq8",
                    "w6sgq22")

dat_train_up <- upSample(
  x = dat_train[, predictor_vars],
  y = dat_train$w6sgq4,
  yname = "w6sgq4"
)

cat("\nTraining set size before upsampling:", nrow(dat_train), "\n")
cat("Training set size after upsampling: ", nrow(dat_train_up), "\n")
cat("\nClass shares after upsampling\n")
print(round(100 * prop.table(table(dat_train_up$w6sgq4)), 1))

## Refit the selected model on the upsampled training data

fit_up <- multinom(
  w6sgq4 ~ cntry + w6age_c + I(w6age_c^2) + gndr + mnactic + hinctnta +
    hincfel + w6sgq11 + w6sgq12 + w6sgq5 + w6wq2 + w6wq8 + w6sgq22,
  data = dat_train_up, trace = FALSE, maxit = 300
)

## Predictions and confusion matrix on the original, untouched test set

pred_up <- predict(fit_up, newdata = dat_test)

cm_up <- confusionMatrix(pred_up, dat_test$w6sgq4)

print(cm_up)

## Side-by-side comparison of class-wise sensitivity before and after

sens_before <- cm_orig$byClass[, "Sensitivity"]
sens_after  <- cm_up$byClass[, "Sensitivity"]

sens_compare <- data.frame(
  class               = names(sens_before),
  sensitivity_before  = round(sens_before, 3),
  sensitivity_after   = round(sens_after, 3)
)

print(sens_compare)

cat("\nOverall accuracy before upsampling:", round(cm_orig$overall["Accuracy"], 3), "\n")
cat("Overall accuracy after upsampling: ", round(cm_up$overall["Accuracy"], 3), "\n")
cat("\nKappa before upsampling:", round(cm_orig$overall["Kappa"], 3), "\n")
cat("Kappa after upsampling: ", round(cm_up$overall["Kappa"], 3), "\n")


## -----------------------------------------------------------------------------

## 8. Interpretation of the final model

## Read in the cleaned dataset and restore types, same preparation as in
## steps 3 to 7

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
  ) %>%
  mutate(
    mnactic = forcats::fct_collapse(mnactic, other = c("other", "community_military")),
    w6sgq4  = relevel(w6sgq4, ref = "gasoline_car"),
    w6age_c = w6age - mean(w6age)
  )

## Reconstruct the same train-test split and the same upsampled training
## set as in step 7

set.seed(2026)

train_idx <- createDataPartition(dat$w6sgq4, p = 0.75, list = FALSE)
dat_train <- dat[train_idx, ]
dat_test  <- dat[-train_idx, ]

predictor_vars <- c("cntry", "w6age_c", "gndr", "mnactic", "hinctnta",
                    "hincfel", "w6sgq11", "w6sgq12", "w6sgq5", "w6wq2", "w6wq8",
                    "w6sgq22")

set.seed(2026)
dat_train_up <- upSample(
  x = dat_train[, predictor_vars],
  y = dat_train$w6sgq4,
  yname = "w6sgq4"
)

fit_final <- multinom(
  w6sgq4 ~ cntry + w6age_c + I(w6age_c^2) + gndr + mnactic + hinctnta +
    hincfel + w6sgq11 + w6sgq12 + w6sgq5 + w6wq2 + w6wq8 + w6sgq22,
  data = dat_train_up, trace = FALSE, maxit = 300
)

## Overall variable importance via likelihood-ratio tests
lr_importance <- Anova(fit_final, type = "II")

print(lr_importance)

## Variable grouping for interpretation
group_demographic   <- c("cntry", "w6age_c", "gndr")
group_socioeconomic <- c("mnactic", "hinctnta", "hincfel")
group_climate        <- c("w6sgq11", "w6sgq12", "w6sgq5", "w6wq2", "w6wq8", "w6sgq22")

lr_table <- as.data.frame(lr_importance) %>%
  tibble::rownames_to_column("variable") %>%
  filter(variable != "Residuals") %>%
  mutate(
    group = case_when(
      variable %in% group_demographic   ~ "demographic",
      variable %in% group_socioeconomic ~ "socioeconomic",
      variable %in% group_climate       ~ "climate attitude / wellbeing",
      TRUE                              ~ "other"
    )
  ) %>%
  arrange(group, desc(`LR Chisq`))

print(lr_table)

## Demographic predictors dominate overall, driven almost entirely by
## cntry. Socioeconomic predictors follow in second place, led by
## hinctnta. Within the climate-attitude group, w6sgq5 (satisfaction with
## public transport) alone exceeds the combined importance of all other
## climate items. The remaining attitude items (w6sgq11, w6sgq22, w6wq2,
## w6wq8, w6sgq12) contribute smaller but still significant effects.

## Odds ratios for the strongest predictor in each group
coef_final <- summary(fit_final)$coefficients
se_final   <- summary(fit_final)$standard.errors
z_final    <- coef_final / se_final
p_final    <- 2 * (1 - pnorm(abs(z_final)))

or_final <- exp(coef_final)

## demographic, cntry
## reported relative to the reference class gasoline_car and the reference
## country (the first alphabetical country code)

cat("\nOdds ratios, country (relative to gasoline_car, reference country)\n")
print(round(or_final[, grepl("^cntry", colnames(or_final))], 2))

or_country <- exp(coef_final[, grep("^cntry", colnames(coef_final))])

p_country <- p_final[, grep("^cntry", colnames(p_final))]

country_results <- cbind(
  expand.grid(
    Class = rownames(or_final),
    Country = colnames(or_final)
  ),
  OR = as.vector(or_final),
  p = as.vector(p_final)
)

country_results

## socioeconomic, hinctnta

cat("\nOdds ratios, income decile (relative to gasoline_car, decile_1)\n")
print(round(or_final[, grepl("^hinctnta", colnames(or_final))], 2))

or_hinctnta <- exp(coef_final[, grep("^hinctnta", colnames(coef_final))])

p_hinctnta <- p_final[, grep("^hinctnta", colnames(p_final))]

hinctnta_results <- cbind(
  expand.grid(
    Class = rownames(or_final),
    Country = colnames(or_final)
  ),
  OR = as.vector(or_final),
  p = as.vector(p_final)
)

hinctnta_results

average_predictions <- lapply(levels(dat_train_up$hinctnta), function(decile) {

  newdat <- dat_train_up
  newdat$hinctnta <- ordered(
    decile,
    levels = levels(dat_train_up$hinctnta)
  )

  probs <- predict(
    fit_final,
    newdata = newdat,
    type = "probs"
  )

  data.frame(
    income_decile = decile,
    class = colnames(probs),
    probability = colMeans(probs)
  )
}) %>%
  bind_rows()

average_predictions <- average_predictions %>%
  mutate(
    income_decile = factor(
      income_decile,
      levels = paste0("decile_", 1:10),
      labels = 1:10,
      ordered = TRUE
    ),
    class = dplyr::recode(
      class,
      gasoline_car = "Gasoline/\ndiesel car",
      walking = "Walking",
      bicycle = "Bicycle",
      public_transport = "Public transport",
      electric_car = "Electric/\nhybrid car"
    )
  )

p_income <- ggplot(
  average_predictions,
  aes(
    x = income_decile,
    y = probability,
    colour = class,
    group = class
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_y_continuous(
    labels = function(x) paste0(round(x * 100), "\\%")
  ) +
  labs(
    x = "Income decile",
    y = "Average predicted \n probability",
    colour = "Mode of transport"
  ) +
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
    axis.title.y = element_text(margin = margin(r = 8)),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing.y = unit(0.15, "cm")
  )

p_income

#library(tikzDevice)
#tikz("lineplot.tex", width = 5, height = 2.5)
#p_income
#dev.off()

## Climate attitude, w6sgq11 and w6sgq5
## these enter as numeric (quasi-metric) predictors, so the odds ratio
## below is the multiplicative change in odds per unit increase of the
## item, not a level-to-level comparison

cat("\nOdds ratio per unit increase, climate concern (w6sgq11)\n")
print(round(or_final[, "w6sgq11"], 3))

cat("\nOdds ratio per unit increase, public transport satisfaction (w6sgq5)\n")
print(round(or_final[, "w6sgq5"], 3))

## Income shows the cleanest gradient in the model: every higher decile has
## lower odds of walking, bicycle, and public_transport relative to
## decile_1, and steadily increasing odds of electric_car (decile_10 OR =
## 2.67). Climate concern (w6sgq11) increases the odds of walking, bicycle,
## and public_transport (OR 1.40 to 1.50), but is not significant for
## electric_car, suggesting that electric-car adoption in this sample is
## driven more by income than by climate attitude.

full_table <- data.frame(
  term       = colnames(coef_final),
  walking_OR = round(or_final["walking", ], 3),
  walking_p  = round(p_final["walking", ], 4),
  bicycle_OR = round(or_final["bicycle", ], 3),
  bicycle_p  = round(p_final["bicycle", ], 4),
  pt_OR      = round(or_final["public_transport", ], 3),
  pt_p       = round(p_final["public_transport", ], 4),
  ev_OR      = round(or_final["electric_car", ], 3),
  ev_p       = round(p_final["electric_car", ], 4)
)

write_csv(full_table, "thema5_final_model_odds_ratios.csv")

print(full_table)


## -----------------------------------------------------------------------------

## 9. Reporting / citations

toBibtex(citation("readr"))
toBibtex(citation("dplyr"))
toBibtex(citation("tidyr"))
toBibtex(citation("ggplot2"))
toBibtex(citation("patchwork"))
toBibtex(citation("nnet"))
toBibtex(citation("car"))
toBibtex(citation("MASS"))
toBibtex(citation("caret"))
toBibtex(citation("forcats"))
toBibtex(citation("detectseparation"))
