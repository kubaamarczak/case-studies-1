# ============================================================
# Classification model explaining mobility choices
# – XGBoost for Case Studies I
#
# 04_xgboost.R
# Requires: 01_data_cleaning.R must be run first
# ============================================================

library(xgboost)
library(caret)
library(dplyr)
library(forcats)
library(Matrix)
library(ggplot2)
library(readr)

## -----------------------------------------------------------------------------

## 1. Load the cleaned dataset

## thema5_cleaned.csv is loaded, which was produced by the MLR script
## (step 1). This guarantees an identical n, identical observations, and
## identical cleaning. n should be 7330.

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
    mnactic = fct_collapse(mnactic, other = c("other", "community_military")),
    w6age_c = w6age - mean(w6age)
  )

cat("n:", nrow(dat), "\n")
## should be 7330

cat("Class distribution:\n")
print(table(dat$w6sgq4))


## -----------------------------------------------------------------------------

## 2. Encode features

## xgboost requires purely numeric input, so the unordered categorical
## variables are one-hot encoded. 12 predictors. Trees do not need a
## reference category, so all k dummies are kept

predictors_12 <- c("cntry", "w6age_c", "gndr", "mnactic",
                   "hinctnta", "hincfel",
                   "w6sgq11", "w6sgq12", "w6sgq5", "w6wq2", "w6wq8", "w6sgq22")

X <- model.matrix(~ . - 1,
                  data = dplyr::select(dat, all_of(predictors_12)))

cat("Feature matrix:", nrow(X), "rows x", ncol(X), "columns\n")

## Encode the target variable as a 0-indexed integer (required for
## multi:softmax in xgboost)
classes <- c("walking", "bicycle", "public_transport", "gasoline_car", "electric_car")
y <- as.integer(dat$w6sgq4) - 1L
## gasoline_car is level 4 here, since relevel() was not applied – check:
cat("Class-to-index mapping:\n")
print(data.frame(label = levels(dat$w6sgq4), index = 0:4))


## -----------------------------------------------------------------------------

## 3. Train-test split

## same seed, same p, same stratification as in step 6 of the MLR script
set.seed(2026)
train_idx <- createDataPartition(dat$w6sgq4, p = 0.75, list = FALSE)

X_train <- X[train_idx, ]
X_test  <- X[-train_idx, ]
y_train <- y[train_idx]
y_test  <- y[-train_idx]

cat("Train n:", nrow(X_train), "  Test n:", nrow(X_test), "\n")

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)


## -----------------------------------------------------------------------------

## 4. Hyperparameters

params <- list(
  objective        = "multi:softmax",
  num_class        = 5L,
  eval_metric      = "mlogloss",
  eta              = 0.05,
  max_depth        = 6L,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 10L
)


## -----------------------------------------------------------------------------

## 5. Cross-validation to choose nrounds

set.seed(2026)
cv_result <- xgb.cv(
  params        = params,
  data          = dtrain,
  nrounds       = 300L,
  nfold         = 5L,
  verbose       = 1,
  print_every_n = 50L
)

best_n <- which.min(cv_result$evaluation_log$test_mlogloss_mean)
cat("\noptimal nrounds:", best_n, "\n")
cat("CV test mlogloss:", round(
  cv_result$evaluation_log$test_mlogloss_mean[best_n], 4), "\n")


## -----------------------------------------------------------------------------

## 6. Training – without upsampling

set.seed(2026)
xgb_model <- xgb.train(
  params  = params,
  data    = dtrain,
  nrounds = best_n,
  evals   = list(train = dtrain, test = dtest),
  verbose = 0
)


## -----------------------------------------------------------------------------

## 7. Evaluation – without upsampling

y_pred      <- predict(xgb_model, dtest)
pred_labels <- levels(dat$w6sgq4)[y_pred + 1L]
true_labels <- levels(dat$w6sgq4)[y_test + 1L]

cm <- confusionMatrix(
  factor(pred_labels, levels = levels(dat$w6sgq4)),
  factor(true_labels, levels = levels(dat$w6sgq4))
)

cat("\n=== xgboost – without upsampling ===\n")
cat("Accuracy: ", round(cm$overall["Accuracy"], 3), "\n")
cat("NIR:      ", round(cm$overall["AccuracyNull"], 3), "\n")
cat("p(Acc>NIR):", round(cm$overall["AccuracyPValue"], 3), "\n")
cat("Kappa:    ", round(cm$overall["Kappa"], 3), "\n")
cat("\nclass-wise sensitivity:\n")
print(round(cm$byClass[, "Sensitivity"], 3))


## -----------------------------------------------------------------------------

## 8. Upsampling – same method as in step 7 of the MLR script

## upSample() on the training data, test set remains untouched
## same seed as in step 7 of the MLR script

train_df <- data.frame(X_train,
                       w6sgq4 = factor(y_train,
                                       levels = 0:4,
                                       labels = levels(dat$w6sgq4)),
                       check.names = FALSE)

set.seed(2026)
train_up <- upSample(
  x     = dplyr::select(train_df, -w6sgq4),
  y     = train_df$w6sgq4,
  yname = "w6sgq4"
)

y_train_up <- as.integer(train_up$w6sgq4) - 1L
X_train_up <- as.matrix(dplyr::select(train_up, -w6sgq4))

cat("\nupsampled training n:", nrow(train_up), "(was", nrow(X_train), ")\n")
cat("Class shares after upsampling:\n")
print(round(100 * prop.table(table(train_up$w6sgq4)), 1))

dtrain_up <- xgb.DMatrix(data = X_train_up, label = y_train_up)

set.seed(2026)

cv_result_up <- xgb.cv(
  params = params,
  data = dtrain_up,
  nrounds = 300L,
  nfold = 5L,
  verbose = 1,
  print_every_n = 50L
)

best_n_up <- which.min(
  cv_result_up$evaluation_log$test_mlogloss_mean
)

xgb_up <- xgb.train(
  params  = params,
  data    = dtrain_up,
  nrounds = best_n_up,
  verbose = 0
)


## -----------------------------------------------------------------------------

## 9. Evaluation – with upsampling

y_pred_up      <- predict(xgb_up, dtest)
pred_up_labels <- levels(dat$w6sgq4)[y_pred_up + 1L]


cm_up <- confusionMatrix(
  factor(pred_up_labels, levels = levels(dat$w6sgq4)),
  factor(true_labels,    levels = levels(dat$w6sgq4))
)

cat("\n=== xgboost – with upsampling ===\n")
cat("Accuracy:", round(cm_up$overall["Accuracy"], 3), "\n")
cat("Kappa:   ", round(cm_up$overall["Kappa"], 3), "\n")
cat("\nclass-wise sensitivity:\n")
print(round(cm_up$byClass[, "Sensitivity"], 3))


## -----------------------------------------------------------------------------

## 10. Feature importance

importance <- xgb.importance(
  feature_names = colnames(X_train),
  model         = xgb_up
)

cat("\nTop 20 features by gain:\n")
print(head(importance, 20))

imp_top20 <- head(importance, 20)

p_importance <- ggplot(imp_top20, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "#1a6faf") +
  coord_flip() +
  labs(x = NULL, y = "Gain",
       caption = "Feature importance (gain), upsampled XGBoost model, top 20 features.") +
  theme_bw(base_size = 11)

p_importance

#library(tikzDevice)
#tikz("xgb_importance.tex", width = 5.5, height = 4)
#p_importance
#dev.off()


## -----------------------------------------------------------------------------

## 11. Direct comparison

cat("\n\n=== direct comparison (same n=8072, same seed, same split) ===\n")
cat(sprintf("%-30s %8s %8s\n", "Metric", "MLR", "XGBoost"))
cat(strrep("-", 50), "\n")
cat(sprintf("%-30s %8s %8s\n", "Accuracy (without upsampling)",
            "0.511", round(cm$overall["Accuracy"], 3)))
cat(sprintf("%-30s %8s %8s\n", "Kappa    (without upsampling)",
            "0.159", round(cm$overall["Kappa"], 3)))
cat(sprintf("%-30s %8s %8s\n", "Accuracy (with upsampling)",
            "0.372", round(cm_up$overall["Accuracy"], 3)))
cat(sprintf("%-30s %8s %8s\n", "Kappa    (with upsampling)",
            "0.189", round(cm_up$overall["Kappa"], 3)))
cat(sprintf("%-30s %8s %8s\n", "bicycle sensitivity (up)", "0.382",
            round(cm_up$byClass["Class: bicycle", "Sensitivity"], 3)))


## -----------------------------------------------------------------------------

## 12. Reporting / citations

toBibtex(citation("xgboost"))
toBibtex(citation("caret"))
toBibtex(citation("dplyr"))
toBibtex(citation("forcats"))
toBibtex(citation("Matrix"))
toBibtex(citation("ggplot2"))
