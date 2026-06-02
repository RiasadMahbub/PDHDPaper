# =========================================================
# LIBRARIES
# =========================================================
library(ranger)
library(Metrics)
library(dplyr)
library(caret)
library(ggplot2)

# =========================================================
# DATA PREP
# =========================================================

# Extract Year from Field_Year (FIXED)
dfharvest$Year <- as.numeric(sub(".*_(\\d{4})$", "\\1", dfharvest$Field_Year))

# Check
table(dfharvest$Year)

dfharvest_pheno <- dfharvest %>%
  dplyr::select(
    RD.RD, EOS_trs.eos, EOS_deriv.eos, a2, cum_vpd, cum_meansrad,
    a1, SOS_deriv.sos, avgsoilorg, cum_soiltemp,
    HDDOY, Year
  ) %>%
  filter(!is.na(HDDOY)) %>%
  drop_na()

# =========================================================
# 🔵 HYPERPARAMETER TUNING (STRONGER)
# =========================================================

set.seed(123)

tune_grid <- expand.grid(
  mtry = c(2, 3, 4, 5),
  splitrule = "variance",
  min.node.size = c(1, 3, 5, 10)
)

rf_tuned <- train(
  HDDOY ~ . - Year,
  data = dfharvest_pheno,
  method = "ranger",
  metric = "RMSE",
  tuneGrid = tune_grid,
  trControl = trainControl(method = "cv", number = 5)
)

best_params <- rf_tuned$bestTune
print(best_params)

# =========================================================
# 🔴 TRAIN / VALIDATION / TEST (BY YEAR)
# =========================================================

years <- sort(unique(dfharvest_pheno$Year))

results_year_split <- data.frame()

counter <- 1

for (test_year in years) {
  
  for (val_year in years) {
    
    if (val_year == test_year) next
    
    train_data <- dfharvest_pheno %>%
      filter(Year != test_year & Year != val_year)
    
    val_data <- dfharvest_pheno %>%
      filter(Year == val_year)
    
    test_data <- dfharvest_pheno %>%
      filter(Year == test_year)
    
    # Train model
    rf_model <- ranger(
      HDDOY ~ . - Year,
      data = train_data,
      num.trees = 300,
      mtry = best_params$mtry,
      min.node.size = best_params$min.node.size,
      importance = "permutation"
    )
    
    # Predictions
    pred_val  <- predict(rf_model, val_data)$predictions
    pred_test <- predict(rf_model, test_data)$predictions
    
    # Store results
    results_year_split <- rbind(results_year_split, data.frame(
      Iteration = counter,
      Test_Year = test_year,
      Val_Year  = val_year,
      
      Val_RMSE = rmse(val_data$HDDOY, pred_val),
      Val_MAE  = mae(val_data$HDDOY, pred_val),
      Val_R2   = summary(lm(val_data$HDDOY ~ pred_val))$r.squared,
      
      Test_RMSE = rmse(test_data$HDDOY, pred_test),
      Test_MAE  = mae(test_data$HDDOY, pred_test),
      Test_R2   = summary(lm(test_data$HDDOY ~ pred_test))$r.squared
    ))
    
    counter <- counter + 1
  }
}

# =========================================================
# 🔵 SUMMARY
# =========================================================

summary_year_split <- results_year_split %>%
  summarise(
    Val_RMSE_mean = mean(Val_RMSE),
    Val_RMSE_sd   = sd(Val_RMSE),
    Test_RMSE_mean = mean(Test_RMSE),
    Test_RMSE_sd   = sd(Test_RMSE),
    
    Val_MAE_mean = mean(Val_MAE),
    Val_MAE_sd   = sd(Val_MAE),
    Test_MAE_mean = mean(Test_MAE),
    Test_MAE_sd   = sd(Test_MAE),
    
    Val_R2_mean = mean(Val_R2),
    Test_R2_mean = mean(Test_R2)
  )

print("Year-based Split Summary:")
print(summary_year_split)

# =========================================================
# 🔵 CHECK OVERFITTING (IMPORTANT FOR REVIEWER)
# =========================================================

ggplot(results_year_split, aes(x = Val_RMSE, y = Test_RMSE)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Validation vs Test RMSE (Year-based splits)",
    x = "Validation RMSE",
    y = "Test RMSE"
  ) +
  theme_minimal()

# =========================================================
# 🔵 SAVE RESULTS
# =========================================================

write.csv(results_year_split, "year_split_results.csv", row.names = FALSE)
# =========================================================
# LIBRARIES
# =========================================================
library(hydroGOF)      # Load FIRST so dplyr wins print conflict
library(Metrics)
library(randomForest)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(caret)
library(viridis)

# =========================================================
# DATA PREP
# =========================================================

# Extract year safely
df$Year <- as.numeric(sub(".*_(\\d{4})$", "\\1", df$Field_Year))

# Build PD dataset
df_pd <- df %>%
  dplyr::select(
    cum_tmin, SOS_trs.sos, cum_gdd,
    cum_vpd, avgsoilorg,
    PDDOY, Year
  ) %>%
  filter(!is.na(PDDOY)) %>%
  drop_na()

cat("Rows in df_pd:", nrow(df_pd), "\n")
cat("Years available:", paste(sort(unique(df_pd$Year)), collapse = ", "), "\n")

# =========================================================
# BASELINE LINEAR MODEL (sanity check before RF)
# =========================================================

cat("\n--- Linear Model Baseline (full data, for reference) ---\n")
lm_baseline_full <- lm(
  PDDOY ~ cum_tmin + SOS_trs.sos + cum_gdd + cum_vpd + avgsoilorg,
  data = df_pd
)
summary(lm_baseline_full)

# =========================================================
# GRID SEARCH (mtry + nodesize tuning)
# =========================================================

set.seed(123)

# Step 1: tune mtry
tune_grid <- expand.grid(
  mtry = c(2, 3, 4, 5)
)

rf_grid_pd <- train(
  PDDOY ~ . - Year,
  data      = df_pd,
  method    = "rf",
  metric    = "RMSE",
  tuneGrid  = tune_grid,
  ntree     = 100,                          # reduced from 500
  trControl = trainControl(method = "cv", number = 5)
)

best_mtry <- rf_grid_pd$bestTune$mtry
cat("Best mtry:", best_mtry, "\n")

# Step 2: tune nodesize to reduce overfitting
cat("\n--- Tuning nodesize ---\n")
nodesize_options <- c(3, 5, 10, 15)
nodesize_results <- data.frame()

for (ns in nodesize_options) {
  set.seed(123)
  rf_temp <- train(
    PDDOY ~ . - Year,
    data      = df_pd,
    method    = "rf",
    metric    = "RMSE",
    tuneGrid  = expand.grid(mtry = best_mtry),
    ntree     = 100,
    nodesize  = ns,
    trControl = trainControl(method = "cv", number = 5)
  )
  nodesize_results <- rbind(nodesize_results, data.frame(
    nodesize  = ns,
    CV_RMSE   = min(rf_temp$results$RMSE),
    CV_Rsq    = max(rf_temp$results$Rsquared)
  ))
}

print(as.data.frame(nodesize_results))
best_nodesize <- nodesize_results$nodesize[which.min(nodesize_results$CV_RMSE)]
cat("Best nodesize:", best_nodesize, "\n")

# =========================================================
# YEAR-BASED TRAIN / VAL / TEST LOOP
# =========================================================

years              <- sort(unique(df_pd$Year))
results_year_split <- data.frame()

for (test_year in years) {
  for (val_year in years) {
    
    if (test_year == val_year) next
    
    # -------------------------
    # SPLITS
    # -------------------------
    train_data <- df_pd %>% filter(Year != test_year & Year != val_year)
    val_data   <- df_pd %>% filter(Year == val_year)
    test_data  <- df_pd %>% filter(Year == test_year)
    
    # Skip if any split is too small
    if (nrow(train_data) < 5 || nrow(val_data) < 2 || nrow(test_data) < 2) next
    
    # -------------------------
    # LINEAR BASELINE PER SPLIT
    # -------------------------
    lm_model <- tryCatch(
      lm(PDDOY ~ cum_tmin + SOS_trs.sos + cum_gdd + cum_vpd + avgsoilorg,
         data = train_data),
      error = function(e) NULL
    )
    
    pred_lm_test <- if (!is.null(lm_model)) {
      tryCatch(predict(lm_model, test_data), error = function(e) rep(NA, nrow(test_data)))
    } else rep(NA, nrow(test_data))
    
    # -------------------------
    # RANDOM FOREST MODEL
    # -------------------------
    rf_model <- tryCatch({
      randomForest(
        PDDOY ~ . - Year,
        data       = train_data,
        ntree      = 100,                   # reduced from 500
        mtry       = best_mtry,
        nodesize   = best_nodesize,         # regularization
        maxnodes   = 20,                    # cap tree depth
        importance = TRUE
      )
    }, error = function(e) {
      message("RF failed for test=", test_year, " val=", val_year, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(rf_model)) next
    
    # -------------------------
    # PREDICTIONS
    # -------------------------
    pred_train <- predict(rf_model, train_data)
    pred_val   <- predict(rf_model, val_data)
    pred_test  <- predict(rf_model, test_data)
    
    # -------------------------
    # SAFE R2 HELPER
    # -------------------------
    safe_r2 <- function(obs, pred) {
      tryCatch(
        summary(lm(obs ~ pred))$r.squared,
        error = function(e) NA_real_
      )
    }
    
    # -------------------------
    # STORE RESULTS
    # -------------------------
    results_year_split <- rbind(results_year_split, data.frame(
      test_year = test_year,
      val_year  = val_year,
      
      # TRAIN
      Train_RMSE = hydroGOF::rmse(pred_train, train_data$PDDOY),
      Train_MAE  = hydroGOF::mae(pred_train,  train_data$PDDOY),
      Train_R2   = safe_r2(train_data$PDDOY,  pred_train),
      
      # VALIDATION
      Val_RMSE = hydroGOF::rmse(pred_val, val_data$PDDOY),
      Val_MAE  = hydroGOF::mae(pred_val,  val_data$PDDOY),
      Val_R2   = safe_r2(val_data$PDDOY,  pred_val),
      
      # TEST - RF
      Test_RMSE = hydroGOF::rmse(pred_test, test_data$PDDOY),
      Test_MAE  = hydroGOF::mae(pred_test,  test_data$PDDOY),
      Test_R2   = safe_r2(test_data$PDDOY,  pred_test),
      
      # TEST - LINEAR BASELINE
      LM_Test_RMSE = tryCatch(hydroGOF::rmse(pred_lm_test, test_data$PDDOY), error = function(e) NA_real_),
      LM_Test_MAE  = tryCatch(hydroGOF::mae(pred_lm_test,  test_data$PDDOY), error = function(e) NA_real_),
      LM_Test_R2   = safe_r2(test_data$PDDOY, pred_lm_test),
      
      stringsAsFactors = FALSE
    ))
  }
}

# Add pair label
results_year_split <- results_year_split %>%
  mutate(Pair = paste0("T:", test_year, "_V:", val_year))

# =========================================================
# 1. FULL RAW TABLE (SORTED WORST TEST PERFORMANCE)
# =========================================================

cat("\n--- Full Raw Results (worst Test RMSE first) ---\n")
results_year_split %>%
  arrange(desc(Test_RMSE)) %>%
  as.data.frame() %>%
  head(50) %>%
  print()

# =========================================================
# 2. TEST YEAR DIAGNOSTIC
# =========================================================

cat("\n--- Test Year Summary ---\n")
test_year_summary <- results_year_split %>%
  group_by(test_year) %>%
  summarise(
    mean_Test_RMSE = mean(Test_RMSE, na.rm = TRUE),
    sd_Test_RMSE   = sd(Test_RMSE,   na.rm = TRUE),
    mean_Test_MAE  = mean(Test_MAE,  na.rm = TRUE),
    mean_Test_R2   = mean(Test_R2,   na.rm = TRUE),
    mean_LM_RMSE   = mean(LM_Test_RMSE, na.rm = TRUE),  # LM baseline per year
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_Test_RMSE))

print(as.data.frame(test_year_summary))

# =========================================================
# 3. VALIDATION YEAR DIAGNOSTIC
# =========================================================

cat("\n--- Validation Year Summary ---\n")
val_year_summary <- results_year_split %>%
  group_by(val_year) %>%
  summarise(
    mean_Val_RMSE = mean(Val_RMSE, na.rm = TRUE),
    sd_Val_RMSE   = sd(Val_RMSE,   na.rm = TRUE),
    mean_Val_MAE  = mean(Val_MAE,  na.rm = TRUE),
    mean_Val_R2   = mean(Val_R2,   na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_Val_RMSE))

print(as.data.frame(val_year_summary))

# =========================================================
# 4. HEATMAP
# =========================================================

ggplot(results_year_split,
       aes(x = factor(val_year), y = factor(test_year), fill = Test_RMSE)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Year Interaction Effect on Test RMSE",
    x     = "Validation Year",
    y     = "Test Year",
    fill  = "RMSE"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =========================================================
# 5. OVERALL SUMMARY STATISTICS (RF vs LM comparison)
# =========================================================

cat("\n--- Overall Summary Statistics ---\n")
summary_year_split <- results_year_split %>%
  summarise(
    # TRAIN
    Train_RMSE_mean = mean(Train_RMSE, na.rm = TRUE),
    Train_RMSE_sd   = sd(Train_RMSE,   na.rm = TRUE),
    Train_MAE_mean  = mean(Train_MAE,  na.rm = TRUE),
    Train_MAE_sd    = sd(Train_MAE,    na.rm = TRUE),
    Train_R2_mean   = mean(Train_R2,   na.rm = TRUE),
    
    # VALIDATION
    Val_RMSE_mean = mean(Val_RMSE, na.rm = TRUE),
    Val_RMSE_sd   = sd(Val_RMSE,   na.rm = TRUE),
    Val_MAE_mean  = mean(Val_MAE,  na.rm = TRUE),
    Val_MAE_sd    = sd(Val_MAE,    na.rm = TRUE),
    Val_R2_mean   = mean(Val_R2,   na.rm = TRUE),
    
    # TEST - RF
    Test_RMSE_mean = mean(Test_RMSE, na.rm = TRUE),
    Test_RMSE_sd   = sd(Test_RMSE,   na.rm = TRUE),
    Test_MAE_mean  = mean(Test_MAE,  na.rm = TRUE),
    Test_MAE_sd    = sd(Test_MAE,    na.rm = TRUE),
    Test_R2_mean   = mean(Test_R2,   na.rm = TRUE),
    
    # TEST - LINEAR BASELINE
    LM_RMSE_mean = mean(LM_Test_RMSE, na.rm = TRUE),
    LM_MAE_mean  = mean(LM_Test_MAE,  na.rm = TRUE),
    LM_R2_mean   = mean(LM_Test_R2,   na.rm = TRUE)
  )

print(as.data.frame(summary_year_split))

# Overfitting summary table
cat("\n--- Overfitting Diagnostic ---\n")
overfit_summary <- data.frame(
  Split    = c("Train", "Validation", "Test (RF)", "Test (LM baseline)"),
  RMSE     = c(
    summary_year_split$Train_RMSE_mean,
    summary_year_split$Val_RMSE_mean,
    summary_year_split$Test_RMSE_mean,
    summary_year_split$LM_RMSE_mean
  ),
  MAE      = c(
    summary_year_split$Train_MAE_mean,
    summary_year_split$Val_MAE_mean,
    summary_year_split$Test_MAE_mean,
    summary_year_split$LM_MAE_mean
  ),
  R2       = c(
    summary_year_split$Train_R2_mean,
    summary_year_split$Val_R2_mean,
    summary_year_split$Test_R2_mean,
    summary_year_split$LM_R2_mean
  )
)
overfit_summary$RMSE_gap_vs_train <- overfit_summary$RMSE - overfit_summary$RMSE[1]
overfit_summary$R2_drop_vs_train  <- overfit_summary$R2[1] - overfit_summary$R2

print(overfit_summary)

# =========================================================
# 6. WORST CASE COMBINATIONS
# =========================================================

cat("\n--- Top 20 Worst Test Combinations ---\n")
results_year_split %>%
  arrange(desc(Test_RMSE)) %>%
  dplyr::select(test_year, val_year, Test_RMSE, Test_MAE, Test_R2,
                LM_Test_RMSE, LM_Test_R2) %>%
  head(20) %>%
  as.data.frame() %>%
  print()

# =========================================================
# 7. FINAL MODEL (FULL DATA)
# =========================================================

cat("\n--- Training Final Model on Full Data ---\n")
final_model_pd <- randomForest(
  PDDOY ~ . - Year,
  data       = df_pd,
  ntree      = 100,                         # reduced from 500
  mtry       = best_mtry,
  nodesize   = best_nodesize,               # regularization
  maxnodes   = 20,                          # cap tree depth
  importance = TRUE
)

print(final_model_pd)

# =========================================================
# 8. VARIABLE IMPORTANCE
# =========================================================

cat("\n--- Variable Importance ---\n")
var_imp <- importance(final_model_pd)

var_imp_df <- data.frame(
  Variable      = rownames(var_imp),
  IncMSE        = var_imp[, "%IncMSE"],
  IncNodePurity = var_imp[, "IncNodePurity"],
  stringsAsFactors = FALSE
) %>%
  arrange(desc(IncMSE))

print(as.data.frame(var_imp_df))

# Variable importance plot
ggplot(var_imp_df, aes(x = reorder(Variable, IncMSE), y = IncMSE, fill = IncMSE)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_c(option = "cividis") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance (% Inc MSE)",
    x     = "Variable",
    y     = "% Increase in MSE"
  ) +
  theme_minimal(base_size = 13)

