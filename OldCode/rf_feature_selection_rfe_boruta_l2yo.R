# =========================================================
# APPROACH 1: PROPER RFE WITH YEAR-AWARE CV
# Key fix: relative features recomputed INSIDE each fold
# so they cannot leak test year information
# =========================================================

library(caret)
library(randomForest)
library(tidyverse)

# =========================================================
# FIX: Extract Year from Field_Year
# df has Field_Year (e.g. "F_8246_11_M4_2019") but no Year column
# =========================================================
# ADD THIS — needed throughout the script
safe_r2 <- function(obs, pred) {
  tryCatch(summary(lm(obs ~ pred))$r.squared,
           error = function(e) NA_real_)
}
# Check current state
cat("Has Year column:", "Year" %in% names(df), "\n")
cat("Has Field_Year column:", "Field_Year" %in% names(df), "\n")
cat("Sample Field_Year values:\n")
print(head(unique(df$Field_Year), 5))

# Extract Year from the last 4 digits of Field_Year
df$Year <- as.numeric(sub(".*_(\\d{4})$", "\\1", df$Field_Year))

# Verify
cat("\nYear extracted successfully:\n")
print(table(df$Year))
cat("NAs in Year:", sum(is.na(df$Year)), "\n")

# =========================================================
# DIAGNOSE WHICH OBJECT IS CORRECT
# =========================================================

cat("=== df ===\n")
cat("Rows:", nrow(df), "\n")
cat("Unique Field_Year:", length(unique(df$Field_Year)), "\n")
cat("Unique Year:", length(unique(df$Year)), "\n")
cat("Has SOS_trs.sos:", "SOS_trs.sos" %in% names(df), "\n")


# =========================================================
# STEP 1: USE ONLY LEAK-FREE FEATURES FOR RFE
# Remove relative features (soiltemp_rel, gdd_rel etc.)
# they require recomputing per fold which RFE can't do easily
# =========================================================

# Start from V5 feature set — the best honest version
feature_cols_v5 <- c(
  "SOS_trs.sos", "SD.SD", "DD.DD", "RD.RD", "DOY_maxROC_kNDVI",
  "season_length", "roc_vs_sos", "sd_x_sos",
  "cum_gdd", "cum_vpd", "cum_tmin", "cum_RH", "cum_soiltemp", "avgsoilorg",
  "gdd_x_sos", "vpd_x_sos", "tmin_gdd", "sd_x_gdd",
  "yr_gdd_anom", "yr_vpd_anom", "yr_tmin_anom", "yr_tmax_anom",
  "yr_rh_anom", "yr_rad_anom",
  "yr_gdd_rank", "yr_vpd_rank", "yr_tmin_rank", "yr_rad_rank",
  "yr_stress", "yr_energy_ratio"
)

available_v5 <- intersect(feature_cols_v5, names(df))

df_rfe <- df %>%
  dplyr::select(all_of(available_v5), PDDOY, Year) %>%
  filter(!is.na(PDDOY)) %>%
  drop_na()

cat("Features for RFE:", length(available_v5), "\n")
cat("Rows:", nrow(df_rfe), "\n")



# =========================================================
# STEP 2: YEAR-GROUPED CV FOR RFE
# Uses leave-one-year-out so selection is honest
# =========================================================

set.seed(123)

# Create year-based fold index
year_folds <- df_rfe$Year
unique_years <- sort(unique(year_folds))

# Build fold list: each fold leaves one year out
fold_list <- lapply(unique_years, function(yr) {
  which(year_folds != yr)  # training indices for this fold
})
names(fold_list) <- paste0("Year_", unique_years)

# Custom trainControl using year folds
ctrl_year <- trainControl(
  method   = "cv",
  index    = fold_list,
  verboseIter = FALSE
)

# =========================================================
# STEP 3: RFE — Recursive Feature Elimination
# Tests subsets: 5, 8, 10, 12, 15, 20, 25, 30 features
# =========================================================

cat("\nRunning RFE with year-based CV...\n")
cat("This will take a few minutes.\n\n")

set.seed(123)

rfe_ctrl <- rfeControl(
  functions  = rfFuncs,        # RF importance for ranking
  method     = "cv",
  index      = fold_list,      # year-based folds
  verbose    = FALSE,
  returnResamp = "all"
)

rfe_result <- rfe(
  x         = df_rfe %>% dplyr::select(all_of(available_v5)),
  y         = df_rfe$PDDOY,
  sizes     = c(5, 8, 10, 12, 15, 18, 20, 25, 30),
  rfeControl = rfe_ctrl,
  ntree     = 200,
  nodesize  = 5
)

cat("\n--- RFE Results ---\n")
print(rfe_result)

cat("\n--- RMSE by feature subset size ---\n")
print(rfe_result$results[, c("Variables", "RMSE", "Rsquared")])

cat("\n--- Optimal feature count ---\n")
cat("Best n features:", rfe_result$optsize, "\n")

cat("\n--- Selected features (optimal set) ---\n")
print(rfe_result$optVariables)

# =========================================================
# STEP 4: BORUTA — finds ALL relevant features
# More thorough than RFE, confirms which to keep/drop
# =========================================================

# install.packages("Boruta")
library(Boruta)

cat("\nRunning Boruta feature selection...\n")
cat("This will take ~2-3 minutes.\n\n")

set.seed(123)

boruta_result <- Boruta(
  x         = df_rfe %>% dplyr::select(all_of(available_v5)),
  y         = df_rfe$PDDOY,
  maxRuns   = 100,
  doTrace   = 1,
  ntree     = 200
)

cat("\n--- Boruta Results ---\n")
print(boruta_result)

# Fix tentative features (resolve undecided)
boruta_fixed <- TentativeRoughFix(boruta_result)

# Get confirmed important features
confirmed  <- getSelectedAttributes(boruta_fixed, withTentative = FALSE)
tentative  <- names(boruta_result$finalDecision[
  boruta_result$finalDecision == "Tentative"])
rejected   <- names(boruta_result$finalDecision[
  boruta_result$finalDecision == "Rejected"])

cat("\n--- Boruta Decision Summary ---\n")
cat("Confirmed important (", length(confirmed), "):\n")
print(sort(confirmed))
cat("\nTentative (", length(tentative), "):\n")
print(sort(tentative))
cat("\nRejected (", length(rejected), "):\n")
print(sort(rejected))

# =========================================================
# STEP 5: VISUALISE BORUTA IMPORTANCE
# =========================================================

boruta_df <- data.frame(
  Variable  = names(boruta_result$finalDecision),
  Decision  = as.character(boruta_result$finalDecision),
  MeanImp   = apply(boruta_result$ImpHistory, 2, mean, na.rm = TRUE)[
    names(boruta_result$finalDecision)]
) %>%
  filter(Variable != "shadowMax" &
           Variable != "shadowMean" &
           Variable != "shadowMin") %>%
  arrange(desc(MeanImp))

ggplot(boruta_df,
       aes(x = reorder(Variable, MeanImp),
           y = MeanImp,
           fill = Decision)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Confirmed" = "#2ecc71",
    "Tentative" = "#f39c12",
    "Rejected"  = "#e74c3c"
  )) +
  coord_flip() +
  labs(title = "Boruta Feature Selection",
       subtitle = "Green = keep | Orange = uncertain | Red = drop",
       x = "Variable", y = "Mean Importance") +
  theme_minimal(base_size = 12)

# =========================================================
# STEP 6: COMPARE RFE vs BORUTA CONSENSUS
# =========================================================

cat("\n--- Feature Selection Consensus ---\n")
rfe_features    <- rfe_result$optVariables
boruta_features <- confirmed

in_both   <- intersect(rfe_features, boruta_features)
rfe_only  <- setdiff(rfe_features,   boruta_features)
bor_only  <- setdiff(boruta_features, rfe_features)

cat("In BOTH (most reliable,", length(in_both), "):\n")
print(sort(in_both))
cat("\nRFE only (", length(rfe_only), "):\n")
print(sort(rfe_only))
cat("\nBoruta only (", length(bor_only), "):\n")
print(sort(bor_only))

# =========================================================
# STEP 7: RERUN L2YO WITH OPTIMAL FEATURE SET
# =========================================================

# Use consensus: confirmed by both methods
optimal_features <- union(in_both, bor_only)
cat("\nFinal optimal feature set (", length(optimal_features), "):\n")
print(sort(optimal_features))

df_opt <- df %>%
  dplyr::select(all_of(optimal_features), PDDOY, Year) %>%
  filter(!is.na(PDDOY)) %>%
  drop_na()

cat("Rows:", nrow(df_opt), "\n")

# Quick retune
set.seed(123)
rf_opt_tune <- train(
  PDDOY ~ . - Year,
  data      = df_opt,
  method    = "rf",
  metric    = "RMSE",
  tuneGrid  = expand.grid(mtry = c(4, 6, 8, 10)),
  ntree     = 300,
  nodesize  = 5,
  trControl = trainControl(method = "cv", index = fold_list)
)

best_mtry_opt     <- rf_opt_tune$bestTune$mtry
cat("Optimal mtry:", best_mtry_opt, "\n")
cat("Optimal CV RMSE:", round(min(rf_opt_tune$results$RMSE), 3), "\n")

# L2YO loop
years_opt  <- sort(unique(df_opt$Year))
pairs_opt  <- combn(years_opt, 2, simplify = FALSE)
results_opt <- data.frame()

for (pair in pairs_opt) {
  test_years <- pair
  train_data <- df_opt %>% filter(!Year %in% test_years)
  test_data  <- df_opt %>% filter( Year %in% test_years)
  if (nrow(train_data) < 10 || nrow(test_data) < 2) next
  
  m <- tryCatch(
    randomForest(PDDOY ~ . - Year, data = train_data,
                 ntree = 300, mtry = best_mtry_opt,
                 nodesize = 5, maxnodes = 40),
    error = function(e) NULL
  )
  if (is.null(m)) next
  
  pred_tr <- predict(m, train_data)
  pred_te <- predict(m, test_data)
  
  results_opt <- rbind(results_opt, data.frame(
    test_years    = paste(test_years, collapse = "-"),
    yr1           = test_years[1],
    yr2           = test_years[2],
    RF_Train_RMSE = hydroGOF::rmse(pred_tr, train_data$PDDOY),
    RF_Test_RMSE  = hydroGOF::rmse(pred_te, test_data$PDDOY),
    RF_Test_R2    = safe_r2(test_data$PDDOY, pred_te),
    stringsAsFactors = FALSE
  ))
}

# =========================================================
# STEP 8: FINAL COMPARISON TABLE
# =========================================================

cat("\n╔══════════════════════════════════════════════════════╗\n")
cat("║         FINAL MODEL COMPARISON                       ║\n")
cat("╚══════════════════════════════════════════════════════╝\n")

final_comparison <- data.frame(
  Version    = c("V2: Baseline (5 feat)",
                 "V5: + Phenology (30 feat)",
                 "V7: RFE+Boruta optimal"),
  N_features = c(5, 30, length(optimal_features)),
  Train_RMSE = c(7.49, 6.29,
                 round(mean(results_opt$RF_Train_RMSE), 2)),
  Test_RMSE  = c(14.30, 13.00,
                 round(mean(results_opt$RF_Test_RMSE), 2)),
  Test_R2    = c(0.56, 0.58,
                 round(mean(results_opt$RF_Test_R2), 3))
)
print(final_comparison)

