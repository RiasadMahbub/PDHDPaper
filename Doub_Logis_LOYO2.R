# =========================================================
# PDSOSDER MODEL — LEAVE-2-YEARS-OUT VERSION
# Mirrors the RF L2YO approach exactly
# =========================================================

library(dplyr)
library(ggplot2)
library(hydroGOF)

# =========================================================
# HELPER FUNCTIONS (same as before)
# =========================================================

get_cumulative_gdd_at_sos <- function(vi_df, sos_doy) {
  if (is.null(vi_df) ||
      !("cumulative_gdd_from_pddoy" %in% names(vi_df)) ||
      !("doy" %in% names(vi_df))) return(NA)
  row_idx <- which(vi_df$doy == sos_doy)
  if (length(row_idx) == 0) return(NA)
  vi_df$cumulative_gdd_from_pddoy[row_idx[1]]
}

predict_sos_from_gdd_lag <- function(vi_df, target_gdd_lag) {
  if (is.null(vi_df) ||
      !("cumulative_gdd_from_pddoy" %in% names(vi_df)) ||
      !("doy" %in% names(vi_df))) return(NA)
  valid_rows <- which(!is.na(vi_df$cumulative_gdd_from_pddoy))
  if (length(valid_rows) == 0) return(NA)
  valid_gdd <- vi_df$cumulative_gdd_from_pddoy[valid_rows]
  valid_doy <- vi_df$doy[valid_rows]
  closest   <- which.min(abs(valid_gdd - target_gdd_lag))
  valid_doy[closest]
}

safe_r2 <- function(obs, pred) {
  tryCatch(summary(lm(obs ~ pred))$r.squared,
           error = function(e) NA_real_)
}

# =========================================================
# STEP 1: PREPARE DATA
# Need: sos_eos_df with Year, Field_Year, SOS, observed_SOS
#       vi_data_by_field (vi_list_gt20)
# =========================================================

# Extract Year from Field_Year
sos_eos_df$Year <- as.numeric(sub(".*_(\\d{4})$", "\\1",
                                  sos_eos_df$Field_Year))

# Verify
cat("Rows in sos_eos_df:", nrow(sos_eos_df), "\n")
cat("Years available:\n")
print(table(sos_eos_df$Year))
cat("NA in SOS:", sum(is.na(sos_eos_df$SOS)), "\n")
cat("NA in observed_SOS:", sum(is.na(sos_eos_df$observed_SOS)), "\n")

# =========================================================
# STEP 2: LEAVE-2-YEARS-OUT CV
# For each pair of held-out years:
#   Train = all other years
#   Test  = the 2 held-out years
#   The mean GDD lag is computed from training data only
# =========================================================

years    <- sort(unique(sos_eos_df$Year))
pairs    <- combn(years, 2, simplify = FALSE)
results  <- data.frame()

cat(sprintf("\nRunning L2YO for PDSOSDER (%d pairs)...\n",
            length(pairs)))

for (pair in pairs) {
  
  train_df <- sos_eos_df %>% filter(!Year %in% pair)
  test_df  <- sos_eos_df %>% filter( Year %in% pair)
  
  if (nrow(train_df) < 5 || nrow(test_df) < 2) next
  
  # -------------------------------------------------------
  # TRAINING: compute mean GDD lag from training fields
  # -------------------------------------------------------
  gdd_lags_train <- sapply(train_df$Field_Year, function(fy) {
    sos_doy  <- train_df$SOS[train_df$Field_Year == fy]
    vi_data  <- vi_data_by_field[[fy]]
    if (length(sos_doy) == 0 || is.na(sos_doy)) return(NA)
    get_cumulative_gdd_at_sos(vi_data, sos_doy)
  })
  
  mean_gdd_lag <- mean(gdd_lags_train, na.rm = TRUE)
  sd_gdd_lag   <- sd(gdd_lags_train,   na.rm = TRUE)
  
  # -------------------------------------------------------
  # PREDICTION FUNCTION: apply mean lag to any dataset
  # -------------------------------------------------------
  predict_pdsosder <- function(df) {
    df %>%
      rowwise() %>%
      mutate(
        doy_at_mean_gdd  = predict_sos_from_gdd_lag(
          vi_data_by_field[[Field_Year]], mean_gdd_lag),
        gdd_start_doy    = GDD_Accumulation_Start_DOY,
        calculated_lag   = doy_at_mean_gdd - gdd_start_doy,
        Predicted_PDSOSDER = SOS - calculated_lag
      ) %>%
      ungroup()
  }
  
  train_pred <- predict_pdsosder(train_df)
  test_pred  <- predict_pdsosder(test_df)
  
  # -------------------------------------------------------
  # METRICS
  # -------------------------------------------------------
  results <- rbind(results, data.frame(
    test_years      = paste(pair, collapse = "-"),
    yr1             = pair[1],
    yr2             = pair[2],
    Train_n         = nrow(train_df),
    Test_n          = nrow(test_df),
    mean_gdd_lag    = round(mean_gdd_lag, 2),
    sd_gdd_lag      = round(sd_gdd_lag,   2),
    
    # TRAIN
    Train_RMSE = hydroGOF::rmse(
      train_pred$Predicted_PDSOSDER, train_pred$observed_SOS),
    Train_MAE  = hydroGOF::mae(
      train_pred$Predicted_PDSOSDER, train_pred$observed_SOS),
    Train_R2   = safe_r2(
      train_pred$observed_SOS, train_pred$Predicted_PDSOSDER),
    Train_Bias = mean(train_pred$Predicted_PDSOSDER -
                        train_pred$observed_SOS, na.rm=TRUE),
    
    # TEST
    Test_RMSE  = hydroGOF::rmse(
      test_pred$Predicted_PDSOSDER, test_pred$observed_SOS),
    Test_MAE   = hydroGOF::mae(
      test_pred$Predicted_PDSOSDER, test_pred$observed_SOS),
    Test_R2    = safe_r2(
      test_pred$observed_SOS, test_pred$Predicted_PDSOSDER),
    Test_Bias  = mean(test_pred$Predicted_PDSOSDER -
                        test_pred$observed_SOS, na.rm=TRUE),
    
    stringsAsFactors = FALSE
  ))
  
  cat(sprintf("Pair %s | GDD lag=%.1f±%.1f | Train RMSE=%.2f | Test RMSE=%.2f\n",
              paste(pair, collapse="-"),
              mean_gdd_lag, sd_gdd_lag,
              tail(results$Train_RMSE, 1),
              tail(results$Test_RMSE,  1)))
}

# =========================================================
# STEP 3: RESULTS
# =========================================================

cat("\n--- All pairs (worst first) ---\n")
results %>%
  arrange(desc(Test_RMSE)) %>%
  dplyr::select(test_years, mean_gdd_lag, sd_gdd_lag,
                Train_RMSE, Test_RMSE, Test_MAE,
                Test_R2, Test_Bias, Test_n) %>%
  as.data.frame() %>%
  print()

# =========================================================
# STEP 4: SUMMARY
# =========================================================

cat("\n╔══════════════════════════════════════════════════╗\n")
cat("║       PDSOSDER L2YO PERFORMANCE SUMMARY          ║\n")
cat("╚══════════════════════════════════════════════════╝\n")

sosder_summary <- results %>%
  summarise(
    Train_RMSE_mean = round(mean(Train_RMSE, na.rm=TRUE), 2),
    Train_MAE_mean  = round(mean(Train_MAE,  na.rm=TRUE), 2),
    Train_R2_mean   = round(mean(Train_R2,   na.rm=TRUE), 3),
    Train_Bias_mean = round(mean(Train_Bias, na.rm=TRUE), 2),
    Test_RMSE_mean  = round(mean(Test_RMSE,  na.rm=TRUE), 2),
    Test_RMSE_sd    = round(sd(Test_RMSE,    na.rm=TRUE), 2),
    Test_MAE_mean   = round(mean(Test_MAE,   na.rm=TRUE), 2),
    Test_MAE_sd     = round(sd(Test_MAE,     na.rm=TRUE), 2),
    Test_R2_mean    = round(mean(Test_R2,    na.rm=TRUE), 3),
    Test_Bias_mean  = round(mean(Test_Bias,  na.rm=TRUE), 2),
    GDD_lag_mean    = round(mean(mean_gdd_lag, na.rm=TRUE), 2),
    GDD_lag_sd      = round(mean(sd_gdd_lag,   na.rm=TRUE), 2)
  )

cat(sprintf("\nGDD lag from L2YO training: %.2f ± %.2f °C\n",
            sosder_summary$GDD_lag_mean, sosder_summary$GDD_lag_sd))
cat(sprintf("(Paper reports: 763.35 ± 5.55 °C from random split)\n\n"))

cat("Split        RMSE     MAE      R²       Bias\n")
cat("────────────────────────────────────────────\n")
cat(sprintf("Train        %-8.2f %-8.2f %-8.3f %-8.2f\n",
            sosder_summary$Train_RMSE_mean, sosder_summary$Train_MAE_mean,
            sosder_summary$Train_R2_mean,   sosder_summary$Train_Bias_mean))
cat(sprintf("Test (mean)  %-8.2f %-8.2f %-8.3f %-8.2f\n",
            sosder_summary$Test_RMSE_mean, sosder_summary$Test_MAE_mean,
            sosder_summary$Test_R2_mean,   sosder_summary$Test_Bias_mean))
cat(sprintf("Test (SD)    ±%-7.2f ±%-7.2f\n",
            sosder_summary$Test_RMSE_sd, sosder_summary$Test_MAE_sd))

pddoy_range <- 92
cat(sprintf("\nRelative RMSE: %.1f%% of planting date range\n",
            sosder_summary$Test_RMSE_mean / pddoy_range * 100))

# =========================================================
# STEP 5: COMPARE PDSOSDER vs RF PDDOY
# =========================================================

cat("\n--- PDSOSDER vs RF PDDOY (honest L2YO) ---\n")

comparison <- data.frame(
  Model      = c("PDSOSDER (random split)",
                 "PDSOSDER (L2YO honest)",
                 "RF PDDOY V9★ (L2YO)"),
  N_param    = c(1, 1, 15),
  Train_RMSE = c(NA, sosder_summary$Train_RMSE_mean, 6.45),
  Test_RMSE  = c(summary_metrics_df$Test_RMSE,
                 sosder_summary$Test_RMSE_mean, 12.67),
  Test_MAE   = c(summary_metrics_df$Test_MAE,
                 sosder_summary$Test_MAE_mean,  9.99),
  Test_R2    = c(summary_metrics_df$Test_R2,
                 sosder_summary$Test_R2_mean,   0.592),
  Valid      = c("❌ Leakage", "✅", "✅")
)
print(comparison)

# =========================================================
# STEP 6: PER-YEAR DIFFICULTY FOR PDSOSDER
# =========================================================

cat("\n--- Per-Year Mean RMSE (PDSOSDER L2YO) ---\n")

sosder_year <- bind_rows(
  results %>% dplyr::select(year=yr1, Test_RMSE, Test_MAE, Test_R2),
  results %>% dplyr::select(year=yr2, Test_RMSE, Test_MAE, Test_R2)
) %>%
  group_by(year) %>%
  summarise(
    mean_RMSE = round(mean(Test_RMSE), 2),
    mean_MAE  = round(mean(Test_MAE),  2),
    mean_R2   = round(mean(Test_R2),   3),
    .groups   = "drop"
  ) %>%
  arrange(desc(mean_RMSE))

print(as.data.frame(sosder_year))

# =========================================================
# STEP 7: HEATMAP
# =========================================================

heatmap_sosder <- bind_rows(
  results %>% dplyr::select(yr1, yr2, Test_RMSE),
  results %>% rename(yr1=yr2, yr2=yr1) %>%
    dplyr::select(yr1, yr2, Test_RMSE)
)

ggplot(heatmap_sosder,
       aes(x=factor(yr1), y=factor(yr2), fill=Test_RMSE)) +
  geom_tile(color="white", linewidth=0.6) +
  geom_text(aes(label=round(Test_RMSE,1)),
            size=3, color="white", fontface="bold") +
  scale_fill_viridis_c(option="plasma") +
  labs(title   = "PDSOSDER L2YO Test RMSE",
       subtitle = paste("Mean:", round(sosder_summary$Test_RMSE_mean,2),
                        "| GDD lag:", round(sosder_summary$GDD_lag_mean,1),
                        "± ", round(sosder_summary$GDD_lag_sd,1), "°C"),
       x="Test Year 1", y="Test Year 2", fill="RMSE") +
  theme_minimal(base_size=12) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# =========================================================
# STEP 8: GDD LAG STABILITY ACROSS PAIRS
# How consistent is the lag across different training sets?
# =========================================================

cat("\n--- GDD Lag Stability Across L2YO Training Sets ---\n")
cat(sprintf("Mean GDD lag: %.2f °C\n",
            mean(results$mean_gdd_lag, na.rm=TRUE)))
cat(sprintf("SD across pairs: %.2f °C\n",
            sd(results$mean_gdd_lag, na.rm=TRUE)))
cat(sprintf("Min: %.2f | Max: %.2f\n",
            min(results$mean_gdd_lag, na.rm=TRUE),
            max(results$mean_gdd_lag, na.rm=TRUE)))

ggplot(results, aes(x=reorder(test_years, mean_gdd_lag),
                    y=mean_gdd_lag)) +
  geom_col(fill="#2980b9", alpha=0.8) +
  geom_hline(yintercept=mean(results$mean_gdd_lag, na.rm=TRUE),
             linetype="dashed", color="orange", linewidth=1) +
  geom_hline(yintercept=763.35,
             linetype="dotted", color="red", linewidth=1) +
  annotate("text", x=5,
           y=mean(results$mean_gdd_lag, na.rm=TRUE)+10,
           label=sprintf("L2YO mean=%.1f",
                         mean(results$mean_gdd_lag, na.rm=TRUE)),
           color="orange", size=3.5) +
  annotate("text", x=5, y=763.35+10,
           label="Paper=763.35", color="red", size=3.5) +
  coord_flip() +
  labs(title   = "GDD Lag per Training Set (L2YO)",
       subtitle = "Orange=L2YO mean | Red=paper (random split)",
       x="Test Year Pair", y="Mean GDD Lag (°C)") +
  theme_minimal(base_size=11)