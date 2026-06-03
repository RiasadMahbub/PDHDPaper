# =========================================================
# COMPLETE FINAL REPORT WITH MAE ADDED
# =========================================================

# =========================================================
# 1. REBUILD results_opt WITH MAE INCLUDED
# =========================================================

results_final <- data.frame()

for (pair in combn(sort(unique(df_opt$Year)), 2, simplify = FALSE)) {
  
  test_years <- pair
  tr <- df_opt %>% filter(!Year %in% test_years)
  te <- df_opt %>% filter( Year %in% test_years)
  
  if (nrow(tr) < 10 || nrow(te) < 2) next
  
  m <- tryCatch(
    randomForest(PDDOY ~ . - Year, data = tr,
                 ntree = 300, mtry = best_mtry_opt,
                 nodesize = 5, maxnodes = 40),
    error = function(e) NULL)
  
  if (is.null(m)) next
  
  pred_tr <- predict(m, tr)
  pred_te <- predict(m, te)
  
  results_final <- rbind(results_final, data.frame(
    test_years    = paste(test_years, collapse = "-"),
    yr1           = test_years[1],
    yr2           = test_years[2],
    Train_n       = nrow(tr),
    Test_n        = nrow(te),
    RF_Train_RMSE = hydroGOF::rmse(pred_tr, tr$PDDOY),
    RF_Train_MAE  = hydroGOF::mae(pred_tr,  tr$PDDOY),
    RF_Train_R2   = safe_r2(tr$PDDOY, pred_tr),
    RF_Test_RMSE  = hydroGOF::rmse(pred_te, te$PDDOY),
    RF_Test_MAE   = hydroGOF::mae(pred_te,  te$PDDOY),
    RF_Test_R2    = safe_r2(te$PDDOY, pred_te),
    stringsAsFactors = FALSE
  ))
}

# =========================================================
# 2. FULL 45-PAIR TABLE WITH MAE
# =========================================================

cat("\n--- All 45 Pairs (worst RMSE first) ---\n")
results_final %>%
  arrange(desc(RF_Test_RMSE)) %>%
  dplyr::select(test_years, RF_Train_RMSE, RF_Train_MAE,
                RF_Test_RMSE, RF_Test_MAE, RF_Test_R2, Test_n) %>%
  as.data.frame() %>%
  print()

# =========================================================
# 3. OVERALL SUMMARY WITH RMSE + MAE
# =========================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║              FINAL MODEL PERFORMANCE SUMMARY               ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

overall <- results_final %>%
  summarise(
    Train_RMSE = round(mean(RF_Train_RMSE), 2),
    Train_MAE  = round(mean(RF_Train_MAE),  2),
    Train_R2   = round(mean(RF_Train_R2),   3),
    Test_RMSE  = round(mean(RF_Test_RMSE),  2),
    Test_MAE   = round(mean(RF_Test_MAE),   2),
    Test_R2    = round(mean(RF_Test_R2),    3),
    Test_RMSE_sd = round(sd(RF_Test_RMSE),  2),
    Test_MAE_sd  = round(sd(RF_Test_MAE),   2)
  )

cat("\nSplit        RMSE     MAE      R²\n")
cat("─────────────────────────────────\n")
cat(sprintf("Train        %-8.2f %-8.2f %-6.3f\n",
            overall$Train_RMSE, overall$Train_MAE, overall$Train_R2))
cat(sprintf("Test (mean)  %-8.2f %-8.2f %-6.3f\n",
            overall$Test_RMSE, overall$Test_MAE, overall$Test_R2))
cat(sprintf("Test (SD)    ±%-7.2f ±%-7.2f\n",
            overall$Test_RMSE_sd, overall$Test_MAE_sd))
cat("─────────────────────────────────\n")
cat(sprintf("Overfitting gap (Test-Train RMSE): %.2f days\n",
            overall$Test_RMSE - overall$Train_RMSE))
cat(sprintf("Relative RMSE (RMSE/range):        %.1f%%\n",
            overall$Test_RMSE / 92 * 100))
cat(sprintf("RMSE ≈ MAE ratio:                  %.2f\n",
            overall$Test_RMSE / overall$Test_MAE))

# =========================================================
# 4. PER-YEAR SUMMARY WITH BOTH METRICS
# =========================================================

cat("\n--- Per-Year Mean RMSE + MAE ---\n")
year_summary <- bind_rows(
  results_final %>% dplyr::select(
    year = yr1, RF_Test_RMSE, RF_Test_MAE, RF_Test_R2),
  results_final %>% dplyr::select(
    year = yr2, RF_Test_RMSE, RF_Test_MAE, RF_Test_R2)
) %>%
  group_by(year) %>%
  summarise(
    mean_RMSE = round(mean(RF_Test_RMSE), 2),
    mean_MAE  = round(mean(RF_Test_MAE),  2),
    mean_R2   = round(mean(RF_Test_R2),   3),
    sd_RMSE   = round(sd(RF_Test_RMSE),   2),
    n_pairs   = n(),
    .groups   = "drop"
  ) %>%
  mutate(
    RMSE_MAE_ratio = round(mean_RMSE / mean_MAE, 2),
    hard = year %in% c(2021, 2022)
  ) %>%
  arrange(desc(mean_RMSE))

print(as.data.frame(year_summary))

# =========================================================
# 5. COMPLETE VERSION HISTORY WITH MAE
# =========================================================

cat("\n--- Complete Version History ---\n")
version_history <- data.frame(
  Version = c(
    "V1: 5 feat, random CV",
    "V2: 5 feat, L2YO honest",
    "V3: Regularized",
    "V4: Z-scored (22 feat)",
    "V5: +Phenology (30 feat)",
    "V7: RFE+Boruta (30 feat)",
    "V8: RFE 12 feat"
  ),
  N_feat     = c(5,  5, 22, 22, 30, 30, 12),
  Train_RMSE = c(3.68, 7.49, 9.01, 6.75, 6.29, 6.05,
                 round(mean(results_12$RF_Train_RMSE), 2)),
  Test_RMSE  = c(5.50, 14.30, 14.26, 14.34, 13.00, 13.02,
                 round(mean(results_12$RF_Test_RMSE), 2)),
  Test_R2    = c(0.97, 0.56, 0.48, 0.56, 0.58, 0.577,
                 round(mean(results_12$RF_Test_R2), 3)),
  Valid = c("❌ Leakage", "✅", "✅", "✅", "✅", "✅", "✅")
)
print(version_history)

# =========================================================
# 6. EXCLUSION ANALYSIS — what model achieves on normal years
# =========================================================

cat("\n--- Scenario Analysis ---\n")

excl_2022     <- results_final %>% filter(yr1!=2022 & yr2!=2022)
excl_2021_22  <- results_final %>% filter(yr1!=2022 & yr2!=2022 &
                                            yr1!=2021 & yr2!=2021)
only_2021_22  <- results_final %>% filter(yr1==2022 | yr2==2022 |
                                            yr1==2021 | yr2==2021)

scenarios <- data.frame(
  Scenario = c(
    "All 45 pairs",
    "Excl. 2022 (36 pairs)",
    "Excl. 2021+2022 (28 pairs)",
    "Only 2021+2022 pairs (17 pairs)"
  ),
  N_pairs  = c(45, nrow(excl_2022),
               nrow(excl_2021_22), nrow(only_2021_22)),
  RMSE     = c(round(mean(results_final$RF_Test_RMSE), 2),
               round(mean(excl_2022$RF_Test_RMSE), 2),
               round(mean(excl_2021_22$RF_Test_RMSE), 2),
               round(mean(only_2021_22$RF_Test_RMSE), 2)),
  MAE      = c(round(mean(results_final$RF_Test_MAE), 2),
               round(mean(excl_2022$RF_Test_MAE), 2),
               round(mean(excl_2021_22$RF_Test_MAE), 2),
               round(mean(only_2021_22$RF_Test_MAE), 2)),
  R2       = c(round(mean(results_final$RF_Test_R2), 3),
               round(mean(excl_2022$RF_Test_R2), 3),
               round(mean(excl_2021_22$RF_Test_R2), 3),
               round(mean(only_2021_22$RF_Test_R2), 3))
)
print(scenarios)

# =========================================================
# 7. ENHANCED FINAL PLOTS
# =========================================================

# A) RMSE + MAE dual bar chart by year
year_plot <- bind_rows(
  results_final %>% dplyr::select(
    year = yr1, RF_Test_RMSE, RF_Test_MAE),
  results_final %>% dplyr::select(
    year = yr2, RF_Test_RMSE, RF_Test_MAE)
) %>%
  group_by(year) %>%
  summarise(
    RMSE = mean(RF_Test_RMSE),
    MAE  = mean(RF_Test_MAE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(RMSE, MAE),
               names_to = "metric", values_to = "value") %>%
  mutate(hard = year %in% c(2021, 2022))

ggplot(year_plot,
       aes(x = reorder(factor(year), value),
           y = value, fill = interaction(metric, hard))) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      "RMSE.FALSE" = "#2980b9",
      "MAE.FALSE"  = "#85c1e9",
      "RMSE.TRUE"  = "#c0392b",
      "MAE.TRUE"   = "#f1948a"
    ),
    labels = c("RMSE (normal)", "MAE (normal)",
               "RMSE (hard yr)", "MAE (hard yr)")
  ) +
  geom_hline(yintercept = overall$Test_RMSE,
             linetype = "dashed", color = "orange") +
  geom_hline(yintercept = overall$Test_MAE,
             linetype = "dotted", color = "yellow") +
  coord_flip() +
  labs(title = "Mean L2YO RMSE and MAE by Year",
       subtitle = "Orange dashed = mean RMSE | Yellow dotted = mean MAE",
       x = "Year", y = "Error (days)", fill = "") +
  theme_minimal(base_size = 12)

# B) Obs vs Pred coloured by hard/normal
all_preds <- all_preds %>%
  mutate(hard_year = factor(
    ifelse(Year %in% c(2021, 2022), "Hard (2021/2022)", "Normal"),
    levels = c("Normal", "Hard (2021/2022)")
  ))

ggplot(all_preds, aes(x = obs, y = pred, color = hard_year)) +
  geom_point(alpha = 0.3, size = 1.2) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey50", linewidth = 1) +
  geom_smooth(aes(group = hard_year),
              method = "lm", se = TRUE, linewidth = 1) +
  scale_color_manual(values = c("Normal"         = "#3498db",
                                "Hard (2021/2022)" = "#e74c3c")) +
  labs(title    = "Observed vs Predicted PDDOY by Year Type",
       subtitle = paste("Overall RMSE =",
                        round(hydroGOF::rmse(all_preds$pred,
                                             all_preds$obs), 2),
                        "| MAE =",
                        round(mean(abs(all_preds$pred - all_preds$obs)), 2),
                        "| R² =",
                        round(safe_r2(all_preds$obs, all_preds$pred), 3)),
       x = "Observed PDDOY", y = "Predicted PDDOY",
       color = "") +
  theme_minimal(base_size = 12)

# =========================================================
# 8. WHAT TO REPORT — FINAL PRINTOUT
# =========================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║                  FINAL NUMBERS TO REPORT                   ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")
cat(sprintf("Model:            Random Forest (mtry=%d, nodesize=5,
                  ntree=300, maxnodes=40)\n", best_mtry_opt))
cat(sprintf("Features:         %d (all 30 confirmed by Boruta)\n",
            length(optimal_features)))
cat(sprintf("Validation:       Leave-2-Years-Out (%d pairs, %d years)\n",
            nrow(results_final), length(unique_years)))
cat("─────────────────────────────────────────────────────────────\n")
cat(sprintf("Train RMSE:       %.2f days\n", overall$Train_RMSE))
cat(sprintf("Train MAE:        %.2f days\n", overall$Train_MAE))
cat(sprintf("Train R²:         %.3f\n",      overall$Train_R2))
cat("─────────────────────────────────────────────────────────────\n")
cat(sprintf("Test RMSE:        %.2f ± %.2f days\n",
            overall$Test_RMSE, overall$Test_RMSE_sd))
cat(sprintf("Test MAE:         %.2f ± %.2f days\n",
            overall$Test_MAE, overall$Test_MAE_sd))
cat(sprintf("Test R²:          %.3f\n",  overall$Test_R2))
cat(sprintf("Overfitting gap:  %.2f days (RMSE Test - Train)\n",
            overall$Test_RMSE - overall$Train_RMSE))
cat("─────────────────────────────────────────────────────────────\n")
cat(sprintf("Excl 2021+2022:   RMSE=%.2f  MAE=%.2f  R²=%.3f\n",
            mean(excl_2021_22$RF_Test_RMSE),
            mean(excl_2021_22$RF_Test_MAE),
            mean(excl_2021_22$RF_Test_R2)))
cat(sprintf("Only 2021+2022:   RMSE=%.2f  MAE=%.2f  R²=%.3f\n",
            mean(only_2021_22$RF_Test_RMSE),
            mean(only_2021_22$RF_Test_MAE),
            mean(only_2021_22$RF_Test_R2)))
cat("─────────────────────────────────────────────────────────────\n")
cat(sprintf("Best pair:        %s  RMSE=%.2f\n",
            results_final$test_years[which.min(results_final$RF_Test_RMSE)],
            min(results_final$RF_Test_RMSE)))
cat(sprintf("Worst pair:       %s  RMSE=%.2f\n",
            results_final$test_years[which.max(results_final$RF_Test_RMSE)],
            max(results_final$RF_Test_RMSE)))
cat(sprintf("Relative error:   %.1f%% of planting date range\n",
            overall$Test_RMSE / 92 * 100))

