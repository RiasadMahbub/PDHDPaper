# =========================================================
# DEINES ET AL. 2023 — LEAVE-2-YEARS-OUT VERSION
# Replaces random 80/20 split with honest temporal evaluation
# Features: GCVI params, climate (Apr-Jun), NIR, soil temp
# =========================================================

library(randomForest)
library(hydroGOF)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

safe_r2 <- function(obs, pred) {
  tryCatch(summary(lm(obs ~ pred))$r.squared,
           error = function(e) NA_real_)
}

# =========================================================
# STEP 1: PREPARE DATA — add Year from Field_ID
# =========================================================

# Extract Year from Field_ID (assumes format "FieldName_Year")
deines_results_df$Year <- as.numeric(
  sub(".*_(\\d{4})$", "\\1", deines_results_df$Field_ID)
)

cat("Rows per year:\n")
print(table(deines_results_df$Year))

# =========================================================
# DEINES FEATURE SET (same 16 features as original)
# =========================================================

deines_features <- c(
  "GCVI_b1", "vpd_Jun", "ppt_May", "GCVI_a2",
  "tmax_Apr", "gdd_Mar_May", "tmin_Apr", "ppt_Apr",
  "GCVI_a1", "vpd_May", "GCVI_b2", "nir_b2",
  "soiltemp_May", "nir_a1", "tmax_May", "nir_a2"
)

# =========================================================
# STEP 2: BUILD DATASETS
# =========================================================

df_pd_deines <- deines_results_df %>%
  dplyr::select(all_of(deines_features), PDDOY, Year, Field_ID) %>%
  filter(!is.na(PDDOY)) %>%
  drop_na()

df_hd_deines <- deines_results_df %>%
  dplyr::select(all_of(deines_features), HDDOY, Year, Field_ID) %>%
  filter(!is.na(HDDOY)) %>%
  drop_na()

cat("\nPDDOY dataset:\n")
cat("Rows:", nrow(df_pd_deines), "\n")
print(table(df_pd_deines$Year))

cat("\nHDDOY dataset:\n")
cat("Rows:", nrow(df_hd_deines), "\n")
print(table(df_hd_deines$Year))

# =========================================================
# STEP 3: L2YO FUNCTION — reusable for both targets
# =========================================================

run_deines_l2yo <- function(df_in, target, features,
                            ntree = 100, label) {
  
  feat   <- intersect(features, names(df_in))
  df_use <- df_in %>%
    dplyr::select(all_of(feat), all_of(target), Year) %>%
    drop_na()
  
  years  <- sort(unique(df_use$Year))
  pairs  <- combn(years, 2, simplify = FALSE)
  res    <- data.frame()
  
  cat(sprintf("\n%s: %d rows, %d pairs\n",
              label, nrow(df_use), length(pairs)))
  
  for (pair in pairs) {
    tr <- df_use %>% filter(!Year %in% pair)
    te <- df_use %>% filter( Year %in% pair)
    if (nrow(tr) < 10 || nrow(te) < 2) next
    
    set.seed(123)
    m <- tryCatch(
      randomForest(
        as.formula(paste(target, "~ . - Year")),
        data       = tr,
        ntree      = ntree,
        importance = TRUE
      ),
      error = function(e) NULL
    )
    if (is.null(m)) next
    
    pred_tr <- predict(m, tr)
    pred_te <- predict(m, te)
    
    res <- rbind(res, data.frame(
      test_years    = paste(pair, collapse = "-"),
      yr1           = pair[1],
      yr2           = pair[2],
      Train_n       = nrow(tr),
      Test_n        = nrow(te),
      RF_Train_RMSE = hydroGOF::rmse(pred_tr, tr[[target]]),
      RF_Train_MAE  = hydroGOF::mae(pred_tr,  tr[[target]]),
      RF_Train_NSE  = hydroGOF::NSE(pred_tr,  tr[[target]]),
      RF_Train_R2   = safe_r2(tr[[target]], pred_tr),
      RF_Train_Bias = mean(pred_tr - tr[[target]], na.rm = TRUE),
      RF_Test_RMSE  = hydroGOF::rmse(pred_te, te[[target]]),
      RF_Test_MAE   = hydroGOF::mae(pred_te,  te[[target]]),
      RF_Test_NSE   = hydroGOF::NSE(pred_te,  te[[target]]),
      RF_Test_R2    = safe_r2(te[[target]], pred_te),
      RF_Test_Bias  = mean(pred_te - te[[target]], na.rm = TRUE),
      stringsAsFactors = FALSE
    ))
  }
  
  smry <- res %>%
    summarise(
      Train_RMSE = round(mean(RF_Train_RMSE, na.rm=TRUE), 2),
      Train_MAE  = round(mean(RF_Train_MAE,  na.rm=TRUE), 2),
      Train_NSE  = round(mean(RF_Train_NSE,  na.rm=TRUE), 3),
      Train_R2   = round(mean(RF_Train_R2,   na.rm=TRUE), 3),
      Train_Bias = round(mean(RF_Train_Bias, na.rm=TRUE), 2),
      Test_RMSE  = round(mean(RF_Test_RMSE,  na.rm=TRUE), 2),
      Test_MAE   = round(mean(RF_Test_MAE,   na.rm=TRUE), 2),
      Test_NSE   = round(mean(RF_Test_NSE,   na.rm=TRUE), 3),
      Test_R2    = round(mean(RF_Test_R2,    na.rm=TRUE), 3),
      Test_Bias  = round(mean(RF_Test_Bias,  na.rm=TRUE), 2),
      Test_RMSE_sd = round(sd(RF_Test_RMSE,  na.rm=TRUE), 2),
      Test_MAE_sd  = round(sd(RF_Test_MAE,   na.rm=TRUE), 2)
    )
  
  cat(sprintf("\n%s RESULTS\n", label))
  cat("─────────────────────────────────────────────────\n")
  cat(sprintf("Train  RMSE=%.2f MAE=%.2f NSE=%.3f R²=%.3f\n",
              smry$Train_RMSE, smry$Train_MAE,
              smry$Train_NSE,  smry$Train_R2))
  cat(sprintf("Test   RMSE=%.2f±%.2f MAE=%.2f±%.2f NSE=%.3f R²=%.3f\n",
              smry$Test_RMSE, smry$Test_RMSE_sd,
              smry$Test_MAE,  smry$Test_MAE_sd,
              smry$Test_NSE,  smry$Test_R2))
  cat(sprintf("Gap    %.2f days\n",
              smry$Test_RMSE - smry$Train_RMSE))
  
  list(results = res, summary = smry,
       features = feat, df_used = df_use)
}

# =========================================================
# STEP 4: RUN L2YO FOR BOTH TARGETS
# =========================================================

deines_pd_l2yo <- run_deines_l2yo(
  df_pd_deines, "PDDOY", deines_features,
  ntree = 100, label = "Deines PDDOY L2YO"
)

deines_hd_l2yo <- run_deines_l2yo(
  df_hd_deines, "HDDOY", deines_features,
  ntree = 100, label = "Deines HDDOY L2YO"
)

# =========================================================
# STEP 5: COMPARISON — random split vs L2YO vs our RF
# =========================================================

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║         DEINES MODEL: RANDOM SPLIT vs L2YO               ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")

comparison_deines <- data.frame(
  Target  = c("PDDOY","PDDOY","PDDOY",
              "HDDOY","HDDOY","HDDOY"),
  Model   = c("Deines random split",
              "Deines L2YO (honest)",
              "Our RF V9★ L2YO",
              "Deines random split",
              "Deines L2YO (honest)",
              "Our RF H2★ L2YO"),
  N_feat  = c(16, 16, 15, 16, 16, 10),
  Train_RMSE = c(
    summary_metrics_planting$Train_RMSE_mean,
    deines_pd_l2yo$summary$Train_RMSE,
    6.45,
    summary_metrics_harvest$Train_RMSE_mean,
    deines_hd_l2yo$summary$Train_RMSE,
    5.20
  ),
  Test_RMSE = c(
    test_results_planting$RMSE,
    deines_pd_l2yo$summary$Test_RMSE,
    12.67,
    test_results_harvest$RMSE,
    deines_hd_l2yo$summary$Test_RMSE,
    9.59
  ),
  Test_MAE  = c(
    test_results_planting$MAE,
    deines_pd_l2yo$summary$Test_MAE,
    9.99,
    test_results_harvest$MAE,
    deines_hd_l2yo$summary$Test_MAE,
    7.00
  ),
  Test_R2   = c(
    test_results_planting$R2,
    deines_pd_l2yo$summary$Test_R2,
    0.592,
    test_results_harvest$R2,
    deines_hd_l2yo$summary$Test_R2,
    0.574
  ),
  Test_NSE  = c(
    test_results_planting$NSE,
    deines_pd_l2yo$summary$Test_NSE,
    NA,
    test_results_harvest$NSE,
    deines_hd_l2yo$summary$Test_NSE,
    NA
  ),
  Valid = c("❌ Leakage","✅","✅",
            "❌ Leakage","✅","✅")
)

print(as.data.frame(comparison_deines))

# =========================================================
# STEP 6: PER-YEAR DIFFICULTY
# =========================================================

cat("\n--- Deines PDDOY: Per-Year Mean RMSE ---\n")
bind_rows(
  deines_pd_l2yo$results %>%
    dplyr::select(year=yr1, RF_Test_RMSE),
  deines_pd_l2yo$results %>%
    dplyr::select(year=yr2, RF_Test_RMSE)
) %>%
  group_by(year) %>%
  summarise(mean_RMSE = round(mean(RF_Test_RMSE), 2),
            .groups = "drop") %>%
  arrange(desc(mean_RMSE)) %>%
  as.data.frame() %>%
  print()

cat("\n--- Deines HDDOY: Per-Year Mean RMSE ---\n")
bind_rows(
  deines_hd_l2yo$results %>%
    dplyr::select(year=yr1, RF_Test_RMSE),
  deines_hd_l2yo$results %>%
    dplyr::select(year=yr2, RF_Test_RMSE)
) %>%
  group_by(year) %>%
  summarise(mean_RMSE = round(mean(RF_Test_RMSE), 2),
            .groups = "drop") %>%
  arrange(desc(mean_RMSE)) %>%
  as.data.frame() %>%
  print()

# =========================================================
# STEP 7: HEATMAPS
# =========================================================

make_heatmap <- function(results, title, subtitle) {
  hm <- bind_rows(
    results %>% dplyr::select(yr1, yr2, RF_Test_RMSE),
    results %>% rename(yr1=yr2, yr2=yr1) %>%
      dplyr::select(yr1, yr2, RF_Test_RMSE)
  )
  ggplot(hm, aes(x=factor(yr1), y=factor(yr2),
                 fill=RF_Test_RMSE)) +
    geom_tile(color="white", linewidth=0.6) +
    geom_text(aes(label=round(RF_Test_RMSE,1)),
              size=3, color="white", fontface="bold") +
    scale_fill_viridis_c(option="plasma") +
    labs(title=title, subtitle=subtitle,
         x="Test Year 1", y="Test Year 2",
         fill="RMSE") +
    theme_minimal(base_size=12) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
}

make_heatmap(
  deines_pd_l2yo$results,
  "Deines PDDOY — L2YO Test RMSE",
  paste("Mean RMSE:", deines_pd_l2yo$summary$Test_RMSE,
        "| Random split was:", test_results_planting$RMSE)
)

make_heatmap(
  deines_hd_l2yo$results,
  "Deines HDDOY — L2YO Test RMSE",
  paste("Mean RMSE:", deines_hd_l2yo$summary$Test_RMSE,
        "| Random split was:", test_results_harvest$RMSE)
)

# =========================================================
# STEP 8: VARIABLE IMPORTANCE (L2YO final model)
# =========================================================

get_l2yo_importance <- function(df_in, target, features,
                                ntree, label) {
  feat   <- intersect(features, names(df_in))
  df_use <- df_in %>%
    dplyr::select(all_of(feat), all_of(target), Year) %>%
    drop_na()
  
  set.seed(123)
  m <- randomForest(
    as.formula(paste(target, "~ . - Year")),
    data       = df_use,
    ntree      = ntree,
    importance = TRUE
  )
  
  imp_df <- data.frame(
    Variable = rownames(randomForest::importance(m)),
    IncMSE   = randomForest::importance(m)[, "%IncMSE"]
  ) %>% arrange(desc(IncMSE))
  
  cat(sprintf("\n--- %s Variable Importance ---\n", label))
  print(as.data.frame(imp_df))
  
  ggplot(imp_df,
         aes(x=reorder(Variable, IncMSE),
             y=IncMSE, fill=IncMSE)) +
    geom_col(show.legend=FALSE) +
    scale_fill_viridis_c(option="cividis") +
    coord_flip() +
    labs(title=paste("Variable Importance —", label),
         x="Variable", y="% Inc MSE") +
    theme_minimal(base_size=12)
}

get_l2yo_importance(df_pd_deines, "PDDOY",
                    deines_features, 100,
                    "Deines PDDOY")

get_l2yo_importance(df_hd_deines, "HDDOY",
                    deines_features, 100,
                    "Deines HDDOY")

# =========================================================
# STEP 9: FINAL SUMMARY TABLE FOR PAPER
# =========================================================
