# =========================================================
# COMPLETE PIPELINE — LIMP RF RANDOM SPLIT + L2YO
# PDDOY and HDDOY with ALL metrics: RMSE, MAE, R², MBE
# + VARIABLE IMPORTANCE (averaged across all runs/folds)
# + FOUR IMPORTANCE PLOTS
#
# Features:  from Document 1 (correct/refined feature sets)
# ntree/mtry: from Document 1 (correct per-model settings)
# Importance: from Document 2 (stored per run and per fold)
# Plots:      from Document 2 (Part 10 stacked-bar style)
# =========================================================

library(randomForest)
library(hydroGOF)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)
library(ggrepel)
library(patchwork)
library(Metrics)

safe_r2 <- function(obs, pred) {
  tryCatch(summary(lm(obs ~ pred))$r.squared,
           error = function(e) NA_real_)
}

safe_mbe <- function(pred, obs) {
  round(Metrics::bias(obs, pred), 2)
}

# =========================================================
# STEP 0: BUILD df_pd_rfe AND df_hd_rfe
# df_pd_rfe  <- from df          (planting window)
# df_hd_rfe  <- from dfharvest   (harvest window)
# =========================================================

cat("=== STEP 0: Build feature datasets ===\n")

year_signal <- df %>%
  mutate(Year = as.numeric(
    sub(".*_(\\d{4})$", "\\1", Field_Year))) %>%
  group_by(Year) %>%
  summarise(
    year_vpd  = mean(yearcumvpd,  na.rm = TRUE),
    year_gdd  = mean(yearcumgdd,  na.rm = TRUE),
    year_tmin = mean(yearcumtmin, na.rm = TRUE),
    year_RH   = mean(yearcumrh,   na.rm = TRUE),
    year_rad  = mean(yearcumrad,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    z_vpd  = as.numeric(scale(year_vpd)),
    z_gdd  = as.numeric(scale(year_gdd)),
    z_tmin = as.numeric(scale(year_tmin)),
    z_RH   = as.numeric(scale(year_RH)),
    z_rad  = as.numeric(scale(year_rad))
  )

# ---- PDDOY: source is df (planting window cumulatives) ----
df_pd_rfe <- df %>%                          # <-- correct source per Doc1
  mutate(Year = as.numeric(
    sub(".*_(\\d{4})$", "\\1", Field_Year))) %>%
  left_join(year_signal, by = "Year") %>%
  mutate(
    gdd_x_sos  = cum_gdd  * SOS_trs.sos,
    vpd_x_sos  = cum_vpd  * SOS_trs.sos,
    tmin_gdd   = cum_tmin * cum_gdd,
    vpd_x_tmin = cum_vpd  * cum_tmin,
    vpd_gdd    = cum_vpd  * cum_gdd,
    vpd_year_anom  = year_vpd  - mean(year_vpd,  na.rm = TRUE),
    gdd_year_anom  = year_gdd  - mean(year_gdd,  na.rm = TRUE),
    tmin_year_anom = year_tmin - mean(year_tmin, na.rm = TRUE),
    stress_year_index = as.numeric(scale(year_vpd)) *
      as.numeric(scale(year_tmin)),
    climate_energy = year_gdd / (year_vpd + 1),
    vpd_year_rank  = rank(year_vpd)  / n(),
    gdd_year_rank  = rank(year_gdd)  / n(),
    tmin_year_rank = rank(year_tmin) / n()
  ) %>%
  dplyr::select(
    PDDOY, Year,
    SOS_trs.sos, SOS_deriv.sos, UD.UD, SD.SD, DD.DD, RD.RD,
    #Greenup.Greenup,
    POS.pos,
    cum_gdd, cum_vpd, cum_tmin, cum_RH, cum_soiltemp, cum_meansrad,
    avgsoilorg,
    year_vpd, year_gdd, year_tmin, year_RH, year_rad,
    z_vpd, z_gdd, z_tmin, z_RH, z_rad,
    vpd_year_anom, gdd_year_anom, tmin_year_anom,
    vpd_year_rank, gdd_year_rank, tmin_year_rank,
    gdd_x_sos, vpd_x_sos, tmin_gdd, vpd_x_tmin, vpd_gdd,
    stress_year_index, climate_energy
  ) %>%
  filter(!is.na(PDDOY)) %>%
  drop_na()

# ---- HDDOY: source is dfharvest (harvest window cumulatives) ----
df_hd_rfe <- dfharvest %>%                  # <-- correct source per Doc1
  mutate(Year = as.numeric(
    sub(".*_(\\d{4})$", "\\1", Field_Year))) %>%
  left_join(year_signal, by = "Year") %>%
  mutate(
    eos_x_sos      = EOS_trs.eos * SOS_trs.sos,
    tmin_gdd       = cum_tmin    * cum_gdd,
    sd_x_gdd       = SD.SD       * cum_gdd,
    maturity_x_gdd = Maturity.Maturity * cum_gdd,
    vpd_year_anom  = year_vpd  - mean(year_vpd,  na.rm = TRUE),
    gdd_year_anom  = year_gdd  - mean(year_gdd,  na.rm = TRUE),
    tmin_year_anom = year_tmin - mean(year_tmin, na.rm = TRUE),
    stress_year_index = as.numeric(scale(year_vpd)) *
      as.numeric(scale(year_tmin)),
    climate_energy = year_gdd / (year_vpd + 1)
  ) %>%
  dplyr::select(
    HDDOY, Year,
    EOS_trs.eos, EOS_deriv.eos, RD.RD, DD.DD, SD.SD,
    SOS_trs.sos, Dormancy.Dormancy, Maturity.Maturity,
    a1, a2, rau.rau, DOY_max_before_min_fit,
    cum_gdd, cum_vpd, cum_tmin, cum_RH, cum_soiltemp,
    avgsoilorg, avgsoilclay,
    year_vpd, year_gdd, year_tmin, z_vpd, z_gdd, z_tmin,
    vpd_year_anom, gdd_year_anom, tmin_year_anom,
    stress_year_index, climate_energy,
    eos_x_sos, tmin_gdd, sd_x_gdd
  ) %>%
  filter(!is.na(HDDOY)) %>%
  drop_na()

cat("df_pd_rfe rows:", nrow(df_pd_rfe), "\n")
cat("df_hd_rfe rows:", nrow(df_hd_rfe), "\n")
print(table(df_pd_rfe$Year))
print(table(df_hd_rfe$Year))

# ---- Confirmed optimal features (Doc1 — correct refined sets) ----
pd_feats <- intersect(
  c("avgsoilorg", "cum_soiltemp", "cum_tmin",
    #"DD.DD",
    #"gdd_x_sos",
    #"Greenup.Greenup",
    #"RD.RD",
    "UD.UD",
    "SOS_deriv.sos", "SOS_trs.sos"
    #,"tmin_gdd"
  ),
  names(df_pd_rfe))

hd_feats <- intersect(
  c("a1", "a2", "avgsoilorg",
    "cum_gdd", "cum_RH", "cum_soiltemp", "cum_vpd",
    "DOY_max_before_min_fit",
    "EOS_deriv.eos", "EOS_trs.eos",
    "eos_x_sos",
    "RD.RD", "SD.SD", "sd_x_gdd",
    "SOS_trs.sos"),
  names(df_hd_rfe))

cat("\nPDDOY features:", length(pd_feats), "\n")
cat("HDDOY features:", length(hd_feats), "\n")

# =========================================================
# PART 1: RANDOM SPLIT — 100 runs (RMSE + MAE + R² + MBE)
#          + importance stored per run (averaged at end)
# =========================================================

cat("\n=== PART 1: Random Split (100 runs) ===\n")

run_random_split <- function(df_in, target, features,
                             ntree, mtry, n_runs = 100) {
  feat   <- intersect(features, names(df_in))
  df_use <- df_in %>%
    dplyr::select(all_of(feat), all_of(target), Year) %>%
    drop_na()
  
  set.seed(123)
  n        <- nrow(df_use)
  test_idx <- sample(1:n, size = floor(0.2 * n))
  test_df  <- df_use[ test_idx, ]
  remain   <- df_use[-test_idx, ]
  
  per_run      <- data.frame()
  var_imp_list <- list()   # collect importance every run
  
  for (i in 1:n_runs) {
    set.seed(100 + i)
    nr     <- nrow(remain)
    tr_idx <- sample(1:nr, size = floor(0.75 * nr))
    tr     <- remain[ tr_idx, ]
    vl     <- remain[-tr_idx, ]
    
    m <- tryCatch(
      randomForest(as.formula(paste(target, "~ . - Year")),
                   data = tr, ntree = ntree, mtry = mtry,
                   nodesize = 5, importance = TRUE),   # importance = TRUE
      error = function(e) NULL)
    if (is.null(m)) next
    
    pred_tr <- predict(m, tr)
    pred_vl <- predict(m, vl)
    
    per_run <- rbind(per_run, data.frame(
      run        = i,
      Train_RMSE = hydroGOF::rmse(pred_tr, tr[[target]]),
      Train_MAE  = hydroGOF::mae( pred_tr, tr[[target]]),
      Train_R2   = safe_r2(tr[[target]], pred_tr),
      Train_MBE  = safe_mbe(pred_tr, tr[[target]]),
      Val_RMSE   = hydroGOF::rmse(pred_vl, vl[[target]]),
      Val_MAE    = hydroGOF::mae( pred_vl, vl[[target]]),
      Val_R2     = safe_r2(vl[[target]], pred_vl),
      Val_MBE    = safe_mbe(pred_vl, vl[[target]])
    ))
    
    # Store importance per run
    vi <- randomForest::importance(m)
    var_imp_list[[i]] <- data.frame(
      variable      = rownames(vi),
      X.IncMSE      = vi[, "%IncMSE"],
      IncNodePurity = vi[, "IncNodePurity"],
      run           = i,
      row.names     = NULL
    )
  }
  
  # Average importance across all runs
  all_vi     <- do.call(rbind, var_imp_list)
  vi_summary <- all_vi %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      MeanIncMSE        = mean(X.IncMSE,     na.rm = TRUE),
      SD_IncMSE         = sd(X.IncMSE,       na.rm = TRUE),
      MeanIncNodePurity = mean(IncNodePurity, na.rm = TRUE),
      SD_IncNodePurity  = sd(IncNodePurity,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(MeanIncMSE))
  
  # Final model on full 80% for test
  m_final   <- randomForest(
    as.formula(paste(target, "~ . - Year")),
    data = remain, ntree = ntree, mtry = mtry, nodesize = 5)
  pred_test <- predict(m_final, test_df)
  
  smry <- per_run %>% summarise(
    Train_RMSE_mean = round(mean(Train_RMSE), 2),
    Train_RMSE_sd   = round(sd(Train_RMSE),   2),
    Train_MAE_mean  = round(mean(Train_MAE),  2),
    Train_MAE_sd    = round(sd(Train_MAE),    2),
    Train_R2_mean   = round(mean(Train_R2),   3),
    Train_R2_sd     = round(sd(Train_R2),     3),
    Train_MBE_mean  = round(mean(Train_MBE),  2),
    Train_MBE_sd    = round(sd(Train_MBE),    2),
    Val_RMSE_mean   = round(mean(Val_RMSE),   2),
    Val_RMSE_sd     = round(sd(Val_RMSE),     2),
    Val_MAE_mean    = round(mean(Val_MAE),    2),
    Val_MAE_sd      = round(sd(Val_MAE),      2),
    Val_R2_mean     = round(mean(Val_R2),     3),
    Val_R2_sd       = round(sd(Val_R2),       3),
    Val_MBE_mean    = round(mean(Val_MBE),    2),
    Val_MBE_sd      = round(sd(Val_MBE),      2)
  )
  
  test_metrics <- data.frame(
    Test_RMSE = round(hydroGOF::rmse(pred_test, test_df[[target]]), 2),
    Test_MAE  = round(hydroGOF::mae( pred_test, test_df[[target]]), 2),
    Test_R2   = round(safe_r2(test_df[[target]], pred_test),        3),
    Test_MBE  = safe_mbe(pred_test, test_df[[target]])
  )
  
  list(per_run    = per_run,
       summary    = smry,
       test       = test_metrics,
       vi_summary = vi_summary,   # averaged importance
       features   = feat,
       df_used    = df_use,
       ntree      = ntree,
       mtry       = mtry)
}

# Doc1 ntree/mtry values
pd_random <- run_random_split(df_pd_rfe, "PDDOY", pd_feats, 350, 8)
hd_random <- run_random_split(df_hd_rfe, "HDDOY", hd_feats, 500, 3)

cat("\n--- PDDOY Random Split ---\n")
cat(sprintf("Train  RMSE=%.2f±%.2f MAE=%.2f±%.2f R²=%.3f MBE=%.2f\n",
            pd_random$summary$Train_RMSE_mean, pd_random$summary$Train_RMSE_sd,
            pd_random$summary$Train_MAE_mean,  pd_random$summary$Train_MAE_sd,
            pd_random$summary$Train_R2_mean,   pd_random$summary$Train_MBE_mean))
cat(sprintf("Val    RMSE=%.2f±%.2f MAE=%.2f±%.2f R²=%.3f MBE=%.2f\n",
            pd_random$summary$Val_RMSE_mean,   pd_random$summary$Val_RMSE_sd,
            pd_random$summary$Val_MAE_mean,    pd_random$summary$Val_MAE_sd,
            pd_random$summary$Val_R2_mean,     pd_random$summary$Val_MBE_mean))
cat(sprintf("Test   RMSE=%.2f MAE=%.2f R²=%.3f MBE=%.2f\n",
            pd_random$test$Test_RMSE, pd_random$test$Test_MAE,
            pd_random$test$Test_R2,   pd_random$test$Test_MBE))

cat("\n--- HDDOY Random Split ---\n")
cat(sprintf("Train  RMSE=%.2f±%.2f MAE=%.2f±%.2f R²=%.3f MBE=%.2f\n",
            hd_random$summary$Train_RMSE_mean, hd_random$summary$Train_RMSE_sd,
            hd_random$summary$Train_MAE_mean,  hd_random$summary$Train_MAE_sd,
            hd_random$summary$Train_R2_mean,   hd_random$summary$Train_MBE_mean))
cat(sprintf("Val    RMSE=%.2f±%.2f MAE=%.2f±%.2f R²=%.3f MBE=%.2f\n",
            hd_random$summary$Val_RMSE_mean,   hd_random$summary$Val_RMSE_sd,
            hd_random$summary$Val_MAE_mean,    hd_random$summary$Val_MAE_sd,
            hd_random$summary$Val_R2_mean,     hd_random$summary$Val_MBE_mean))
cat(sprintf("Test   RMSE=%.2f MAE=%.2f R²=%.3f MBE=%.2f\n",
            hd_random$test$Test_RMSE, hd_random$test$Test_MAE,
            hd_random$test$Test_R2,   hd_random$test$Test_MBE))

# =========================================================
# PART 2: L2YO (RMSE + MAE + R² + MBE)
#          + importance stored per fold (averaged at end)
# =========================================================

cat("\n=== PART 2: L2YO ===\n")

run_final_l2yo <- function(df_in, target, features,
                           ntree, mtry, label) {
  feat   <- intersect(features, names(df_in))
  df_use <- df_in %>%
    dplyr::select(all_of(feat), all_of(target), Year) %>%
    drop_na()
  pairs        <- combn(sort(unique(df_use$Year)), 2, simplify = FALSE)
  res          <- data.frame()
  var_imp_list <- list()   # collect importance every fold
  fold_idx     <- 0
  
  for (pair in pairs) {
    tr <- df_use %>% filter(!Year %in% pair)
    te <- df_use %>% filter( Year %in% pair)
    if (nrow(tr) < 10 || nrow(te) < 2) next
    fold_idx <- fold_idx + 1
    
    m <- tryCatch(
      randomForest(as.formula(paste(target, "~ . - Year")),
                   data = tr, ntree = ntree, mtry = mtry,
                   nodesize = 5, maxnodes = 40,
                   importance = TRUE),    # importance = TRUE
      error = function(e) NULL)
    if (is.null(m)) next
    
    pred_tr <- predict(m, tr)
    pred_te <- predict(m, te)
    
    res <- rbind(res, data.frame(
      test_years    = paste(pair, collapse = "-"),
      yr1 = pair[1], yr2 = pair[2],
      Train_n = nrow(tr), Test_n = nrow(te),
      RF_Train_RMSE = hydroGOF::rmse(pred_tr, tr[[target]]),
      RF_Train_MAE  = hydroGOF::mae( pred_tr, tr[[target]]),
      RF_Train_R2   = safe_r2(tr[[target]], pred_tr),
      RF_Train_MBE  = safe_mbe(pred_tr, tr[[target]]),
      RF_Test_RMSE  = hydroGOF::rmse(pred_te, te[[target]]),
      RF_Test_MAE   = hydroGOF::mae( pred_te, te[[target]]),
      RF_Test_R2    = safe_r2(te[[target]], pred_te),
      RF_Test_MBE   = safe_mbe(pred_te, te[[target]]),
      stringsAsFactors = FALSE))
    
    # Store importance per fold
    vi <- randomForest::importance(m)
    var_imp_list[[fold_idx]] <- data.frame(
      variable      = rownames(vi),
      X.IncMSE      = vi[, "%IncMSE"],
      IncNodePurity = vi[, "IncNodePurity"],
      fold          = fold_idx,
      row.names     = NULL
    )
  }
  
  # Average importance across all L2YO folds
  all_vi     <- do.call(rbind, var_imp_list)
  vi_summary <- all_vi %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      MeanIncMSE        = mean(X.IncMSE,     na.rm = TRUE),
      SD_IncMSE         = sd(X.IncMSE,       na.rm = TRUE),
      MeanIncNodePurity = mean(IncNodePurity, na.rm = TRUE),
      SD_IncNodePurity  = sd(IncNodePurity,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(MeanIncMSE))
  
  smry <- res %>% summarise(
    Train_RMSE   = round(mean(RF_Train_RMSE), 2),
    Train_MAE    = round(mean(RF_Train_MAE),  2),
    Train_R2     = round(mean(RF_Train_R2),   3),
    Train_MBE    = round(mean(RF_Train_MBE),  2),
    Test_RMSE    = round(mean(RF_Test_RMSE),  2),
    Test_MAE     = round(mean(RF_Test_MAE),   2),
    Test_R2      = round(mean(RF_Test_R2),    3),
    Test_MBE     = round(mean(RF_Test_MBE),   2),
    Test_RMSE_sd = round(sd(RF_Test_RMSE),    2),
    Test_MAE_sd  = round(sd(RF_Test_MAE),     2),
    Test_MBE_sd  = round(sd(RF_Test_MBE),     2)
  )
  
  cat(sprintf("\n%s\n", label))
  cat("─────────────────────────────────────────\n")
  cat(sprintf("ntree=%d mtry=%d feat=%d folds=%d\n",
              ntree, mtry, length(feat), fold_idx))
  cat(sprintf("Train RMSE=%.2f MAE=%.2f R²=%.3f MBE=%.2f\n",
              smry$Train_RMSE, smry$Train_MAE,
              smry$Train_R2,   smry$Train_MBE))
  cat(sprintf("Test  RMSE=%.2f±%.2f MAE=%.2f±%.2f R²=%.3f MBE=%.2f±%.2f\n",
              smry$Test_RMSE,  smry$Test_RMSE_sd,
              smry$Test_MAE,   smry$Test_MAE_sd,
              smry$Test_R2,    smry$Test_MBE,
              smry$Test_MBE_sd))
  cat(sprintf("Gap   %.2f days\n",
              smry$Test_RMSE - smry$Train_RMSE))
  
  list(results    = res,
       summary    = smry,
       vi_summary = vi_summary,   # averaged importance
       features   = feat,
       df_used    = df_use,
       ntree      = ntree,
       mtry       = mtry)
}

# Doc1 ntree/mtry values (450/3 for PD, 200/4 for HD)
pd_final <- run_final_l2yo(
  df_pd_rfe, "PDDOY", pd_feats, 450, 3,
  "PDDOY L2YO (ntree=450 mtry=3)")

hd_final <- run_final_l2yo(
  df_hd_rfe, "HDDOY", hd_feats, 200, 4,
  "HDDOY L2YO (ntree=200 mtry=4)")

# =========================================================
# PART 3: MASTER COMPARISON TABLE
# =========================================================

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║          RANDOM SPLIT vs L2YO — ALL METRICS              ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")

comparison <- data.frame(
  Target     = c("PDDOY", "PDDOY", "HDDOY", "HDDOY"),
  Method     = c("Random Split", "L2YO", "Random Split", "L2YO"),
  Valid      = c("❌", "✅", "❌", "✅"),
  Train_RMSE = c(pd_random$summary$Train_RMSE_mean, pd_final$summary$Train_RMSE,
                 hd_random$summary$Train_RMSE_mean, hd_final$summary$Train_RMSE),
  Train_MAE  = c(pd_random$summary$Train_MAE_mean,  pd_final$summary$Train_MAE,
                 hd_random$summary$Train_MAE_mean,  hd_final$summary$Train_MAE),
  Train_R2   = c(pd_random$summary$Train_R2_mean,   pd_final$summary$Train_R2,
                 hd_random$summary$Train_R2_mean,   hd_final$summary$Train_R2),
  Train_MBE  = c(pd_random$summary$Train_MBE_mean,  pd_final$summary$Train_MBE,
                 hd_random$summary$Train_MBE_mean,  hd_final$summary$Train_MBE),
  Val_RMSE   = c(pd_random$summary$Val_RMSE_mean,   NA,
                 hd_random$summary$Val_RMSE_mean,   NA),
  Val_MAE    = c(pd_random$summary$Val_MAE_mean,    NA,
                 hd_random$summary$Val_MAE_mean,    NA),
  Val_R2     = c(pd_random$summary$Val_R2_mean,     NA,
                 hd_random$summary$Val_R2_mean,     NA),
  Val_MBE    = c(pd_random$summary$Val_MBE_mean,    NA,
                 hd_random$summary$Val_MBE_mean,    NA),
  Test_RMSE  = c(pd_random$test$Test_RMSE, pd_final$summary$Test_RMSE,
                 hd_random$test$Test_RMSE, hd_final$summary$Test_RMSE),
  Test_MAE   = c(pd_random$test$Test_MAE,  pd_final$summary$Test_MAE,
                 hd_random$test$Test_MAE,  hd_final$summary$Test_MAE),
  Test_R2    = c(pd_random$test$Test_R2,   pd_final$summary$Test_R2,
                 hd_random$test$Test_R2,   hd_final$summary$Test_R2),
  Test_MBE   = c(pd_random$test$Test_MBE,  pd_final$summary$Test_MBE,
                 hd_random$test$Test_MBE,  hd_final$summary$Test_MBE)
)

print(as.data.frame(comparison))

# =========================================================
# PART 4: PER-YEAR L2YO RMSE + MBE
# =========================================================

cat("\n=== PART 4: Per-year L2YO metrics ===\n")

pd_yr <- bind_rows(
  pd_final$results %>% dplyr::select(year = yr1, RF_Test_RMSE, RF_Test_MBE),
  pd_final$results %>% dplyr::select(year = yr2, RF_Test_RMSE, RF_Test_MBE)
) %>% group_by(year) %>%
  summarise(
    mean_RMSE = round(mean(RF_Test_RMSE), 2),
    sd_RMSE   = round(sd(RF_Test_RMSE),   2),
    mean_MBE  = round(mean(RF_Test_MBE),  2),
    sd_MBE    = round(sd(RF_Test_MBE),    2),
    n_pairs   = n(),
    .groups = "drop") %>%
  arrange(desc(mean_RMSE))

hd_yr <- bind_rows(
  hd_final$results %>% dplyr::select(year = yr1, RF_Test_RMSE, RF_Test_MBE),
  hd_final$results %>% dplyr::select(year = yr2, RF_Test_RMSE, RF_Test_MBE)
) %>% group_by(year) %>%
  summarise(
    mean_RMSE = round(mean(RF_Test_RMSE), 2),
    sd_RMSE   = round(sd(RF_Test_RMSE),   2),
    mean_MBE  = round(mean(RF_Test_MBE),  2),
    sd_MBE    = round(sd(RF_Test_MBE),    2),
    n_pairs   = n(),
    .groups = "drop") %>%
  arrange(desc(mean_RMSE))

cat("\n--- PDDOY per-year L2YO ---\n"); print(as.data.frame(pd_yr))
cat("\n--- HDDOY per-year L2YO ---\n"); print(as.data.frame(hd_yr))

# =========================================================
# PART 5: CLIMATE vs RMSE CORRELATIONS
# =========================================================

cat("\n=== PART 5: Climate anomaly vs RMSE ===\n")

pd_yr_clim <- pd_yr %>% left_join(year_signal, by = c("year" = "Year"))
hd_yr_clim <- hd_yr %>% left_join(year_signal, by = c("year" = "Year"))

clim_vars <- c("year_vpd", "year_gdd", "year_tmin", "year_RH", "year_rad",
               "z_vpd",    "z_gdd",    "z_tmin",    "z_RH",    "z_rad")

compute_cors <- function(df, rmse_col = "mean_RMSE", vars, label) {
  out <- data.frame()
  for (v in vars) {
    if (!v %in% names(df)) next
    ct <- tryCatch(cor.test(df[[rmse_col]], df[[v]]), error = function(e) NULL)
    if (is.null(ct)) next
    out <- rbind(out, data.frame(
      Variable = v,
      r        = round(ct$estimate, 3),
      p_value  = round(ct$p.value, 4),
      Sig      = ifelse(ct$p.value < 0.01, "***",
                        ifelse(ct$p.value < 0.05, "**",
                               ifelse(ct$p.value < 0.10, "*", ""))),
      stringsAsFactors = FALSE))
  }
  out <- out %>% arrange(desc(abs(r)))
  cat(sprintf("\n--- %s: RMSE vs Climate ---\n", label))
  print(as.data.frame(out))
  out
}

pd_cors <- compute_cors(pd_yr_clim, "mean_RMSE", clim_vars, "PDDOY")
hd_cors <- compute_cors(hd_yr_clim, "mean_RMSE", clim_vars, "HDDOY")

# =========================================================
# PART 6: YEAR DIFFICULTY TABLE (RMSE + MBE + climate)
# =========================================================

cat("\n=== PART 6: Year difficulty table ===\n")

pd_difficulty <- pd_yr %>%
  left_join(year_signal %>%
              dplyr::select(Year, z_vpd, z_gdd, z_tmin,
                            year_vpd, year_gdd, year_tmin),
            by = c("year" = "Year")) %>%
  mutate(
    stress    = z_vpd * (-z_tmin),
    RMSE_rank = rank(desc(mean_RMSE)),
    MBE_rank  = rank(desc(abs(mean_MBE)))
  )

hd_difficulty <- hd_yr %>%
  left_join(year_signal %>%
              dplyr::select(Year, z_vpd, z_gdd, z_tmin,
                            year_vpd, year_gdd, year_tmin),
            by = c("year" = "Year")) %>%
  mutate(
    stress    = z_vpd * (-z_tmin),
    RMSE_rank = rank(desc(mean_RMSE)),
    MBE_rank  = rank(desc(abs(mean_MBE)))
  )

cat("\n--- PDDOY Year Difficulty (RMSE + MBE + Climate) ---\n")
print(as.data.frame(pd_difficulty))
cat("\n--- HDDOY Year Difficulty (RMSE + MBE + Climate) ---\n")
print(as.data.frame(hd_difficulty))

# =========================================================
# PART 7: SCATTER PLOTS
# =========================================================

cat("\n=== PART 7: Scatter plots ===\n")

make_scatter <- function(df, x_var, y_var = "mean_RMSE",
                         label_var = "year",
                         title = "", color = "#2980b9") {
  if (!x_var %in% names(df)) return(NULL)
  valid <- complete.cases(df[[x_var]], df[[y_var]])
  r_val <- round(cor(df[[x_var]][valid], df[[y_var]][valid]), 3)
  p_val <- tryCatch(
    round(cor.test(df[[x_var]][valid], df[[y_var]][valid])$p.value, 4),
    error = function(e) NA)
  ggplot(df[valid, ], aes_string(x = x_var, y = y_var)) +
    geom_point(color = color, size = 4, alpha = 0.85) +
    geom_smooth(method = "lm", se = TRUE, color = "grey30",
                linetype = "dashed", linewidth = 0.9) +
    ggrepel::geom_text_repel(aes_string(label = label_var),
                             size = 3.5, color = "grey30", max.overlaps = 15) +
    annotate("text", x = Inf, y = Inf,
             label = sprintf("r = %.3f\np = %.4f", r_val, p_val),
             hjust = 1.1, vjust = 1.3, size = 4) +
    labs(title = title,
         x = gsub("year_|z_", "", x_var),
         y = "Mean L2YO Test RMSE (days)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 11))
}

top_pd <- pd_cors %>% filter(!grepl("^z_", Variable)) %>% head(4) %>% pull(Variable)
top_hd <- hd_cors %>% filter(!grepl("^z_", Variable)) %>% head(4) %>% pull(Variable)

pd_plots <- lapply(top_pd, function(v)
  make_scatter(pd_yr_clim, v,
               title = paste("PDDOY vs", gsub("year_", "", v)),
               color = "#2980b9"))
pd_plots <- pd_plots[!sapply(pd_plots, is.null)]

hd_plots <- lapply(top_hd, function(v)
  make_scatter(hd_yr_clim, v,
               title = paste("HDDOY vs", gsub("year_", "", v)),
               color = "#e67e22"))
hd_plots <- hd_plots[!sapply(hd_plots, is.null)]

if (length(pd_plots) >= 2)
  print(wrap_plots(pd_plots, ncol = 2) +
          plot_annotation(
            title = "PDDOY: Year Difficulty vs Climate (L2YO)",
            theme = theme(plot.title = element_text(face = "bold", size = 13))))

if (length(hd_plots) >= 2)
  print(wrap_plots(hd_plots, ncol = 2) +
          plot_annotation(
            title = "HDDOY: Year Difficulty vs Climate (L2YO)",
            theme = theme(plot.title = element_text(face = "bold", size = 13))))

# =========================================================
# PART 8: CLIMATE ANOMALY HEATMAP
# =========================================================

yr_heat <- year_signal %>%
  dplyr::select(Year, z_vpd, z_gdd, z_tmin, z_RH, z_rad) %>%
  pivot_longer(-Year, names_to = "Variable", values_to = "Z_score") %>%
  mutate(Variable = gsub("z_", "", Variable))

print(
  ggplot(yr_heat, aes(x = factor(Year), y = Variable, fill = Z_score)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(Z_score, 1)),
              size = 3, color = "white", fontface = "bold") +
    scale_fill_gradient2(low = "#2980b9", mid = "grey20",
                         high = "#e74c3c", midpoint = 0) +
    labs(title    = "Year Climate Anomaly Profile (Z-scores)",
         subtitle = "Red=above average | Blue=below average",
         x = "Year", y = NULL, fill = "Z-score") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

# =========================================================
# PART 9: PAIR-LEVEL CLIMATE DISTANCE vs RMSE
# =========================================================

cat("\n=== PART 9: Pair-level climate distance ===\n")

enrich_pairs <- function(results_df, yr_signal) {
  df <- results_df
  for (v in c("z_vpd", "z_gdd", "z_tmin")) {
    y1 <- yr_signal[[v]][match(df$yr1, yr_signal$Year)]
    y2 <- yr_signal[[v]][match(df$yr2, yr_signal$Year)]
    df[[paste0(v, "_diff")]] <- abs(y1 - y2)
    df[[paste0(v, "_mean")]] <- (y1 + y2) / 2
  }
  df
}

pd_pairs <- enrich_pairs(pd_final$results, year_signal)
hd_pairs <- enrich_pairs(hd_final$results, year_signal)
diff_cols <- c("z_vpd_diff", "z_gdd_diff", "z_tmin_diff")

cat("\n--- PDDOY: Pair RMSE vs climate distance ---\n")
for (v in diff_cols) {
  r <- cor(pd_pairs$RF_Test_RMSE, pd_pairs[[v]], use = "complete.obs")
  cat(sprintf("%-20s r = %.3f\n", v, r))
}

cat("\n--- HDDOY: Pair RMSE vs climate distance ---\n")
for (v in diff_cols) {
  r <- cor(hd_pairs$RF_Test_RMSE, hd_pairs[[v]], use = "complete.obs")
  cat(sprintf("%-20s r = %.3f\n", v, r))
}

# ---- Pair-level scatter plots (from Doc1 appendix) ----
library(tidyverse)
library(broom)

pd_pairs_long <- pd_pairs %>%
  dplyr::select(RF_Test_RMSE, z_vpd_mean, z_gdd_mean, z_tmin_mean) %>%
  pivot_longer(cols = c(z_vpd_mean, z_gdd_mean, z_tmin_mean),
               names_to = "variable", values_to = "value")

stats_pd <- pd_pairs_long %>%
  group_by(variable) %>%
  summarise(cor_test = list(cor.test(value, RF_Test_RMSE)), .groups = "drop") %>%
  mutate(
    r     = map_dbl(cor_test, ~ .x$estimate),
    p     = map_dbl(cor_test, ~ .x$p.value),
    label = paste0("r = ", round(r, 3), "\np = ", signif(p, 3))
  )

print(
  ggplot(pd_pairs_long, aes(x = value, y = RF_Test_RMSE)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    facet_wrap(~ variable, ncol = 1, scales = "free_x") +
    theme_classic() +
    labs(x = "Climate Driver (z-score mean)", y = "RF Test RMSE (PD)") +
    geom_text(data = stats_pd, aes(x = -Inf, y = Inf, label = label),
              inherit.aes = FALSE, hjust = -0.1, vjust = 1.2, size = 4)
)
stats_pd %>% dplyr::select(variable, r, p) %>% print()

hd_pairs_long <- hd_pairs %>%
  dplyr::select(RF_Test_RMSE, z_vpd_mean, z_gdd_mean, z_tmin_mean) %>%
  pivot_longer(cols = c(z_vpd_mean, z_gdd_mean, z_tmin_mean),
               names_to = "variable", values_to = "value")

stats_hd <- hd_pairs_long %>%
  group_by(variable) %>%
  summarise(cor_test = list(cor.test(value, RF_Test_RMSE)), .groups = "drop") %>%
  mutate(
    r     = map_dbl(cor_test, ~ .x$estimate),
    p     = map_dbl(cor_test, ~ .x$p.value),
    label = paste0("r = ", round(r, 3), "\np = ", signif(p, 3))
  )

print(
  ggplot(hd_pairs_long, aes(x = value, y = RF_Test_RMSE)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    facet_wrap(~ variable, ncol = 1, scales = "free_x") +
    theme_classic() +
    labs(x = "Climate Driver (z-score mean)", y = "RF Test RMSE (HD)") +
    geom_text(data = stats_hd, aes(x = -Inf, y = Inf, label = label),
              inherit.aes = FALSE, hjust = -0.1, vjust = 1.2, size = 4)
)
stats_hd %>% dplyr::select(variable, r, p) %>% print()

# =========================================================
# SAVE OUTPUTS
# =========================================================

out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"

write.csv(comparison,
          file.path(out_dir, "LIMP_RandomVsL2YO_allmetrics.csv"),     row.names = FALSE)
write.csv(pd_difficulty,
          file.path(out_dir, "PDDOY_year_difficulty_RMSE_MBE_climate.csv"), row.names = FALSE)
write.csv(hd_difficulty,
          file.path(out_dir, "HDDOY_year_difficulty_RMSE_MBE_climate.csv"), row.names = FALSE)

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║                  FINAL SUMMARY                           ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")
print(as.data.frame(comparison))

cat("\n✓ Done. Objects ready: pd_final, hd_final, pd_random, hd_random\n")
cat("  Each object now contains $vi_summary with averaged importance.\n")

# =========================================================
# PART 10: FOUR VARIABLE IMPORTANCE PLOTS
#          (stacked-bar style, averaged across runs/folds)
# =========================================================

cat("\n=== PART 10: Variable Importance Plots ===\n")

# Readable label lookup
get_readable_label <- function(var_name) {
  lookup <- c(
    "SOS_trs.sos"            = "SOS[TRS]",
    "SOS_deriv.sos"          = "SOS[Deriv]",
    "EOS_trs.eos"            = "EOS[TRS]",
    "EOS_deriv.eos"          = "EOS[Deriv]",
    "UD.UD"                  = "UD",
    "DD.DD"                  = "DD",
    "RD.RD"                  = "RD",
    "SD.SD"                  = "SD",
    #"Greenup.Greenup"        = "Greenup",
    "Dormancy.Dormancy"      = "Dormancy",
    "Maturity.Maturity"      = "Maturity",
    "POS.pos"                = "POS",
    "a1"                     = "kNDVI[a1]",
    "a2"                     = "kNDVI[a2]",
    "a3.a3"                  = "kNDVI[a3]",
    "rau.rau"                = "kNDVI[rau]",
    "DOY_max_before_min_fit" = "DOY[earlymin_kNDVI]",
    "cum_gdd"                = "GDD[cum]",
    "cum_vpd"                = "VPD[cum]",
    "cum_tmin"               = "AirTmin[cum]",
    "cum_tmax"               = "AirTmax[cum]",
    "cum_RH"                 = "RH[cum]",
    "cum_soiltemp"           = "SoilTmean[cum]",
    "cum_meansrad"           = "Srad[cum]",
    "avgsoilorg"             = "SOC[mean]",
    "avgsoilclay"            = "Clay[mean]",
    "gdd_x_sos"              = "GDD%*%SOS",
    "tmin_gdd"               = "Tmin%*%GDD",
    "eos_x_sos"              = "EOS[TRS]%*%SOS[TRS]",
    "vpd_x_sos"              = "VPD%*%SOS",
    "vpd_x_tmin"             = "VPD%*%Tmin",
    "vpd_gdd"                = "VPD%*%GDD",
    "sd_x_gdd"               = "SD%*%GDD",
    "year_vpd"               = "VPD[year]",
    "year_gdd"               = "GDD[year]",
    "year_tmin"              = "Tmin[year]",
    "year_RH"                = "RH[year]",
    "year_rad"               = "Srad[year]",
    "z_vpd"                  = "zVPD",
    "z_gdd"                  = "zGDD",
    "z_tmin"                 = "zTmin",
    "z_RH"                   = "zRH",
    "z_rad"                  = "zSrad"
  )
  if (var_name %in% names(lookup)) lookup[[var_name]] else var_name
}

# Plot builder (stacked-bar, same as harvest/planting style)
make_importance_plot <- function(vi_summary, plot_title,
                                 gini_scale    = 5000,
                                 bar_color_mse  = "#3C5488FF",
                                 bar_color_gini = "#9C51B6") {
  
  vi_combined <- vi_summary %>%
    rename(`%IncMSE` = MeanIncMSE, Gini = MeanIncNodePurity) %>%
    mutate(
      Gini_scaled  = Gini / gini_scale,
      Total_scaled = `%IncMSE` + Gini_scaled
    )
  
  vi_combined$variable <- sapply(as.character(vi_combined$variable),
                                 get_readable_label)
  
  variable_labels <- setNames(
    lapply(vi_combined$variable, function(v) {
      tryCatch(parse(text = v)[[1]], error = function(e) v)
    }),
    vi_combined$variable
  )
  
  vi_long <- vi_combined %>%
    dplyr::select(variable, `%IncMSE`, Gini_scaled, Total_scaled) %>%
    pivot_longer(cols = c(`%IncMSE`, Gini_scaled),
                 names_to  = "Metric",
                 values_to = "Value") %>%
    mutate(variable = reorder(variable, Total_scaled))
  
  ggplot(vi_long, aes(x = Value, y = variable, fill = Metric)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(
      values = c("%IncMSE"     = bar_color_mse,
                 "Gini_scaled" = bar_color_gini),
      labels = c("%IncMSE"     = "%IncMSE",
                 "Gini_scaled" = "Gini (scaled)"),
      name   = "Importance Metric"
    ) +
    scale_y_discrete(labels = variable_labels) +
    labs(title = plot_title,
         x     = "Total Rescaled Importance",
         y     = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y        = element_text(face = "bold"),
      legend.position    = "bottom",
      panel.grid.major.y = element_blank(),
      plot.title         = element_text(face = "bold", size = 13)
    )
}

# Build the four plots
p_pd_random <- make_importance_plot(
  vi_summary = pd_random$vi_summary,
  plot_title = "Planting Date (Random Split): Variable Importance"
)

p_pd_l2yo <- make_importance_plot(
  vi_summary = pd_final$vi_summary,
  plot_title = "Planting Date (L2YO): Variable Importance"
)

p_hd_random <- make_importance_plot(
  vi_summary = hd_random$vi_summary,
  plot_title = "Harvest Date (Random Split): Variable Importance"
)

p_hd_l2yo <- make_importance_plot(
  vi_summary = hd_final$vi_summary,
  plot_title = "Harvest Date (L2YO): Variable Importance"
)

# Print all four individually
print(p_pd_random)
print(p_pd_l2yo)
print(p_hd_random)
print(p_hd_l2yo)

# Combined 2×2 panel
combined_importance_panel <-
  (p_pd_random | p_pd_l2yo) /
  (p_hd_random | p_hd_l2yo) +
  plot_annotation(
    title    = "Variable Importance: Planting & Harvest Date Models",
    subtitle = "Left = Random Split (100 runs avg) | Right = L2YO (all folds avg)",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 12, color = "grey40")
    )
  )

print(combined_importance_panel)

# Save individual plots
ggsave("PD_random_variable_importance.jpeg", plot = p_pd_random,
       path = out_dir, dpi = 300, width = 10, height = 8, units = "in")
ggsave("PD_L2YO_variable_importance.jpeg",   plot = p_pd_l2yo,
       path = out_dir, dpi = 300, width = 10, height = 8, units = "in")
ggsave("HD_random_variable_importance.jpeg", plot = p_hd_random,
       path = out_dir, dpi = 300, width = 10, height = 8, units = "in")
ggsave("HD_L2YO_variable_importance.jpeg",   plot = p_hd_l2yo,
       path = out_dir, dpi = 300, width = 10, height = 8, units = "in")
ggsave("ALL_variable_importance_2x2_panel.jpeg", plot = combined_importance_panel,
       path = out_dir, dpi = 300, width = 20, height = 16, units = "in")

cat("\n✓ All four importance plots saved to:", out_dir, "\n")
cat("  p_pd_random, p_pd_l2yo, p_hd_random, p_hd_l2yo\n")
cat("  combined_importance_panel (2×2)\n")


p_pd_random_labeled <- p_pd_random +
  annotate("text", x = Inf, y = Inf, label = "A",
           hjust = 1.2, vjust = 1, fontface = "bold", size = 6)

p_pd_l2yo_labeled <- p_pd_l2yo +
  annotate("text", x = Inf, y = Inf, label = "B",
           hjust = 1.2, vjust = 1, fontface = "bold", size = 6)

pd_panel <-
  (p_pd_random_labeled | p_pd_l2yo_labeled) +
  plot_annotation(

    theme = theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 12, color = "grey40")
    )
  )

ggsave("PD_variable_importance_AB.jpeg",
       plot = pd_panel,
       path = out_dir,
       dpi = 300,
       width = 16, height = 8, units = "in")
p_hd_random_labeled <- p_hd_random +
  annotate("text", x = Inf, y = Inf, label = "A",
           hjust = 1.2, vjust = 1, fontface = "bold", size = 6)

p_hd_l2yo_labeled <- p_hd_l2yo +
  annotate("text", x = Inf, y = Inf, label = "B",
           hjust = 1.2, vjust = 1, fontface = "bold", size = 6)

hd_panel <-
  (p_hd_random_labeled | p_hd_l2yo_labeled) +
  plot_annotation(
    theme = theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 12, color = "grey40")
    )
  )

ggsave("HD_variable_importance_AB.jpeg",
       plot = hd_panel,
       path = out_dir,
       dpi = 300,
       width = 16, height = 8, units = "in")