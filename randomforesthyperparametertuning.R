# =========================================================
# COMPLETE PIPELINE — SELF-CONTAINED
# Builds pd_v9_out, hd_v2_out, then full hyperparameter tuning
# Starting point: df_sos (673 rows, has SOS_trs.sos)
# =========================================================
# Quick NA summary for PD and HD features
# colSums(is.na(df_pd[, pd_features])) %>% .[. > 0] %>% sort(decreasing=TRUE)
# colSums(is.na(df_hd[, hd_features])) %>% .[. > 0] %>% sort(decreasing=TRUE)

library(randomForest)
library(hydroGOF)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

# =========================================================
# HELPER FUNCTIONS
# =========================================================

safe_r2 <- function(obs, pred) {
  tryCatch(summary(lm(obs ~ pred))$r.squared,
           error = function(e) NA_real_)
}

l2yo_one_combo <- function(df_in, target, features,
                           ntree, mtry, nodesize = 5,
                           maxnodes = 40) {
  feat   <- intersect(features, names(df_in))
  df_use <- df_in %>%
    dplyr::select(all_of(feat), all_of(target), Year) %>%
    drop_na()
  pairs <- combn(sort(unique(df_use$Year)), 2, simplify = FALSE)
  res   <- numeric(0)
  for (pair in pairs) {
    tr <- df_use %>% filter(!Year %in% pair)
    te <- df_use %>% filter( Year %in% pair)
    if (nrow(tr) < 10 || nrow(te) < 2) next
    m <- tryCatch(
      randomForest(as.formula(paste(target, "~ . - Year")),
                   data=tr, ntree=ntree, mtry=mtry,
                   nodesize=nodesize, maxnodes=maxnodes),
      error = function(e) NULL)
    if (is.null(m)) next
    pred_te <- predict(m, te)
    res <- c(res, hydroGOF::rmse(pred_te, te[[target]]))
  }
  if (length(res) == 0) return(NA_real_)
  mean(res, na.rm = TRUE)
}

run_final_l2yo <- function(df_in, target, features,
                           ntree, mtry, label) {
  feat   <- intersect(features, names(df_in))
  df_use <- df_in %>%
    dplyr::select(all_of(feat), all_of(target), Year) %>%
    drop_na()
  pairs <- combn(sort(unique(df_use$Year)), 2, simplify = FALSE)
  res   <- data.frame()
  for (pair in pairs) {
    tr <- df_use %>% filter(!Year %in% pair)
    te <- df_use %>% filter( Year %in% pair)
    if (nrow(tr) < 10 || nrow(te) < 2) next
    m <- tryCatch(
      randomForest(as.formula(paste(target, "~ . - Year")),
                   data=tr, ntree=ntree, mtry=mtry,
                   nodesize=5, maxnodes=40),
      error = function(e) NULL)
    if (is.null(m)) next
    pred_tr <- predict(m, tr)
    pred_te <- predict(m, te)
    res <- rbind(res, data.frame(
      test_years    = paste(pair, collapse="-"),
      yr1=pair[1], yr2=pair[2],
      Train_n=nrow(tr), Test_n=nrow(te),
      RF_Train_RMSE = hydroGOF::rmse(pred_tr, tr[[target]]),
      RF_Train_MAE  = hydroGOF::mae(pred_tr,  tr[[target]]),
      RF_Train_R2   = safe_r2(tr[[target]], pred_tr),
      RF_Test_RMSE  = hydroGOF::rmse(pred_te, te[[target]]),
      RF_Test_MAE   = hydroGOF::mae(pred_te,  te[[target]]),
      RF_Test_R2    = safe_r2(te[[target]], pred_te),
      stringsAsFactors = FALSE))
  }
  smry <- res %>% summarise(
    Train_RMSE   = round(mean(RF_Train_RMSE), 2),
    Train_MAE    = round(mean(RF_Train_MAE),  2),
    Train_R2     = round(mean(RF_Train_R2),   3),
    Test_RMSE    = round(mean(RF_Test_RMSE),  2),
    Test_MAE     = round(mean(RF_Test_MAE),   2),
    Test_R2      = round(mean(RF_Test_R2),    3),
    Test_RMSE_sd = round(sd(RF_Test_RMSE),    2),
    Test_MAE_sd  = round(sd(RF_Test_MAE),     2)
  )
  cat(sprintf("\n%s\n", label))
  cat("─────────────────────────────────────────\n")
  cat(sprintf("ntree=%d mtry=%d feat=%d\n", ntree, mtry, length(feat)))
  cat(sprintf("Train RMSE=%.2f MAE=%.2f R²=%.3f\n",
              smry$Train_RMSE, smry$Train_MAE, smry$Train_R2))
  cat(sprintf("Test  RMSE=%.2f±%.2f MAE=%.2f±%.2f R²=%.3f\n",
              smry$Test_RMSE, smry$Test_RMSE_sd,
              smry$Test_MAE,  smry$Test_MAE_sd, smry$Test_R2))
  cat(sprintf("Gap   %.2f days\n", smry$Test_RMSE - smry$Train_RMSE))
  list(results=res, summary=smry, features=feat,
       df_used=df_use, ntree=ntree, mtry=mtry)
}

# =========================================================
# STEP 1: SET MASTER DATAFRAME FROM df_sos
# =========================================================

cat("=== STEP 1: Build master df from df_sos ===\n")

# =========================================================
# STEP 0: BUILD df_pd_rfe AND df_hd_rfe
# df_pd_rfe  ← from df          (planting window)
# df_hd_rfe  ← from dfharvest   (harvest window)
# =========================================================

cat("=== STEP 0: Build feature datasets ===\n")

year_signal <- df %>%
  mutate(Year = as.numeric(
    sub(".*_(\\d{4})$", "\\1", Field_Year))) %>%
  group_by(Year) %>%
  summarise(
    year_vpd  = mean(yearcumvpd,  na.rm=TRUE),
    year_gdd  = mean(yearcumgdd,  na.rm=TRUE),
    year_tmin = mean(yearcumtmin, na.rm=TRUE),
    year_RH   = mean(yearcumrh,   na.rm=TRUE),
    year_rad  = mean(yearcumrad,  na.rm=TRUE),
    .groups="drop"
  ) %>%
  mutate(
    z_vpd  = as.numeric(scale(year_vpd)),
    z_gdd  = as.numeric(scale(year_gdd)),
    z_tmin = as.numeric(scale(year_tmin)),
    z_RH   = as.numeric(scale(year_RH)),
    z_rad  = as.numeric(scale(year_rad))
  )

# ---- PDDOY: source is df (planting window cumulatives) ----
df_pd_rfe <- df %>%                          
  mutate(Year = as.numeric(
    sub(".*_(\\d{4})$", "\\1", Field_Year))) %>%
  left_join(year_signal, by = "Year") %>%
  mutate(
    # Existing interactions
    gdd_x_sos  = cum_gdd  * SOS_trs.sos,
    vpd_x_sos  = cum_vpd  * SOS_trs.sos,
    tmin_gdd   = cum_tmin * cum_gdd,
    vpd_x_tmin = cum_vpd  * cum_tmin,
    vpd_gdd    = cum_vpd  * cum_gdd,
    
    # Newly added interaction features
    sd_x_sos       = SD.SD * SOS_trs.sos,
    sd_x_gdd       = SD.SD * cum_gdd,
    roc_vs_sos     = DOY_maxROC_kNDVI - SOS_trs.sos,
    pos_x_sos      = POS.pos * SOS_trs.sos,
    #maturity_x_gdd = mx.mx * cum_gdd,
    
    # Phenology structure metrics
    season_length   = RD.RD - SOS_trs.sos,
    sos_to_pos      = POS.pos - SOS_trs.sos,
    curve_amplitude = mx.mx - mn.mn,
    rise_vs_fall    = (POS.pos - SOS_trs.sos) /
      (RD.RD - POS.pos + 1),
    
    # Year anomaly metrics
    yr_vpd_anom  = year_vpd  - mean(year_vpd,  na.rm = TRUE),
    yr_gdd_anom  = year_gdd  - mean(year_gdd,  na.rm = TRUE),
    yr_tmin_anom = year_tmin - mean(year_tmin, na.rm = TRUE),
    #yr_tmax_anom = year_tmax - mean(year_tmax, na.rm = TRUE),
    yr_rh_anom   = year_RH   - mean(year_RH,   na.rm = TRUE),
    yr_rad_anom  = year_rad  - mean(year_rad,  na.rm = TRUE),
    
    # Stress / energy metrics
    yr_stress = as.numeric(scale(year_vpd)) *
      as.numeric(scale(year_tmin)),
    
    yr_energy_ratio = year_gdd / (year_vpd + 1),
    
    yr_aridity = year_vpd / (year_RH + 1),
    
    # Rank metrics
    yr_vpd_rank  = rank(year_vpd)  / n(),
    yr_gdd_rank  = rank(year_gdd)  / n(),
    yr_tmin_rank = rank(year_tmin) / n(),
    yr_rad_rank  = rank(year_rad)  / n()
  ) %>%
  dplyr::select(
    PDDOY, Year,
    
    # Phenology
    SOS_trs.sos, SOS_deriv.sos,
    UD.UD, SD.SD, DD.DD, RD.RD,
    POS.pos,
    t0.t0, mn.mn, mx.mx, rsp.rsp,
    a3.a3, rau.rau, a5.a5,
    
    # Climate cumulative
    cum_gdd, cum_vpd, cum_tmin, cum_tmax,
    cum_RH, cum_soiltemp, cum_meansrad,
    
    # Soil
    avgsoilorg, avgsoilclay,
    
    # Remote sensing
    DOY_maxROC_kNDVI,
    DOY_max_before_min_fit,
    DOY_min_fit,
    
    # Year metrics
    year_vpd, year_gdd, year_tmin,
    #year_tmax, 
    year_RH, year_rad,
    
    # Z scores
    z_vpd, z_gdd, z_tmin, z_RH, z_rad,
    
    # Anomalies
    yr_vpd_anom, yr_gdd_anom,
    yr_tmin_anom, 
    #yr_tmax_anom,
    yr_rh_anom, yr_rad_anom,
    
    # Ranks
    yr_vpd_rank, yr_gdd_rank,
    yr_tmin_rank, yr_rad_rank,
    
    # Interactions
    gdd_x_sos, vpd_x_sos,
    tmin_gdd, vpd_x_tmin, vpd_gdd,
    sd_x_sos, sd_x_gdd,
    roc_vs_sos, pos_x_sos,
    #maturity_x_gdd,
    
    # Structure metrics
    season_length,
    sos_to_pos,
    curve_amplitude,
    rise_vs_fall,
    
    # Stress metrics
    yr_stress,
    yr_energy_ratio,
    yr_aridity
  ) %>%
  filter(!is.na(PDDOY)) %>%
  drop_na()
# ---- HDDOY: source is dfharvest (harvest window cumulatives) ----
df_hd_rfe <- dfharvest %>%                  
  mutate(
    Year = as.numeric(sub(".*_(\\d{4})$", "\\1", Field_Year))
  ) %>%
  left_join(year_signal, by = "Year") %>%
  mutate(
    
    # Existing interactions
    eos_x_sos      = EOS_trs.eos * SOS_trs.sos,
    tmin_gdd       = cum_tmin * cum_gdd,
    sd_x_gdd       = SD.SD * cum_gdd,
    #maturity_x_gdd = Maturity.Maturity * cum_gdd,
    
    # Year anomalies
    yr_vpd_anom  = year_vpd  - mean(year_vpd,  na.rm = TRUE),
    yr_gdd_anom  = year_gdd  - mean(year_gdd,  na.rm = TRUE),
    yr_tmin_anom = year_tmin - mean(year_tmin, na.rm = TRUE),
    #yr_tmax_anom = year_tmax - mean(year_tmax, na.rm = TRUE),
    yr_rh_anom   = year_RH   - mean(year_RH,   na.rm = TRUE),
    yr_rad_anom  = year_rad  - mean(year_rad,  na.rm = TRUE),
    
    # Stress / energy metrics
    yr_stress = as.numeric(scale(year_vpd)) *
      as.numeric(scale(year_tmin)),
    
    yr_energy_ratio = year_gdd / (year_vpd + 1),
    
    yr_aridity = year_vpd / (year_RH + 1),
    
    # Rank metrics
    yr_vpd_rank  = rank(year_vpd)  / n(),
    yr_gdd_rank  = rank(year_gdd)  / n(),
    yr_tmin_rank = rank(year_tmin) / n(),
    yr_rad_rank  = rank(year_rad)  / n()
  ) %>%
  dplyr::select(
    HDDOY, Year,
    
    # Phenology
    EOS_trs.eos, EOS_deriv.eos,
    RD.RD, DD.DD, SD.SD,
    SOS_trs.sos,
    #Dormancy.Dormancy,
    #Maturity.Maturity,
    POS.pos,
    
    # Curve parameters
    t0.t0, mn.mn, mx.mx, rsp.rsp,
    a3.a3, rau.rau, a5.a5,
    a1, a2,
    
    # Remote sensing
    DOY_max_before_min_fit,
    DOY_min_fit,
    DOY_maxROC_kNDVI,
    
    # Cumulative climate
    cum_gdd, cum_vpd, cum_tmin,
    cum_tmax, cum_RH,
    cum_soiltemp, cum_meansrad,
    
    # Soil
    avgsoilorg, avgsoilclay,
    
    # Year climate
    year_vpd, year_gdd, year_tmin,
    #year_tmax, 
    year_RH, year_rad,
    
    # Z scores
    z_vpd, z_gdd, z_tmin,
    
    # Anomalies
    yr_vpd_anom, yr_gdd_anom,
    yr_tmin_anom, 
    #yr_tmax_anom,
    yr_rh_anom, yr_rad_anom,
    
    # Rank metrics
    yr_vpd_rank, yr_gdd_rank,
    yr_tmin_rank, yr_rad_rank,
    
    # Stress metrics
    yr_stress,
    yr_energy_ratio,
    yr_aridity,
    
    # Interactions
    eos_x_sos,
    tmin_gdd,
    sd_x_gdd,
    #maturity_x_gdd
  ) %>%
  filter(!is.na(HDDOY)) %>%
  drop_na()

cat("df_pd_rfe rows:", nrow(df_pd_rfe), "\n")
cat("df_hd_rfe rows:", nrow(df_hd_rfe), "\n")
print(table(df_pd_rfe$Year))
print(table(df_hd_rfe$Year))

# Extract Year
df_master$Year <- as.numeric(
  sub(".*_(\\d{4})$", "\\1", df_master$Field_Year))

cat("Rows:", nrow(df_master), "\n")
cat("Years:\n"); print(table(df_master$Year))

# =========================================================
# STEP 2: BUILD YEAR CLIMATE ANOMALIES
# Find which yearcum* columns exist
# =========================================================

cat("\n=== STEP 2: Year climate anomalies ===\n")

# Detect available cumulative climate columns
cum_check <- c("yearcumgdd","yearcumvpd","yearcumtmin",
               "yearcumtmax","yearcumrh","yearcumrad")
available_cum <- intersect(cum_check, names(df_master))
cat("Available yearcum vars:", paste(available_cum, collapse=", "), "\n")

# Build year_climate from whatever is available
year_climate <- df_master %>%
  group_by(Year) %>%
  summarise(across(all_of(available_cum),
                   ~ mean(.x, na.rm=TRUE)),
            .groups="drop")

# Rename to standard short names
rename_map <- c(yearcumgdd="yr_gdd", yearcumvpd="yr_vpd",
                yearcumtmin="yr_tmin", yearcumtmax="yr_tmax",
                yearcumrh="yr_rh", yearcumrad="yr_rad")
names(year_climate) <- ifelse(
  names(year_climate) %in% names(rename_map),
  rename_map[names(year_climate)],
  names(year_climate)
)

# Compute z-score anomalies for whichever raw vars exist
raw_vars <- intersect(c("yr_gdd","yr_vpd","yr_tmin",
                        "yr_tmax","yr_rh","yr_rad"),
                      names(year_climate))

for (v in raw_vars) {
  anom_name <- paste0(v, "_anom")
  rank_name <- paste0(v, "_rank")
  year_climate[[anom_name]] <- as.numeric(scale(year_climate[[v]]))
  year_climate[[rank_name]] <- rank(year_climate[[v]]) /
    nrow(year_climate)
}

# Composite indices (only if base vars exist)
if (all(c("yr_vpd_anom","yr_tmin_anom") %in% names(year_climate))) {
  year_climate$yr_stress     <- year_climate$yr_vpd_anom *
    (-year_climate$yr_tmin_anom)
  year_climate$yr_vpd_x_tmin <- year_climate$yr_vpd_anom *
    year_climate$yr_tmin_anom
}
if (all(c("yr_gdd_anom","yr_vpd_anom") %in% names(year_climate))) {
  year_climate$yr_energy_ratio <- year_climate$yr_gdd_anom -
    year_climate$yr_vpd_anom
}
if (all(c("yr_vpd_anom","yr_rh_anom") %in% names(year_climate))) {
  year_climate$yr_aridity <- year_climate$yr_vpd_anom -
    year_climate$yr_rh_anom
}

cat("\nYear climate table:\n")
yr_anom_cols <- names(year_climate)[grep("_anom|_stress|_energy",
                                         names(year_climate))]
print(as.data.frame(year_climate %>%
                      dplyr::select(Year, any_of(yr_anom_cols))))

# Join back to df_master
yr_join_cols <- setdiff(names(year_climate), raw_vars)
df_master <- df_master %>%
  dplyr::select(-any_of(setdiff(yr_join_cols, "Year"))) %>%
  left_join(year_climate %>%
              dplyr::select(all_of(yr_join_cols)),
            by = "Year")

cat("df_master after join:", nrow(df_master), "rows,",
    ncol(df_master), "cols\n")

# =========================================================
# STEP 3: BUILD INTERACTION FEATURES
# =========================================================

cat("\n=== STEP 3: Interaction features ===\n")

# Only build if base columns exist
safe_mutate <- function(df, new_col, expr) {
  tryCatch({ df[[new_col]] <- expr; df },
           error = function(e) df)
}

if (all(c("cum_gdd","SOS_trs.sos") %in% names(df_master)))
  df_master$gdd_x_sos <- df_master$cum_gdd * df_master$SOS_trs.sos
if (all(c("cum_vpd","SOS_trs.sos") %in% names(df_master)))
  df_master$vpd_x_sos <- df_master$cum_vpd * df_master$SOS_trs.sos
if (all(c("cum_tmin","cum_gdd") %in% names(df_master)))
  df_master$tmin_gdd <- df_master$cum_tmin * df_master$cum_gdd
if (all(c("DD.DD","UD.UD") %in% names(df_master)))
  df_master$season_length <- df_master$DD.DD - df_master$UD.UD
if (all(c("DOY_maxROC_kNDVI","SOS_trs.sos") %in% names(df_master)))
  df_master$roc_vs_sos <- df_master$DOY_maxROC_kNDVI -
  df_master$SOS_trs.sos
if (all(c("SD.SD","SOS_trs.sos") %in% names(df_master)))
  df_master$sd_x_sos <- df_master$SD.SD * df_master$SOS_trs.sos
if (all(c("SD.SD","cum_gdd") %in% names(df_master)))
  df_master$sd_x_gdd <- df_master$SD.SD * df_master$cum_gdd
if (all(c("Maturity.Maturity","Greenup.Greenup") %in% names(df_master)))
  df_master$greenup_to_maturity <- df_master$Maturity.Maturity -
  df_master$Greenup.Greenup
if (all(c("POS.pos","Greenup.Greenup") %in% names(df_master)))
  df_master$greenup_to_pos <- df_master$POS.pos -
  df_master$Greenup.Greenup
if (all(c("Senescence.Senescence","POS.pos") %in% names(df_master)))
  df_master$pos_to_senescence <- df_master$Senescence.Senescence -
  df_master$POS.pos
if (all(c("POS.pos","SOS_trs.sos") %in% names(df_master)))
  df_master$sos_to_pos <- df_master$POS.pos - df_master$SOS_trs.sos
if (all(c("mx.mx","mn.mn") %in% names(df_master)))
  df_master$curve_amplitude <- df_master$mx.mx - df_master$mn.mn
if (all(c("rsp.rsp","rau.rau") %in% names(df_master)))
  df_master$rise_vs_fall <- df_master$rsp.rsp /
  (df_master$rau.rau + 0.001)
if (all(c("POS.pos","SOS_trs.sos") %in% names(df_master)))
  df_master$pos_x_sos <- df_master$POS.pos * df_master$SOS_trs.sos
if (all(c("Maturity.Maturity","cum_gdd") %in% names(df_master)))
  df_master$maturity_x_gdd <- df_master$Maturity.Maturity *
  df_master$cum_gdd
if (all(c("EOS_trs.eos","SOS_trs.sos") %in% names(df_master)))
  df_master$eos_x_sos <- df_master$EOS_trs.eos * df_master$SOS_trs.sos

cat("Interaction features built. Cols now:", ncol(df_master), "\n")

# =========================================================
# STEP 4: DEFINE FULL FEATURE CANDIDATE LISTS
# =========================================================

cat("\n=== STEP 4: Define feature candidates ===\n")

# All possible PDDOY features
pd_candidate_features <- c(
  # Phenology DOY
  "SOS_trs.sos","SOS_deriv.sos","UD.UD","SD.SD","DD.DD","RD.RD",
  "POS.pos",
  #"Greenup.Greenup","Maturity.Maturity","Senescence.Senescence",
  "t0.t0","mn.mn","mx.mx","rsp.rsp","a3.a3","rau.rau","a5.a5",
  # Cumulative climate
  "cum_gdd","cum_vpd","cum_tmin","cum_tmax","cum_RH",
  "cum_soiltemp","cum_meansrad",
  # Soil
  "avgsoilorg","avgsoilclay",
  # Remote sensing
  "DOY_maxROC_kNDVI","DOY_max_before_min_fit","DOY_min_fit",
  # Interactions
  "gdd_x_sos","vpd_x_sos","tmin_gdd","sd_x_sos","sd_x_gdd",
  "season_length","roc_vs_sos","pos_x_sos",
  #"maturity_x_gdd",
  
  #"greenup_to_maturity","greenup_to_pos","pos_to_senescence",
  "sos_to_pos","curve_amplitude","rise_vs_fall",
  # Year anomalies
  "yr_gdd_anom","yr_vpd_anom","yr_tmin_anom","yr_tmax_anom",
  "yr_rh_anom","yr_rad_anom",
  "yr_gdd_rank","yr_vpd_rank","yr_tmin_rank","yr_rad_rank",
  "yr_stress","yr_energy_ratio","yr_aridity"
)

# All possible HDDOY features
hd_candidate_features <- c(
  # Phenology DOY
  "EOS_trs.eos","EOS_deriv.eos","DD.DD","RD.RD","SD.SD",
  #"Dormancy.Dormancy","Senescence.Senescence","Maturity.Maturity",
  "POS.pos","SOS_trs.sos","t0.t0","mn.mn","mx.mx",
  "rsp.rsp","a1","a2","rau.rau","a3.a3","a5.a5",
  # Remote sensing
  "DOY_max_before_min_fit","DOY_min_fit","DOY_maxROC_kNDVI",
  # Cumulative climate
  "cum_gdd","cum_vpd","cum_tmin","cum_tmax","cum_RH",
  "cum_soiltemp","cum_meansrad",
  # Soil
  "avgsoilorg","avgsoilclay",
  # Interactions
  "eos_x_sos","tmin_gdd","sd_x_gdd",
  #"maturity_x_gdd",
  # Year anomalies
  "yr_gdd_anom","yr_vpd_anom","yr_tmin_anom","yr_tmax_anom",
  "yr_rh_anom","yr_rad_anom",
  "yr_gdd_rank","yr_vpd_rank","yr_tmin_rank","yr_rad_rank",
  "yr_stress","yr_energy_ratio","yr_aridity"
)

# Keep only columns that actually exist in df_master
pd_features <- intersect(pd_candidate_features, names(df_master))
hd_features <- intersect(hd_candidate_features, names(df_master))

cat("PDDOY candidate features available:", length(pd_features), "\n")
cat("HDDOY candidate features available:", length(hd_features), "\n")

# =========================================================
# STEP 5: BUILD MODEL DATASETS
# =========================================================

df_pd <- df_pd_rfe %>%
  dplyr::select(all_of(pd_features), PDDOY, Year) %>%
  dplyr::filter(!is.na(PDDOY)) %>%
  drop_na()

df_hd <- df_hd_rfe %>%
  dplyr::select(all_of(hd_features), HDDOY, Year) %>%
  dplyr::filter(!is.na(HDDOY)) %>%
  drop_na()

cat("\nPDDOY dataset:", nrow(df_pd), "rows\n")
print(table(df_pd$Year))
cat("\nHDDOY dataset:", nrow(df_hd), "rows\n")
print(table(df_hd$Year))

# Store as v9/v2 style output objects so downstream code works
pd_v9_out <- list(
  df_used  = df_pd,
  features = pd_features,
  target   = "PDDOY"
)
hd_v2_out <- list(
  df_used  = df_hd,
  features = hd_features,
  target   = "HDDOY"
)

cat("\npd_v9_out and hd_v2_out built successfully.\n")

# =========================================================
# STEP 6: L2YO ntree SEARCH
# =========================================================

cat("\n=== STEP 6: L2YO ntree search ===\n")

ntree_options <- c(50, 100, 150, 200, 300, 400, 500, 600, 800)

pd_ntree_l2yo <- data.frame()
hd_ntree_l2yo <- data.frame()

for (nt in ntree_options) {
  mt_pd <- max(3, floor(sqrt(length(pd_features))))
  mt_hd <- max(3, floor(sqrt(length(hd_features))))
  
  rmse_pd <- l2yo_one_combo(df_pd, "PDDOY", pd_features,
                            ntree=nt, mtry=mt_pd)
  rmse_hd <- l2yo_one_combo(df_hd, "HDDOY", hd_features,
                            ntree=nt, mtry=mt_hd)
  
  pd_ntree_l2yo <- rbind(pd_ntree_l2yo, data.frame(
    ntree=nt, L2YO_RMSE=round(rmse_pd,3), Target="PDDOY"))
  hd_ntree_l2yo <- rbind(hd_ntree_l2yo, data.frame(
    ntree=nt, L2YO_RMSE=round(rmse_hd,3), Target="HDDOY"))
  
  cat(sprintf("ntree=%4d | PDDOY=%.3f | HDDOY=%.3f\n",
              nt, rmse_pd, rmse_hd))
}

# Plot
bind_rows(pd_ntree_l2yo, hd_ntree_l2yo) %>%
  ggplot(aes(x=ntree, y=L2YO_RMSE, color=Target, group=Target)) +
  geom_line(linewidth=1.3) + geom_point(size=3.5) +
  geom_text(aes(label=round(L2YO_RMSE,2)),
            vjust=-0.8, size=3, show.legend=FALSE) +
  scale_color_manual(values=c("PDDOY"="#2980b9","HDDOY"="#e67e22")) +
  labs(title="L2YO RMSE vs ntree",
       x="ntree", y="Mean L2YO RMSE (days)") +
  theme_minimal(base_size=13)

best_ntree_pd <- pd_ntree_l2yo$ntree[which.min(pd_ntree_l2yo$L2YO_RMSE)]
best_ntree_hd <- hd_ntree_l2yo$ntree[which.min(hd_ntree_l2yo$L2YO_RMSE)]
cat(sprintf("\nBest ntree PDDOY: %d\n", best_ntree_pd))
cat(sprintf("Best ntree HDDOY: %d\n", best_ntree_hd))

# =========================================================
# STEP 7: FEATURE RANKING BY IMPORTANCE
# =========================================================

cat("\n=== STEP 7: Feature ranking ===\n")

rank_features <- function(df_in, target, features, ntree, label) {
  feat   <- intersect(features, names(df_in))
  df_use <- df_in %>%
    dplyr::select(all_of(feat), all_of(target), Year) %>%
    drop_na()
  set.seed(123)
  m <- randomForest(
    as.formula(paste(target, "~ . - Year")),
    data=df_use, ntree=ntree,
    mtry=max(3, floor(sqrt(length(feat)))),
    nodesize=5, maxnodes=40, importance=TRUE)
  imp_df <- data.frame(
    var    = rownames(randomForest::importance(m)),
    IncMSE = randomForest::importance(m)[,"%IncMSE"]
  ) %>% arrange(desc(IncMSE))
  cat(sprintf("\n%s top 20:\n", label))
  print(head(as.data.frame(imp_df), 20))
  imp_df$var
}

pd_ranked_vars <- rank_features(df_pd, "PDDOY", pd_features,
                                best_ntree_pd, "PDDOY")
hd_ranked_vars <- rank_features(df_hd, "HDDOY", hd_features,
                                best_ntree_hd, "HDDOY")

# =========================================================
# STEP 8: FULL GRID SEARCH ntree × mtry × n_features
# =========================================================

cat("\n=== STEP 8: Grid search ===\n")
# Different feature set sizes for PD and HD
pd_size_grid <- c(6, 8, 10,12, 14,15)
hd_size_grid <- c(10, 12, 15, 18)
mtry_grid <- c(3, 4, 5, 6, 8)

size_grid <- c(15)
mtry_grid <- c(3, 4, 5, 6, 8)

ntree_grid_pd <- unique(c(best_ntree_pd,
                          max(50, best_ntree_pd - 50),
                          min(800, best_ntree_pd + 100)))

ntree_grid_hd <- unique(c(best_ntree_hd,
                          max(50, best_ntree_hd - 50),
                          min(800, best_ntree_hd + 100)))

# ---- PDDOY ----
cat("\n--- PDDOY Grid ---\n")
pd_grid <- data.frame()
i <- 0; total <- length(ntree_grid_pd)*length(pd_size_grid)*length(mtry_grid)

for (nt in ntree_grid_pd) {
  for (ns in pd_size_grid) {
    feat_ns <- pd_ranked_vars[1:ns]
    for (mt in mtry_grid) {
      if (mt > ns) next
      i <- i + 1
      rmse_val <- l2yo_one_combo(df_pd, "PDDOY",
                                 feat_ns, ntree=nt, mtry=mt)
      pd_grid <- rbind(pd_grid, data.frame(
        ntree=nt, n_feat=ns, mtry=mt,
        L2YO_RMSE=round(rmse_val,3)))
      cat(sprintf("[%2d/%d] ntree=%3d feat=%2d mtry=%d → %.3f\n",
                  i, total, nt, ns, mt, rmse_val))
    }
  }
}

cat("\n--- PDDOY Top 10 ---\n")
pd_grid %>% arrange(L2YO_RMSE) %>% head(10) %>%
  as.data.frame() %>% print()
best_pd <- pd_grid %>% arrange(L2YO_RMSE) %>% slice(1)
cat(sprintf("\nPDDOY WINNER: ntree=%d feat=%d mtry=%d RMSE=%.3f\n",
            best_pd$ntree, best_pd$n_feat,
            best_pd$mtry, best_pd$L2YO_RMSE))

# ---- HDDOY ----
cat("\n--- HDDOY Grid ---\n")
hd_grid <- data.frame()
i <- 0; total <- length(ntree_grid_hd)*length(hd_size_grid)*length(mtry_grid)

for (nt in ntree_grid_hd) {
  for (ns in hd_size_grid) {
    feat_ns <- hd_ranked_vars[1:ns]
    for (mt in mtry_grid) {
      if (mt > ns) next
      i <- i + 1
      rmse_val <- l2yo_one_combo(df_hd, "HDDOY",
                                 feat_ns, ntree=nt, mtry=mt)
      hd_grid <- rbind(hd_grid, data.frame(
        ntree=nt, n_feat=ns, mtry=mt,
        L2YO_RMSE=round(rmse_val,3)))
      cat(sprintf("[%2d/%d] ntree=%3d feat=%2d mtry=%d → %.3f\n",
                  i, total, nt, ns, mt, rmse_val))
    }
  }
}

cat("\n--- HDDOY Top 10 ---\n")
hd_grid %>% arrange(L2YO_RMSE) %>% head(10) %>%
  as.data.frame() %>% print()
best_hd <- hd_grid %>% arrange(L2YO_RMSE) %>% slice(1)
cat(sprintf("\nHDDOY WINNER: ntree=%d feat=%d mtry=%d RMSE=%.3f\n",
            best_hd$ntree, best_hd$n_feat,
            best_hd$mtry, best_hd$L2YO_RMSE))

# =========================================================
# STEP 9: HEATMAP PLOTS
# =========================================================

pd_best_grid <- pd_grid %>% filter(ntree == best_pd$ntree)
ggplot(pd_best_grid,
       aes(x=factor(mtry), y=factor(n_feat), fill=L2YO_RMSE)) +
  geom_tile(color="white", linewidth=0.5) +
  geom_text(aes(label=round(L2YO_RMSE,2)),
            size=3.5, color="white", fontface="bold") +
  scale_fill_viridis_c(option="plasma") +
  labs(title=sprintf("PDDOY Grid (ntree=%d)", best_pd$ntree),
       x="mtry", y="N Features", fill="RMSE") +
  theme_minimal(base_size=13)

hd_best_grid <- hd_grid %>% filter(ntree == best_hd$ntree)
ggplot(hd_best_grid,
       aes(x=factor(mtry), y=factor(n_feat), fill=L2YO_RMSE)) +
  geom_tile(color="white", linewidth=0.5) +
  geom_text(aes(label=round(L2YO_RMSE,2)),
            size=3.5, color="white", fontface="bold") +
  scale_fill_viridis_c(option="plasma") +
  labs(title=sprintf("HDDOY Grid (ntree=%d)", best_hd$ntree),
       x="mtry", y="N Features", fill="RMSE") +
  theme_minimal(base_size=13)

# =========================================================
# STEP 10: FINAL MODELS WITH BEST HYPERPARAMETERS
# =========================================================

cat("\n=== STEP 10: Final models ===\n")

final_pd_feat <- pd_ranked_vars[1:best_pd$n_feat]
final_hd_feat <- hd_ranked_vars[1:best_hd$n_feat]

cat("\nFinal PDDOY features (", length(final_pd_feat), "):\n")
print(sort(final_pd_feat))
cat("\nFinal HDDOY features (", length(final_hd_feat), "):\n")
print(sort(final_hd_feat))

pd_final <- run_final_l2yo(
  df_pd, "PDDOY", final_pd_feat,
  best_pd$ntree, best_pd$mtry,
  sprintf("PDDOY FINAL (ntree=%d feat=%d mtry=%d)",
          best_pd$ntree, best_pd$n_feat, best_pd$mtry))

hd_final <- run_final_l2yo(
  df_hd, "HDDOY", final_hd_feat,
  best_hd$ntree, best_hd$mtry,
  sprintf("HDDOY FINAL (ntree=%d feat=%d mtry=%d)",
          best_hd$ntree, best_hd$n_feat, best_hd$mtry))

# Also update v9_out/v2_out so consolidation script works
pd_v9_out$df_used  <- df_pd
pd_v9_out$features <- pd_features
hd_v2_out$df_used  <- df_hd
hd_v2_out$features <- hd_features

# =========================================================
# STEP 11: MASTER SUMMARY
# =========================================================

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║              FINAL OPTIMIZED MODEL SUMMARY               ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")

master <- data.frame(
  Target     = c("PDDOY","HDDOY"),
  ntree      = c(best_pd$ntree, best_hd$ntree),
  mtry       = c(best_pd$mtry,  best_hd$mtry),
  N_features = c(best_pd$n_feat, best_hd$n_feat),
  Train_RMSE = c(pd_final$summary$Train_RMSE,
                 hd_final$summary$Train_RMSE),
  Train_MAE  = c(pd_final$summary$Train_MAE,
                 hd_final$summary$Train_MAE),
  Train_R2   = c(pd_final$summary$Train_R2,
                 hd_final$summary$Train_R2),
  Test_RMSE  = c(pd_final$summary$Test_RMSE,
                 hd_final$summary$Test_RMSE),
  Test_MAE   = c(pd_final$summary$Test_MAE,
                 hd_final$summary$Test_MAE),
  Test_R2    = c(pd_final$summary$Test_R2,
                 hd_final$summary$Test_R2),
  Test_RMSE_sd = c(pd_final$summary$Test_RMSE_sd,
                   hd_final$summary$Test_RMSE_sd),
  Rel_RMSE   = c(
    paste0(round(pd_final$summary$Test_RMSE/92*100,1),"%"),
    paste0(round(hd_final$summary$Test_RMSE/90*100,1),"%")
  )
)
print(as.data.frame(master))

# Per-year difficulty
cat("\n--- PDDOY per-year mean RMSE ---\n")
bind_rows(
  pd_final$results %>% dplyr::select(year=yr1, RF_Test_RMSE),
  pd_final$results %>% dplyr::select(year=yr2, RF_Test_RMSE)
) %>% group_by(year) %>%
  summarise(mean_RMSE=round(mean(RF_Test_RMSE),2), .groups="drop") %>%
  arrange(desc(mean_RMSE)) %>% as.data.frame() %>% print()

cat("\n--- HDDOY per-year mean RMSE ---\n")
bind_rows(
  hd_final$results %>% dplyr::select(year=yr1, RF_Test_RMSE),
  hd_final$results %>% dplyr::select(year=yr2, RF_Test_RMSE)
) %>% group_by(year) %>%
  summarise(mean_RMSE=round(mean(RF_Test_RMSE),2), .groups="drop") %>%
  arrange(desc(mean_RMSE)) %>% as.data.frame() %>% print()

# L2YO heatmaps
make_heatmap <- function(results, title, mean_rmse) {
  hm <- bind_rows(
    results %>% dplyr::select(yr1, yr2, RF_Test_RMSE),
    results %>% rename(yr1=yr2, yr2=yr1) %>%
      dplyr::select(yr1, yr2, RF_Test_RMSE))
  ggplot(hm, aes(x=factor(yr1), y=factor(yr2), fill=RF_Test_RMSE)) +
    geom_tile(color="white", linewidth=0.6) +
    geom_text(aes(label=round(RF_Test_RMSE,1)),
              size=3, color="white", fontface="bold") +
    scale_fill_viridis_c(option="plasma") +
    labs(title=title,
         subtitle=paste("Mean RMSE:", round(mean_rmse,2)),
         x="Test Year 1", y="Test Year 2", fill="RMSE") +
    theme_minimal(base_size=12) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
}

print(make_heatmap(pd_final$results, "PDDOY L2YO RMSE",
                   pd_final$summary$Test_RMSE))
print(make_heatmap(hd_final$results, "HDDOY L2YO RMSE",
                   hd_final$summary$Test_RMSE))

cat("\n✓ All objects ready: pd_v9_out, hd_v2_out, pd_final, hd_final\n")
cat("✓ Ready for consolidation and plotting scripts\n")