# =========================================================
# INTERANNUAL VARIABILITY DIAGNOSTIC
# =========================================================

library(dplyr)
library(tidyr)

# ---------------------------------------------------------
# STEP 1: Ensure Year exists
# ---------------------------------------------------------
# If you already have Year, skip this
# df$Year <- as.numeric(sub(".*_(\\d{4})$", "\\1", df$Field_Year))

# ---------------------------------------------------------
# STEP 2: Select your planting features + Year
# ---------------------------------------------------------

df_diag <- df %>%
  dplyr::select(
    Year,
    
    SOS_trs.sos, SOS_deriv.sos, cum_RH, cum_tmin, avgsoilorg, a2, UD.UD, DD.DD,
    EOS_trs.eos, RD.RD, cum_soiltemp, mean_GCC, EOS_deriv.eos, mx.mx, mean_ExG,
    cum_meansrad, cum_vpd, cum_gdd, mean_nir,
    
    DOY_maxROC_ExG, DOY_maxROC_NMDI, Value_max_obs, DOY_maxROC_EVI,
    DOY_min_fit, DOY_maxROC_sNIRvNDVILSWIS,
    
    Laglocalmaxglomax, Laglocalminglomax, Laglocalmaxlocalmin,
    DOY_max_before_min_fit,
    
    mean_AFRI1600, mean_AFRI2100, mean_DSI, mean_DSWI5, mean_ExGR,
    mean_GVMI, mean_MNDVI, mean_MNLI, mean_MSI, mean_NDII,
    mean_NDMI, mean_NDPI, mean_NDVI, mean_NRFIg, mean_NRFIr,
    mean_SLAVI, mean_sNIRvLSWI, mean_sNIRvNDPI, mean_sNIRvNDVILSWIP,
    mean_sNIRvNDVILSWIS, mean_sNIRvSWIR, mean_LSWI, mean_MBWI,
    mean_MLSWI27, mean_WI1, mean_WI2015, mean_BaI, mean_NDSoI,
    mean_NSDS, mean_NSDSI2, mean_NSDSI3, mean_kIPVI, mean_kNDVI,
    
    PDDOY
  ) %>%
  drop_na()

# ---------------------------------------------------------
# STEP 3: FUNCTION — compute interannual signal strength
# ---------------------------------------------------------

compute_year_signal <- function(data, var_name) {
  
  temp <- data %>%
    group_by(Year) %>%
    summarise(
      mean_year = mean(.data[[var_name]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # total variance across years
  total_var <- var(temp$mean_year, na.rm = TRUE)
  
  # within-year variance (noise)
  within_var <- data %>%
    group_by(Year) %>%
    summarise(v = var(.data[[var_name]], na.rm = TRUE), .groups = "drop") %>%
    summarise(mean(v, na.rm = TRUE)) %>%
    pull()
  
  # signal-to-noise ratio (key metric)
  snr <- total_var / within_var
  
  return(data.frame(
    variable = var_name,
    interannual_variance = total_var,
    within_year_variance = within_var,
    snr_year_signal = snr
  ))
}

# ---------------------------------------------------------
# STEP 4: RUN FOR ALL VARIABLES
# ---------------------------------------------------------

features <- setdiff(names(df_diag), c("Year", "PDDOY"))

results <- lapply(features, function(v) {
  compute_year_signal(df_diag, v)
})

signal_table <- bind_rows(results)

# ---------------------------------------------------------
# STEP 5: SORT BY STRONGEST YEAR SIGNAL
# ---------------------------------------------------------

signal_table <- signal_table %>%
  arrange(desc(snr_year_signal))

print(signal_table)

# ---------------------------------------------------------
# STEP 6: TOP FEATURES WITH STRONG INTERANNUAL SIGNAL
# ---------------------------------------------------------

cat("\n--- TOP 20 INTERANNUAL SIGNAL FEATURES ---\n")

print(signal_table %>%
        dplyr::select(variable, snr_year_signal) %>%
        head(20))


pca <- prcomp(df_pd %>% select(-PDDOY, -Year), scale. = TRUE)
plot(pca$x[,1], pca$x[,2], col = df_pd$Year)
centroid <- df_pd %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(across(where(is.numeric), mean))
centroid


df$Year <- as.numeric(sub(".*_(\\d{4})$", "\\1", df$Field_Year))
year_summary <- df %>%
  group_by(Year) %>%
  summarise(
    mean_year_vpd = mean(yearcumvpd, na.rm = TRUE),
    mean_year_gdd = mean(yearcumgdd, na.rm = TRUE),
    mean_year_tmin = mean(yearcumtmin, na.rm = TRUE),
    .groups = "drop"
  )

# Now scale properly (OUTSIDE summarise)
scale_vars <- year_summary %>%
  mutate(
    z_vpd = as.numeric(scale(mean_year_vpd)),
    z_gdd = as.numeric(scale(mean_year_gdd)),
    z_tmin = as.numeric(scale(mean_year_tmin))
  )

scale_vars

df <- df %>%
  mutate(
    vpd_x_tmin = cum_vpd * cum_tmin,
    gdd_x_sos   = cum_gdd * SOS_trs.sos,
    vpd_x_sos   = cum_vpd * SOS_trs.sos
  )

df %>%
  group_by(Year) %>%
  summarise(
    z_vpd = mean(scale(cum_vpd)),
    z_tmin = mean(scale(cum_tmin)),
    z_gdd = mean(scale(cum_gdd)),
    n = n()
  ) %>%
  arrange(z_tmin)


df <- df %>%
  mutate(
    climate_regime = case_when(
      cum_tmin < quantile(cum_tmin, 0.2, na.rm = TRUE) ~ "cold",
      cum_tmin > quantile(cum_tmin, 0.8, na.rm = TRUE) ~ "hot",
      TRUE ~ "normal"
    )
  )

df <- df %>%
  mutate(
    vpd_gdd = cum_vpd * cum_gdd,
    tmin_gdd = cum_tmin * cum_gdd,
    vpd_sos_gdd = cum_vpd * SOS_trs.sos * cum_gdd
  )

df <- df %>%
  mutate(
    vpd_gdd = cum_vpd * cum_gdd,
    tmin_gdd = cum_tmin * cum_gdd,
    vpd_sos_gdd = cum_vpd * SOS_trs.sos * cum_gdd
  )
df$climate_regime <- as.factor(df$climate_regime)

df <- df %>%
  mutate(
    vpd_year_anom = yearcumvpd - mean(yearcumvpd, na.rm = TRUE),
    gdd_year_anom = yearcumgdd - mean(yearcumgdd, na.rm = TRUE),
    tmin_year_anom = yearcumtmin - mean(yearcumtmin, na.rm = TRUE)
  )
df <- df %>%
  mutate(
    vpd_gdd_year = yearcumvpd * yearcumgdd,
    stress_year_index = scale(yearcumvpd) * scale(yearcumtmin),
    climate_energy = yearcumgdd / (yearcumvpd + 1)
  )
df <- df %>%
  mutate(
    vpd_year_rank = rank(yearcumvpd) / n(),
    gdd_year_rank = rank(yearcumgdd) / n(),
    tmin_year_rank = rank(yearcumtmin) / n()
  )

df$vpd_year_rank



# 1. What year-level variables exist
names(df)[grep("year|cum|yr", names(df), ignore.case = TRUE)]

# 2. Quick summary of PDDOY and worst years
cat("PDDOY range:", range(df$PDDOY, na.rm=TRUE), "\n")
cat("PDDOY mean:", mean(df$PDDOY, na.rm=TRUE), "\n")
cat("Rows per year:\n")
print(table(df$Year))
# 3. Which years appear most in worst 10 pairs
worst10 <- results %>% arrange(desc(RF_Test_RMSE)) %>% head(10)
print(worst10[ , c("test_years","RF_Test_RMSE","RF_Test_R2")])

# 4. Year-level climate fingerprint
df %>%
  group_by(Year) %>%
  summarise(across(where(is.numeric), mean, na.rm=TRUE)) %>%
  dplyr::select(Year, starts_with("year"), starts_with("cum")) %>%
  as.data.frame() %>%
  print()



#phenology
# Quick correlation check before adding anything
pheno_vars <- c("UD.UD", "SD.SD", "DD.DD", "RD.RD",
                "DOY_maxROC_kNDVI", "rsp.rsp", "a3.a3",
                "rau.rau", "a5.a5", "avgsoilclay")

# Check availability
available_pheno <- intersect(pheno_vars, names(df))
cat("Available:", paste(available_pheno, collapse = ", "), "\n")

# Correlation with PDDOY
cor_table <- df %>%
  dplyr::select(all_of(available_pheno), PDDOY) %>%
  drop_na() %>%
  summarise(across(all_of(available_pheno), 
                   ~round(cor(.x, PDDOY, use = "complete"), 3))) %>%
  pivot_longer(everything(), 
               names_to = "variable", 
               values_to = "cor_with_PDDOY") %>%
  arrange(desc(abs(cor_with_PDDOY)))

print(as.data.frame(cor_table))

# Also check SNR (interannual signal strength)
# Same function as before
compute_year_signal <- function(data, var_name) {
  temp <- data %>%
    group_by(Year) %>%
    summarise(mean_year = mean(.data[[var_name]], na.rm = TRUE),
              .groups = "drop")
  total_var  <- var(temp$mean_year, na.rm = TRUE)
  within_var <- data %>%
    group_by(Year) %>%
    summarise(v = var(.data[[var_name]], na.rm = TRUE),
              .groups = "drop") %>%
    summarise(mean(v, na.rm = TRUE)) %>%
    pull()
  data.frame(variable = var_name,
             cor_PDDOY = round(cor(data[[var_name]], 
                                   data$PDDOY, use = "complete"), 3),
             SNR       = round(total_var / within_var, 3))
}

df_clean <- df %>% filter(!is.na(PDDOY))

snr_table <- bind_rows(lapply(available_pheno, 
                              function(v) compute_year_signal(df_clean, v))) %>%
  arrange(desc(abs(cor_PDDOY)))

cat("\n--- Correlation + SNR for new candidates ---\n")
print(as.data.frame(snr_table))
# Check if UD.UD adds information beyond SOS_trs.sos
if (all(c("UD.UD", "SOS_trs.sos") %in% names(df))) {
  
  cat("\n--- UD.UD vs SOS_trs.sos ---\n")
  cat("Correlation between them:",
      round(cor(df$UD.UD, df$SOS_trs.sos, use = "complete"), 3), "\n")
  
  # If cor < 0.85, they carry different info — add UD.UD
  # If cor > 0.95, they are redundant — skip it
  
  # Also create disagreement feature
  df$sos_ud_diff <- df$SOS_trs.sos - df$UD.UD
  cat("SOS-UD disagreement cor with PDDOY:",
      round(cor(df$sos_ud_diff, df$PDDOY, use = "complete"), 3), "\n")
}
# Add to your existing feature_cols vector
new_candidates <- c(
  "UD.UD",            # greenup DOY — Gu method
  "DOY_maxROC_kNDVI", # vegetation momentum
  "avgsoilclay",      # soil texture — drainage capacity
  "rsp.rsp",          # green-up speed
  "sos_ud_diff"       # disagreement between SOS methods
)

# Only add after running correlation check above
# Rule: add if |cor with PDDOY| > 0.25 AND not redundant with existing features

