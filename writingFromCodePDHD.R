library(dplyr)
library(tidyr)

# Helper function to compute mode safely (returns NA if multiple modes)
safe_mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  modes <- ux[tab == max(tab)]
  if(length(modes) == 1) return(modes)
  else return(NA)  # multiple modes or no mode
}

summary_stats <- combined_data %>%
  summarise(
    Mean_PD = mean(PDDOY, na.rm = TRUE),
    SD_PD = sd(PDDOY, na.rm = TRUE),
    Median_PD = median(PDDOY, na.rm = TRUE),
    Mode_PD = safe_mode(PDDOY),
    Min_PD = min(PDDOY, na.rm = TRUE),
    Max_PD = max(PDDOY, na.rm = TRUE),
    Mean_HD = mean(HDDOY, na.rm = TRUE),
    SD_HD = sd(HDDOY, na.rm = TRUE),
    Median_HD = median(HDDOY, na.rm = TRUE),
    Mode_HD = safe_mode(HDDOY),
    Min_HD = min(HDDOY, na.rm = TRUE),
    Max_HD = max(HDDOY, na.rm = TRUE)
  )

# Create mode text or skip if NA
mode_text_pd <- ifelse(is.na(summary_stats$Mode_PD), "", paste0(", mode = ", summary_stats$Mode_PD))
mode_text_hd <- ifelse(is.na(summary_stats$Mode_HD), "", paste0(", mode = ", summary_stats$Mode_HD))

# Print means, medians, modes
cat(sprintf(
  "Planting Day (PD) has a mean of %.1f ± %.1f, median of %d%s; Harvest Day (HD) has a mean of %.1f ± %.1f, median of %d%s.\n",
  summary_stats$Mean_PD, summary_stats$SD_PD, summary_stats$Median_PD, mode_text_pd,
  summary_stats$Mean_HD, summary_stats$SD_HD, summary_stats$Median_HD, mode_text_hd
))

# Print min and max values
cat(sprintf(
  "The planting days range from %d to %d, while the harvest days range from %d to %d.\n",
  summary_stats$Min_PD, summary_stats$Max_PD, summary_stats$Min_HD, summary_stats$Max_HD
))

#---------------------------------------------------------
#Features used to predict LIMP Random forest
#---------------------------------------------------------

# --- Print features used for Planting Date (PD)
cat("Features used in LIMP Random Forest to predict Planting Date (PDDOY):\n")
cat(paste(sort(setdiff(LIMPRFPlantingFeatures, "PDDOY")), collapse = ", "))

# --- Print features used for Harvest Date (HD)
cat("\n\nFeatures used in LIMP Random Forest to predict Harvest Date (HDDOY):\n")
cat(paste(sort(setdiff(LIMPRFHarvestFeatures, "HDDOY")), collapse = ", "))

# --- Print features used in Deines Random Forest (Planting)
cat("\n\nFeatures used in Deines Random Forest to predict Planting Date (PDDOY):\n")
cat(paste(sort(setdiff(DeinesRandomForestPlantingFeatures, "PDDOY")), collapse = ", "))


# Replace newline with comma + space for readability
PDmetrics_label1_clean <- gsub("\n", ", ", metrics_label1)
PDmetrics_label2_clean <- gsub("\n", ", ", metrics_label2)
PDmetrics_label3_clean <- gsub("\n", ", ", metrics_label3)
PDmetrics_label4_clean <- gsub("\n", ", ", metrics_label4)

# Combine into a single sentence
paste0(
  "The Threshold method performed with ", PDmetrics_label1_clean, 
  "; the Derivative method performed with ", PDmetrics_label2_clean, 
  "the Inflection method performed with ", PDmetrics_label3_clean, 
  "; and the Gu method performed with ", PDmetrics_label4_clean
)


# Combine into a single sentence
paste0(
  "The Threshold method performed with ", metrics_label1_clean, 
  "; the Derivative method performed with ", metrics_label2_clean, 
  "the Inflection method performed with ", metrics_label3_clean, 
  "; and the Gu method performed with ", metrics_label4_clean
)

HDmetrics_label1_clean <- gsub("\n", ", ", metrics_label1)
HDmetrics_label2_clean <- gsub("\n", ", ", metrics_label2)
HDmetrics_label3_clean <- gsub("\n", ", ", metrics_label3)
HDmetrics_label4_clean <- gsub("\n", ", ", metrics_label4)

# Combine into a single sentence
paste0(
  "The Threshold method performed with ", HDmetrics_label1_clean, 
  "; the Derivative method performed with ", HDmetrics_label2_clean, 
  "the Inflection method performed with ", HDmetrics_label3_clean, 
  "; and the Gu method performed with ", HDmetrics_label4_clean
)




#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
# CREATE THE PERCENTILE GRAPHS
#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------
df_percentile <- df %>%
  mutate(PD_group = cut(PDDOY,
                        breaks = quantile(PDDOY, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                        include.lowest = TRUE,
                        labels = c("Early", "Mid", "Late")))
# Compute mean and SD
summary_df <- df_percentile %>%
  group_by(PD_group) %>%
  summarise(across(all_of(cum_cols),
                   list(mean = ~mean(as.numeric(.x), na.rm = TRUE),
                        sd   = ~sd(as.numeric(.x), na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# Pivot longer safely using names_pattern
summary_long <- summary_df %>%
  pivot_longer(
    cols = -PD_group,
    names_to = c("variable", "stat"),
    names_pattern = "(.*)_(mean|sd)$",
    values_drop_na = TRUE
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(mean_sd = paste0(round(as.numeric(mean), 2), " ± ", round(as.numeric(sd), 2))) %>%
  select(PD_group, variable, mean_sd)
print(summary_long)
unique_groups <- unique(summary_long$PD_group)
for (grp in unique_groups) {
  grp_data <- summary_long %>% dplyr::filter(PD_group == grp)
  
  # Create a string with all variables and their mean ± SD
  vars_string <- paste(paste0(grp_data$variable, " = ", grp_data$mean_sd), collapse = "; ")
  
  cat("The", grp, "percentile of PD fields has: ", vars_string, ".\n\n")
}



for (grp in unique_groups) {
  grp_data <- summary_long %>% filter(PD_group == grp)
  
  # Helper function to extract mean ± sd string for a given variable
  get_val <- function(var) {
    val <- grp_data %>% filter(variable == var) %>% pull(mean_sd)
    if (length(val) == 0) return(NA)
    val
  }
  
  # Extract key variable values
  soilT <- get_val("cum_soiltemp")        # °C
  #tmean <- get_val("cum_tmean")           # °C
  tmin  <- get_val("cum_tmin")            # °C
  #tmax  <- get_val("cum_tmax")            # °C
  gdd   <- get_val("cum_gdd")             # °C
  vpd   <- get_val("cum_vpd")             # kPa
  rh    <- get_val("cum_RH")              # %
  srad  <- get_val("cum_meansrad")        # W m^-2
  
  # Generate narrative
  cat(paste0(
    grp, " PD fields (", grp, " percentile) experienced soils of ", soilT, " °C (SoilTmean_cum), ",
  "min temperature of ", tmin, " °C (Tmin) ", "heat accumulation of ", gdd, " °C (GDDcum), mean daily vapor pressure deficit of ", vpd, " kPa (VPDcum), ",
    "daily maximum relative humidity of ", rh, " % (RHcum), and daily surface downward shortwave radiation of ", srad, " W m^-2 (Sradcum).\n\n"
  ))
}

#-----------------------------------------------------
# CREATE THE PERCENTILE GROUPS FOR HD
#------------------------------------------------------------
# Create quantile-based groups for HDDOY
df_percentile_HD <- dfharvest %>%
  mutate(HD_group = cut(HDDOY,
                        breaks = quantile(HDDOY, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                        include.lowest = TRUE,
                        labels = c("Early", "Mid", "Late")))
cum_cols <- names(dfharvest)[grepl("cum", names(df))]
# Compute mean and SD for each group
summary_df_HD <- df_percentile_HD %>%
  group_by(HD_group) %>%
  summarise(across(all_of(cum_cols),
                   list(mean = ~mean(as.numeric(.x), na.rm = TRUE),
                        sd   = ~sd(as.numeric(.x), na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# Pivot longer safely using names_pattern
summary_long_HD <- summary_df_HD %>%
  pivot_longer(
    cols = -HD_group,
    names_to = c("variable", "stat"),
    names_pattern = "(.*)_(mean|sd)$",
    values_drop_na = TRUE
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(mean_sd = paste0(round(as.numeric(mean), 2), " ± ", round(as.numeric(sd), 2))) %>%
  select(HD_group, variable, mean_sd)

print(summary_long_HD)

# Loop through each HD_group to print summary
unique_groups_HD <- unique(summary_long_HD$HD_group)

for (grp in unique_groups_HD) {
  grp_data <- summary_long_HD %>% dplyr::filter(HD_group == grp)
  
  # Create a string with all variables and their mean ± SD
  vars_string <- paste(paste0(grp_data$variable, " = ", grp_data$mean_sd), collapse = "; ")
  
  cat("The", grp, "percentile of HD fields has: ", vars_string, ".\n\n")
}

#-----------------------------------------------------
# CREATE THE PERCENTILE GROUPS FOR PD
#-----------------------------------------------------
df_percentile <- df %>%
  mutate(PD_group = cut(PDDOY,
                        breaks = quantile(PDDOY, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                        include.lowest = TRUE,
                        labels = c("Early", "Mid", "Late")))

# Define lag variables including rsp.rsp
lag_vars <- c("lagsosderpddoy", "lagsostrspddoy", "lagudpddoy", "rsp.rsp")

# Compute mean and SD for lag variables
lag_summary_df <- df_percentile %>%
  group_by(PD_group) %>%
  summarise(across(all_of(lag_vars),
                   list(mean = ~mean(as.numeric(.x), na.rm = TRUE),
                        sd   = ~sd(as.numeric(.x), na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# Pivot longer for nicer format
lag_summary_df <- df_percentile %>%
  group_by(PD_group) %>%
  summarise(across(all_of(lag_vars),
                   list(mean = ~mean(as.numeric(.x), na.rm = TRUE),
                        sd   = ~sd(as.numeric(.x), na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# Pivot longer for nicer format
lag_summary_long <- lag_summary_df %>%
  pivot_longer(
    cols = -PD_group,
    names_to = c("variable", "stat"),
    names_pattern = "(.*)_(mean|sd)$",
    values_drop_na = TRUE
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  
  # *** FIX: Conditionally set precision: 4 for rsp.rsp, 2 for others ***
  
  mutate(
    mean_sd = case_when(
      # Use 4 decimal places for rsp.rsp
      variable == "rsp.rsp" ~ paste0(round(as.numeric(mean), 4), " ± ", round(as.numeric(sd), 4)),
      # Use 2 decimal places for all other variables
      TRUE ~ paste0(round(as.numeric(mean), 2), " ± ", round(as.numeric(sd), 2))
    )
  ) %>%
  select(PD_group, variable, mean_sd)

# Print nicely
unique_groups <- unique(lag_summary_long$PD_group)
for (grp in unique_groups) {
  grp_data <- lag_summary_long %>% dplyr::filter(PD_group == grp)
  
  # Create a string with all variables and their mean ± SD
  vars_string <- paste(paste0(grp_data$variable, " = ", grp_data$mean_sd), collapse = "; ")
  
  cat("The", grp, "percentile of PD fields has lag values: ", vars_string, ".\n\n")
}

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#Random Forest: Random Forest prediction of PD and HD and PDSOSDeriv
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# Extract MAE and SD values for Deines Random Forest (Planting)
mae_train <- plot1_df_planting_deines$Value[plot1_df_planting_deines$Metric == "MAE (DOY)" & plot1_df_planting_deines$Dataset == "Train"]
mae_val   <- plot1_df_planting_deines$Value[plot1_df_planting_deines$Metric == "MAE (DOY)" & plot1_df_planting_deines$Dataset == "Validation"]
mae_test  <- plot1_df_planting_deines$Value[plot1_df_planting_deines$Metric == "MAE (DOY)" & plot1_df_planting_deines$Dataset == "Test"]
sd_mae_train <- plot1_df_planting_deines$SD[plot1_df_planting_deines$Metric == "MAE (DOY)" & plot1_df_planting_deines$Dataset == "Train"]
sd_mae_val   <- plot1_df_planting_deines$SD[plot1_df_planting_deines$Metric == "MAE (DOY)" & plot1_df_planting_deines$Dataset == "Validation"]
# Extract R² and SD values
r2_train <- plot2_df_planting_deines$Value[plot2_df_planting_deines$Metric == "R²" & plot2_df_planting_deines$Dataset == "Train"]
r2_val   <- plot2_df_planting_deines$Value[plot2_df_planting_deines$Metric == "R²" & plot2_df_planting_deines$Dataset == "Validation"]
r2_test  <- plot2_df_planting_deines$Value[plot2_df_planting_deines$Metric == "R²" & plot2_df_planting_deines$Dataset == "Test"]
sd_r2_train <- plot2_df_planting_deines$SD[plot2_df_planting_deines$Metric == "R²" & plot2_df_planting_deines$Dataset == "Train"]
sd_r2_val   <- plot2_df_planting_deines$SD[plot2_df_planting_deines$Metric == "R²" & plot2_df_planting_deines$Dataset == "Validation"]
# Create summary sentence with two significant figures
# Helper function: show 3 decimals if value rounds to 0.00
fmt <- function(x) {
  if (round(x, 2) == 0) {
    format(round(x, 3), nsmall = 3)
  } else {
    format(round(x, 2), nsmall = 2)
  }
}

# Create summary sentence
summary_sentence <- paste0(
  "The Deines Random Forest model achieved an MAE of ", fmt(mae_train), " ± ", fmt(sd_mae_train),
  " days (R² = ", fmt(r2_train), " ± ", fmt(sd_r2_train), ") on the training set, ",
  fmt(mae_val), " ± ", fmt(sd_mae_val), " days (R² = ", fmt(r2_val), " ± ", fmt(sd_r2_val), ") on the validation set, and ",
  fmt(mae_test), " days (R² = ", fmt(r2_test), ") on the test set."
)
print(summary_sentence)


# Extract MAE and SD values for LIMP Random Forest (Planting)
mae_train <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (day)" & plot1_df_planting_pheno$Dataset == "Train"]
mae_val   <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (day)" & plot1_df_planting_pheno$Dataset == "Validation"]
mae_test  <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (day)" & plot1_df_planting_pheno$Dataset == "Test"]
sd_mae_train <- plot1_df_planting_pheno$SD[plot1_df_planting_pheno$Metric == "MAE (day)" & plot1_df_planting_pheno$Dataset == "Train"]
sd_mae_val   <- plot1_df_planting_pheno$SD[plot1_df_planting_pheno$Metric == "MAE (day)" & plot1_df_planting_pheno$Dataset == "Validation"]
# Extract R² and SD values
r2_train <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Train"]
r2_val   <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Validation"]
r2_test  <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Test"]
sd_r2_train <- plot2_df_planting_pheno$SD[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Train"]
sd_r2_val   <- plot2_df_planting_pheno$SD[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Validation"]
# Create summary sentence with ± SD
# Helper function: format numeric values safely
fmt <- function(x, decimals = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("")  # handle missing or undefined values
  }
  if (round(x, decimals) == 0) {
    return(format(round(x, decimals + 1), nsmall = decimals + 1))
  } else {
    return(format(round(x, decimals), nsmall = decimals))
  }
}

# Create summary sentence
summary_sentence <- paste0(
  "The LIMP Random Forest model achieved an MAE of ", fmt(mae_train, 2), " ± ", fmt(sd_mae_train, 2),
  " days (R² = ", fmt(r2_train, 2), " ± ", fmt(sd_r2_train, 3), ") on the training set, ",
  fmt(mae_val, 2), " ± ", fmt(sd_mae_val, 2), " days (R² = ", fmt(r2_val, 2), " ± ", fmt(sd_r2_val, 2), ") on the validation set, and ",
  fmt(mae_test, 2), " days (R² = ", fmt(r2_test, 2), ") on the test set."
)

print(summary_sentence)
plot2_df
# --- Extract key metrics ---
mae_train <- plot1_df$Value[plot1_df$Metric == "MAE" & plot1_df$Dataset == "Train"]
sd_mae_train <- plot1_df$SD[plot1_df$Metric == "MAE" & plot1_df$Dataset == "Train"]

mae_val <- plot1_df$Value[plot1_df$Metric == "MAE" & plot1_df$Dataset == "Validation"]
sd_mae_val <- plot1_df$SD[plot1_df$Metric == "MAE" & plot1_df$Dataset == "Validation"]

mae_test <- plot1_df$Value[plot1_df$Metric == "MAE" & plot1_df$Dataset == "Test"]

r2_train <- plot2_r2_df$Value[plot2_r2_df$Dataset == "Train"]
sd_r2_train <- plot2_r2_df$SD[plot2_r2_df$Dataset == "Train"]

r2_val <- plot2_r2_df$Value[plot2_r2_df$Dataset == "Validation"]
sd_r2_val <- plot2_r2_df$SD[plot2_r2_df$Dataset == "Validation"]

r2_test <- plot2_r2_df$Value[plot2_r2_df$Dataset == "Test"]
# --- Helper function ---
fmt <- function(x, decimals = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("")  # handle missing or undefined values
  }
  if (round(x, decimals) == 0) {
    return(format(round(x, decimals + 1), nsmall = decimals + 1))
  } else {
    return(format(round(x, decimals), nsmall = decimals))
  }
}

# --- Create summary sentence ---
summary_sentence <- paste0(
  "The PDSOSfderiv  model achieved an MAE of ", fmt(mae_train, 2), " ± ", fmt(sd_mae_train, 2),
  " days (R² = ", fmt(r2_train, 2), " ± ", fmt(sd_r2_train, 2), ") on the training set, ",
  fmt(mae_val, 2), " ± ", fmt(sd_mae_val, 2), " days (R² = ", fmt(r2_val, 3), " ± ", fmt(sd_r2_val, 2), ") on the validation set, and ",
  fmt(mae_test, 2), " days (R² = ", fmt(r2_test, 2), ") on the test set."
)

print(summary_sentence)



# Deines Random Forest MAE and R²
mae_train_deines <- plot1_df_planting_deines$Value[plot1_df_planting_deines$Metric == "MAE (DOY)" & plot1_df_planting_deines$Dataset == "Train"]
mae_val_deines   <- plot1_df_planting_deines$Value[plot1_df_planting_deines$Metric == "MAE (DOY)" & plot1_df_planting_deines$Dataset == "Validation"]
mae_test_deines  <- plot1_df_planting_deines$Value[plot1_df_planting_deines$Metric == "MAE (DOY)" & plot1_df_planting_deines$Dataset == "Test"]

r2_train_deines <- plot2_df_planting_deines$Value[plot2_df_planting_deines$Metric == "R²" & plot2_df_planting_deines$Dataset == "Train"]
r2_val_deines   <- plot2_df_planting_deines$Value[plot2_df_planting_deines$Metric == "R²" & plot2_df_planting_deines$Dataset == "Validation"]
r2_test_deines  <- plot2_df_planting_deines$Value[plot2_df_planting_deines$Metric == "R²" & plot2_df_planting_deines$Dataset == "Test"]

# LIMP Random Forest MAE and R²
mae_train_limp <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (day)" & plot1_df_planting_pheno$Dataset == "Train"]
mae_val_limp   <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (day)" & plot1_df_planting_pheno$Dataset == "Validation"]
mae_test_limp  <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (day)" & plot1_df_planting_pheno$Dataset == "Test"]

r2_train_limp <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Train"]
r2_val_limp   <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Validation"]
r2_test_limp  <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Test"]

# Calculate performance difference: Deines - LIMP
mae_train_diff <- mae_train_deines - mae_train_limp
mae_val_diff   <- mae_val_deines - mae_val_limp
mae_test_diff  <- mae_test_deines - mae_test_limp

r2_train_diff <- r2_train_limp - r2_train_deines
r2_val_diff   <- r2_val_limp - r2_val_deines
r2_test_diff  <- r2_test_limp - r2_test_deines


performance_sentence <- paste0(
  "The LIMP Random Forest model outperformed the Deines Random Forest model by reducing the MAE by ",
  round(mae_train_diff,2), " days on the training set, ",
  round(mae_val_diff,2), " days on the validation set, and ",
  round(mae_test_diff,2), " days on the test set. ",
  "It also slightly improved R² by ", round(r2_train_diff,3), " on the training set, ",
  round(r2_val_diff,3), " on the validation set, and increased  by ", round(abs(r2_test_diff),3), 
  " on the test set."
)
print(performance_sentence)

# -----------------------------
# LIMP vs PDSOSderiv model
# -----------------------------
fmt <- function(x, decimals = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("")
  if (round(x, decimals) == 0) {
    format(round(x, decimals + 1), nsmall = decimals + 1)
  } else {
    format(round(x, decimals), nsmall = decimals)
  }
}

diff_train <- mae_train_pdsos - mae_train_limp
diff_val   <- mae_val_pdsos - mae_val_limp
diff_test  <- mae_test_pdsos - mae_test_limp

# -----------------------------
# Create summary sentence
# -----------------------------
summary_sentence <- paste0(
  "Compared to the LIMP model (MAE = ", fmt(mae_train_limp), ", ", fmt(mae_val_limp), ", and ", fmt(mae_test_limp),
  " days for training, validation, and test), the PDSOSderiv model had MAE values higher by approximately ",
  fmt(diff_train), ", ", fmt(diff_val), ", and ", fmt(diff_test),
  " days for the respective datasets."
)
cat(summary_sentence, "\n")


# Select cumulative columns
cum_cols <- names(df)[grepl("cum", names(df))]
names(df)[grepl("cum", names(df))]
cum_cols <- names(df)[grepl("cum", names(df))]



#-------------------------------
#HARVEST-----------------------
#-------------------------------

# Extract MAE and SD values for LIMP Random Forest (Harvest)
mae_train <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (day)" & plot1_df_harvest_pheno$Dataset == "Train"]
mae_val   <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (day)" & plot1_df_harvest_pheno$Dataset == "Validation"]
mae_test  <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (day)" & plot1_df_harvest_pheno$Dataset == "Test"]
sd_mae_train <- plot1_df_harvest_pheno$SD[plot1_df_harvest_pheno$Metric == "MAE (day)" & plot1_df_harvest_pheno$Dataset == "Train"]
sd_mae_val   <- plot1_df_harvest_pheno$SD[plot1_df_harvest_pheno$Metric == "MAE (day)" & plot1_df_harvest_pheno$Dataset == "Validation"]
# Extract R² and SD values
r2_train <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Train"]
r2_val   <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Validation"]
r2_test  <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Test"]
sd_r2_train <- plot2_df_harvest_pheno$SD[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Train"]
sd_r2_val   <- plot2_df_harvest_pheno$SD[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Validation"]
# Create summary sentence with ± SD
# Safer formatter: 2 decimals normally, 3 if rounded to 0.00
fmt <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("")  # handle NULL or NA safely
  }
  if (round(x, 2) == 0) {
    return(format(round(x, 3), nsmall = 3))
  } else {
    return(format(round(x, 2), nsmall = 2))
  }
}

# Create summary sentence
summary_sentence <- paste0(
  "The LIMP Random Forest model achieved an MAE of ", fmt(mae_train), " ± ", fmt(sd_mae_train),
  " days (R² = ", fmt(r2_train), " ± ", fmt(sd_r2_train), ") on the training set, ",
  fmt(mae_val), " ± ", fmt(sd_mae_val), " days (R² = ", fmt(r2_val), " ± ", fmt(sd_r2_val), ") on the validation set, and ",
  fmt(mae_test), " days (R² = ", fmt(r2_test), ") on the test set."
)

print(summary_sentence)

#------------------------------------
#LIMP MAE PD VS LIMP MAE HD
#------------------------------------
# Extract MAE values for LIMP
mae_pd_train <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (day)" & plot1_df_planting_pheno$Dataset == "Train"]
mae_pd_val   <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (day)" & plot1_df_planting_pheno$Dataset == "Validation"]
mae_pd_test  <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (day)" & plot1_df_planting_pheno$Dataset == "Test"]

mae_hd_train <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (day)" & plot1_df_harvest_pheno$Dataset == "Train"]
mae_hd_val   <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (day)" & plot1_df_harvest_pheno$Dataset == "Validation"]
mae_hd_test  <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (day)" & plot1_df_harvest_pheno$Dataset == "Test"]

# Extract R² values for LIMP
r2_pd_train <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Train"]
r2_pd_val   <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Validation"]
r2_pd_test  <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Test"]

r2_hd_train <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Train"]
r2_hd_val   <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Validation"]
r2_hd_test  <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Test"]
#------------------------------------
# Calculate differences: PD - HD
#------------------------------------
mae_train_diff <- mae_pd_train - mae_hd_train
mae_val_diff   <- mae_pd_val - mae_hd_val
mae_test_diff  <- mae_pd_test - mae_hd_test

r2_train_diff <- r2_pd_train - r2_hd_train
r2_val_diff   <- r2_pd_val - r2_hd_val
r2_test_diff  <- r2_pd_test - r2_hd_test

#------------------------------------
# Determine direction for MAE and R²
# Using PD - HD: 
#   - MAE: positive → HD lower (better), negative → HD higher (worse)
#   - R²: positive → HD lower, negative → HD higher
#------------------------------------
mae_train_dir <- ifelse(mae_train_diff >= 0, "lower", "higher")
mae_val_dir   <- ifelse(mae_val_diff >= 0, "lower", "higher")
mae_test_dir  <- ifelse(mae_test_diff >= 0, "lower", "higher")

r2_train_dir <- ifelse(r2_train_diff >= 0, "lower", "higher")
r2_val_dir   <- ifelse(r2_val_diff >= 0, "lower", "higher")
r2_test_dir  <- ifelse(r2_test_diff >= 0, "lower", "higher")

#------------------------------------
# Print summary sentence
#------------------------------------
summary_sentence <- paste0(
  "For the LIMP Random Forest model, harvest prediction (HD) has ", 
  mae_train_dir, " MAE than planting prediction (PD) by ", abs(round(mae_train_diff,2)), " days on the training set, ",
  mae_val_dir, " by ", abs(round(mae_val_diff,2)), " days on the validation set, and ",
  mae_test_dir, " by ", abs(round(mae_test_diff,2)), " days on the test set. ",
  "It also has ", r2_train_dir, " R² by ", abs(round(r2_train_diff,3)), " on the training set, ",
  r2_val_dir, " by ", abs(round(r2_val_diff,3)), " on the validation set, and ",
  r2_test_dir, " by ", abs(round(r2_test_diff,3)), " on the test set."
)

print(summary_sentence)
#------------------------------------
# Determine direction (increase/decrease)
#------------------------------------
r2_train_change <- ifelse(r2_train_diff < 0, "increased", "decreased")
r2_val_change   <- ifelse(r2_val_diff < 0, "increased", "decreased")
r2_test_change  <- ifelse(r2_test_diff < 0, "increased", "decreased")

#------------------------------------
# Construct summary sentence
#------------------------------------
summary_sentence <- paste0(
  "Compared with PD prediction, HD prediction by LIMP reduced MAE by ",
  abs(round(mae_train_diff, 2)), ", ",
  abs(round(mae_val_diff, 2)), ", and ",
  abs(round(mae_test_diff, 2)),
  " days on the training, validation, and test sets, respectively, while R² slightly ",
  r2_train_change, " by ", abs(round(r2_train_diff, 3))," on the training set, ",
 " by ", abs(round(r2_val_diff, 3)), " on the validation set, and ",
  r2_test_change, " by ", abs(round(r2_test_diff, 3)), " on the test set."
)

cat(summary_sentence, "\n")





#--------------------------------------------------------------------
#Important Features
#----#-------------PD---------------------
top_vars_pd <- planting_importance_long %>%
  distinct(Variable, Total_scaled) %>%   # keep only one row per variable
  arrange(desc(Total_scaled)) %>%
  slice(1:7)
cat(
  paste0(top_vars_pd$Variable, " (", round(top_vars_pd$Total_scaled, 2), ")",
         collapse = "; "),
  "\n"
)
top_vars_pd <- top_vars_pd %>% slice(1:7)  # replace top_vars_pd with your PD tibble
# Make sure the column names are correct
pd_sentence <- paste0(
  "For PD prediction, the most important variables based on the composite variable importance score were ",
  paste0(top_vars_pd$Variable, " (", round(top_vars_pd$Total_scaled, 2), ")", collapse = ", "),
  " (Figure S5)."
)
cat(pd_sentence, "\n")


#-------------HD---------------------
#------------- HD ---------------------
top_vars_hd <- harvest_importance_long %>%
  distinct(variable, Total_scaled) %>%   # keep only one row per variable
  arrange(desc(Total_scaled)) %>%
  slice(1:7)

# Print HD variables
cat(
  paste0(top_vars_hd$variable, " (", round(top_vars_hd$Total_scaled, 2), ")",
         collapse = "; "),
  "\n"
)

# Create HD summary sentence
hd_sentence <- paste0(
  "For HD prediction, the most important variables based on the composite variable importance score were ",
  paste0(top_vars_hd$variable, " (", round(top_vars_hd$Total_scaled, 2), ")", collapse = ", "),
  " (Figure S4)."
)
cat(hd_sentence, "\n")

###----------------------------------------------------------
#Correlation
###----------------------------------------------------------

# Print top 7 correlations of PD with other features (descending)
df_corr <- df %>%
  dplyr::select(
    cum_RH, SOS_trs.sos, SOS_deriv.sos, UD.UD,
    cum_meansrad, avgsoilorg, cum_tmin, EOS_trs.eos,
    EOS_deriv.eos, cum_soiltemp, cum_gdd, DD.DD, cum_vpd,
    Value_max_obs, PDDOY
  ) %>%
  # Filter and drop NA as in your original script
  filter(!is.na(PDDOY)) %>%
  drop_na()

# Create the correlation matrix
corr_mat <- cor(df_corr, use = "pairwise.complete.obs")
pd_corrs <- corr_mat["PDDOY", colnames(corr_mat) != "PDDOY"]
top7 <- sort(pd_corrs, decreasing = TRUE)[1:7]
top7_rounded <- round(top7, 2)
print(top7_rounded)
# optional single-line print
cat("Top 7 correlations with PD:", paste(names(top7_rounded), top7_rounded, collapse = "; "), "\n")

###----------------------------------------------------------
## rsp and rau
#------------------------------------------------------------------

#    # get SOS and EOS according to first order derivative
#sos.index <- median(which.max(der1[1:(half.season-5)]))
#eos.index <- median(which.min(der1[(half.season+5):length(der1)])) + half.season

#Based on the code, rsp (rate of spring) is calculated as the first derivative's value at the point of maximum increase during the spring, while rau (rate of autumn) is the first derivative's value at the point of maximum decrease during the autumn.
# Calculate mean and SD
mean_rsp <- mean(phenology_df$rsp.rsp, na.rm = TRUE)
sd_rsp <- sd(phenology_df$rsp.rsp, na.rm = TRUE)
mean_rau <- mean(phenology_df$rau.rau, na.rm = TRUE)
sd_rau <- sd(phenology_df$rau.rau, na.rm = TRUE)

# Print with three significant figures
cat("Mean rsp:", signif(mean_rsp, 1), "\n")
cat("SD rsp:", signif(sd_rsp,1), "\n")
cat("Mean rau:", signif(mean_rau, 1), "\n")
cat("SD rau:", signif(sd_rau, 1), "\n")

#Print with three significant figures
cat(
  "The rate of spring green-up (rsp), calculated as the first derivative at the point of maximum increase during the start-of-season (SOS), had a mean of",
  sprintf("%.4f ± %.4f", mean_rsp, sd_rsp),
  ", while the rate of autumn senescence (rau), calculated as the first derivative at the point of maximum decrease during the end-of-season (EOS), had a mean of",
  sprintf("%.4f ± %.4f.", mean_rau, sd_rau), "\n"
)


#----------------------------------------------------------------------------
## MAXDAYS of HD AND PD
#-----------------------------
# Add PDMaxdays and HDMaxdays to df
df <- df %>%
  mutate(
    PDMaxdays = abs(DOY_max_obs - PDDOY),
    HDMaxdays = abs(HDDOY - DOY_max_obs)
  )

stats <- df %>%
  summarise(
    mean_PD = mean(abs(DOY_max_obs - PDDOY), na.rm = TRUE),
    sd_PD   = sd(abs(DOY_max_obs - PDDOY), na.rm = TRUE),
    mean_HD = mean(abs(HDDOY - DOY_max_obs), na.rm = TRUE),
    sd_HD   = sd(abs(HDDOY - DOY_max_obs), na.rm = TRUE)
  )

PD_line <- paste0("The mean time to peak of PD is ",
                  round(stats$mean_PD, 2), " ± ",
                  round(stats$sd_PD, 2), " days.")

HD_line <- paste0("The mean time to peak of HD is ",
                  round(stats$mean_HD, 2), " ± ",
                  round(stats$sd_HD, 2), " days.")

print(PD_line)
print(HD_line)


#---------------------------------------------------]
#MONTHS OF PD
#---------------------------------------------------
# ------------------------------------------------------------
# Identify DOY ranges for Early, Mid, Late planting groups
# ------------------------------------------------------------

# Get quantile breaks you used earlier
pd_breaks <- quantile(df$PDDOY, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

pd_breaks

# Create a helper function to convert DOY → month name
doy_to_month <- function(doy) {
  as.character(format(as.Date(doy - 1, origin = "2025-01-01"), "%B"))
}

# Convert breakpoints to month labels
month_labels <- sapply(pd_breaks, doy_to_month)

month_labels

# Create readable ranges
early_range <- paste0(pd_breaks[1], "–", pd_breaks[2], 
                      " (", month_labels[1], "–", month_labels[2], ")")

mid_range   <- paste0(pd_breaks[2], "–", pd_breaks[3], 
                      " (", month_labels[2], "–", month_labels[3], ")")

late_range  <- paste0(pd_breaks[3], "–", pd_breaks[4], 
                      " (", month_labels[3], "–", month_labels[4], ")")

# Print narrative summary
cat("\nPLANTING DATE GROUPS BY MONTH RANGE\n")
cat("Early-planted fields: DOY ", early_range, "\n")
cat("Mid-planted fields:   DOY ", mid_range, "\n")
cat("Late-planted fields:  DOY ", late_range, "\n\n")



####################
#Scott Matthews report
####################
total_rows <- nrow(combined_data)
seeding_rows <- sum(combined_data$source == "seeding_rice_data")

total_rows
seeding_rows

cat("Total is", nrow(combined_data), "and Scott Matthews provided this seedling data.\n")
cat(
  "Total is", nrow(combined_data),
  "and Scott Matthews provided this seedling data (",
  sum(combined_data$source == "seeding_rice_data"), "records ).\n",
  sep = " "
)


