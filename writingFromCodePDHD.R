library(dplyr)
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

# Print features used for Planting Date (PD)
cat("Features used in LIMP Random Forest to predict Planting Date (PDDOY):\n")
cat(paste(setdiff(LIMPRFPlantingFeatures, "PDDOY"), collapse = ", "))


# Print features used for Harvest Date (HD)
cat("\nFeatures used in LIMP Random Forest to predict Harvest Date (HDDOY):\n")
cat(paste(setdiff(LIMPRFHarvestFeatures, "HDDOY"), collapse = ", "))

cat("\nFeatures used in Deines Random Forest to predict Harvest Date (HDDOY):\n")
print(setdiff(DeinesRandomForestPlantingFeatures , "PDDOY"))


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


#--------------------------------------------------------------------------------
#Random Forest
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
# Create summary sentence
summary_sentence <- paste0(
  "The Deines Random Forest model achieved an MAE of ", round(mae_train,2), " ± ", round(sd_mae_train,2),
  " days (R² = ", round(r2_train,3), " ± ", round(sd_r2_train,3), ") on the training set, ",
  round(mae_val,2), " ± ", round(sd_mae_val,2), " days (R² = ", round(r2_val,3), " ± ", round(sd_r2_val,3), ") on the validation set, and ",
  round(mae_test,2), " days (R² = ", round(r2_test,3), ") on the test set."
)
print(summary_sentence)



# Extract MAE and SD values for LIMP Random Forest (Harvest)
mae_train <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (DOY)" & plot1_df_harvest_pheno$Dataset == "Train"]
mae_val   <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (DOY)" & plot1_df_harvest_pheno$Dataset == "Validation"]
mae_test  <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (DOY)" & plot1_df_harvest_pheno$Dataset == "Test"]
sd_mae_train <- plot1_df_harvest_pheno$SD[plot1_df_harvest_pheno$Metric == "MAE (DOY)" & plot1_df_harvest_pheno$Dataset == "Train"]
sd_mae_val   <- plot1_df_harvest_pheno$SD[plot1_df_harvest_pheno$Metric == "MAE (DOY)" & plot1_df_harvest_pheno$Dataset == "Validation"]
# Extract R² and SD values
r2_train <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Train"]
r2_val   <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Validation"]
r2_test  <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Test"]
sd_r2_train <- plot2_df_harvest_pheno$SD[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Train"]
sd_r2_val   <- plot2_df_harvest_pheno$SD[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Validation"]
# Create summary sentence with ± SD
summary_sentence <- paste0(
  "The LIMP Random Forest model achieved an MAE of ", round(mae_train,2), " ± ", round(sd_mae_train,2),
  " days (R² = ", round(r2_train,3), " ± ", round(sd_r2_train,3), ") on the training set, ",
  round(mae_val,2), " ± ", round(sd_mae_val,2), " days (R² = ", round(r2_val,3), " ± ", round(sd_r2_val,3), ") on the validation set, and ",
  round(mae_test,2), " days (R² = ", round(r2_test,3), ") on the test set."
)
print(summary_sentence)


# Extract MAE and SD values for LIMP Random Forest (Planting)
mae_train <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (DOY)" & plot1_df_planting_pheno$Dataset == "Train"]
mae_val   <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (DOY)" & plot1_df_planting_pheno$Dataset == "Validation"]
mae_test  <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (DOY)" & plot1_df_planting_pheno$Dataset == "Test"]
sd_mae_train <- plot1_df_planting_pheno$SD[plot1_df_planting_pheno$Metric == "MAE (DOY)" & plot1_df_planting_pheno$Dataset == "Train"]
sd_mae_val   <- plot1_df_planting_pheno$SD[plot1_df_planting_pheno$Metric == "MAE (DOY)" & plot1_df_planting_pheno$Dataset == "Validation"]
# Extract R² and SD values
r2_train <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Train"]
r2_val   <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Validation"]
r2_test  <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Test"]
sd_r2_train <- plot2_df_planting_pheno$SD[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Train"]
sd_r2_val   <- plot2_df_planting_pheno$SD[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Validation"]
# Create summary sentence with ± SD
summary_sentence <- paste0(
  "The LIMP Random Forest model achieved an MAE of ", round(mae_train,2), " ± ", round(sd_mae_train,2),
  " days (R² = ", round(r2_train,3), " ± ", round(sd_r2_train,3), ") on the training set, ",
  round(mae_val,2), " ± ", round(sd_mae_val,2), " days (R² = ", round(r2_val,3), " ± ", round(sd_r2_val,3), ") on the validation set, and ",
  round(mae_test,2), " days (R² = ", round(r2_test,3), ") on the test set."
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
mae_train_limp <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (DOY)" & plot1_df_planting_pheno$Dataset == "Train"]
mae_val_limp   <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (DOY)" & plot1_df_planting_pheno$Dataset == "Validation"]
mae_test_limp  <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (DOY)" & plot1_df_planting_pheno$Dataset == "Test"]

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
  "It also improved R² by ", round(r2_train_diff,3), " on the training set, ",
  round(r2_val_diff,3), " on the validation set, and decreased slightly by ", round(abs(r2_test_diff),3), 
  " on the test set."
)
print(performance_sentence)

# Extract MAE values for LIMP
mae_pd_train <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (DOY)" & plot1_df_planting_pheno$Dataset == "Train"]
mae_pd_val   <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (DOY)" & plot1_df_planting_pheno$Dataset == "Validation"]
mae_pd_test  <- plot1_df_planting_pheno$Value[plot1_df_planting_pheno$Metric == "MAE (DOY)" & plot1_df_planting_pheno$Dataset == "Test"]

mae_hd_train <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (DOY)" & plot1_df_harvest_pheno$Dataset == "Train"]
mae_hd_val   <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (DOY)" & plot1_df_harvest_pheno$Dataset == "Validation"]
mae_hd_test  <- plot1_df_harvest_pheno$Value[plot1_df_harvest_pheno$Metric == "MAE (DOY)" & plot1_df_harvest_pheno$Dataset == "Test"]

# Extract R² values for LIMP
r2_pd_train <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Train"]
r2_pd_val   <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Validation"]
r2_pd_test  <- plot2_df_planting_pheno$Value[plot2_df_planting_pheno$Metric == "R²" & plot2_df_planting_pheno$Dataset == "Test"]

r2_hd_train <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Train"]
r2_hd_val   <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Validation"]
r2_hd_test  <- plot2_df_harvest_pheno$Value[plot2_df_harvest_pheno$Metric == "R²" & plot2_df_harvest_pheno$Dataset == "Test"]

# Calculate differences: PD - HD
mae_train_diff <- mae_pd_train - mae_hd_train
mae_val_diff   <- mae_pd_val - mae_hd_val
mae_test_diff  <- mae_pd_test - mae_hd_test
# Determine R² direction
r2_train_dir <- ifelse(r2_train_diff >= 0, "higher", "lower")
r2_val_dir   <- ifelse(r2_val_diff >= 0, "higher", "lower")
r2_test_dir  <- ifelse(r2_test_diff >= 0, "higher", "lower")

# Print summary
summary_sentence <- paste0(
  "For the LIMP Random Forest model, harvest prediction (HD) has lower MAE than planting prediction (PD) by ",
  round(mae_train_diff,2), " days on the training set, ",
  round(mae_val_diff,2), " days on the validation set, and ",
  round(mae_test_diff,2), " days on the test set. ",
  "It also has ", r2_train_dir, " R² by ", abs(round(r2_train_diff,3)), " on the training set, ",
  abs(round(r2_val_diff,3)), " on the validation set, and ",
  abs(round(r2_test_diff,3)), " on the test set."
)

print(summary_sentence)


# Select cumulative columns
cum_cols <- names(df)[grepl("cum", names(df))]
names(df)[grepl("cum", names(df))]


cum_cols <- names(df)[grepl("cum", names(df))]


#------------------------------------------------------------
# CREATE THE PERCENTILE GRAPHS
#------------------------------------------------------------
df_percentile <- df %>%
  mutate(PD_group = cut(PDDOY,
                        breaks = quantile(PDDOY, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                        include.lowest = TRUE,
                        labels = c("Low", "Medium", "High")))

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
# Loop through each PD_group
unique_groups <- unique(summary_long$PD_group)

for (grp in unique_groups) {
  grp_data <- summary_long %>% dplyr::filter(PD_group == grp)
  
  # Create a string with all variables and their mean ± SD
  vars_string <- paste(paste0(grp_data$variable, " = ", grp_data$mean_sd), collapse = "; ")
  
  cat("The", grp, "percentile of PD fields has: ", vars_string, ".\n\n")
}

#-----------------------------------------------------
# CREATE THE PERCENTILE GROUPS FOR PD
#------------------------------------------------------------
df_percentile <- df %>%
  mutate(PD_group = cut(PDDOY,
                        breaks = quantile(PDDOY, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                        include.lowest = TRUE,
                        labels = c("Low", "Medium", "High")))

# Define lag variables
lag_vars <- c("lagsosderpddoy", "lagsostrspddoy", "lagudpddoy")

# Compute mean and SD for lag variables
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
  mutate(mean_sd = paste0(round(as.numeric(mean), 2), " ± ", round(as.numeric(sd), 2))) %>%
  select(PD_group, variable, mean_sd)

print(lag_summary_long)

#-----------------------------------------------------
# PRINT OUT RESULTS FOR EACH GROUP
#-----------------------------------------------------
unique_groups <- unique(lag_summary_long$PD_group)

for (grp in unique_groups) {
  grp_data <- lag_summary_long %>% dplyr::filter(PD_group == grp)
  
  # Create a string with all variables and their mean ± SD
  vars_string <- paste(paste0(grp_data$variable, " = ", grp_data$mean_sd), collapse = "; ")
  
  cat("The", grp, "percentile of PD fields has lag values: ", vars_string, ".\n\n")
}


#--------------------------------------------------------------------
#Important Features
#--------------------------------------------------------------------
----#-------------HD---------------------
# Get the top 7 variables ranked by Total_scaled
top_vars <- harvest_importance_long %>%
  distinct(variable, Total_scaled) %>%   # keep only one row per variable
  arrange(desc(Total_scaled)) %>%
  slice(1:7)

# Print in a single line
cat(
  paste0(top_vars$variable, " (", round(top_vars$Total_scaled, 2), ")",
         collapse = "; "),
  "\n"
)


#----#-------------PD---------------------
top_vars <- planting_importance_long %>%
  distinct(Variable, Total_scaled) %>%   # keep only one row per variable
  arrange(desc(Total_scaled)) %>%
  slice(1:7)

# Print in a single line
cat(
  paste0(top_vars$Variable, " (", round(top_vars$Total_scaled, 2), ")",
         collapse = "; "),
  "\n"
)


###----------------------------------------------------------
## rsp and rau
#------------------------------------------------------------------

#    # get SOS and EOS according to first order derivative
#sos.index <- median(which.max(der1[1:(half.season-5)]))
#eos.index <- median(which.min(der1[(half.season+5):length(der1)])) + half.season

#Based on the code, rsp (rate of spring) is calculated as the first derivative's value at the point of maximum increase during the spring, while rau (rate of autumn) is the first derivative's value at the point of maximum decrease during the autumn.
mean(phenology_df$rsp.rsp)
sd(phenology_df$rsp.rsp)
mean(phenology_df$rau.rau)
sd(phenology_df$rau.rau)



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


