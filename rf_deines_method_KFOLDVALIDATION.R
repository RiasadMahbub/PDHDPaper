library(randomForest)
library(Metrics)
library(hydroGOF) # for NSE
library(dplyr)
library(ggplot2)
library(tidyr)
# 1. Create a consistent join key in sos_eos_df
# This pattern replaces the last underscore followed by 4 digits (year) with "_VI_" and the 4 digits.
sos_eos_with_lag <- sos_eos_with_lag %>%
  mutate(Field_ID_Join = stringr::str_replace(Field_Year, "(_\\d{4})$", "_VI\\1"))

# 2. Perform the left_join to merge sos_eos_df into combined_df
# Select specific columns from sos_eos_df to avoid bringing all, and rename R2, RMSE, MAE, NSE
# to prevent conflicts with performance metrics later.
combined_df <- combined_df %>%
  left_join(sos_eos_with_lag %>%
              dplyr::select(
                Field_ID_Join, SOS, EOS, a, b, c, d, e, f,Lag_Days, cumulative_GDD_at_SOS, 
                sos_eos_R2 = R2, sos_eos_RMSE = RMSE, sos_eos_MAE = MAE, sos_eos_NSE = NSE,
                observed_SOS, observed_EOS, cumulative_GDD_at_SOS
              ),
            by = c("Field_ID" = "Field_ID_Join"))
#========================================================
#HARVEST k FOLD VALIDATION
#========================================================
# Training:60
# Testing: 20
# Validation: 20
df_harvest <- combined_df %>%
  dplyr::select(
    a1, b1, a2, b2, DOY_max_fit, DOY_max_obs, Value_max_obs, Value_max_fit, mean_HDDOY,
    #mean_vpd_M4,mean_vpd_M5,mean_vpd_M6,mean_vpd_M7, mean_vpd_M8, mean_vpd_M9,
    
    #mean_Es_M4, mean_Es_M5, mean_Es_M6,
    #mean_gdd_M4, mean_gdd_M5,
    #mean_gdd_M6,mean_gdd_M7, mean_gdd_M8, mean_gdd_M9,
    #meanRH_M4, meanRH_M5,
    #meanRH_M6,meanRH_M7,meanRH_M8, meanRH_M9,
    #mean_tmean_M3, mean_tmean_M4, mean_tmean_M5,
    mean_tmean_M6, mean_tmean_M7,mean_tmean_M8, mean_tmean_M9,
    #mean_tmin_M3,
    mean_tmin_M4, mean_tmin_M5,
    mean_tmin_M6,mean_tmin_M7,mean_tmin_M8, mean_tmin_M9,
    #mean_tmax_M3,
    mean_tmax_M4, mean_tmax_M5,
    mean_tmax_M6,mean_tmax_M7,mean_tmax_M8, mean_tmax_M9,
    #meanNDWI_M3, meanNDWI_M4,
    meanNDWI_M5, meanNDWI_M6,meanNDWI_M7,meanNDWI_M8, meanNDWI_M9,
    #meanMLSWI26_M3, meanMLSWI26_M4,
    #meanMLSWI26_M5, meanMLSWI26_M6,meanMLSWI26_M7,
    #meanRNDVI_M3, meanRNDWI_M4, meanRNDWI_M5,
    meanRNDVI_M6, meanRNDVI_M7, meanRNDVI_M8, meanRNDVI_M9
    # meanLai_M3, meanLai_M4, meanLai_M5, meanLai_M6,meanLai_M7,meanLai_M8,meanLai_M9,
    #meanIAVI_M3,meanIAVI_M4, meanIAVI_M5, meanIAVI_M6,meanIAVI_M7,
    #meanGDVI_M3, meanGDVI_M4, meanGDVI_M5, meanGDVI_M6,meanGDVI_M7,
    #meanVARI_M3, meanVARI_M4, meanVARI_M5, meanVARI_M6, meanVARI_M7
  ) %>% # drop Field_ID for modeling
  dplyr::filter(!is.na(mean_HDDOY))%>%
  drop_na() # This removes rows with NA in any column

#-------------------------------
set.seed(42)
n_harvest <- nrow(df_harvest)
test_idx_harvest <- sample(1:n_harvest, size = 0.2 * n_harvest) # Fixed test set (20%)
test_set_harvest <- df_harvest[test_idx_harvest, ]
remaining_df_harvest <- df_harvest[-test_idx_harvest, ]
results_harvest <- data.frame() # Create empty results dataframe
harvest_var_imp_list <- list() # Create list to store variable importance from each run

for (i in 1:100) { # Start 100 runs
  set.seed(100 + i)
  n_remain_harvest <- nrow(remaining_df_harvest) # Split remaining_df into 75% train and 25% validation
  train_idx_harvest <- sample(1:n_remain_harvest, size = 0.75 * n_remain_harvest)
  train_set_harvest <- remaining_df_harvest[train_idx_harvest, ]
  val_set_harvest <- remaining_df_harvest[-train_idx_harvest, ]
  rf_model_harvest <- randomForest::randomForest(mean_HDDOY ~ ., data = train_set_harvest, ntree = 100, importance = TRUE) # Train random forest model
  train_pred_harvest <- predict(rf_model_harvest, newdata = train_set_harvest) # ---- Predictions ----
  val_pred_harvest <- predict(rf_model_harvest, newdata = val_set_harvest)
  obs_train_harvest <- train_set_harvest$mean_HDDOY
  obs_val_harvest <- val_set_harvest$mean_HDDOY # ---- Store metrics ----
  results_harvest <- rbind(results_harvest, data.frame(
    run = i,
    Train_R2 = summary(stats::lm(obs_train_harvest ~ train_pred_harvest))$r.squared,
    Train_MAE = Metrics::mae(obs_train_harvest, train_pred_harvest),
    Train_RMSE = Metrics::rmse(obs_train_harvest, train_pred_harvest),
    Train_NSE = hydroGOF::NSE(train_pred_harvest, obs_train_harvest),
    Train_Bias = Metrics::bias(train_pred_harvest, obs_train_harvest),
    Val_R2 = summary(stats::lm(obs_val_harvest ~ val_pred_harvest))$r.squared,
    Val_MAE = Metrics::mae(obs_val_harvest, val_pred_harvest),
    Val_RMSE = Metrics::rmse(obs_val_harvest, val_pred_harvest),
    Val_NSE = hydroGOF::NSE(val_pred_harvest, obs_val_harvest),
    Val_Bias = Metrics::bias(val_pred_harvest, obs_val_harvest)
  ))
  # ---- Store variable importance ----
  var_imp_harvest <- randomForest::importance(rf_model_harvest)
  # Explicitly extract and name %IncMSE and IncNodePurity for consistency
  var_imp_df_harvest <- data.frame(
    variable = rownames(var_imp_harvest),
    `X.IncMSE` = var_imp_harvest[, "%IncMSE"], # Mean Decrease in Accuracy
    IncNodePurity = var_imp_harvest[, "IncNodePurity"], # Mean Decrease Gini
    run = i,
    row.names = NULL
  )
  harvest_var_imp_list[[i]] <- var_imp_df_harvest
}


# Combine all variable importance dataframes for harvest
harvest_all_var_imp <- do.call(rbind, harvest_var_imp_list)

# Calculate average importance across all runs for harvest, consistent with planting
harvest_var_imp_summary <- harvest_all_var_imp %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    MeanIncMSE = mean(`X.IncMSE`, na.rm = TRUE),
    SD_IncMSE = sd(`X.IncMSE`, na.rm = TRUE),
    MeanIncNodePurity = mean(IncNodePurity, na.rm = TRUE),
    SD_IncNodePurity = sd(IncNodePurity, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(MeanIncMSE)) # Arrange by MeanIncMSE

#-------------------------------
# 4. Final model on full 80% -> test set for harvest
#-------------------------------
final_model_harvest <- randomForest(mean_HDDOY ~ ., data = remaining_df_harvest, ntree = 100)
test_pred_harvest <- predict(final_model_harvest, newdata = test_set_harvest)
obs_test_harvest <- test_set_harvest$mean_HDDOY
test_results_harvest <- data.frame(
  run = 101,
  phase = "Test",
  R2 = summary(lm(obs_test_harvest ~ test_pred_harvest))$r.squared,
  MAE = mae(obs_test_harvest, test_pred_harvest),
  RMSE = rmse(obs_test_harvest, test_pred_harvest),
  NSE = NSE(test_pred_harvest, obs_test_harvest),
  Bias = bias(test_pred_harvest, obs_test_harvest)
)

#---------------------------------------
# 5. Compute mean & SD for training/validation for harvest
#---------------------------------------
summary_metrics_harvest <- results_harvest %>%
  summarise(
    Train_R2_mean = mean(Train_R2), Train_R2_sd = sd(Train_R2),
    Train_MAE_mean = mean(Train_MAE), Train_MAE_sd = sd(Train_MAE),
    Train_RMSE_mean = mean(Train_RMSE), Train_RMSE_sd = sd(Train_RMSE),
    Train_NSE_mean = mean(Train_NSE), Train_NSE_sd = sd(Train_NSE),
    Train_Bias_mean = mean(Train_Bias), Train_Bias_sd = sd(Train_Bias),
    
    Val_R2_mean = mean(Val_R2), Val_R2_sd = sd(Val_R2),
    Val_MAE_mean = mean(Val_MAE), Val_MAE_sd = sd(Val_MAE),
    Val_RMSE_mean = mean(Val_RMSE), Val_RMSE_sd = sd(Val_RMSE),
    Val_NSE_mean = mean(Val_NSE), Val_NSE_sd = sd(Val_NSE),
    Val_Bias_mean = mean(Val_Bias), Val_Bias_sd = sd(Val_Bias)
  )



#---------------------------------------
# 7. Outputs for harvest
#---------------------------------------
print("Harvesting Metrics Summary:")
print(summary_metrics_harvest)
print("Harvesting Test Results:")
print(test_results_harvest)
print("Harvesting Variable Importance Summary (Top 6 by MeanIncMSE):")
print(head(harvest_var_imp_summary)) # Now uses the consistent summary

# Optionally save to CSV
# write.csv(results_harvest, "rf_100runs_detailed_results_harvest.csv", row.names = FALSE)
# write.csv(summary_metrics_harvest, "rf_100runs_summary_stats_harvest.csv", row.names = FALSE)
# write.csv(test_results_harvest, "rf_testset_performance_harvest.csv", row.names = FALSE)
# write.csv(harvest_var_imp_summary, "rf_variable_importance_summary_harvest.csv", row.names = FALSE) # Save the consistent summary
sapply(train_set_harvest, class)
# === Show Plots ===
# Plot data for Harvest
plot1_df_harvest <- tibble(
  Metric = rep(c("RMSE (DOY)", "MAE (DOY)"), each = 3),
  Dataset = rep(c("Train", "Validation", "Test"), times = 2),
  Value = c(summary_metrics_harvest$Train_RMSE_mean,
            summary_metrics_harvest$Val_RMSE_mean,
            test_results_harvest$RMSE,
            summary_metrics_harvest$Train_MAE_mean,
            summary_metrics_harvest$Val_MAE_mean,
            test_results_harvest$MAE),
  SD = c(summary_metrics_harvest$Train_RMSE_sd,
         summary_metrics_harvest$Val_RMSE_sd,
         NA,  # Test has no SD
         summary_metrics_harvest$Train_MAE_sd,
         summary_metrics_harvest$Val_MAE_sd,
         NA)
)

plot2_df_harvest <- tibble(
  Metric = rep(c("R²", "Bias (DOY)"), each = 3),
  Dataset = rep(c("Train", "Validation", "Test"), times = 2),
  Value = c(summary_metrics_harvest$Train_R2_mean,
            summary_metrics_harvest$Val_R2_mean,
            test_results_harvest$R2,
            summary_metrics_harvest$Train_Bias_mean,
            summary_metrics_harvest$Val_Bias_mean,
            test_results_harvest$Bias),
  SD = c(summary_metrics_harvest$Train_R2_sd,
         summary_metrics_harvest$Val_R2_sd,
         NA,  # Test has no SD
         summary_metrics_harvest$Train_Bias_sd,
         summary_metrics_harvest$Val_Bias_sd,
         NA)
)

# === Plot 1: RMSE and MAE for Harvest ===
p1_harvest <- ggplot(plot1_df_harvest, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "Harvesting: RMSE and MAE", y = "Error", x = "Metric") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Plot 2: R² and Bias for Harvest ===
p2_harvest <- ggplot(plot2_df_harvest, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "Harvesting: R² and Bias", y = "Value", x = "Metric") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Show Plots for Harvest ===
print(p1_harvest)
print(p2_harvest)

# === Save Plots as JPEG for Harvest ===
ggsave(
  filename = "harvestingdatermsemae.jpeg",
  plot = p1_harvest,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  filename = "harvestingdater2bias.jpeg",
  plot = p2_harvest,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

# This part was problematic in your original code, referring to `varimp_list` and `varimp_summary` which would be overwritten.
# Now using the specific harvest variables:
all_varimp_harvest <- dplyr::bind_rows(harvest_var_imp_list)
harvest_var_imp_summary_detailed <- all_varimp_harvest %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    MeanIncMSE = mean(`X.IncMSE`, na.rm = TRUE),
    SD_IncMSE = sd(`X.IncMSE`, na.rm = TRUE),
    MeanIncNodePurity = mean(IncNodePurity, na.rm = TRUE),
    SD_IncNodePurity = sd(IncNodePurity, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(MeanIncMSE))
harvest_var_imp_summary_detailed %>%
  arrange(desc(MeanIncMSE)) %>%
  print(n = 60)


#========================================================
#========================================================
#PLANTING
#========================================================
#----------------------------------------------------------------



# Total number of points
# 1. Filter valid rows (no NA in response or predictors)
df_planting <- combined_df %>%
  dplyr::select(
    a1, b1, a2, b2, DOY_max_fit, DOY_max_obs, Value_max_obs, mean_PDDOY,
    mean_vpd_M4, mean_vpd_M5, mean_vpd_M6,
    #mean_Es_M4, mean_Es_M5, mean_Es_M6,
    #mean_gdd_M4, mean_gdd_M5, 
    mean_gdd_M6, mean_gdd_M7,
    meanRH_M4, meanRH_M5,
    #meanRH_M6,meanRH_M7,
    #mean_tmean_M3,
    mean_tmean_M4, mean_tmean_M5,mean_tmean_M6, mean_tmean_M7,
    mean_tmin_M3, mean_tmin_M4, mean_tmin_M5, mean_tmin_M6,mean_tmin_M7,
    mean_tmax_M4, mean_tmax_M5, mean_tmax_M6,mean_tmax_M7,
    meanNDWI_M3,meanNDWI_M4, 
    #meanNDWI_M5, meanNDWI_M6,meanNDWI_M7,
    #meanRNDVI_M2, meanRNDVI_M3, meanRNDVI_M4, 
    meanRNDVI_M5, meanRNDVI_M6, meanRNDVI_M7,
    #meanLai_M3, meanLai_M3, meanLai_M4, meanLai_M5, 
    #meanLai_M6,meanLai_M7,
    #meanIAVI_M3,meanIAVI_M4, meanIAVI_M5, 
    meanIAVI_M6,
    #meanGDVI_M3, meanGDVI_M4, meanGDVI_M5, 
    meanGDVI_M6, 
    #meanGDVI_M7
    #meanVARI_M3, meanVARI_M4, meanVARI_M5, meanVARI_M6, meanVARI_M7
    #SOS, Lag_Days, cumulative_GDD_at_SOS,
    #ATSAVI_0.50_DOY
  ) %>% # drop Field_ID for modeling
  dplyr::filter(!is.na(mean_PDDOY))%>%
  drop_na() # This removes rows with NA in any column
df_planting <- df_planting %>%
  dplyr::filter(DOY_max_fit >= 60)
# # Apply Z-score normalization to predictor variables in df_planting
# target_var_planting_name <- "mean_PDDOY"
# predictors_planting_df <- df_planting %>% dplyr::select(-!!sym(target_var_planting_name))
# target_planting_df <- df_planting %>% dplyr::select(!!sym(target_var_planting_name))
# scaled_predictors_planting_df <- as.data.frame(scale(predictors_planting_df))
# df_planting <- cbind(scaled_predictors_planting_df, target_planting_df)

#------------------------------
set.seed(42)
n_planting <- nrow(df_planting)
# Fixed test set (20%)
test_idx_planting <- sample(1:n_planting, size = 0.2 * n_planting)
test_set_planting <- df_planting[test_idx_planting, ]
remaining_df_planting <- df_planting[-test_idx_planting, ]

# Create empty results dataframe
results_planting <- data.frame()
# Create list to store variable importance from each run
planting_var_imp_list <- list()

#-------------------------------
# 100 repeated runs for planting
#-------------------------------
for (i in 1:100) {
  set.seed(100 + i)
  
  # Split remaining_df into 75% train and 25% validation
  n_remain_planting <- nrow(remaining_df_planting)
  train_idx_planting <- sample(1:n_remain_planting, size = 0.75 * n_remain_planting)
  train_set_planting <- remaining_df_planting[train_idx_planting, ]
  val_set_planting <- remaining_df_planting[-train_idx_planting, ]
  
  # Train random forest model
  rf_model_planting <- randomForest::randomForest(mean_PDDOY ~ ., data = train_set_planting, ntree = 100, importance = TRUE)
  
  # Predictions
  train_pred_planting <- predict(rf_model_planting, newdata = train_set_planting)
  val_pred_planting <- predict(rf_model_planting, newdata = val_set_planting)
  
  obs_train_planting <- train_set_planting$mean_PDDOY
  obs_val_planting <- val_set_planting$mean_PDDOY
  
  # Metrics
  results_planting <- rbind(results_planting, data.frame(
    run = i,
    Train_R2 = summary(stats::lm(obs_train_planting ~ train_pred_planting))$r.squared,
    Train_MAE = Metrics::mae(obs_train_planting, train_pred_planting),
    Train_RMSE = Metrics::rmse(obs_train_planting, train_pred_planting),
    Train_NSE = hydroGOF::NSE(train_pred_planting, obs_train_planting),
    Train_Bias = Metrics::bias(train_pred_planting, obs_train_planting),
    Val_R2 = summary(stats::lm(obs_val_planting ~ val_pred_planting))$r.squared,
    Val_MAE = Metrics::mae(obs_val_planting, val_pred_planting),
    Val_RMSE = Metrics::rmse(obs_val_planting, val_pred_planting),
    Val_NSE = hydroGOF::NSE(val_pred_planting, obs_val_planting),
    Val_Bias = Metrics::bias(val_pred_planting, obs_val_planting)
  ))
  
  # Variable Importance
  var_imp_planting <- randomForest::importance(rf_model_planting)
  var_imp_df_planting <- data.frame(
    Variable = rownames(var_imp_planting),
    `%IncMSE` = var_imp_planting[, "%IncMSE"],
    IncNodePurity = var_imp_planting[, "IncNodePurity"],
    run = i,
    row.names = NULL
  )
  planting_var_imp_list[[i]] <- var_imp_df_planting
}

#------------------------------s---------
# Combine variable importance across runs for planting
#---------------------------------------
planting_all_var_imp <- dplyr::bind_rows(planting_var_imp_list)
planting_var_imp_summary <- planting_all_var_imp %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(
    MeanIncMSE = mean(`X.IncMSE`, na.rm = TRUE), # Corrected column name to X.IncMSE
    SD_IncMSE = sd(`X.IncMSE`, na.rm = TRUE),    # Corrected column name to X.IncMSE
    MeanIncNodePurity = mean(IncNodePurity, na.rm = TRUE),
    SD_IncNodePurity = sd(IncNodePurity, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(MeanIncMSE))


#---------------------------------------
# Final model on full 80% -> test set for planting
#---------------------------------------
final_model_planting <- randomForest::randomForest(mean_PDDOY ~ ., data = remaining_df_planting, ntree = 100)
test_pred_planting <- predict(final_model_planting, newdata = test_set_planting)
obs_test_planting <- test_set_planting$mean_PDDOY

test_results_planting <- data.frame(
  run = 101,
  phase = "Test",
  R2 = summary(stats::lm(obs_test_planting ~ test_pred_planting))$r.squared,
  MAE = Metrics::mae(obs_test_planting, test_pred_planting),
  RMSE = Metrics::rmse(obs_test_planting, test_pred_planting),
  NSE = hydroGOF::NSE(test_pred_planting, obs_test_planting),
  Bias = Metrics::bias(test_pred_planting, obs_test_planting)
)

#---------------------------------------
# Compute mean & SD for training/validation for planting
#---------------------------------------
summary_metrics_planting <- results_planting %>%
  dplyr::summarise(
    Train_R2_mean = mean(Train_R2), Train_R2_sd = sd(Train_R2),
    Train_MAE_mean = mean(Train_MAE), Train_MAE_sd = sd(Train_MAE),
    Train_RMSE_mean = mean(Train_RMSE), Train_RMSE_sd = sd(Train_RMSE),
    Train_NSE_mean = mean(Train_NSE), Train_NSE_sd = sd(Train_NSE),
    Train_Bias_mean = mean(Train_Bias), Train_Bias_sd = sd(Train_Bias),
    
    Val_R2_mean = mean(Val_R2), Val_R2_sd = sd(Val_R2),
    Val_MAE_mean = mean(Val_MAE), Val_MAE_sd = sd(Val_MAE),
    Val_RMSE_mean = mean(Val_RMSE), Val_RMSE_sd = sd(Val_RMSE),
    Val_NSE_mean = mean(Val_NSE), Val_NSE_sd = sd(Val_NSE),
    Val_Bias_mean = mean(Val_Bias), Val_Bias_sd = sd(Val_Bias)
  )

#---------------------------------------
# Output for planting
#---------------------------------------
print("Planting Metrics Summary:")
print(summary_metrics_planting)
print("Planting Test Results:")
print(test_results_planting)
print("Planting Variable Importance Summary:")
print((planting_var_imp_summary))

planting_var_imp_summary %>%
  arrange(desc(MeanIncMSE)) %>%
  print(n = 60)

# Optional: save results to CSV
# write.csv(results_planting, "rf_PDDOY_100runs_detailed_results_planting.csv", row.names = FALSE)
# write.csv(summary_metrics_planting, "rf_PDDOY_100runs_summary_stats_planting.csv", row.names = FALSE)
# write.csv(test_results_planting, "rf_PDDOY_testset_performance_planting.csv", row.names = FALSE)
# write.csv(planting_var_imp_summary, "rf_PDDOY_variable_importance_summary_planting.csv", row.names = FALSE)

# Check training set column types
sapply(train_set_planting, class)

# === RMSE & MAE (with Test) for Planting ===
plot1_df_planting <- tibble(
  Metric = rep(c("RMSE (DOY)", "MAE (DOY)"), each = 3),
  Dataset = rep(c("Train", "Validation", "Test"), times = 2),
  Value = c(summary_metrics_planting$Train_RMSE_mean,
            summary_metrics_planting$Val_RMSE_mean,
            test_results_planting$RMSE,
            summary_metrics_planting$Train_MAE_mean,
            summary_metrics_planting$Val_MAE_mean,
            test_results_planting$MAE),
  SD = c(summary_metrics_planting$Train_RMSE_sd,
         summary_metrics_planting$Val_RMSE_sd,
         NA,  # Test has no SD
         summary_metrics_planting$Train_MAE_sd,
         summary_metrics_planting$Val_MAE_sd,
         NA)
)

# === R² & Bias (with Test) for Planting ===
plot2_df_planting <- tibble(
  Metric = rep(c("R²", "Bias"), each = 3),
  Dataset = rep(c("Train", "Validation", "Test"), times = 2),
  Value = c(summary_metrics_planting$Train_R2_mean,
            summary_metrics_planting$Val_R2_mean,
            test_results_planting$R2,
            summary_metrics_planting$Train_Bias_mean,
            summary_metrics_planting$Val_Bias_mean,
            test_results_planting$Bias),
  SD = c(summary_metrics_planting$Train_R2_sd,
         summary_metrics_planting$Val_R2_sd,
         NA,  # Test has no SD
         summary_metrics_planting$Train_Bias_sd,
         summary_metrics_planting$Val_Bias_sd,
         NA)
)

# === Plot 1: RMSE and MAE for Planting ===
p1_planting <- ggplot(plot1_df_planting, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "Planting: RMSE and MAE", y = "Error", x = "Metric") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Plot 2: R² and Bias for Planting ===
p2_planting <- ggplot(plot2_df_planting, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "Planting: R² and Bias", y = "Value", x = "Metric") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Show Plots for Planting ===
print(p1_planting)
print(p2_planting)

# === Save Plots as JPEG for Planting ===
ggsave(
  filename = "plantingdatermsemae.jpeg",
  plot = p1_planting,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  filename = "plantingdater2bias.jpeg",
  plot = p2_planting,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)


#========================================================
# 6. PLOT VARIABLE IMPORTANCE (HARVESTING)
#========================================================
# Prepare data for plotting Harvest variable importance
harvest_importance_combined <- harvest_var_imp_summary %>%
  rename(
    `%IncMSE` = MeanIncMSE,
    Gini = MeanIncNodePurity
  ) %>%
  mutate(
    # Apply Z-score normalization
    `%IncMSE_scaled` = scale(`%IncMSE`),
    `Gini_scaled` = scale(Gini),
    Total_scaled = `%IncMSE_scaled` + `Gini_scaled`
  )

# Convert to long format for Harvest
harvest_importance_long <- harvest_importance_combined %>%
  select(variable, `%IncMSE_scaled`, `Gini_scaled`, Total_scaled) %>%
  pivot_longer(cols = c(`%IncMSE_scaled`, `Gini_scaled`), names_to = "Metric", values_to = "Value") %>%
  mutate(variable = reorder(variable, Total_scaled))

# Plot: absolute stacked values for Harvest
p_harvest_total_importance <- ggplot(harvest_importance_long, aes(x = Value, y = variable, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = c("%IncMSE_scaled" = "#3C5488FF", "Gini_scaled" = "#00A087FF"),
    name = "Importance Metric"
  ) +
  labs(
    title = "Harvesting: Variable Importance (Combined Standardized %IncMSE and Gini)",
    x = "Total Standardized Importance",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

print(p_harvest_total_importance)

# Save the plot for Harvest
ggsave(
  filename = "harvesting_variable_importance_total_stacked_scaled.jpeg", # Changed filename
  plot = p_harvest_total_importance,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 10,
  height = 8,
  units = "in"
)


#========================================================
# 6. PLOT VARIABLE IMPORTANCE (PLANTING)
#========================================================
# Prepare data for plotting Planting variable importance
planting_importance_combined <- planting_var_imp_summary %>%
  rename(
    `%IncMSE` = MeanIncMSE,
    Gini = MeanIncNodePurity
  ) %>%
  mutate(
    # Apply Z-score normalization
    `%IncMSE_scaled` = scale(`%IncMSE`),
    `Gini_scaled` = scale(Gini),
    Total_scaled = `%IncMSE_scaled` + `Gini_scaled`
  )

# Convert to long format for Planting
planting_importance_long <- planting_importance_combined %>%
  select(Variable, `%IncMSE_scaled`, `Gini_scaled`, Total_scaled) %>%
  pivot_longer(cols = c(`%IncMSE_scaled`, `Gini_scaled`), names_to = "Metric", values_to = "Value") %>%
  mutate(Variable = reorder(Variable, Total_scaled))

# Plot: absolute stacked values for Planting
p_planting_total_importance <- ggplot(planting_importance_long, aes(x = Value, y = Variable, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = c("%IncMSE_scaled" = "#3C5488FF", "Gini_scaled" = "#00A087FF"),
    name = "Importance Metric"
  ) +
  labs(
    title = "Planting: Variable Importance (Combined Standardized %IncMSE and Gini)",
    x = "Total Standardized Importance",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

print(p_planting_total_importance)

# Save the plot for Planting
ggsave(
  filename = "planting_variable_importance_total_stacked_scaled.jpeg", # Changed filename
  plot = p_planting_total_importance,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 10,
  height = 8,
  units = "in"
)

#========================================================
# 7. PLOT TOP 15 FEATURES VS. TARGET VARIABLE (HARVESTING)
#========================================================
# Get the top 15 features for Harvesting based on Total_scaled importance
top_15_harvest_features <- harvest_importance_combined %>%
  arrange(desc(Total_scaled)) %>%
  head(15) %>%
  pull(variable)

# Loop through the top 15 features and create scatter plots
for (feature in top_15_harvest_features) {
  # Create a data frame for plotting (using combined_df as the source)
  plot_data <- combined_df %>%
    dplyr::select(!!sym(feature), mean_HDDOY) %>%
    drop_na() # Ensure no NAs in the selected columns for plotting
  
  p <- ggplot(plot_data, aes_string(x = feature, y = "mean_HDDOY")) +
    geom_point(alpha = 0.6, color = "#3C5488FF") +
    geom_smooth(method = "lm", se = FALSE, color = "#DC0000FF") + # Add a linear regression line
    labs(
      title = paste("Harvesting: mean_HDDOY vs.", feature),
      x = feature,
      y = "mean_HDDOY"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    )
  
  print(p)
  
  # Save the plot
  ggsave(
    filename = paste0("harvesting_mean_HDDOY_vs_", feature, ".jpeg"),
    plot = p,
    path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
    dpi = 300,
    width = 8,
    height = 6,
    units = "in"
  )
}


#========================================================
# 7. PLOT TOP 15 FEATURES VS. TARGET VARIABLE (PLANTING)
#========================================================
# Get the top 15 features for Planting based on Total_scaled importance
top_15_planting_features <- planting_importance_combined %>%
  arrange(desc(Total_scaled)) %>%
  head(15) %>%
  pull(Variable) # Note: 'Variable' column name for planting_importance_combined

# Loop through the top 15 features and create scatter plots
for (feature in top_15_planting_features) {
  # Create a data frame for plotting (using combined_df as the source)
  plot_data <- combined_df %>%
    dplyr::select(!!sym(feature), mean_PDDOY) %>%
    drop_na() # Ensure no NAs in the selected columns for plotting
  
  # Fit a linear model to get the R-squared value
  lm_model <- lm(as.formula(paste("mean_PDDOY ~", feature)), data = plot_data)
  r_squared <- summary(lm_model)$r.squared
  
  p <- ggplot(plot_data, aes_string(x = feature, y = "mean_PDDOY")) +
    geom_point(alpha = 0.6, color = "#00A087FF") +
    geom_smooth(method = "lm", se = FALSE, color = "#DC0000FF") + # Add a linear regression line
    labs(
      title = paste0("Planting: mean_PDDOY vs. ", feature, " (R² = ", sprintf("%.2f", r_squared), ")"), # Add R-squared to title
      x = feature,
      y = "mean_PDDOY"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    )
  
  print(p)
  
  # Save the plot
  ggsave(
    filename = paste0("planting_mean_PDDOY_vs_", feature, ".jpeg"),
    plot = p,
    path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
    dpi = 300,
    width = 8,
    height = 6,
    units = "in"
  )
}

df_planting$Field_ID[df_planting$DOY_max_fit<60]

