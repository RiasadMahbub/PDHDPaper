library(randomForest)
library(Metrics)
library(hydroGOF) # for NSE
library(dplyr)
library(ggplot2)
library(tidyr)

df$lagtrs <- df$SOS_trs.sos - df$PDDOY
df$lagtrsupdate <- df$SOS_trs.sos - df$UD.UD
df$lagtrsgreenup <- df$SOS_trs.sos - df$Greenup.Greenup
df$lagtrsminfit <- df$SOS_trs.sos - df$DOY_min_fit
#========================================================
#DATAFRAME HARVEST: 
#========================================================

#========================================================
#HARVEST k FOLD VALIDATION
#========================================================
# Training:60
# Testing: 20
# Validation: 20

dfharvest_pheno <- dfharvest %>%
  dplyr::select(  EOS_trs.eos, RD.RD, Dormancy.Dormancy, EOS_deriv.eos, 
                  cum_meansrad, UD.UD, a2, DD.DD, SOS_trs.sos, SOS_deriv.sos,
                  avgsoilorg, cum_vpd, cum_gdd, cum_RH, b1, avgsoilclay, cum_soiltemp, 
                  cum_tmin, DOY_max_before_min_fit, cum_tmax, a1, lagtrsupdate,
                  DOY_min_fit, rsp.rsp, 
                  DOY_maxROC_EVI, 
                  DOY_maxROC_TGI, POS.pos,
                  DOY_maxROC_ExGR, mx.mx, a3.a3, DOY_maxROC_NMDI, 
                  #DOY_maxROC_DSI,         #DSI and MSI is the same 
                  DOY_maxROC_MSI , HDDOY) %>% # drop Field_ID for modeling
  dplyr::filter(!is.na(HDDOY))%>%
  drop_na() # This removes rows with NA in any column


dfharvest_pheno <- dfharvest %>%
  dplyr::select(  EOS_trs.eos, RD.RD, Dormancy.Dormancy, EOS_deriv.eos, 
                  avgsoilorg, a2, DD.DD, 
                  DOY_maxROC_EVI, DOY_maxROC_IAVI,DOY_maxROC_MRBVI, DOY_maxROC_TGI, 
                  DOY_maxROC_NMDI,DOY_maxROC_ExGR, cumGDVI,  
                  cum_RH, avgsoilclay, cum_vpd, cum_soiltemp,
                  UD.UD, SOS_trs.sos, a1, DOY_max_before_min_fit, 
                  cum_tmin, b1, SOS_deriv.sos, cum_gdd, cum_tmax, 
                  DOY_min_fit, cum_meansrad, a3.a3 , HDDOY) %>% # drop Field_ID for modeling
  dplyr::filter(!is.na(HDDOY))%>%
  drop_na() # This removes rows with NA in any column
# store column names in a variable
LIMPRFHarvestFeatures <- colnames(dfharvest_pheno)
#-------------------------------
set.seed(123)
n_harvest <- nrow(dfharvest_pheno)
test_idx_harvest <- sample(1:n_harvest, size = 0.2 * n_harvest) # Fixed test set (20%)
test_set_harvest <- dfharvest_pheno[test_idx_harvest, ]
remaining_df_harvest <- dfharvest_pheno[-test_idx_harvest, ]
results_harvest <- data.frame() # Create empty results dataframe
harvest_var_imp_list <- list() # Create list to store variable importance from each run

for (i in 1:100) { # Start 100 runs
  set.seed(100 + i)
  n_remain_harvest <- nrow(remaining_df_harvest) # Split remaining_df into 75% train and 25% validation
  train_idx_harvest <- sample(1:n_remain_harvest, size = 0.75 * n_remain_harvest)
  train_set_harvest <- remaining_df_harvest[train_idx_harvest, ]
  val_set_harvest <- remaining_df_harvest[-train_idx_harvest, ]
  rf_model_harvest <- randomForest::randomForest(HDDOY ~ ., data = train_set_harvest, ntree = 100, importance = TRUE) # Train random forest model
  train_pred_harvest <- predict(rf_model_harvest, newdata = train_set_harvest) # ---- Predictions ----
  val_pred_harvest <- predict(rf_model_harvest, newdata = val_set_harvest)
  obs_train_harvest <- train_set_harvest$HDDOY
  obs_val_harvest <- val_set_harvest$HDDOY # ---- Store metrics ----
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
final_model_harvest <- randomForest(HDDOY ~ ., data = remaining_df_harvest, ntree = 100)
test_pred_harvest <- predict(final_model_harvest, newdata = test_set_harvest)
obs_test_harvest <- test_set_harvest$HDDOY
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
plot1_df_harvest_pheno <- tibble(
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

plot2_df_harvest_pheno <- tibble(
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
p1_harvest_pheno <- ggplot(plot1_df_harvest_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
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
p2_harvest_pheno <- ggplot(plot2_df_harvest_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
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
print(p1_harvest_pheno)
print(p2_harvest_pheno)

# === Save Plots as JPEG for Harvest ===
ggsave(
  filename = "harvestingdatermsemae.jpeg",
  plot = p1_harvest_pheno,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  filename = "harvestingdater2bias.jpeg",
  plot = p2_harvest_pheno,
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
df

df_planting_pheno <- df %>%
  dplyr::select(
    # --- Top 20 RFE variables ---
    SOS_trs.sos, RD.RD, cum_RH, avgsoilorg, mean_ExG, SOS_deriv.sos, cum_tmin, 
    EOS_trs.eos, mx.mx, EOS_deriv.eos, cum_soiltemp, cum_meansrad, cum_gdd, mean_nir, 
    Value_max_obs, DD.DD, a2, cum_tmax, UD.UD, cum_vpd, 
    #Senescence.Senescence , 
    PDDOY
  ) %>% # drop Field_ID for modeling
  dplyr::filter(!is.na(PDDOY))%>%
  drop_na() # This removes rows with NA in any column
#%>%dplyr::filter(DOY_max_fit >= 60)

# Total number of points
df_planting_pheno <- df %>%
  dplyr::select(
    cum_RH, SOS_trs.sos, SOS_deriv.sos, UD.UD,
    cum_meansrad, avgsoilorg, cum_tmin, EOS_trs.eos, 
    EOS_deriv.eos, cum_soiltemp, cum_gdd, DD.DD, cum_vpd,
    Value_max_obs , PDDOY
  ) %>% # drop Field_ID for modeling
  dplyr::filter(!is.na(PDDOY))%>%
  drop_na() # This removes rows with NA in any column

# store column names in a variable
LIMPRFPlantingFeatures <- colnames(df_planting_pheno)

# # Apply Z-score normalization to predictor variables in df_planting_pheno
# target_var_planting_name <- "PDDOY"
# predictors_planting_df <- df_planting_pheno %>% dplyr::select(-!!sym(target_var_planting_name))
# target_planting_df <- df_planting_pheno %>% dplyr::select(!!sym(target_var_planting_name))
# scaled_predictors_planting_df <- as.data.frame(scale(predictors_planting_df))
# df_planting_pheno <- cbind(scaled_predictors_planting_df, target_planting_df)

#------------------------------
set.seed(123)
n_planting <- nrow(df_planting_pheno)
# Fixed test set (20%)
test_idx_planting <- sample(1:n_planting, size = 0.2 * n_planting)
test_set_planting <- df_planting_pheno[test_idx_planting, ]
remaining_df_planting <- df_planting_pheno[-test_idx_planting, ]

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
  rf_model_planting <- randomForest::randomForest(PDDOY ~ ., data = train_set_planting, ntree = 100, importance = TRUE)
  
  # Predictions
  train_pred_planting <- predict(rf_model_planting, newdata = train_set_planting)
  val_pred_planting <- predict(rf_model_planting, newdata = val_set_planting)
  
  obs_train_planting <- train_set_planting$PDDOY
  obs_val_planting <- val_set_planting$PDDOY
  
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
final_model_planting <- randomForest::randomForest(PDDOY ~ ., data = remaining_df_planting, ntree = 100)
test_pred_planting <- predict(final_model_planting, newdata = test_set_planting)
obs_test_planting <- test_set_planting$PDDOY

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
plot1_df_planting_pheno <- tibble(
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
plot2_df_planting_pheno <- tibble(
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
p1_planting_pheno <- ggplot(plot1_df_planting_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
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
p2_planting_pheno <- ggplot(plot2_df_planting_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
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

print(p2_planting_pheno)
print(p1_planting_pheno)
# === Save Plots as JPEG for Planting ===
ggsave(
  filename = "phenologymethodplantingdatermsemae.jpeg",
  plot = p1_planting_pheno,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  filename = "phenologymethodplantingdater2bias.jpeg",
  plot = p2_planting_pheno,
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
    # Simple rescaling: divide Gini by a constant to bring it to same order as %IncMSE
    Gini_scaled = Gini / 5000,  # adjust 5000 as needed
    Total_scaled = `%IncMSE` + Gini_scaled
  )

# Convert to long format for Harvest
harvest_importance_long <- harvest_importance_combined %>%
  select(variable, `%IncMSE`, `Gini_scaled`, Total_scaled) %>%
  pivot_longer(cols = c(`%IncMSE`, `Gini_scaled`), names_to = "Metric", values_to = "Value") %>%
  mutate(variable = reorder(variable, Total_scaled))
library(ggplot2)
library(dplyr)
library(tidyr)

# Original variables in your dataframe
original_vars <- c(
  "RD.RD", "Dormancy.Dormancy", "EOS_trs.eos", "EOS_deriv.eos", "cum_RH", 
  "a2", "avgsoilorg", "UD.UD", "DOY_maxROC_IAVI", "SOS_trs.sos", "cum_meansrad",
  "cum_soiltemp", "DOY_maxROC_MRBVI", "DOY_maxROC_NMDI", "DD.DD", "avgsoilclay",
  "DOY_maxROC_TGI", "SOS_deriv.sos", "cum_gdd", "cum_tmax", "cum_vpd",
  "DOY_maxROC_ExGR", "cumGDVI", "cum_tmin", "b1", "DOY_max_before_min_fit",
  "DOY_maxROC_EVI", "a1", "a3.a3", "DOY_min_fit"
)

# New names (simplified and consistent)
new_vars <- c(
  "RD", "Dormancy", "EOSTRS", "EOSDeriv", "RHcum", 
  "kNDVIa2", "SOCmean", "UD", "DOY_mxROCPoD_IAVI", "SOSTRS", "SRADcum",
  "SoilTmeancum", "DOY_mxROCPoD_MRBVI", "DOY_mxROCPoD_NMDI", "DD", "clayCmean",
  "DOY_mxROCPoD_TGI", "SOSDeriv", "GDDcum", "AirTmax_cum", "VPDcum",
  "DOY_mxROCPoD_ExGR", "GDVIcum", "AirTmincum", "kNDVIb1", "DOY_earlymin_kNDVI",
  "DOY_mxROCPoD_EVI", "kNDVIa1", "a3", "DOY_earlymax_kNDVI"
)

# Assign the new variable names
harvest_importance_combined$variable <- factor(
  harvest_importance_combined$variable,
  levels = original_vars,
  labels = new_vars
)

# Map readable labels with subscripts for plotting
variable_labels <- c(
  "SOCmean" = expression(SOC[mean]),
  "RHcum" = expression(RH[cum]),
  "kNDVImax" = expression(kNDVI[max]),
  "EOSTRS" = expression(EOS[TRS]),
  "AirTmincum" = expression(AirTmin[cum]),
  "AirTmax_cum"= expression(AirTax[cum]),
  "SOSDeriv" = expression(SOS[Deriv]),
  "SOSTRS" = expression(SOS[TRS]),
  "SoilTmeancum" = expression(SoilTmean[cum]),
  "EOSDeriv" = expression(EOS[Deriv]),
  "DD" = expression(DD),
  "VPDcum" = expression(VPD[cum]),
  "GDDcum" = expression(GDD[cum]),
  "SRADcum" = expression(Srad[cum]),
  "UD" = expression(UD)
)

# Convert to long format for plotting
harvest_importance_long <- harvest_importance_combined %>%
  select(variable, `%IncMSE`, `Gini_scaled`, Total_scaled) %>%
  pivot_longer(cols = c(`%IncMSE`, `Gini_scaled`), names_to = "Metric", values_to = "Value") %>%
  mutate(variable = reorder(variable, Total_scaled))

# Plot with subscripts
p_harvest_total_importance <- ggplot(harvest_importance_long, aes(x = Value, y = variable, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = c("%IncMSE" = "#3C5488FF", "Gini_scaled" = "#9C51B6"),
    name = "Importance Metric"
  ) +
  scale_y_discrete(labels = variable_labels) +  # Apply subscript labels
  labs(
    title = "Harvesting: Variable Importance (Combined %IncMSE and Gini)",
    x = "Total Rescaled Importance",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

print(p_harvest_total_importance)



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
# planting_importance_combined <- planting_var_imp_summary %>%
#   rename(
#     `%IncMSE` = MeanIncMSE,
#     Gini = MeanIncNodePurity
#   ) %>%
#   mutate(
#     # Apply Z-score normalization
#     `%IncMSE` = scale(`%IncMSE`),
#     `Gini_scaled` = scale(Gini),
#     Total_scaled = `%IncMSE` + `Gini_scaled`
#   )

planting_importance_combined <- planting_var_imp_summary %>%
  rename(
    `%IncMSE` = MeanIncMSE,
    Gini = MeanIncNodePurity
  ) %>%
  mutate(
    # Simple rescaling: divide Gini by a constant to bring it to same order as %IncMSE
    Gini_scaled = Gini / 5000,  # adjust 5000 as needed
    Total_scaled = `%IncMSE` + Gini_scaled
  )


# Convert to long format for Planting
planting_importance_long <- planting_importance_combined %>%
  select(Variable, `%IncMSE`, `Gini_scaled`, Total_scaled) %>%
  pivot_longer(cols = c(`%IncMSE`, `Gini_scaled`), names_to = "Metric", values_to = "Value") %>%
  mutate(Variable = reorder(Variable, Total_scaled))



# Map original variable names to readable names
planting_importance_long <- planting_importance_long %>%
  mutate(
    Variable = recode(
      Variable,
      "avgsoilorg" = "SOCmean",
      "cum_RH" = "RHcum",
      "Value_max_obs" = "kNDVImax",
      "EOS_trs.eos" = "EOSTRS",
      "cum_tmin" = "AirTmincum",
      "SOS_deriv.sos" = "SOSDeriv",
      "SOS_trs.sos" = "SOSTRS",
      "cum_soiltemp" = "SoilTmeancum",
      "EOS_deriv.eos" = "EOSDeriv",
      "DD.DD" = "DD",
      "cum_vpd" = "VPDcum",
      "cum_gdd" = "GDDcum",
      "cum_meansrad" = "Sradcum",
      "UD.UD" = "UD"
    ),
    Variable = reorder(Variable, Total_scaled)
  )
# Map readable labels with subscripts
variable_labels <- c(
  "SOCmean" = expression(SOC[mean]),
  "RHcum" = expression(RH[cum]),
  "kNDVImax" = expression(kNDVI[max]),
  "EOSTRS" = expression(EOS[TRS]),
  "AirTmincum" = expression(AirTmin[cum]),
  "SOSDeriv" = expression(SOS[Deriv]),
  "SOSTRS" = expression(SOS[TRS]),
  "SoilTmeancum" = expression(SoilTmean[cum]),
  "EOSDeriv" = expression(EOS[Deriv]),
  "DD" = expression(DD),
  "VPDcum" = expression(VPD[cum]),
  "GDDcum" = expression(GDD[cum]),
  "Sradcum" = expression(Srad[cum]),
  "UD" = expression(UD)
)


# Plot with formatted y-axis
p_planting_total_importance <- ggplot(planting_importance_long, aes(x = Value, y = Variable, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = c("%IncMSE" = "#3C5488FF", "Gini_scaled" = "#9C51B6"),
    name = "Importance Metric"
  ) +
  scale_y_discrete(labels = variable_labels) +
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
  plot_data <- dfharvest_pheno %>%
    dplyr::select(!!sym(feature), HDDOY) %>%
    drop_na() # Ensure no NAs in the selected columns for plotting
  
  p <- ggplot(plot_data, aes_string(x = feature, y = "HDDOY")) +
    geom_point(alpha = 0.6, color = "#3C5488FF") +
    geom_smooth(method = "lm", se = FALSE, color = "#DC0000FF") + # Add a linear regression line
    labs(
      title = paste("Harvesting: HDDOY vs.", feature),
      x = feature,
      y = "HDDOY"
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
    filename = paste0("harvesting_HDDOY_vs_", feature, ".jpeg"),
    plot = p,
    path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/Features",
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
  plot_data <- df %>%
    dplyr::select(!!sym(feature), PDDOY) %>%
    drop_na() # Ensure no NAs in the selected columns for plotting
  
  # Fit a linear model to get the R-squared value
  lm_model <- lm(as.formula(paste("PDDOY ~", feature)), data = plot_data)
  r_squared <- summary(lm_model)$r.squared
  
  p <- ggplot(plot_data, aes_string(x = feature, y = "PDDOY")) +
    geom_point(alpha = 0.6, color = "#00A087FF") +
    geom_smooth(method = "lm", se = FALSE, color = "#DC0000FF") + # Add a linear regression line
    labs(
      title = paste0("Planting: PDDOY vs. ", feature, " (R² = ", sprintf("%.2f", r_squared), ")"), # Add R-squared to title
      x = feature,
      y = "PDDOY"
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
    filename = paste0("planting_PDDOY_vs_", feature, ".jpeg"),
    plot = p,
    path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/Features",
    dpi = 300,
    width = 8,
    height = 6,
    units = "in"
  )
}




