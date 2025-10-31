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
#---------------fix dormancy-----------------------
# 1. Count the number of NA values
na_count <- sum(is.na(dfharvest$Dormancy.Dormancy))
na_count

# 2. Calculate the mean of (SOS_deriv.sos - Dormancy.Dormancy), ignoring NAs
mean_diff <- mean(dfharvest$SOS_deriv.sos - dfharvest$Dormancy.Dormancy, na.rm = TRUE)
mean_diff

# 3. Replace NA values in Dormancy.Dormancy with this mean difference
dfharvest$Dormancy.Dormancy[is.na(dfharvest$Dormancy.Dormancy)] <- mean_diff

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
  dplyr::select( RD.RD, EOS_trs.eos, EOS_deriv.eos, a2, cum_vpd, cum_meansrad, 
                 a1, SOS_deriv.sos, avgsoilorg, cum_soiltemp,
                HDDOY) %>% # drop Field_ID for modeling
  dplyr::filter(!is.na(HDDOY))%>%
  drop_na() # This removes rows with NA in any column


# dfharvest_pheno <- dfharvest %>%
#   dplyr::select( EOS_trs.eos, RD.RD, EOS_deriv.eos, a2, 
#                  #mean_NSDSI2, mean_NSDSI3, mean_NSDS, 
#                  cum_vpd,
#                  avgsoilorg, a1, DOY_max_before_min_fit, cum_meansrad_PD, DD.DD, cum_meansrad, cumGDVI_PD, 
#                  avgsoilclay, SOS_deriv.sos, cum_soiltemp, cum_RH, cum_vpd_PD,
#                   HDDOY) %>% # drop Field_ID for modeling
#   dplyr::filter(!is.na(HDDOY))%>%
#   drop_na() # This removes rows with NA in any column
#store column names in a variable
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
  Metric = rep(c("RMSE (day)", "MAE (day)"), each = 3),
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
  Metric = rep(c("R²", "Bias (day)"), each = 3),
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
# ==========================
# Split Harvest Data
# ==========================
plot2_bias_harvest_df <- plot2_df_harvest_pheno %>%
  dplyr::filter(Metric == "Bias (day)")

plot3_r2_harvest_df <- plot2_df_harvest_pheno %>%
  dplyr::filter(Metric == "R²")

# === Plot 1: RMSE and MAE for Harvest ===
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
  scale_fill_manual(values = c("#0072B2","#FFDB6D", "#D16103")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Plot 2: R² and Bias for Harvest ===
# p2_harvest_pheno <- ggplot(plot2_df_harvest_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
#   geom_col(position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
#                 position = position_dodge(width = 0.7),
#                 width = 0.2, na.rm = TRUE) +
#   geom_text(aes(label = ifelse(is.na(SD),
#                                sprintf("%.2f", Value),
#                                sprintf("%.2f ± %.2f", Value, SD))),
#             position = position_dodge(width = 0.7),
#             vjust = -0.8,
#             size = 4.5) +
#   labs(title = "Harvesting: R² and Bias", y = "Value", x = "Metric") +
#   scale_fill_manual(values = c("#0072B2","#FFDB6D", "#D16103")) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18, face = "bold")
#   )
# ==========================
# ✅ p2 = Harvest Bias Plot
# ==========================
p2_harvest_pheno <- ggplot(plot2_bias_harvest_df, aes(x = Dataset, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            vjust = -0.8, size = 4.5) +
  labs(title = "Harvesting: Bias (day)", y = "Bias (day)", x = "") +
  scale_fill_manual(values = c("#0072B2", "#FFDB6D", "#D16103")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# ==========================
# ✅ p3 = Harvest R² Plot
# ==========================
p3_harvest_pheno <- ggplot(plot3_r2_harvest_df, aes(x = Dataset, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            vjust = -0.8, size = 4.5) +
  labs(title = "Harvesting: R²", y = "R²", x = "") +
  scale_fill_manual(values = c("#0072B2", "#FFDB6D", "#D16103")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Show Plots for Harvest ===

print(p2_harvest_pheno)
print(p3_harvest_pheno)
print(p1_harvest_pheno)

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
# 
# df_planting_pheno <- df %>%
#   dplyr::select(
#     # --- Top 20 RFE variables ---
#     SOS_trs.sos, RD.RD, cum_RH, avgsoilorg, mean_ExG, SOS_deriv.sos, cum_tmin, 
#     EOS_trs.eos, mx.mx, EOS_deriv.eos, cum_soiltemp, cum_meansrad, cum_gdd, mean_nir, 
#     Value_max_obs, DD.DD, a2, cum_tmax, UD.UD, cum_vpd, 
#     #Senescence.Senescence , 
#     PDDOY
#   ) %>% # drop Field_ID for modeling
#   dplyr::filter(!is.na(PDDOY))%>%
#   drop_na() # This removes rows with NA in any column
# #%>%dplyr::filter(DOY_max_fit >= 60)

# Total number of points
df_planting_pheno <- df %>%
  dplyr::select(
    cum_tmin, cum_soiltemp, SOS_trs.sos, cum_gdd, SOS_deriv.sos, cum_RH, cum_vpd, avgsoilorg,
    PDDOY
  ) %>% # drop Field_ID for modeling
  dplyr::filter(!is.na(PDDOY))%>%
  drop_na() # This removes rows with NA in any column
# 
# df_planting_pheno <- df %>%
#   dplyr::select(SOS_trs.sos, cum_tmin, SOS_deriv.sos, Value_max_obs,
# EOS_deriv.eos, EOS_trs.eos, UD.UD, cum_soiltemp, cum_gdd,
# cum_meansrad, avgsoilorg, cum_vpd, mx.mx, DD.DD, RD.RD, mean_ExG, 
# a2, DOY_maxROC_ExG, mean_GCC, Laglocalmaxlocalmin, cum_RH, DOY_max_before_min_fit,
# DOY_maxROC_EVI, DOY_maxROC_sNIRvNDVILSWIS, mean_NRFIr, mean_ExGR, mean_NRFIg, 
# mean_NDSoI, mean_WI1, PDDOY )%>% # drop Field_ID for modeling
#   dplyr::filter(!is.na(PDDOY))%>%
#   drop_na() # This removes rows with NA in any column

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
  Metric = rep(c("RMSE (day)", "MAE (day)"), each = 3),
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
plot2_bias_df <- plot2_df_planting_pheno %>%
  dplyr::filter(Metric == "Bias")

plot3_r2_df <- plot2_df_planting_pheno %>%
  dplyr::filter(Metric == "R²")

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
  labs( y = "Error", x = "Metric") +
  scale_fill_manual(values = c("#0072B2","#FFDB6D", "#D16103")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Plot 2: R² and Bias for Planting ===
# p2_planting_pheno <- ggplot(plot2_df_planting_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
#   geom_col(position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
#                 position = position_dodge(width = 0.7),
#                 width = 0.2, na.rm = TRUE) +
#   geom_text(aes(label = ifelse(is.na(SD),
#                                sprintf("%.2f", Value),
#                                sprintf("%.2f ± %.2f", Value, SD))),
#             position = position_dodge(width = 0.7),
#             vjust = -0.8,
#             size = 4.5) +
#   labs( y = "Value", x = "Metric") +
#   scale_fill_manual(values = c("#0072B2","#FFDB6D", "#D16103")) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18, face = "bold")
#   )

# ==========================
# ✅ p2 = Bias Plot
# ==========================
p2_planting_pheno <- ggplot(plot2_bias_df, aes(x = Dataset, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            vjust = -0.8, size = 4.5) +
  labs( y = "Bias", x = "") +
  scale_fill_manual(values = c("#0072B2", "#FFDB6D", "#D16103")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# ==========================
# ✅ p3 = R² Plot
# ==========================
p3_planting_pheno <- ggplot(plot3_r2_df, aes(x = Dataset, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            vjust = -0.8, size = 4.5) +
  labs( y = "R²", x = "") +
  scale_fill_manual(values = c("#0072B2", "#FFDB6D", "#D16103")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )



# === Show Plots for Planting ===
print(p3_planting_pheno)
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

rm(p2_planting_pheno)
rm(p1_planting_pheno)

#-----------------------------------------------------
#FEATURES
#-------------------------------------------------------
# This script calculates and prints descriptive statistics for a specified set of features
# from the data frame 'df_planting_pheno'.
# NOTE: The data frame 'df_planting_pheno' must be defined and loaded into your R environment.

# Define the features to analyze as requested.
features_to_analyze <- c(
  "cum_RH", "SOS_trs.sos", "SOS_deriv.sos", "UD.UD",
  "cum_meansrad", "avgsoilorg", "cum_tmin", "EOS_trs.eos",
  "EOS_deriv.eos", "cum_soiltemp", "cum_gdd", "DD.DD", "cum_vpd",
  "Value_max_obs", "PDDOY"
)

#' Calculates and prints key descriptive statistics for a given feature.
#'
#' @param df The data frame containing the feature.
#' @param feature_name The name of the column (feature) to analyze.
print_descriptive_stats <- function(df, feature_name) {
  # Check if the feature exists in the data frame
  if (!(feature_name %in% names(df))) {
    cat(sprintf("Feature '%s' not found in data frame.\n", feature_name))
    return()
  }
  
  data <- df[[feature_name]]
  
  # Check if the data is numeric, as required for these calculations
  if (!is.numeric(data)) {
    cat(sprintf("Feature '%s' is not numeric and cannot be summarized.\n", feature_name))
    return()
  }
  
  # Calculate the 25th and 75th percentiles
  qts <- quantile(data, probs = c(0.25, 0.75), na.rm = TRUE)
  
  # Consolidate all statistics
  stats <- c(
    Min = min(data, na.rm = TRUE),
    Max = max(data, na.rm = TRUE),
    SD = sd(data, na.rm = TRUE),
    Mean = mean(data, na.rm = TRUE),
    Percentile_75 = qts["75%"],
    Percentile_25 = qts["25%"]
  )
  
  # Print formatted output using cat()
  cat("\n======================================================\n")
  cat(sprintf("Feature: %s\n", feature_name))
  cat("======================================================\n")
  cat(sprintf("Min:              %.4f\n", stats["Min"]))
  cat(sprintf("Max:              %.4f\n", stats["Max"]))
  cat(sprintf("Mean:             %.4f\n", stats["Mean"]))
  cat(sprintf("SD:               %.4f\n", stats["SD"]))
  cat(sprintf("25th Percentile:  %.4f\n", stats["Percentile_25"]))
  cat(sprintf("75th Percentile:  %.4f\n", stats["Percentile_75"]))
}

# Loop through all specified features and print the summary for each
for (feature in features_to_analyze) {
  # Assuming df_planting_pheno is the name of your data frame
  print_descriptive_stats(df_planting_pheno, feature)
}

hist(df_planting_pheno$cum_meansrad)
# #========================================================
# # 7. PLOT TOP 15 FEATURES VS. TARGET VARIABLE (HARVESTING)
# #========================================================
# # Get the top 15 features for Harvesting based on Total_scaled importance
# top_15_harvest_features <- harvest_importance_combined %>%
#   arrange(desc(Total_scaled)) %>%
#   head(15) %>%
#   pull(variable)
# 
# # Loop through the top 15 features and create scatter plots
# for (feature in top_15_harvest_features) {
#   # Create a data frame for plotting (using combined_df as the source)
#   plot_data <- dfharvest_pheno %>%
#     dplyr::select(!!sym(feature), HDDOY) %>%
#     drop_na() # Ensure no NAs in the selected columns for plotting
#   
#   p <- ggplot(plot_data, aes_string(x = feature, y = "HDDOY")) +
#     geom_point(alpha = 0.6, color = "#3C5488FF") +
#     geom_smooth(method = "lm", se = FALSE, color = "#DC0000FF") + # Add a linear regression line
#     labs(
#       title = paste("Harvesting: HDDOY vs.", feature),
#       x = feature,
#       y = "HDDOY"
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       plot.title = element_text(size = 16, face = "bold"),
#       axis.title.x = element_text(size = 14),
#       axis.title.y = element_text(size = 14)
#     )
#   
#   print(p)
#   
#   # Save the plot
#   ggsave(
#     filename = paste0("harvesting_HDDOY_vs_", feature, ".jpeg"),
#     plot = p,
#     path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/Features",
#     dpi = 300,
#     width = 8,
#     height = 6,
#     units = "in"
#   )
# }
# 
# 
# #========================================================
# # 7. PLOT TOP 15 FEATURES VS. TARGET VARIABLE (PLANTING)
# #========================================================
# # Get the top 15 features for Planting based on Total_scaled importance
# top_15_planting_features <- planting_importance_combined %>%
#   arrange(desc(Total_scaled)) %>%
#   head(15) %>%
#   pull(Variable) # Note: 'Variable' column name for planting_importance_combined
# 
# # Loop through the top 15 features and create scatter plots
# for (feature in top_15_planting_features) {
#   # Create a data frame for plotting (using combined_df as the source)
#   plot_data <- df %>%
#     dplyr::select(!!sym(feature), PDDOY) %>%
#     drop_na() # Ensure no NAs in the selected columns for plotting
#   
#   # Fit a linear model to get the R-squared value
#   lm_model <- lm(as.formula(paste("PDDOY ~", feature)), data = plot_data)
#   r_squared <- summary(lm_model)$r.squared
#   
#   p <- ggplot(plot_data, aes_string(x = feature, y = "PDDOY")) +
#     geom_point(alpha = 0.6, color = "#00A087FF") +
#     geom_smooth(method = "lm", se = FALSE, color = "#DC0000FF") + # Add a linear regression line
#     labs(
#       title = paste0("Planting: PDDOY vs. ", feature, " (R² = ", sprintf("%.2f", r_squared), ")"), # Add R-squared to title
#       x = feature,
#       y = "PDDOY"
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       plot.title = element_text(size = 16, face = "bold"),
#       axis.title.x = element_text(size = 14),
#       axis.title.y = element_text(size = 14)
#     )
#   
#   print(p)
#   
#   # Save the plot
#   ggsave(
#     filename = paste0("planting_PDDOY_vs_", feature, ".jpeg"),
#     plot = p,
#     path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/Features",
#     dpi = 300,
#     width = 8,
#     height = 6,
#     units = "in"
#   )
# }



library(reprtree)
reprtree:::plot.getTree(rf_model_planting, k = 3, depth = 5)  # Plot first tree, depth limited




