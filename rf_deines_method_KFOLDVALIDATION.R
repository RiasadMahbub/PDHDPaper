library(randomForest)
library(Metrics)
library(hydroGOF) # for NSE
library(dplyr)
library(ggplot2)
library(tidyr)

#========================================================
#HARVEST k FOLD VALIDATION
#========================================================
# Training:60
# Testing: 20
# Validation: 20
# Validation:
df_harvest <- deines_results_df %>%
  dplyr::select(
    #Field_ID, # Keep Field_ID to identify rows
    GCVI_b1,
    vpd_Jun,
    ppt_May,
    GCVI_a2,
    tmax_Apr,
    gdd_Mar_May,
    tmin_Apr,
    ppt_Apr,
    vpd_Jun,
    GCVI_a1,
    vpd_May,
    GCVI_b2,
    nir_b2,
    soiltemp_May,
    nir_a1,
    tmax_May,
    nir_a2,
    HDDOY
    
    
  ) %>%
  drop_na() # This removes rows with NA in any column

#-------------------------------
set.seed(123)
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
plot1_df_harvest_deines <- tibble(
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

plot2_df_harvest_deines <- tibble(
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

# Filter Bias and R² separately
plot2_bias_df_harvest <- plot2_df_harvest_deines %>%
  dplyr::filter(Metric == "Bias (day)")

plot3_r2_df_harvest <- plot2_df_harvest_deines %>%
  dplyr::filter(Metric == "R²")

# Ensure Dataset factor order
plot1_df_harvest_deines$Dataset <- factor(plot1_df_harvest_deines$Dataset,
                                          levels = c("Train", "Validation", "Test"))
plot2_bias_df_harvest$Dataset <- factor(plot2_bias_df_harvest$Dataset,
                                        levels = c("Train", "Validation", "Test"))
plot3_r2_df_harvest$Dataset <- factor(plot3_r2_df_harvest$Dataset,
                                      levels = c("Train", "Validation", "Test"))

# Van Gogh colors
library(vangogh)
vvg_colors <- vangogh_palette("SunflowersMunich", n = 5, type = "discrete")
vg_colors <- vvg_colors[c(1,3,5)]


# ----------------------------
# Plot 1: RMSE and MAE
# ----------------------------
p1_harvest_deines <- ggplot(plot1_df_harvest_deines, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7), width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8, size = 4.5) +
  labs(title = "Harvesting: RMSE and MAE", y = "Error", x = "Metric") +
  scale_fill_manual(values = vg_colors) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# ----------------------------
# Plot 2: Bias
# ----------------------------
p2_harvest_deines <- ggplot(plot2_bias_df_harvest, aes(x = Dataset, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            vjust = -0.8, size = 4.5) +
  labs(y = "Bias (day)", x = "") +
  scale_fill_manual(values = vg_colors) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# ----------------------------
# Plot 3: R²
# ----------------------------
p3_harvest_deines <- ggplot(plot3_r2_df_harvest, aes(x = Dataset, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            vjust = -0.8, size = 4.5) +
  labs(y = "R²", x = "") +
  scale_fill_manual(values = vg_colors) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# ----------------------------
# Show Plots
# ----------------------------
print(p1_harvest_deines)
print(p3_harvest_deines)
print(p2_harvest_deines)

# # === Plot 1: RMSE and MAE for Harvest ===
# p1_harvest_deines <- ggplot(plot1_df_harvest_deines, aes(x = Metric, y = Value, fill = Dataset)) +
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
#   labs(title = "Harvesting: RMSE and MAE", y = "Error", x = "Metric") +
#   scale_fill_brewer(palette = "Set2") +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18, face = "bold")
#   )
# 
# # === Plot 2: R² and Bias for Harvest ===
# p2_harvest_deines <- ggplot(plot2_df_harvest_deines, aes(x = Metric, y = Value, fill = Dataset)) +
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
#   scale_fill_brewer(palette = "Set2") +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18, face = "bold")
#   )
# 
# # === Show Plots for Harvest ===
# print(p1_harvest_deines)
# print(p2_harvest_deines)

# === Save Plots as JPEG for Harvest ===
ggsave(
  filename = "Deinesharvestingdatermsemae.jpeg",
  plot = p1_harvest_deines,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  filename = "Deinesharvestingdater2bias.jpeg",
  plot = p2_harvest_deines,
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
df_planting <- deines_results_df %>%
  dplyr::select(
    #Field_ID, # Keep Field_ID to identify rows
    GCVI_b1,
    vpd_Jun,
    ppt_May,
    GCVI_a2,
    tmax_Apr,
    gdd_Mar_May,
    tmin_Apr,
    ppt_Apr,
    vpd_Jun,
    GCVI_a1,
    vpd_May,
    GCVI_b2,
    nir_b2,
    soiltemp_May,
    nir_a1,
    tmax_May,
    nir_a2,
    PDDOY,
    Field_ID
    
    
  ) %>%
  drop_na() # This removes rows with NA in any column
DeinesRandomForestPlantingFeatures  <- colnames(df_planting)

#------------------------------
set.seed(123)
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

#-------------------------------
# 100 repeated runs with FIELD ID In considereation
#-------------------------------
for (i in 1:100) {
  set.seed(100 + i)
  
  # Split remaining_df into 75% train and 25% validation
  n_remain_planting <- nrow(remaining_df_planting)
  train_idx_planting <- sample(1:n_remain_planting, size = 0.75 * n_remain_planting)
  train_set_planting <- remaining_df_planting[train_idx_planting, ]
  val_set_planting <- remaining_df_planting[-train_idx_planting, ]
  
  # Keep Field_ID for later residuals
  train_field_id <- train_set_planting$Field_ID
  val_field_id   <- val_set_planting$Field_ID
  
  # Remove Field_ID for model training
  train_set_model <- train_set_planting %>% dplyr::select(-Field_ID)
  val_set_model   <- val_set_planting %>% dplyr::select(-Field_ID)
  
  # Train random forest model
  rf_model_planting <- randomForest::randomForest(PDDOY ~ ., data = train_set_model, ntree = 100, importance = TRUE)
  
  # Predictions
  train_pred_planting <- predict(rf_model_planting, newdata = train_set_model)
  val_pred_planting   <- predict(rf_model_planting, newdata = val_set_model)
  
  obs_train_planting <- train_set_model$PDDOY
  obs_val_planting   <- val_set_model$PDDOY
  
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

# Check training set column types
sapply(train_set_planting, class)

#----------------------------
#Prepare Planting Data
#----------------------------
# === RMSE & MAE (with Test) for Planting ===
plot1_df_planting_deines <- tibble(
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
plot2_df_planting_deines <- tibble(
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
# Filter Bias and R² separately
plot2_bias_df_planting <- plot2_df_planting_deines %>%
  dplyr::filter(Metric == "Bias")

plot3_r2_df_planting <- plot2_df_planting_deines %>%
  dplyr::filter(Metric == "R²")

# Ensure Dataset factor order
plot1_df_planting_deines$Dataset <- factor(plot1_df_planting_deines$Dataset,
                                           levels = c("Train", "Validation", "Test"))
plot2_bias_df_planting$Dataset <- factor(plot2_bias_df_planting$Dataset,
                                         levels = c("Train", "Validation", "Test"))
plot3_r2_df_planting$Dataset <- factor(plot3_r2_df_planting$Dataset,
                                       levels = c("Train", "Validation", "Test"))

# Van Gogh colors
library(vangogh)
vvg_colors <- vangogh_palette("SunflowersMunich", n = 5, type = "discrete")
vg_colors <- vvg_colors[c(1,3,5)]

# ----------------------------
# Plot 1: RMSE and MAE
# ----------------------------
p1_planting_deines <- ggplot(plot1_df_planting_deines, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7), width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8, size = 4.5) +
  labs(title = "Planting: RMSE and MAE", y = "Error", x = "Metric") +
  scale_fill_manual(values = vg_colors) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# ----------------------------
# Plot 2: Bias
# ----------------------------
p2_planting_deines <- ggplot(plot2_bias_df_planting, aes(x = Dataset, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            vjust = -0.8, size = 4.5) +
  labs(y = "Bias (day)", x = "") +
  scale_fill_manual(values = vg_colors) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# ----------------------------
# Plot 3: R²
# ----------------------------
p3_planting_deines <- ggplot(plot3_r2_df_planting, aes(x = Dataset, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            vjust = -0.8, size = 4.5) +
  labs(y = "R²", x = "") +
  scale_fill_manual(values = vg_colors) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# ----------------------------
# Show Plots
# ----------------------------
print(p1_planting_deines)
print(p3_planting_deines)
print(p2_planting_deines)



# === Save Plots as JPEG for Planting ===
ggsave(
  filename = "Deinesplantingdatermsemae.jpeg",
  plot = p1_planting_deines,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

ggsave(
  filename = "Deinesplantingdater2bias.jpeg",
  plot = p2_planting_deines,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)


#---------------------------------------
# Predict PDDOY for the original df_planting
#---------------------------------------
# Remove Field_ID for prediction
# Remove Field_ID before training final model
final_model_planting <- randomForest::randomForest(
  PDDOY ~ ., 
  data = remaining_df_planting %>% dplyr::select(-Field_ID),
  ntree = 100
)

# Predict on df_planting (remove Field_ID)
df_planting$PDDeines <- predict(final_model_planting, newdata = df_planting %>% dplyr::select(-Field_ID))
df_planting$PDDeines <- predict(final_model_planting, newdata = df_planting)
df_planting_deines_residual <- df_planting %>%
  dplyr::mutate(residual_PDDeines = PDDeines - PDDOY)

#---------------------------------------
# Create new dataframe for residuals
#---------------------------------------
df_planting_deines_residual <- df_planting %>%
  dplyr::mutate(
    residual_PDDeines = PDDeines - PDDOY
  )

#---------------------------------------
# Preview
#---------------------------------------
head(df_planting_deines_residual)


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
  filename = "Deinesharvesting_variable_importance_total_stacked_scaled.jpeg", # Changed filename
  plot = p_harvest_total_importance,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 10,
  height = 8,
  units = "in"
)


val100_residuals <- data.frame(
  Field_ID = val_field_id,
  obs = obs_val_planting,
  pred = val_pred_planting,
  residual = obs_val_planting - val_pred_planting,
  set = "Validation"
)

test_residuals <- data.frame(
  Field_ID = test_set_planting$Field_ID,
  obs = obs_test_planting,
  pred = test_pred_planting,
  residual = obs_test_planting - test_pred_planting,
  set = "Test"
)

# Combine into one dataframe
val100test_define_set <- rbind(val100_residuals, test_residuals)


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
  filename = "Deinesplanting_variable_importance_total_stacked_scaled.jpeg", # Changed filename
  plot = p_planting_total_importance,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 10,
  height = 8,
  units = "in"
)
