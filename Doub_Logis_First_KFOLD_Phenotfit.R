# NEW: Calculate the DOY when cumulative GDD first becomes non-NA for each field
phenology_df$GDD_Accumulation_Start_DOY <- sapply(phenology_df$Field_Year, function(field_year) {
  vi_data <- vi_data_by_field[[field_year]]
  if (is.null(vi_data) || !("cumulative_gdd_from_pddoy" %in% names(vi_data)) || !("doy" %in% names(vi_data))) {
    return(NA)
  }
  first_non_na_idx <- which(!is.na(vi_data$cumulative_gdd_from_pddoy))[1]
  if (is.na(first_non_na_idx)) {
    return(NA) # No non-NA GDD values found
  }
  return(vi_data$doy[first_non_na_idx])
})

# --- Inside CV loop ---
for (i in 1:n_runs) {
  
  # 1. Split Data
  unique_fields <- unique(phenology_df$Field_Year)
  n_fields <- length(unique_fields)
  train_indices <- sample(n_fields, size = floor(n_fields * train_ratio))
  remaining_indices <- setdiff(1:n_fields, train_indices)
  val_indices <- sample(remaining_indices, size = floor(n_fields * val_ratio))
  test_indices <- setdiff(remaining_indices, val_indices)
  
  train_fields <- unique_fields[train_indices]
  val_fields <- unique_fields[val_indices]
  test_fields <- unique_fields[test_indices]
  
  train_df_run <- phenology_df %>% dplyr::filter(Field_Year %in% train_fields)
  val_df_run   <- phenology_df %>% dplyr::filter(Field_Year %in% val_fields)
  test_df_run  <- phenology_df %>% dplyr::filter(Field_Year %in% test_fields)
  
  # --- 2. Calculate GDD accumulation for training fields ---
  train_df_run$GDD_Accumulation_Start_DOY <- sapply(train_df_run$Field_Year, function(fy) {
    vi_data <- vi_data_by_field[[fy]]
    first_non_na <- which(!is.na(vi_data$cumulative_gdd_from_pddoy))[1]
    if (is.na(first_non_na)) return(NA)
    vi_data$doy[first_non_na]
  })
  
  # Calculate cumulative GDD at SOS for training set
  gdd_lags_train <- sapply(train_df_run$Field_Year, function(fy) {
    vi_data <- vi_data_by_field[[fy]]
    sos_doy <- train_df_run$SOS_deriv.sos[train_df_run$Field_Year == fy]
    get_cumulative_gdd_at_sos(vi_data, sos_doy)
  })
  
  current_mean_lag <- mean(gdd_lags_train, na.rm = TRUE)
  all_mean_gdd_lags[i] <- current_mean_lag
  
  # --- 3. Predict SOSPD2 for training and validation sets ---
  predict_sos_for_df <- function(df, mean_lag) {
    df %>% rowwise() %>%
      mutate(
        doy_at_mean_gdd = predict_sos_from_gdd_lag(vi_data_by_field[[Field_Year]], mean_lag),
        calculated_lag = doy_at_mean_gdd - GDD_Accumulation_Start_DOY,
        Predicted_SOSPD2 = SOS_deriv.sos - calculated_lag,
        Run_ID = i
      ) %>%
      ungroup()
  }
  
  train_df_run <- predict_sos_for_df(train_df_run, current_mean_lag)
  val_df_run   <- predict_sos_for_df(val_df_run, current_mean_lag)
  
  all_train_predictions[[i]] <- train_df_run
  all_val_predictions[[i]]   <- val_df_run
}


# Mean of training GDD lags across all runs
final_mean_gdd_lag_for_test <- mean(all_mean_gdd_lags, na.rm = TRUE)

# Calculate GDD_Accumulation_Start_DOY for all test fields
final_test_predictions_df <- phenology_df %>%
  dplyr::filter(Field_Year %in% unique_fields) %>%  # or just test fields
  dplyr::mutate(
    GDD_Accumulation_Start_DOY = sapply(Field_Year, function(fy) {
      vi_data <- vi_data_by_field[[fy]]
      first_non_na <- which(!is.na(vi_data$cumulative_gdd_from_pddoy))[1]
      if (is.na(first_non_na)) return(NA)
      vi_data$doy[first_non_na]
    })
  ) %>%
  predict_sos_for_df(final_mean_gdd_lag_for_test)



# --- Helper function for metrics ---
# Calculate mean and SD for train and validation across runs
summary_metrics_df <- final_per_run_metrics_df %>%
  dplyr::summarise(
    Mean_GDD_Lag_Train_Mean = mean(Mean_GDD_Lag_Train, na.rm = TRUE),
    Mean_GDD_Lag_Train_SD   = sd(Mean_GDD_Lag_Train, na.rm = TRUE),
    
    Train_RMSE_Mean = mean(Train_RMSE, na.rm = TRUE),
    Train_RMSE_SD   = sd(Train_RMSE, na.rm = TRUE),
    Train_MAE_Mean  = mean(Train_MAE, na.rm = TRUE),
    Train_MAE_SD    = sd(Train_MAE, na.rm = TRUE),
    Train_R2_Mean   = mean(Train_R2, na.rm = TRUE),
    Train_R2_SD     = sd(Train_R2, na.rm = TRUE),
    Train_Bias_Mean = mean(Train_Bias, na.rm = TRUE),
    Train_Bias_SD   = sd(Train_Bias, na.rm = TRUE),
    
    Val_RMSE_Mean = mean(Val_RMSE, na.rm = TRUE),
    Val_RMSE_SD   = sd(Val_RMSE, na.rm = TRUE),
    Val_MAE_Mean  = mean(Val_MAE, na.rm = TRUE),
    Val_MAE_SD    = sd(Val_MAE, na.rm = TRUE),
    Val_R2_Mean   = mean(Val_R2, na.rm = TRUE),
    Val_R2_SD     = sd(Val_R2, na.rm = TRUE),
    Val_Bias_Mean = mean(Val_Bias, na.rm = TRUE),
    Val_Bias_SD   = sd(Val_Bias, na.rm = TRUE)
  )



# --- Calculate metrics ---
train_metrics <- calculate_metrics(final_train_predictions_df$observed_SOS ,
                                   final_train_predictions_df$Predicted_SOSPD2)

val_metrics   <- calculate_metrics(final_val_predictions_df$observed_SOS ,
                                   final_val_predictions_df$Predicted_SOSPD2)

test_metrics  <- calculate_metrics(final_test_predictions_df$observed_SOS ,
                                   final_test_predictions_df$Predicted_SOSPD2)
# Add test set metrics (single value, no SD)
summary_metrics_df$Test_RMSE  <- test_metrics$RMSE
summary_metrics_df$Test_MAE   <- test_metrics$MAE
summary_metrics_df$Test_R2    <- test_metrics$R2
summary_metrics_df$Test_Bias  <- test_metrics$Bias

print(summary_metrics_df)


# --- Aggregate predictions across runs ---
final_train_predictions_df <- do.call(rbind, all_train_predictions)
final_val_predictions_df   <- do.call(rbind, all_val_predictions)
# final_test_predictions_df is already created using mean lag across runs


# --- Display results ---
cat("\n--- Training Set Metrics ---\n")
print(train_metrics)

cat("\n--- Validation Set Metrics ---\n")
print(val_metrics)

cat("\n--- Test Set Metrics ---\n")
print(test_metrics)

# Optionally combine into a single data frame
metrics_summary_df <- data.frame(
  Set = c("Train", "Validation", "Test"),
  RMSE = c(train_metrics$RMSE, val_metrics$RMSE, test_metrics$RMSE),
  MAE  = c(train_metrics$MAE, val_metrics$MAE, test_metrics$MAE),
  R2   = c(train_metrics$R2, val_metrics$R2, test_metrics$R2),
  Bias = c(train_metrics$Bias, val_metrics$Bias, test_metrics$Bias)
)
print(metrics_summary_df)

library(ggplot2)

# Create a data frame for plotting
gdd_lags_df <- data.frame(Mean_GDD_Lag = all_mean_gdd_lags)

# Plot histogram
ggplot(gdd_lags_df, aes(x = Mean_GDD_Lag)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Mean Cumulative GDD Across 100 Training Runs",
    x = "Mean Cumulative GDD at SOS (Training Set)",
    y = "Frequency"
  ) +
  theme_bw()



library(ggplot2)
library(dplyr)
library(tidyr)

# === RMSE & MAE ===
plot1_df <- tibble(
  Metric = rep(c("RMSE", "MAE"), each = 3),
  Dataset = rep(c("Train", "Validation", "Test"), times = 2),
  Value = c(summary_metrics_df$Train_RMSE_Mean,
            summary_metrics_df$Val_RMSE_Mean,
            summary_metrics_df$Test_RMSE,
            summary_metrics_df$Train_MAE_Mean,
            summary_metrics_df$Val_MAE_Mean,
            summary_metrics_df$Test_MAE),
  SD = c(summary_metrics_df$Train_RMSE_SD,
         summary_metrics_df$Val_RMSE_SD,
         NA,
         summary_metrics_df$Train_MAE_SD,
         summary_metrics_df$Val_MAE_SD,
         NA)
)

# === R² & Bias ===
plot2_df <- tibble(
  Metric = rep(c("R²", "Bias"), each = 3),
  Dataset = rep(c("Train", "Validation", "Test"), times = 2),
  Value = c(summary_metrics_df$Train_R2_Mean,
            summary_metrics_df$Val_R2_Mean,
            summary_metrics_df$Test_R2,
            summary_metrics_df$Train_Bias_Mean,
            summary_metrics_df$Val_Bias_Mean,
            summary_metrics_df$Test_Bias),
  SD = c(summary_metrics_df$Train_R2_SD,
         summary_metrics_df$Val_R2_SD,
         NA,
         summary_metrics_df$Train_Bias_SD,
         summary_metrics_df$Val_Bias_SD,
         NA)
)

# === Plot 1: RMSE and MAE ===
p1 <- ggplot(plot1_df, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  labs(title = "RMSE and MAE (DOY)", y = "Error", x = "Metric") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +  # Base font size
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Plot 2: R² and Bias ===
p2 <- ggplot(plot2_df, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  labs(title = "R² and Bias", y = "Value", x = "Metric") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Show Plots ===
print(p1)
print(p2)
plot1_df
plot2_df

final_mean_gdd_lag_for_test <- mean(all_mean_gdd_lags, na.rm = TRUE)
# --- Print mean and SD of training cumulative GDD lag used for test set ---
mean_gdd_lag_train <- mean(all_mean_gdd_lags, na.rm = TRUE)
sd_gdd_lag_train <- sd(all_mean_gdd_lags, na.rm = TRUE)

cat("\n--- Training Cumulative GDD Lag Summary ---\n")
cat("Mean cumulative GDD (used for test set):", round(mean_gdd_lag_train, 2), "\n")
cat("SD of cumulative GDD (across runs):", round(sd_gdd_lag_train, 2), "\n")

