
# Add observed values from vi_list_gt20
# PDDOY is assumed to be observed_SOS, HDDOY is observed_EOS
sos_eos_df$observed_SOS <- sapply(vi_list_gt20, function(df) df$PDDOY[1])
sos_eos_df$observed_EOS <- sapply(vi_list_gt20, function(df) df$HDDOY[1])

# Handle missing values for observed_EOS (if "NULL" strings are present)
sos_eos_df$observed_EOS[sos_eos_df$observed_EOS == "NULL"] <- NA
sos_eos_df$observed_EOS <- as.numeric(sos_eos_df$observed_EOS)

# Extract field names and years to create Field_Year if not already in results
# (This part might be redundant if 'Field_Year' is already correctly in 'results' as in dummy data)
if (!("Field_Year" %in% names(sos_eos_df))) {
  field_names <- sapply(vi_list_gt20, function(df) {
    unique_name <- unique(na.omit(df$FIELD_NAME))
    if (length(unique_name) > 0) unique_name[1] else NA
  })
  field_years <- sapply(vi_list_gt20, function(df) {
    valid_dates <- na.omit(df$Date)
    if (length(valid_dates) > 0) format(valid_dates[1], "%Y") else NA
  })
  sos_eos_df$Field_Year <- paste(field_names, field_years, sep = "_")
}

# Ensure vi_data_by_field is a named list of data frames, where names are 'Field_Year'
# and each data frame contains 'cumulative_gdd_from_pddoy' and potentially 'gdd'.
# For this example, vi_list_gt20 serves as vi_data_by_field.
vi_data_by_field <- vi_list_gt20

# NEW: Calculate the DOY when cumulative GDD first becomes non-NA for each field
sos_eos_df$GDD_Accumulation_Start_DOY <- sapply(sos_eos_df$Field_Year, function(field_year) {
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


# Re-define calculate_metrics if not in current environment
calculate_metrics <- function(observed, predicted) {
  valid_df <- na.omit(data.frame(observed, predicted))
  if (nrow(valid_df) == 0) {
    return(list(RMSE = NA, MAE = NA, R2 = NA, Bias = NA))
  }
  list(
    RMSE = round(sqrt(mean((valid_df$observed - valid_df$predicted)^2)), 2),
    MAE = round(mean(abs(valid_df$observed - valid_df$predicted)), 2),
    R2 = round(summary(lm(predicted ~ observed, data = valid_df))$r.squared, 2),
    Bias = round(mean(valid_df$predicted - valid_df$observed), 2) # Corrected bias calculation
  )
}

# --- New Helper Functions for GDD Lag Logic ---

#' Get Cumulative GDD at a Specific SOS Day of Year (DOY)
#'
#' This function retrieves the cumulative Growing Degree Days (GDD) value
#' from a field's VI data at the specified Start of Season (SOS) DOY.
#' It assumes the `cumulative_gdd_from_pddoy` column and `doy` column exist.
#'
#' @param vi_df A data frame containing VI data for a single field-year,
#'   must have 'cumulative_gdd_from_pddoy' and 'doy' columns.
#' @param sos_doy The Start of Season Day of Year (numeric).
#' @return The cumulative GDD value at `sos_doy`, or NA if `sos_doy` is not found
#'   in the `doy` column or the GDD value at that DOY is NA.
get_cumulative_gdd_at_sos <- function(vi_df, sos_doy) {
  # Find the row where the doy matches sos_doy
  row_idx <- which(vi_df$doy == sos_doy)
  
  if (length(row_idx) == 0) {
    return(NA) # sos_doy not found in doy column
  }
  
  # Return the cumulative GDD from that row
  gdd_value <- vi_df$cumulative_gdd_from_pddoy[row_idx[1]] # Take first match if duplicates
  return(gdd_value)
}

#' Predict SOS DOY based on a Target Cumulative GDD Lag
#'
#' This function finds the Day of Year (DOY) in a field's VI data where the
#' `cumulative_gdd_from_pddoy` is closest to a given `target_gdd_lag`.
#' It returns the DOY from the 'doy' column.
#'
#' @param vi_df A data frame containing VI data for a single field-year,
#'   must have 'cumulative_gdd_from_pddoy' and 'doy' columns.
#' @param target_gdd_lag The target cumulative GDD value (the mean lag from training).
#' @return The predicted SOS DOY, or NA if no valid GDD data is available.
predict_sos_from_gdd_lag <- function(vi_df, target_gdd_lag) {
  # Get indices of non-NA cumulative GDD values
  valid_gdd_rows <- which(!is.na(vi_df$cumulative_gdd_from_pddoy))
  
  if (length(valid_gdd_rows) == 0) {
    return(NA) # No valid GDD data to make a prediction
  }
  
  # Get the actual non-NA cumulative GDD values and their corresponding DOYs
  valid_gdd_values <- vi_df$cumulative_gdd_from_pddoy[valid_gdd_rows]
  valid_doy_values <- vi_df$doy[valid_gdd_rows]
  
  # Find the index within `valid_gdd_values` that is closest to `target_gdd_lag`
  closest_relative_idx <- which.min(abs(valid_gdd_values - target_gdd_lag))
  
  # Return the DOY corresponding to this closest GDD value
  predicted_doy <- valid_doy_values[closest_relative_idx]
  return(predicted_doy)
}


# --- K-Fold Cross-Validation Setup ---
n_runs <- 100 # Number of runs
train_ratio <- 0.6
val_ratio <- 0.2
test_ratio <- 0.2

# Store results for each run
all_train_predictions <- list()
all_val_predictions <- list()
all_test_predictions_by_field <- list() # To store test predictions for averaging later
all_mean_gdd_lags <- numeric(n_runs) # To store the mean GDD lag from each training run

# NEW: List to store per-run metrics
per_run_metrics_list <- list()

# Set up parallel processing for the runs (optional, but good for n_runs=100)
# plan(multisession, workers = 4) # Adjust workers as needed
# handlers(global = TRUE)
# handlers("txtprogressbar")

cat("Starting K-fold cross-validation...\n")

# Use with_progress for a progress bar if parallel processing is enabled
# with_progress({
#    p <- progressor(along = 1:n_runs)

for (i in 1:n_runs) {
  # p(sprintf("Running iteration %d/%d", i, n_runs)) # Update progress bar
  
  # 1. Split Data
  # Ensure reproducibility for each split if needed, or comment out for true randomness
  # set.seed(i)
  
  # Get unique Field_Years for splitting
  unique_fields <- unique(sos_eos_df$Field_Year)
  n_fields <- length(unique_fields)
  
  # Randomly sample indices for each set
  train_indices <- sample(n_fields, size = floor(n_fields * train_ratio))
  remaining_indices <- setdiff(1:n_fields, train_indices)
  val_indices <- sample(remaining_indices, size = floor(n_fields * val_ratio))
  test_indices <- setdiff(remaining_indices, val_indices)
  
  # Get the actual Field_Year names for each set
  train_fields <- unique_fields[train_indices]
  val_fields <- unique_fields[val_indices]
  test_fields <- unique_fields[test_indices]
  
  # Subset sos_eos_df for each set
  train_df_run <- sos_eos_df %>% dplyr::filter(Field_Year %in% train_fields)
  val_df_run <- sos_eos_df %>% dplyr::filter(Field_Year %in% val_fields)
  test_df_run <- sos_eos_df %>% dplyr::filter(Field_Year %in% test_fields)
  
  # 2. Training Phase: Calculate mean GDD lag from training data
  # For each training field, find the cumulative GDD at its SOS DOY
  gdd_lags_train_list <- purrr::map(train_fields, function(field_year) {
    # Get the SOS DOY for the current field_year from train_df_run
    sos_doy <- train_df_run$SOS[train_df_run$Field_Year == field_year]
    # Get the corresponding VI data
    vi_data <- vi_data_by_field[[field_year]]
    
    if (is.null(vi_data) || length(sos_doy) == 0) {
      return(NA) # Handle cases where data or SOS is missing
    }
    
    # Use the new function to get the cumulative GDD at this SOS DOY
    get_cumulative_gdd_at_sos(vi_data, sos_doy)
  })
  
  gdd_lags_train <- unlist(gdd_lags_train_list)
  
  # Calculate the mean GDD lag from the training set, handling NAs
  current_mean_lag <- mean(gdd_lags_train, na.rm = TRUE)
  
  # Store the calculated mean GDD lag for this run
  all_mean_gdd_lags[i] <- current_mean_lag
  
  # Predict SOSPD2 for training set using the new lag definition
  train_df_run <- train_df_run %>%
    dplyr::rowwise() %>% # Apply function row by row for each field
    dplyr::mutate(
      # Step 1: Find the DOY where this field's cumulative GDD reaches the training-derived mean GDD
      doy_at_mean_gdd = predict_sos_from_gdd_lag(vi_data_by_field[[Field_Year]], current_mean_lag),
      
      # Step 2: Get the GDD_Accumulation_Start_DOY for this field (already in sos_eos_df)
      gdd_start_doy = GDD_Accumulation_Start_DOY,
      
      # Step 3: Calculate the lag for this specific field
      calculated_lag = doy_at_mean_gdd - gdd_start_doy,
      
      # Step 4: Calculate Predicted_SOSPD2
      Predicted_SOSPD2 = SOS - calculated_lag,
      Run_ID = i
    ) %>%
    dplyr::ungroup() # Remove rowwise grouping
  all_train_predictions[[i]] <- train_df_run
  
  # 3. Validation Phase: Apply mean lag to validation data
  val_df_run <- val_df_run %>%
    dplyr::rowwise() %>% # Apply function row by row
    dplyr::mutate(
      doy_at_mean_gdd = predict_sos_from_gdd_lag(vi_data_by_field[[Field_Year]], current_mean_lag),
      gdd_start_doy = GDD_Accumulation_Start_DOY,
      calculated_lag = doy_at_mean_gdd - gdd_start_doy,
      Predicted_SOSPD2 = SOS - calculated_lag,
      Run_ID = i
    ) %>%
    dplyr::ungroup() # Remove rowwise grouping
  all_val_predictions[[i]] <- val_df_run
  
  # 4. Testing Phase: Apply mean lag to testing data
  test_df_run <- test_df_run %>%
    dplyr::rowwise() %>% # Apply function row by row
    dplyr::mutate(
      doy_at_mean_gdd = predict_sos_from_gdd_lag(vi_data_by_field[[Field_Year]], current_mean_lag),
      gdd_start_doy = GDD_Accumulation_Start_DOY,
      calculated_lag = doy_at_mean_gdd - gdd_start_doy,
      Predicted_SOSPD2 = SOS - calculated_lag,
      Run_ID = i
    ) %>%
    dplyr::ungroup() # Remove rowwise grouping
  all_test_predictions_by_field[[i]] <- test_df_run
  
  # Calculate and store metrics for the current run's training and validation sets
  train_metrics_current_run <- calculate_metrics(
    train_df_run$observed_SOS,
    train_df_run$Predicted_SOSPD2
  )
  val_metrics_current_run <- calculate_metrics(
    val_df_run$observed_SOS,
    val_df_run$Predicted_SOSPD2
  )
  
  # Store these metrics in a list for later combination
  per_run_metrics_list[[i]] <- data.frame(
    Run_ID = i,
    Mean_GDD_Lag_Train = current_mean_lag,
    Train_RMSE = train_metrics_current_run$RMSE,
    Train_MAE = train_metrics_current_run$MAE,
    Train_R2 = train_metrics_current_run$R2,
    Train_Bias = train_metrics_current_run$Bias,
    Val_RMSE = val_metrics_current_run$RMSE,
    Val_MAE = val_metrics_current_run$MAE,
    Val_R2 = val_metrics_current_run$R2,
    Val_Bias = val_metrics_current_run$Bias
  )
}
# }) # End with_progress

cat("K-fold cross-validation complete. Aggregating results...\n")

# --- Aggregate Per-Run Metrics ---
final_per_run_metrics_df <- do.call(rbind, per_run_metrics_list)
cat("\n--- Per-Run Metrics (Training and Validation) ---\n")
print(head(final_per_run_metrics_df)) # Show first few rows
cat(sprintf("Total %d runs' metrics stored.\n", nrow(final_per_run_metrics_df)))

# Calculate Overall Performance Evaluation (Aggregated) for Test Set
# This is needed before creating summary_metrics_df
test_metrics_overall <- calculate_metrics(
  final_test_predictions_df$Observed_SOS,
  final_test_predictions_df$Mean_Predicted_SOSPD2_test
)

# NEW: Calculate and print mean and standard deviation of per-run metrics
cat("\n--- Mean and Standard Deviation of Per-Run Metrics ---\n")
summary_metrics_df <- final_per_run_metrics_df %>%
  dplyr::summarise(
    Mean_GDD_Lag_Train_Mean = mean(Mean_GDD_Lag_Train, na.rm = TRUE),
    Mean_GDD_Lag_Train_SD = sd(Mean_GDD_Lag_Train, na.rm = TRUE),
    Train_RMSE_Mean = mean(Train_RMSE, na.rm = TRUE),
    Train_RMSE_SD = sd(Train_RMSE, na.rm = TRUE),
    Train_MAE_Mean = mean(Train_MAE, na.rm = TRUE),
    Train_MAE_SD = sd(Train_MAE, na.rm = TRUE),
    Train_R2_Mean = mean(Train_R2, na.rm = TRUE),
    Train_R2_SD = sd(Train_R2, na.rm = TRUE),
    Train_Bias_Mean = mean(Train_Bias, na.rm = TRUE),
    Train_Bias_SD = sd(Train_Bias, na.rm = TRUE),
    Val_RMSE_Mean = mean(Val_RMSE, na.rm = TRUE),
    Val_RMSE_SD = sd(Val_RMSE, na.rm = TRUE),
    Val_MAE_Mean = mean(Val_MAE, na.rm = TRUE),
    Val_MAE_SD = sd(Val_MAE, na.rm = TRUE),
    Val_R2_Mean = mean(Val_R2, na.rm = TRUE),
    Val_R2_SD = sd(Val_R2, na.rm = TRUE),
    Val_Bias_Mean = mean(Val_Bias, na.rm = TRUE),
    Val_Bias_SD = sd(Val_Bias, na.rm = TRUE)
  )
summary_metrics_df_dl_first<-summary_metrics_df
# NEW: Add single run test metrics to summary_metrics_df
summary_metrics_df$Test_RMSE = test_metrics_overall$RMSE
summary_metrics_df$Test_MAE = test_metrics_overall$MAE
summary_metrics_df$Test_R2 = test_metrics_overall$R2
summary_metrics_df$Test_Bias = test_metrics_overall$Bias

print(summary_metrics_df)

summary_metrics_df
# Define output directory and file path
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DoubleLogistics/kfold"
output_file <- file.path(output_dir, "summary_metrics_df.csv")

# Create directory if it doesn't exist
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save the dataframe to CSV
write.csv(summary_metrics_df, output_file, row.names = FALSE)
# --- Aggregate All Predictions ---

# Combine all training predictions
final_train_predictions_df <- do.call(rbind, all_train_predictions)

# Combine all validation predictions
final_val_predictions_df <- do.call(rbind, all_val_predictions)

# Combine all test predictions and calculate the mean prediction per Field_Year
# A field might appear in the test set multiple times across 100 runs
final_test_predictions_df <- do.call(rbind, all_test_predictions_by_field) %>%
  dplyr::group_by(Field_Year) %>%
  dplyr::summarise(Mean_Predicted_SOSPD2_test = mean(Predicted_SOSPD2, na.rm = TRUE),
                   Observed_SOS = dplyr::first(observed_SOS)) # Get the observed SOS for evaluation

# --- Overall Performance Evaluation (Aggregated) ---

cat("\n--- Overall Performance Evaluation (Aggregated Across All Runs) ---\n")

# Training Set Performance (overall across all training instances)
train_metrics_overall <- calculate_metrics(
  final_train_predictions_df$observed_SOS,
  final_train_predictions_df$Predicted_SOSPD2
)
cat("Training Set Metrics (overall aggregated):\n")
print(train_metrics_overall)

# Validation Set Performance (overall across all validation instances)
val_metrics_overall <- calculate_metrics(
  final_val_predictions_df$observed_SOS,
  final_val_predictions_df$Predicted_SOSPD2
)
cat("\nValidation Set Metrics (overall aggregated):\n")
print(val_metrics_overall)

# Testing Set Performance (using the mean ptrain_metrics_overallrediction for each field in test sets)
# This calculation is now done before summary_metrics_df for use there
# test_metrics_overall <- calculate_metrics(
#   final_test_predictions_df$Observed_SOS,
#   final_test_predictions_df$Mean_Predicted_SOSPD2_test
# )
cat("\nTesting Set Metrics (mean prediction per field across runs):\n")
print(test_metrics_overall)

# --- Visualization of Final Testing Results ---

cat("\nGenerating plot for final testing results...\n")

# Plot for the mean test predictions
ggplot(final_test_predictions_df, aes(x = Observed_SOS, y = Mean_Predicted_SOSPD2_test)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "darkred") +
  annotate("text",
           x = quantile(final_test_predictions_df$Observed_SOS, 0.1, na.rm = TRUE),
           y = quantile(final_test_predictions_df$Mean_Predicted_SOSPD2_test, 0.9, na.rm = TRUE),
           label = sprintf("RMSE: %.1f\nMAE: %.1f\nR²: %.2f",
                           test_metrics_overall$RMSE, test_metrics_overall$MAE, test_metrics_overall$R2)) +
  labs(x = "Observed SOS (DOY)", y = "Predicted SOS (DOY) [Mean of 100 Runs]") +
  ggtitle("K-Fold Cross-Validation: Mean Predicted SOS for Test Set") +
  theme_bw()

# Histogram of Mean GDD Lags from Training Runs
cat("\nGenerating histogram for mean GDD lags from training runs...\n")
ggplot(data.frame(Mean_GDD_Lag = all_mean_gdd_lags), aes(x = Mean_GDD_Lag)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) + # Adjust binwidth as needed
  labs(x = "Mean Cumulative GDD at Predicted SOS (Training Set)", y = "Frequency") +
  ggtitle("Distribution of Mean GDD Lag Across 100 Training Runs") +
  theme_bw()
#====================================================================================
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
