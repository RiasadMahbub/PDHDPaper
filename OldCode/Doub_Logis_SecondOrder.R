# Load necessary libraries if not already loaded
# library(dplyr)
# library(purrr)
# library(ggplot2)
# library(future)
# library(progressr)
# library(minpack.lm) # For nlsLM

# Ensure the 'extract_sos_eos' function and 'calculate_metrics' function
# are defined and accessible from your previous code.
# Also ensure 'vi_list_gt20' and 'vi_data_by_field' are prepared as per your script.

# Dummy data for demonstration if not already loaded (replace with your actual data)
# For demonstration purposes, let's assume sos_eos_df and vi_data_by_field exist
# If running this independently, you would need to run the preceding parts of your script
# to generate these data structures.
# For example:
# source("your_phenology_script.R") # Assuming your previous code is in this file

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

# Ensure sos_eos_df and vi_data_by_field are available and correctly structured
# For this script to run, 'sos_eos_df' must contain 'SOS', 'observed_SOS', 'Field_Year'.
# 'vi_data_by_field' must be a named list of data frames, where names are 'Field_Year'
# and each data frame contains 'cumulative_gdd_from_pddoy' and 'gdd'.

# --- K-Fold Cross-Validation Setup ---
n_runs <- 100 # Number of runs
train_ratio <- 0.6
val_ratio <- 0.2
test_ratio <- 0.2

# Store results for each run
all_train_predictions <- list()
all_val_predictions <- list()
all_test_predictions_by_field <- list() # To store test predictions for averaging later

# Set up parallel processing for the runs (optional, but good for n_runs=100)
# plan(multisession, workers = 4) # Adjust workers as needed
# handlers(global = TRUE)
# handlers("txtprogressbar")

cat("Starting K-fold cross-validation...\n")

# Use with_progress for a progress bar if parallel processing is enabled
# with_progress({
#   p <- progressor(along = 1:n_runs)

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
  # Filter vi_data_by_field to only include training fields
  train_vi_data_for_lag <- vi_data_by_field[train_fields]
  
  # Calculate gdd_lags for the training set
  gdd_lags_train_list <- map(train_vi_data_for_lag, ~ calculate_gdd_lag_precise(.x))
  gdd_lags_train <- unlist(gdd_lags_train_list)
  
  # Calculate the mean lag from the training set, handling NAs
  current_mean_lag <- mean(gdd_lags_train, na.rm = TRUE)
  
  # Store the mean lag for this run (optional, but good for debugging)
  # This is the "lag of 100 runs of training" part
  # We will use this mean lag for prediction
  
  # Predict SOSPD2 for training set
  train_df_run <- train_df_run %>%
    dplyr::mutate(Predicted_SOSPD2 = SOS - current_mean_lag,
                  Run_ID = i)
  all_train_predictions[[i]] <- train_df_run
  
  # 3. Validation Phase: Apply mean lag to validation data
  val_df_run <- val_df_run %>%
    dplyr::mutate(Predicted_SOSPD2 = SOS - current_mean_lag,
                  Run_ID = i)
  all_val_predictions[[i]] <- val_df_run
  
  # 4. Testing Phase: Apply mean lag to testing data
  test_df_run <- test_df_run %>%
    dplyr::mutate(Predicted_SOSPD2 = SOS - current_mean_lag,
                  Run_ID = i)
  all_test_predictions_by_field[[i]] <- test_df_run
}
# }) # End with_progress

cat("K-fold cross-validation complete. Aggregating results...\n")

# --- Aggregate Results ---

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

# --- Performance Evaluation ---

cat("\n--- Performance Evaluation ---\n")

# Training Set Performance (overall across all training instances)
train_metrics_overall <- calculate_metrics(
  final_train_predictions_df$observed_SOS,
  final_train_predictions_df$Predicted_SOSPD2
)
cat("Training Set Metrics (across all runs):\n")
print(train_metrics_overall)

# Validation Set Performance (overall across all validation instances)
val_metrics_overall <- calculate_metrics(
  final_val_predictions_df$observed_SOS,
  final_val_predictions_df$Predicted_SOSPD2
)
cat("\nValidation Set Metrics (across all runs):\n")
print(val_metrics_overall)

# Testing Set Performance (using the mean prediction for each field in test sets)
test_metrics_overall <- calculate_metrics(
  final_test_predictions_df$Observed_SOS,
  final_test_predictions_df$Mean_Predicted_SOSPD2_test
)
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

# You can also visualize training and validation results similarly if needed
# For example:
# ggplot(final_train_predictions_df, aes(x = observed_SOS, y = Predicted_SOSPD2)) +
#   geom_point(color = "blue", alpha = 0.6) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   geom_smooth(method = "lm", color = "red") +
#   labs(x = "Observed SOS (DOY)", y = "Predicted SOS (DOY) [Training]") +
#   ggtitle("K-Fold Cross-Validation: Training Set Predictions") +
#   theme_bw()

# ggplot(final_val_predictions_df, aes(x = observed_SOS, y = Predicted_SOSPD2)) +
#   geom_point(color = "purple", alpha = 0.6) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   geom_smooth(method = "lm", color = "orange") +
#   labs(x = "Observed SOS (DOY)", y = "Predicted SOS (DOY) [Validation]") +
#   ggtitle("K-Fold Cross-Validation: Validation Set Predictions") +
#   theme_bw()