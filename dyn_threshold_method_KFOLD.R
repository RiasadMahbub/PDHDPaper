library(ggplot2)
library(MLmetrics) # Load the MLmetrics package for R2_Score
library(dplyr) # For %>% and other data manipulation functions
library(knitr) # For kable for Markdown table formatting

# Helper function to plot predictions vs. observations with metrics
plot_predictions_vs_observations <- function(y_pred, y_true, title_suffix, file_path, mae, rmse, r2) {
  plot_data <- data.frame(Observed = y_true, Predicted = y_pred)
  
  # Remove NA values for plotting
  plot_data <- plot_data[!is.na(plot_data$Observed) & !is.na(plot_data$Predicted), ]
  
  if (nrow(plot_data) < 1) { # No valid points to plot
    warning(paste("No valid data points to plot for:", title_suffix))
    return(NULL) # Return NULL if no plot is generated
  }
  
  metrics_label <- sprintf("MAE: %.2f\nRMSE: %.2f\nR2: %.2f", mae, rmse, r2)
  
  p <- ggplot(plot_data, aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # 1:1 line
    geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add linear regression line
    labs(
      title = paste(title_suffix, "Predictions vs. Observations"),
      x = "Observed DOY",
      y = "Predicted DOY"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = min(plot_data$Observed, na.rm = TRUE),
             y = max(plot_data$Predicted, na.rm = TRUE),
             hjust = -0.1, vjust = 1.1, # Adjust position to top-left corner
             label = metrics_label, size = 3, color = "blue")
  
  ggsave(filename = file_path, plot = p, width = 7, height = 6, units = "in", dpi = 300)
  return(p) # Return plot object in case it's needed for display in RStudio
}

run_vi_cv_test <- function(vi_list, vi_column, target_col, extract_func,
                           thresholds = seq(0.1, 1, by = 0.05), seed = 123,
                           base_plot_dir = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DTM/KFOLDPlots",
                           plot_specific_run = NULL) {
  set.seed(seed)
  n <- length(vi_list)
  
  # Create base plot directory if it doesn't exist
  dir.create(base_plot_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Step 1: Split into 20% test set, 80% for cross-validation pool
  # The test set is held out and evaluated only once at the end.
  test_idx <- sample(1:n, size = floor(0.2 * n))
  test_set <- vi_list[test_idx]
  cv_pool_set <- vi_list[-test_idx] # This is the 80% pool for train/validation CV
  
  # Initialize a dataframe to store validation results for each threshold
  validation_results <- data.frame(
    Threshold = thresholds,
    MAE_train_mean = NA, MAE_train_sd = NA,
    RMSE_train_mean = NA, RMSE_train_sd = NA,
    R2_train_mean = NA, R2_train_sd = NA,
    MAE_val_mean = NA, MAE_val_sd = NA,
    RMSE_val_mean = NA, RMSE_val_sd = NA,
    R2_val_mean = NA, R2_val_sd = NA,
    ErrorScore_mean = NA, ErrorScore_sd = NA, # Overall composite error (train + val)
    Val_Composite_Error_mean = NA, Val_Composite_Error_sd = NA, # Validation-only composite error
    Train_Composite_Error_mean = NA, Train_Composite_Error_sd = NA # Training-only composite error
  )
  
  # Initialize a dataframe to store test results for each threshold
  test_results_all_thresholds <- data.frame(
    Threshold = thresholds,
    MAE_test = NA,
    RMSE_test = NA,
    R2_test = NA,
    Composite_Error_Test = NA # Composite error for test set at each threshold
  )
  
  # Loop through each defined threshold
  for (th in thresholds) {
    # Matrix to store metrics for each of the 100 cross-validation iterations
    metric_matrix <- matrix(NA, nrow = 100, ncol = 9)
    colnames(metric_matrix) <- c("MAE_train", "RMSE_train", "R2_train",
                                 "MAE_val", "RMSE_val", "R2_val",
                                 "ErrorScore_iter", "Val_Composite_Error_iter", "Train_Composite_Error_iter")
    
    # Run 100 cross-validation iterations for the current threshold
    for (i in 1:100) {
      set.seed(seed + i)
      idx_pool <- sample(seq_along(cv_pool_set))
      split_point <- floor(0.75 * length(idx_pool))
      train_idx_iter <- idx_pool[1:split_point]
      val_idx_iter <- idx_pool[(split_point + 1):length(idx_pool)]
      
      current_train_set <- cv_pool_set[train_idx_iter]
      current_val_set <- cv_pool_set[val_idx_iter]
      
      # --- Training Predictions ---
      pred_train <- sapply(current_train_set, function(df)
        extract_func(df, vi_column, "Date", VIthd = th))
      true_train <- sapply(current_train_set, function(df)
        df[[target_col]][1])
      valid_train <- !is.na(pred_train) & !is.na(true_train)
      
      if (sum(valid_train) >= 3) {
        obs_train <- true_train[valid_train]
        pred_tr <- pred_train[valid_train]
        mae_train <- mean(abs(obs_train - pred_tr))
        rmse_train <- sqrt(mean((obs_train - pred_tr)^2))
        r2_train <- MLmetrics::R2_Score(y_pred = pred_tr, y_true = obs_train)
        
        # Conditional plotting for training set
        if (!is.null(plot_specific_run) && vi_column == plot_specific_run$vi && th == plot_specific_run$threshold && i == 1) {
          plot_filename <- file.path(base_plot_dir, paste0(vi_column, "_", target_col, "_Train_Th", sprintf("%.2f", th), "_Iter", i, ".jpeg"))
          plot_predictions_vs_observations(pred_tr, obs_train,
                                           paste0(vi_column, " Train (Th ", sprintf("%.2f", th), ", Iter ", i, ")"),
                                           plot_filename, mae_train, rmse_train, r2_train)
        }
      } else {
        mae_train <- rmse_train <- r2_train <- NA
      }
      
      # --- Validation Predictions ---
      pred_val <- sapply(current_val_set, function(df)
        extract_func(df, vi_column, "Date", VIthd = th))
      true_val <- sapply(current_val_set, function(df)
        df[[target_col]][1])
      valid_val <- !is.na(pred_val) & !is.na(true_val)
      
      if (sum(valid_val) >= 3) {
        obs_val <- true_val[valid_val]
        pred_v <- pred_val[valid_val]
        mae_val <- mean(abs(obs_val - pred_v))
        rmse_val <- sqrt(mean((obs_val - pred_v)^2))
        r2_val <- MLmetrics::R2_Score(y_pred = pred_v, y_true = obs_val)
        
        # Conditional plotting for validation set
        if (!is.null(plot_specific_run) && vi_column == plot_specific_run$vi && th == plot_specific_run$threshold && i == 1) {
          plot_filename <- file.path(base_plot_dir, paste0(vi_column, "_", target_col, "_Val_Th", sprintf("%.2f", th), "_Iter", i, ".jpeg"))
          plot_predictions_vs_observations(pred_v, obs_val,
                                           paste0(vi_column, " Val (Th ", sprintf("%.2f", th), ", Iter ", i, ")"),
                                           plot_filename, mae_val, rmse_val, r2_val)
        }
      } else {
        mae_val <- rmse_val <- r2_val <- NA
      }
      
      error_score_iter <- mean(
        c(mae_train, rmse_train, 1 - r2_train,
          mae_val, rmse_val, 1 - r2_val),
        na.rm = TRUE
      )
      
      val_composite_error_iter <- mean(
        c(mae_val, rmse_val, 1 - r2_val),
        na.rm = TRUE
      )
      
      train_composite_error_iter <- mean(
        c(mae_train, rmse_train, 1 - r2_train),
        na.rm = TRUE
      )
      
      metric_matrix[i, ] <- c(mae_train, rmse_train, r2_train, mae_val, rmse_val, r2_val,
                              error_score_iter, val_composite_error_iter, train_composite_error_iter)
    }
    
    train_means <- colMeans(metric_matrix[, 1:3], na.rm = TRUE)
    train_sds <- apply(metric_matrix[, 1:3], 2, sd, na.rm = TRUE)
    val_means <- colMeans(metric_matrix[, 4:6], na.rm = TRUE)
    val_sds <- apply(metric_matrix[, 4:6], 2, sd, na.rm = TRUE)
    error_score_mean <- mean(metric_matrix[, "ErrorScore_iter"], na.rm = TRUE)
    error_score_sd <- sd(metric_matrix[, "ErrorScore_iter"], na.rm = TRUE)
    val_composite_error_mean <- mean(metric_matrix[, "Val_Composite_Error_iter"], na.rm = TRUE)
    val_composite_error_sd <- sd(metric_matrix[, "Val_Composite_Error_iter"], na.rm = TRUE)
    train_composite_error_mean <- mean(metric_matrix[, "Train_Composite_Error_iter"], na.rm = TRUE)
    train_composite_error_sd <- sd(metric_matrix[, "Train_Composite_Error_iter"], na.rm = TRUE)
    
    validation_results[validation_results$Threshold == th, ] <- c(
      th,
      train_means["MAE_train"], train_sds["MAE_train"],
      train_means["RMSE_train"], train_sds["RMSE_train"],
      train_means["R2_train"], train_sds["R2_train"],
      val_means["MAE_val"], val_sds["MAE_val"],
      val_means["RMSE_val"], val_sds["RMSE_val"],
      val_means["R2_val"], val_sds["R2_val"],
      error_score_mean, error_score_sd,
      val_composite_error_mean, val_composite_error_sd,
      train_composite_error_mean, train_composite_error_sd
    )
    
    pred_test_th <- sapply(test_set, function(df)
      extract_func(df, vi_column, "Date", VIthd = th))
    true_test_th <- sapply(test_set, function(df)
      df[[target_col]][1])
    valid_test_th <- !is.na(pred_test_th) & !is.na(true_test_th)
    
    if (sum(valid_test_th) >= 3) {
      obs_test_th <- true_test_th[valid_test_th]
      pred_t_th <- pred_test_th[valid_test_th]
      mae_test_th <- mean(abs(obs_test_th - pred_t_th))
      rmse_test_th <- sqrt(mean((obs_test_th - pred_t_th)^2))
      r2_test_th <- MLmetrics::R2_Score(y_pred = pred_t_th, y_true = obs_test_th)
      composite_error_test_th <- mae_test_th + rmse_test_th + (1 - r2_test_th)
      
      # Conditional plotting for test set (for the specific threshold requested)
      if (!is.null(plot_specific_run) && vi_column == plot_specific_run$vi && th == plot_specific_run$threshold) {
        plot_filename <- file.path(base_plot_dir, paste0(vi_column, "_", target_col, "_Test_Th", sprintf("%.2f", th), ".jpeg"))
        plot_predictions_vs_observations(pred_t_th, obs_test_th,
                                         paste0(vi_column, " Test (Th ", sprintf("%.2f", th), ")"),
                                         plot_filename, mae_test_th, rmse_test_th, r2_test_th)
      }
    } else {
      mae_test_th <- rmse_test_th <- r2_test_th <- composite_error_test_th <- NA
    }
    test_results_all_thresholds[test_results_all_thresholds$Threshold == th, ] <- c(
      th, mae_test_th, rmse_test_th, r2_test_th, composite_error_test_th
    )
  } # Closing brace for the 'for (th in thresholds)' loop
  
  # Determine the best threshold based on the minimum mean overall composite error score
  best_row <- validation_results[which.min(validation_results$ErrorScore_mean), ]
  best_threshold <- best_row$Threshold

  pred_test_final <- sapply(test_set, function(df)
    extract_func(df, vi_column, "Date", VIthd = best_threshold))
  true_test_final <- sapply(test_set, function(df)
    df[[target_col]][1])
  valid_test_final <- !is.na(pred_test_final) & !is.na(true_test_final)
  
  if (sum(valid_test_final) >= 3) {
    obs_test_final <- true_test_final[valid_test_final]
    pred_t_final <- pred_test_final[valid_test_final]
    mae_test_final <- mean(abs(obs_test_final - pred_t_final))
    rmse_test_final <- sqrt(mean((obs_test_final - pred_t_final)^2))
    r2_test_final <- MLmetrics::R2_Score(y_pred = pred_t_final, y_true = obs_test_final)
  } else {
    mae_test_final <- rmse_test_final <- r2_test_final <- NA
  }
  
  test_metrics_summary <- data.frame(
    Threshold = best_threshold,
    MAE_test = mae_test_final,
    RMSE_test = rmse_test_final,
    R2_test = r2_test_final
  )
  
  return(list(
    validation_results = validation_results,
    test_metrics_summary = test_metrics_summary,
    test_results_all_thresholds = test_results_all_thresholds,
    best_threshold = best_threshold
  ))
}

extract_sos_wrapper <- function(df, vi_col, date_col, VIthd) {
  if (!vi_col %in% colnames(df) || nrow(df) < 2) return(NA)
  
  # Ensure VI column is numeric
  vi_values <- as.numeric(df[[vi_col]])
  # Remove NA values for calculation of min/max
  vi_values_clean <- vi_values[!is.na(vi_values)]
  
  if (length(vi_values_clean) < 2) return(NA)
  
  min_vi <- min(vi_values_clean, na.rm = TRUE)
  max_vi <- max(vi_values_clean, na.rm = TRUE)
  
  # Calculate the threshold value based on the VI range and VIthd
  dynamic_threshold <- min_vi + (max_vi - min_vi) * VIthd
  cross_idx <- which(vi_values > dynamic_threshold)
  
  if (length(cross_idx) > 0) {
    # Get the date of the first crossing
    sos_date <- df[[date_col]][min(cross_idx)]
    # Convert date to Day of Year (DOY)
    sos_doy <- as.numeric(format(sos_date, "%j"))
    # Add some random noise to simulate real-world variability
    return(sos_doy )
  } else {
    return(NA) # No crossing found
  }
}

# It finds the last day the VI stays above a percentage of its range (or first day it drops below).
extract_eos_wrapper <- function(df, vi_col, date_col, VIthd) {
  if (!vi_col %in% colnames(df) || nrow(df) < 2) return(NA)
  
  # Ensure VI column is numeric
  vi_values <- as.numeric(df[[vi_col]])
  # Remove NA values for calculation of min/max
  vi_values_clean <- vi_values[!is.na(vi_values)]
  
  if (length(vi_values_clean) < 2) return(NA)
  
  min_vi <- min(vi_values_clean, na.rm = TRUE)
  max_vi <- max(vi_values_clean, na.rm = TRUE)

  dynamic_threshold <- min_vi + (max_vi - min_vi) * (1 - VIthd)

  cross_idx <- which(vi_values > dynamic_threshold)
  if (length(cross_idx) > 0) {
    # Get the date of the last point above the threshold
    eos_date <- df[[date_col]][max(cross_idx)]
    # Convert date to Day of Year (DOY)
    eos_doy <- as.numeric(format(eos_date, "%j"))
    # Add some random noise
    return(eos_doy )
  } else {
    return(NA) # No such point found
  }
}


# ==== Run SOS performance tests for all VIs ====
sos_results <- list(
  NDVI = run_vi_cv_test(vi_list_gt20, "NDVI", "PDDOY", extract_sos_wrapper),
  kNDVI = run_vi_cv_test(vi_list_gt20, "kNDVI", "PDDOY", extract_sos_wrapper),
  EVI = run_vi_cv_test(vi_list_gt20, "EVI", "PDDOY", extract_sos_wrapper),
  ATSAVI = run_vi_cv_test(vi_list_gt20, "ATSAVI", "PDDOY", extract_sos_wrapper),
  GDVI = run_vi_cv_test(vi_list_gt20, "GDVI", "PDDOY", extract_sos_wrapper)
)

# ==== Run EOS performance tests for all VIs ====
eos_results <- list(
  NDVI = run_vi_cv_test(vi_list_eos, "NDVI", "HDDOY", extract_eos_wrapper),
  kNDVI = run_vi_cv_test(vi_list_eos, "kNDVI", "HDDOY", extract_eos_wrapper),
  EVI = run_vi_cv_test(vi_list_eos, "EVI", "HDDOY", extract_eos_wrapper),
  ATSAVI = run_vi_cv_test(vi_list_eos, "ATSAVI", "HDDOY", extract_eos_wrapper),
  GDVI = run_vi_cv_test(vi_list_eos, "GDVI", "HDDOY", extract_eos_wrapper)
)

# --- Print Example Results ---
cat("SOS NDVI Validation Results:\n")
print(sos_results$NDVI$validation_results)
cat("\nSOS NDVI Test Metrics:\n")
print(sos_results$NDVI$test_metrics)

cat("\nEOS GDVI Validation Results:\n")
print(eos_results$GDVI$validation_results)
cat("\nEOS GDVI Test Metrics:\n")
print(eos_results$GDVI$test_metrics)

# Helper function to prepare data for plotting
prepare_plot_dataframe <- function(results_list, metric_type = "overall_composite") {
  plot_df_list <- lapply(names(results_list), function(vi_name) {
    df <- results_list[[vi_name]]$validation_results
    df$VI <- vi_name
    
    # Construct column names based on the metric_type
    if (metric_type == "overall_composite") {
      mean_col_name <- "ErrorScore_mean"
      sd_col_name <- "ErrorScore_sd"
      y_axis_label <- "Overall Composite Error Score Mean (MAE + RMSE + (1 - R²))"
    } else if (metric_type == "validation_composite") {
      mean_col_name <- "Val_Composite_Error_mean"
      sd_col_name <- "Val_Composite_Error_sd"
      y_axis_label <- "Validation Composite Error Score Mean (MAE + RMSE + (1 - R²))"
    } else if (metric_type == "training_composite") { # New metric type for training composite
      mean_col_name <- "Train_Composite_Error_mean"
      sd_col_name <- "Train_Composite_Error_sd"
      y_axis_label <- "Training Composite Error Score Mean (MAE + RMSE + (1 - R²))"
    } else if (metric_type == "MAE_val") {
      mean_col_name <- "MAE_val_mean"
      sd_col_name <- "MAE_val_sd"
      y_axis_label <- "Mean Absolute Error (MAE)"
    } else {
      stop(paste("Invalid metric_type specified:", metric_type))
    }
    # Check if columns exist before selecting
    if (!all(c(mean_col_name, sd_col_name) %in% colnames(df))) {
      stop(paste("Metric columns not found in validation_results for metric_type", metric_type, ":", mean_col_name, sd_col_name))
    }
    # Select the necessary columns for plotting
    selected_df <- df[, c("Threshold", mean_col_name, sd_col_name, "VI")]
    # Rename the metric columns to a generic name for consistent plotting
    colnames(selected_df)[colnames(selected_df) == mean_col_name] <- "Metric_mean"
    colnames(selected_df)[colnames(selected_df) == sd_col_name] <- "Metric_sd"
    selected_df$Y_Axis_Label <- y_axis_label # Store label for plot title/axis
    
    return(selected_df)
  })
  plot_df <- do.call(rbind, plot_df_list)
  plot_df$Metric_mean <- as.numeric(plot_df$Metric_mean)
  plot_df$Metric_sd <- as.numeric(plot_df$Metric_sd)
  return(plot_df)
}

# Helper function to prepare test set data for plotting
prepare_test_plot_dataframe <- function(results_list) {
  test_plot_df_list <- lapply(names(results_list), function(vi_name) {
    df <- results_list[[vi_name]]$test_metrics
    df$VI <- vi_name
    # Calculate composite error score for the test set (MAE + RMSE + (1 - R2))
    # Note: Test metrics are for the single best threshold, so no SD across thresholds.
    df$Composite_Error_Test <- df$MAE_test + df$RMSE_test + (1 - df$R2_test)
    return(df[, c("VI", "MAE_test", "Composite_Error_Test")]) # Include MAE_test
  })
  test_plot_df <- do.call(rbind, test_plot_df_list)
  return(test_plot_df)
}

# ==== Plot overall composite error scores for all SOS VIs ====
plot_df_sos_error_score <- prepare_plot_dataframe(sos_results, "overall_composite")

# Generate the ggplot for SOS overall composite error scores with standard deviation error bars.
ggplot(plot_df_sos_error_score, aes(x = Threshold, y = Metric_mean, color = VI)) +
  geom_line(size = 1) + # Line connecting the mean error scores
  geom_point() +       # Points for each threshold
  # Add error bars representing the standard deviation of the composite error score.
  geom_errorbar(aes(ymin = Metric_mean - Metric_sd,
                    ymax = Metric_mean + Metric_sd),
                width = 0.02, # Width of the error bar caps
                position = position_dodge(0.01)) + # Slightly dodge to prevent overlap
  ggtitle("SOS VIs - Overall Composite Error Score Across Thresholds (with SD)") + # Plot title
  ylab(unique(plot_df_sos_error_score$Y_Axis_Label)) + # Y-axis label
  xlab("Threshold") + # X-axis label
  theme_minimal() + # Minimal theme for a clean look
  theme(plot.title = element_text(hjust = 0.5)) # Center the plot title

# ==== Plot overall composite error scores for all EOS VIs ====
plot_df_eos_error_score <- prepare_plot_dataframe(eos_results, "overall_composite")

# Generate the ggplot for EOS overall composite error scores with standard deviation error bars.
ggplot(plot_df_eos_error_score, aes(x = Threshold, y = Metric_mean, color = VI)) +
  geom_line(size = 1) + # Line connecting the mean error scores
  geom_point() +       # Points for each threshold
  # Add error bars representing the standard deviation of the composite error score.
  geom_errorbar(aes(ymin = Metric_mean - Metric_sd,
                    ymax = Metric_mean + Metric_sd),
                width = 0.02, # Width of the error bar caps
                position = position_dodge(0.01)) + # Slightly dodge to prevent overlap
  ggtitle("EOS VIs - Overall Composite Error Score Across Thresholds (with SD)") + # Plot title
  ylab(unique(plot_df_eos_error_score$Y_Axis_Label)) + # Y-axis label
  xlab("Threshold") + # X-axis label
  theme_minimal() + # Minimal theme for a clean look
  theme(plot.title = element_text(hjust = 0.5)) # Center the plot title


# ==== Plot MAE for all SOS VIs ====
plot_df_sos_mae <- prepare_plot_dataframe(sos_results, "MAE_val")

# Generate the ggplot for SOS MAE with standard deviation error bars.
ggplot(plot_df_sos_mae, aes(x = Threshold, y = Metric_mean, color = VI)) +
  geom_line(size = 1) + # Line connecting the mean MAE values
  geom_point() +       # Points for each threshold
  # Add error bars representing the standard deviation of MAE.
  geom_errorbar(aes(ymin = Metric_mean - Metric_sd,
                    ymax = Metric_mean + Metric_sd),
                width = 0.02, # Width of the error bar caps
                position = position_dodge(0.01)) + # Slightly dodge to prevent overlap
  ggtitle("SOS VIs - Mean Absolute Error (MAE) Across Thresholds (with SD)") + # Plot title
  ylab(unique(plot_df_sos_mae$Y_Axis_Label)) + # Y-axis label
  xlab("Threshold") + # X-axis label
  theme_minimal() + # Minimal theme for a clean look
  theme(plot.title = element_text(hjust = 0.5)) # Center the plot title

# ==== Plot MAE for all EOS VIs ====
plot_df_eos_mae <- prepare_plot_dataframe(eos_results, "MAE_val")

# Generate the ggplot for EOS MAE with standard deviation error bars.
ggplot(plot_df_eos_mae, aes(x = Threshold, y = Metric_mean, color = VI)) +
  geom_line(size = 1) + # Line connecting the mean MAE values
  geom_point() +       # Points for each threshold
  # Add error bars representing the standard deviation of MAE.
  geom_errorbar(aes(ymin = Metric_mean - Metric_sd,
                    ymax = Metric_mean + Metric_sd),
                width = 0.02, # Width of the error bar caps
                position = position_dodge(0.01)) + # Slightly dodge to prevent overlap
  ggtitle("EOS VIs - Mean Absolute Error (MAE) Across Thresholds (with SD)") + # Plot title
  ylab(unique(plot_df_eos_mae$Y_Axis_Label)) + # Y-axis label
  xlab("Threshold") + # X-axis label
  theme_minimal() + # Minimal theme for a clean look
  theme(plot.title = element_text(hjust = 0.5)) # Center the plot title

# ==== Plot Validation Composite Error Scores for all SOS VIs ====
plot_df_sos_val_composite <- prepare_plot_dataframe(sos_results, "validation_composite")
ggplot(plot_df_sos_val_composite, aes(x = Threshold, y = Metric_mean, color = VI)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = Metric_mean - Metric_sd, ymax = Metric_mean + Metric_sd),
                width = 0.02, position = position_dodge(0.01)) +
  ggtitle("SOS VIs - Validation Composite Error Score Across Thresholds (with SD)") +
  ylab(unique(plot_df_sos_val_composite$Y_Axis_Label)) +
  xlab("Threshold") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ==== Plot Validation Composite Error Scores for all EOS VIs ====
plot_df_eos_val_composite <- prepare_plot_dataframe(eos_results, "validation_composite")
ggplot(plot_df_eos_val_composite, aes(x = Threshold, y = Metric_mean, color = VI)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = Metric_mean - Metric_sd, ymax = Metric_mean + Metric_sd),
                width = 0.02, position = position_dodge(0.01)) +
  ggtitle("EOS VIs - Validation Composite Error Score Across Thresholds (with SD)") +
  ylab(unique(plot_df_eos_val_composite$Y_Axis_Label)) +
  xlab("Threshold") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ==== Plot Training Composite Error Scores for all SOS VIs ====
plot_df_sos_train_composite <- prepare_plot_dataframe(sos_results, "training_composite")
ggplot(plot_df_sos_train_composite, aes(x = Threshold, y = Metric_mean, color = VI)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = Metric_mean - Metric_sd, ymax = Metric_mean + Metric_sd),
                width = 0.02, position = position_dodge(0.01)) +
  ggtitle("SOS VIs - Training Composite Error Score Across Thresholds (with SD)") +
  ylab(unique(plot_df_sos_train_composite$Y_Axis_Label)) +
  xlab("Threshold") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ==== Plot Training Composite Error Scores for all EOS VIs ====
plot_df_eos_train_composite <- prepare_plot_dataframe(eos_results, "training_composite")
ggplot(plot_df_eos_train_composite, aes(x = Threshold, y = Metric_mean, color = VI)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = Metric_mean - Metric_sd, ymax = Metric_mean + Metric_sd),
                width = 0.02, position = position_dodge(0.01)) +
  ggtitle("EOS VIs - Training Composite Error Score Across Thresholds (with SD)") +
  ylab(unique(plot_df_eos_train_composite$Y_Axis_Label)) +
  xlab("Threshold") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ==== Plot Test Set Composite Error Scores for SOS VIs ====
plot_df_sos_test_composite <- prepare_test_plot_dataframe(sos_results)
ggplot(plot_df_sos_test_composite, aes(x = VI, y = Composite_Error_Test, fill = VI)) +
  geom_col(position = "dodge") + # Bar chart
  ggtitle("SOS VIs - Test Set Composite Error Score (at Best Threshold)") +
  ylab("Composite Error Score (MAE + RMSE + (1 - R²))") +
  xlab("Vegetation Index (VI)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ==== Plot Test Set Composite Error Scores for EOS VIs ====
plot_df_eos_test_composite <- prepare_test_plot_dataframe(eos_results)
ggplot(plot_df_eos_test_composite, aes(x = VI, y = Composite_Error_Test, fill = VI)) +
  geom_col(position = "dodge") + # Bar chart
  ggtitle("EOS VIs - Test Set Composite Error Score (at Best Threshold)") +
  ylab("Composite Error Score (MAE + RMSE + (1 - R²))") +
  xlab("Vegetation Index (VI)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ==== Scatter Plot: Test Set MAE vs. Composite Error for SOS VIs ====
ggplot(plot_df_sos_test_composite, aes(x = MAE_test, y = Composite_Error_Test, color = VI)) +
  geom_point(size = 3) + # Scatter points
  geom_text(aes(label = VI), vjust = -1, hjust = 0.5, show.legend = FALSE) + # Label points with VI name
  ggtitle("SOS VIs - Test Set MAE vs. Composite Error (at Best Threshold)") +
  ylab("Composite Error Score (MAE + RMSE + (1 - R²))") +
  xlab("Mean Absolute Error (MAE)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ==== Scatter Plot: Test Set MAE vs. Composite Error for EOS VIs ====
ggplot(plot_df_eos_test_composite, aes(x = MAE_test, y = Composite_Error_Test, color = VI)) +
  geom_point(size = 3) + # Scatter points
  geom_text(aes(label = VI), vjust = -1, hjust = 0.5, show.legend = FALSE) + # Label points with VI name
  ggtitle("EOS VIs - Test Set MAE vs. Composite Error (at Best Threshold)") +
  ylab("Composite Error Score (MAE + RMSE + (1 - R²))") +
  xlab("Mean Absolute Error (MAE)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Helper function to prepare data for the summary table
prepare_summary_table <- function(results_list) {
  summary_data <- lapply(names(results_list), function(vi_name) {
    res <- results_list[[vi_name]]
    
    # Get the best threshold
    best_th <- res$best_threshold
    
    # Initialize a default empty row in case no valid best_val_row is found
    default_val_row <- data.frame(
      MAE_train_mean = NA, MAE_train_sd = NA,
      RMSE_train_mean = NA, RMSE_train_sd = NA,
      R2_train_mean = NA, R2_train_sd = NA,
      MAE_val_mean = NA, MAE_val_sd = NA,
      RMSE_val_mean = NA, RMSE_val_sd = NA,
      R2_val_mean = NA, R2_val_sd = NA
    )
    
    # Attempt to get the row corresponding to the best threshold from validation_results
    best_val_row <- res$validation_results %>%
      dplyr::filter(Threshold == best_th) %>%
      dplyr::select( # Explicitly use dplyr::select
        MAE_train_mean, MAE_train_sd,
        RMSE_train_mean, RMSE_train_sd,
        R2_train_mean, R2_train_sd,
        MAE_val_mean, MAE_val_sd,
        RMSE_val_mean, RMSE_val_sd,
        R2_val_mean, R2_val_sd
      )
    
    # If best_val_row is empty (0 rows), use the default_val_row
    if (nrow(best_val_row) == 0) {
      best_val_row <- default_val_row
      # If best_th was numeric(0), set it to NA for display in the table
      if (length(best_th) == 0) best_th <- NA
    }
    
    # Initialize a default test metrics row
    default_test_metrics <- data.frame(
      MAE_test = NA,
      RMSE_test = NA,
      R2_test = NA
    )
    
    # Get test metrics (prioritize test_metrics_summary, then fallback to test_metrics)
    test_metrics <- NULL
    if (!is.null(res$test_metrics_summary) && is.data.frame(res$test_metrics_summary) && nrow(res$test_metrics_summary) > 0) {
      test_metrics <- res$test_metrics_summary
    } else if (!is.null(res$test_metrics) && is.data.frame(res$test_metrics) && nrow(res$test_metrics) > 0) {
      # Fallback to test_metrics if test_metrics_summary is not available or empty
      test_metrics <- res$test_metrics
    }
    
    # If after all checks, test_metrics is still not a valid data.frame, use the default
    if (is.null(test_metrics) || !is.data.frame(test_metrics) || nrow(test_metrics) == 0) {
      test_metrics <- default_test_metrics
    }
    
    # Format the mean and SD for training and validation
    format_metric <- function(mean_val, sd_val) {
      if (is.na(mean_val) || is.na(sd_val)) {
        return(NA_character_)
      }
      return(sprintf("%.2f (%.2f)", mean_val, sd_val))
    }
    
    data.frame(
      VI = vi_name,
      Best_Threshold = sprintf("%.2f", best_th),
      MAE_train = format_metric(best_val_row$MAE_train_mean, best_val_row$MAE_train_sd),
      RMSE_train = format_metric(best_val_row$RMSE_train_mean, best_val_row$RMSE_train_sd),
      R2_train = format_metric(best_val_row$R2_train_mean, best_val_row$R2_train_sd),
      MAE_val = format_metric(best_val_row$MAE_val_mean, best_val_row$MAE_val_sd),
      RMSE_val = format_metric(best_val_row$RMSE_val_mean, best_val_row$RMSE_val_sd),
      R2_val = format_metric(best_val_row$R2_val_mean, best_val_row$R2_val_sd),
      MAE_test = sprintf("%.2f", test_metrics$MAE_test),
      RMSE_test = sprintf("%.2f", test_metrics$RMSE_test),
      R2_test = sprintf("%.2f", test_metrics$R2_test),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, summary_data)
}

# Generate and print the SOS summary table
sos_summary_table <- prepare_summary_table(sos_results)
cat("## Summary Table for SOS VIs Performance\n\n")
print(knitr::kable(sos_summary_table, 
                   col.names = c("VI", "Best Threshold", "MAE_train_mean (SD)", "RMSE_train_mean (SD)", "R2_train_mean (SD)",
                                 "MAE_val_mean (SD)", "RMSE_val_mean (SD)", "R2_val_mean (SD)",
                                 "MAE_test", "RMSE_test", "R2_test")))
cat("\n\n")

# Generate and print the EOS summary table
eos_summary_table <- prepare_summary_table(eos_results)
cat("## Summary Table for EOS VIs Performance\n\n")
print(knitr::kable(eos_summary_table, 
                   col.names = c("VI", "Best Threshold", "MAE_train_mean (SD)", "RMSE_train_mean (SD)", "R2_train_mean (SD)",
                                 "MAE_val_mean (SD)", "RMSE_val_mean (SD)", "R2_val_mean (SD)",
                                 "MAE_test", "RMSE_test", "R2_test")))


sos_results

# Example call for SOS ATSAVI at threshold 0.50
sos_results_atsavi_plot <- run_vi_cv_test(
  vi_list = vi_list_gt20,
  vi_column = "ATSAVI",
  target_col = "PDDOY",
  extract_func = extract_sos_wrapper,
  thresholds = seq(0.1, 1, by = 0.05), # Keep all thresholds for internal calculations
  seed = 123,
  base_plot_dir = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DTM/KFOLDPlots",
  plot_specific_run = list(vi = "ATSAVI", threshold = 0.50)
)

# Example call for EOS ATSAVI at threshold 0.50
eos_results_atsavi_plot <- run_vi_cv_test(
  vi_list = vi_list_eos,
  vi_column = "ATSAVI",
  target_col = "HDDOY",
  extract_func = extract_eos_wrapper,
  thresholds = seq(0.1, 1, by = 0.05), # Keep all thresholds for internal calculations
  seed = 123,
  base_plot_dir = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DTM/KFOLDPlots",
  plot_specific_run = list(vi = "ATSAVI", threshold = 0.50)
)

#====================================================================
# Helper function to prepare data for combined plots (Training, Validation, Test)
prepare_combined_plot_dataframe <- function(results_list, metric_name) {
  combined_df_list <- lapply(names(results_list), function(vi_name) {
    val_df <- results_list[[vi_name]]$validation_results
    test_df <- results_list[[vi_name]]$test_results_all_thresholds
    
    # Extract training and validation means/sds
    train_data <- data.frame(
      Threshold = val_df$Threshold,
      VI = vi_name,
      Set_Type = "Training",
      Metric_Value_Mean = val_df[[paste0(metric_name, "_train_mean")]],
      Metric_Value_SD = val_df[[paste0(metric_name, "_train_sd")]]
    )
    
    val_data <- data.frame(
      Threshold = val_df$Threshold,
      VI = vi_name,
      Set_Type = "Validation",
      Metric_Value_Mean = val_df[[paste0(metric_name, "_val_mean")]],
      Metric_Value_SD = val_df[[paste0(metric_name, "_val_sd")]]
    )
    
    # Extract test data. Note: test_results_all_thresholds already has metrics per threshold
    test_metric_col <- paste0(metric_name, "_test")
    if (metric_name == "Composite_Error") { # Special handling for composite error column name
      test_metric_col <- "Composite_Error_Test"
    }
    
    test_data <- data.frame(
      Threshold = test_df$Threshold,
      VI = vi_name,
      Set_Type = "Test",
      Metric_Value_Mean = test_df[[test_metric_col]],
      Metric_Value_SD = NA # No SD for test set in this context
    )
    
    # Combine and return
    rbind(train_data, val_data, test_data)
  })
  do.call(rbind, combined_df_list)
}

# Define mapping for y-axis labels
metric_y_labels <- list(
  MAE = "Mean Absolute Error (MAE)",
  RMSE = "Root Mean Squared Error (RMSE)",
  R2 = "Coefficient of Determination (R²)",
  Composite_Error = "Composite Error Score (MAE + RMSE + (1 - R²))"
)

# Plotting function for combined plots (Training, Validation, Test)
plot_combined_metrics <- function(data_df, metric_type, pheno_event, base_plot_dir) {
  y_label <- metric_y_labels[[metric_type]]
  if (is.null(y_label)) {
    y_label <- paste(metric_type, "Metric Value") # Fallback
  }
  
  plot_title <- paste0(pheno_event, " VIs - ", y_label, " Across Thresholds")
  file_name <- file.path(base_plot_dir, paste0(pheno_event, "_AllVIs_", metric_type, "_Combined.jpeg"))
  
  p <- ggplot(data_df, aes(x = Threshold, y = Metric_Value_Mean, color = VI, group = interaction(VI, Set_Type))) +
    # Lines for Training and Validation
    geom_line(aes(linetype = Set_Type), data = . %>% dplyr::filter(Set_Type %in% c("Training", "Validation")), size = 1) +
    # Points for Test
    geom_point(aes(shape = Set_Type), data = . %>% dplyr::filter(Set_Type == "Test"), size = 3) +
    # Error bars for Training and Validation
    geom_errorbar(aes(ymin = Metric_Value_Mean - Metric_Value_SD, ymax = Metric_Value_Mean + Metric_Value_SD),
                  data = . %>% dplyr::filter(Set_Type %in% c("Training", "Validation")),
                  width = 0.02, position = position_dodge(0.01)) +
    scale_linetype_manual(values = c("Training" = "solid", "Validation" = "dashed")) +
    scale_shape_manual(values = c("Test" = 16)) + # Solid circle for test
    labs(
      title = plot_title,
      x = "Threshold",
      y = y_label,
      color = "Vegetation Index",
      linetype = "Set Type",
      shape = "Set Type"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(filename = file_name, plot = p, width = 10, height = 7, units = "in", dpi = 300)
  return(p)
}


# Define the base plot directory
base_plot_dir_path <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DTM/KFOLDPlots"
dir.create(base_plot_dir_path, recursive = TRUE, showWarnings = FALSE) # Ensure directory exists

# ==== Generate and save combined plots for SOS ====
# MAE
plot_combined_metrics(prepare_combined_plot_dataframe(sos_results, "MAE"), "MAE", "SOS", base_plot_dir_path)
# RMSE
plot_combined_metrics(prepare_combined_plot_dataframe(sos_results, "RMSE"), "RMSE", "SOS", base_plot_dir_path)
# R2
plot_combined_metrics(prepare_combined_plot_dataframe(sos_results, "R2"), "R2", "SOS", base_plot_dir_path)
# Composite Error
plot_combined_metrics(prepare_combined_plot_dataframe(sos_results, "Composite_Error"), "Composite_Error", "SOS", base_plot_dir_path)


# ==== Generate and save combined plots for EOS ====
# MAE
plot_combined_metrics(prepare_combined_plot_dataframe(eos_results, "MAE"), "MAE", "EOS", base_plot_dir_path)
# RMSE
plot_combined_metrics(prepare_combined_plot_dataframe(eos_results, "RMSE"), "RMSE", "EOS", base_plot_dir_path)
# R2
plot_combined_metrics(prepare_combined_plot_dataframe(eos_results, "R2"), "R2", "EOS", base_plot_dir_path)
# Composite Error
plot_combined_metrics(prepare_combined_plot_dataframe(eos_results, "Composite_Error"), "Composite_Error", "EOS", base_plot_dir_path)


# Helper function to prepare data for the summary table
prepare_summary_table <- function(results_list) {
  summary_data <- lapply(names(results_list), function(vi_name) {
    res <- results_list[[vi_name]]
    
    # Get the best threshold
    best_th <- res$best_threshold
    
    # Initialize a default empty row in case no valid best_val_row is found
    default_val_row <- data.frame(
      MAE_train_mean = NA, MAE_train_sd = NA,
      RMSE_train_mean = NA, RMSE_train_sd = NA,
      R2_train_mean = NA, R2_train_sd = NA,
      MAE_val_mean = NA, MAE_val_sd = NA,
      RMSE_val_mean = NA, RMSE_val_sd = NA,
      R2_val_mean = NA, R2_val_sd = NA
    )
    
    # Attempt to get the row corresponding to the best threshold from validation_results
    best_val_row <- res$validation_results %>%
      dplyr::dplyr::filter(Threshold == best_th) %>%
      dplyr::select( # Explicitly use dplyr::select
        MAE_train_mean, MAE_train_sd,
        RMSE_train_mean, RMSE_train_sd,
        R2_train_mean, R2_train_sd,
        MAE_val_mean, MAE_val_sd,
        RMSE_val_mean, RMSE_val_sd,
        R2_val_mean, R2_val_sd
      )
    
    # If best_val_row is empty (0 rows), use the default_val_row
    if (nrow(best_val_row) == 0) {
      best_val_row <- default_val_row
      # If best_th was numeric(0), set it to NA for display in the table
      if (length(best_th) == 0) best_th <- NA
    }
    
    # Initialize a default test metrics row
    default_test_metrics <- data.frame(
      MAE_test = NA,
      RMSE_test = NA,
      R2_test = NA
    )
    
    # Get test metrics (prioritize test_metrics_summary, then fallback to test_metrics)
    test_metrics <- NULL
    if (!is.null(res$test_metrics_summary) && is.data.frame(res$test_metrics_summary) && nrow(res$test_metrics_summary) > 0) {
      test_metrics <- res$test_metrics_summary
    } else if (!is.null(res$test_metrics) && is.data.frame(res$test_metrics) && nrow(res$test_metrics) > 0) {
      # Fallback to test_metrics if test_metrics_summary is not available or empty
      test_metrics <- res$test_metrics
    }
    
    # If after all checks, test_metrics is still not a valid data.frame, use the default
    if (is.null(test_metrics) || !is.data.frame(test_metrics) || nrow(test_metrics) == 0) {
      test_metrics <- default_test_metrics
    }
    
    # Format the mean and SD for training and validation
    format_metric <- function(mean_val, sd_val) {
      if (is.na(mean_val) || is.na(sd_val)) {
        return(NA_character_)
      }
      return(sprintf("%.2f (%.2f)", mean_val, sd_val))
    }
    
    data.frame(
      VI = vi_name,
      Best_Threshold = sprintf("%.2f", best_th),
      MAE_train = format_metric(best_val_row$MAE_train_mean, best_val_row$MAE_train_sd),
      RMSE_train = format_metric(best_val_row$RMSE_train_mean, best_val_row$RMSE_train_sd),
      R2_train = format_metric(best_val_row$R2_train_mean, best_val_row$R2_train_sd),
      MAE_val = format_metric(best_val_row$MAE_val_mean, best_val_row$MAE_val_sd),
      RMSE_val = format_metric(best_val_row$RMSE_val_mean, best_val_row$RMSE_val_sd),
      R2_val = format_metric(best_val_row$R2_val_mean, best_val_row$R2_val_sd),
      MAE_test = sprintf("%.2f", test_metrics$MAE_test),
      RMSE_test = sprintf("%.2f", test_metrics$RMSE_test),
      R2_test = sprintf("%.2f", test_metrics$R2_test),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, summary_data)
}

# Generate and print the SOS summary table
sos_summary_table <- prepare_summary_table(sos_results)
cat("## Summary Table for SOS VIs Performance\n\n")
print(knitr::kable(sos_summary_table,
                   col.names = c("VI", "Best Threshold", "MAE_train_mean (SD)", "RMSE_train_mean (SD)", "R2_train_mean (SD)",
                                 "MAE_val_mean (SD)", "RMSE_val_mean (SD)", "R2_val_mean (SD)",
                                 "MAE_test", "RMSE_test", "R2_test")))
cat("\n\n")

# Generate and print the EOS summary table
eos_summary_table <- prepare_summary_table(eos_results)
cat("## Summary Table for EOS VIs Performance\n\n")
print(knitr::kable(eos_summary_table,
                   col.names = c("VI", "Best Threshold", "MAE_train_mean (SD)", "RMSE_train_mean (SD)", "R2_train_mean (SD)",
                                 "MAE_val_mean (SD)", "RMSE_val_mean (SD)", "R2_val_mean (SD)",
                                 "MAE_test", "RMSE_test", "R2_test")))


# Example call for SOS ATSAVI at threshold 0.50 (for individual scatter plots)
# This will generate the specific plots for ATSAVI at threshold 0.50 as JPEGs.
sos_results_atsavi_plot <- run_vi_cv_test(
  vi_list = vi_list_gt20,
  vi_column = "ATSAVI",
  target_col = "PDDOY",
  extract_func = extract_sos_wrapper,
  thresholds = seq(0.1, 1, by = 0.05), # Keep all thresholds for internal calculations
  seed = 123,
  base_plot_dir = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DTM/KFOLDPlots",
  plot_specific_run = list(vi = "ATSAVI", threshold = 0.50)
)

# Example call for EOS ATSAVI at threshold 0.50 (for individual scatter plots)
# This will generate the specific plots for ATSAVI at threshold 0.50 as JPEGs.
eos_results_atsavi_plot <- run_vi_cv_test(
  vi_list = vi_list_eos,
  vi_column = "ATSAVI",
  target_col = "HDDOY",
  extract_func = extract_eos_wrapper,
  thresholds = seq(0.1, 1, by = 0.05), # Keep all thresholds for internal calculations
  seed = 123,
  base_plot_dir = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DTM/KFOLDPlots",
  plot_specific_run = list(vi = "ATSAVI", threshold = 0.50)
)

best_sos_model <- sos_summary_table[which.min(sos_summary_table$`MAE_val_mean (SD)`), ]
print(knitr::kable(best_sos_model))

# Extract numeric MAE_val from the "MAE_val" column
mae_val_numeric <- as.numeric(sub(" .*", "", sos_summary_table$MAE_val))
# Find the row with the minimum MAE_val
best_sos_model <- sos_summary_table[which.min(mae_val_numeric), ]
best_sos_model

# Extract numeric MAE_val from the "MAE_val" column
mae_val_numeric_eos <- as.numeric(sub(" .*", "", eos_summary_table$MAE_val))
# Find the row with the minimum MAE_val
best_eos_model <- eos_summary_table[which.min(mae_val_numeric_eos), ]
# Print the result
knitr::kable(best_eos_model)
best_eos_model
