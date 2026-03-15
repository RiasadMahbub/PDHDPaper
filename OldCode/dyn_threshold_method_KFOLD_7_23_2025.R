library(ggplot2)

run_vi_cv_test <- function(vi_list, vi_column, target_col, extract_func,
                           thresholds = seq(0.1, 1, by = 0.05), seed = 123) {
  set.seed(seed)
  n <- length(vi_list)
  
  # Split into 80% trainval, 20% test
  test_idx <- sample(1:n, size = floor(0.2 * n))
  test_set <- vi_list[test_idx]
  trainval_set <- vi_list[-test_idx]
  
  validation_results <- data.frame(
    Threshold = thresholds,
    MAE_train_mean = NA, MAE_train_sd = NA,
    RMSE_train_mean = NA, RMSE_train_sd = NA,
    R2_train_mean = NA, R2_train_sd = NA,
    MAE_val_mean = NA, MAE_val_sd = NA,
    RMSE_val_mean = NA, RMSE_val_sd = NA,
    R2_val_mean = NA, R2_val_sd = NA,
    ErrorScore = NA
  )
  
  for (th in thresholds) {
    metric_matrix <- matrix(NA, nrow = 100, ncol = 6)
    colnames(metric_matrix) <- c("MAE_train", "RMSE_train", "R2_train", 
                                 "MAE_val", "RMSE_val", "R2_val")
    
    for (i in 1:100) {
      set.seed(seed + i)
      idx_pool <- sample(seq_along(trainval_set))
      split_point <- floor(0.75 * length(idx_pool))
      train_idx <- idx_pool[1:split_point]
      val_idx <- idx_pool[(split_point + 1):length(idx_pool)]
      
      # Training predictions
      pred_train <- sapply(trainval_set[train_idx], function(df)
        extract_func(df, vi_column, "Date", VIthd = th))
      true_train <- sapply(trainval_set[train_idx], function(df)
        df[[target_col]][1])
      valid_train <- !is.na(pred_train) & !is.na(true_train)
      if (sum(valid_train) >= 3) {
        obs_train <- true_train[valid_train]
        pred_tr <- pred_train[valid_train]
        mae_train <- mean(abs(obs_train - pred_tr))
        rmse_train <- sqrt(mean((obs_train - pred_tr)^2))
        r2_train <- 1 - sum((obs_train - pred_tr)^2) / sum((obs_train - mean(obs_train))^2)
      } else {
        mae_train <- rmse_train <- r2_train <- NA
      }
      
      # Validation predictions
      pred_val <- sapply(trainval_set[val_idx], function(df)
        extract_func(df, vi_column, "Date", VIthd = th))
      true_val <- sapply(trainval_set[val_idx], function(df)
        df[[target_col]][1])
      valid_val <- !is.na(pred_val) & !is.na(true_val)
      if (sum(valid_val) >= 3) {
        obs_val <- true_val[valid_val]
        pred_v <- pred_val[valid_val]
        mae_val <- mean(abs(obs_val - pred_v))
        rmse_val <- sqrt(mean((obs_val - pred_v)^2))
        r2_val <- 1 - sum((obs_val - pred_v)^2) / sum((obs_val - mean(obs_val))^2)
      } else {
        mae_val <- rmse_val <- r2_val <- NA
      }
      
      metric_matrix[i, ] <- c(mae_train, rmse_train, r2_train, mae_val, rmse_val, r2_val)
    }
    
    train_means <- colMeans(metric_matrix[, 1:3], na.rm = TRUE)
    train_sds <- apply(metric_matrix[, 1:3], 2, sd, na.rm = TRUE)
    val_means <- colMeans(metric_matrix[, 4:6], na.rm = TRUE)
    val_sds <- apply(metric_matrix[, 4:6], 2, sd, na.rm = TRUE)
    
    error_score <- mean(
      c(train_means["MAE_train"], train_means["RMSE_train"], 1 - train_means["R2_train"],
        val_means["MAE_val"], val_means["RMSE_val"], 1 - val_means["R2_val"]),
      na.rm = TRUE
    )
    
    validation_results[validation_results$Threshold == th, ] <- c(
      th,
      train_means["MAE_train"], train_sds["MAE_train"],
      train_means["RMSE_train"], train_sds["RMSE_train"],
      train_means["R2_train"], train_sds["R2_train"],
      val_means["MAE_val"], val_sds["MAE_val"],
      val_means["RMSE_val"], val_sds["RMSE_val"],
      val_means["R2_val"], val_sds["R2_val"],
      error_score
    )
  }
  
  best_row <- validation_results[which.min(validation_results$ErrorScore), ]
  best_threshold <- best_row$Threshold
  
  # Test set evaluation with best threshold
  pred_test <- sapply(test_set, function(df)
    extract_func(df, vi_column, "Date", VIthd = best_threshold))
  true_test <- sapply(test_set, function(df)
    df[[target_col]][1])
  valid_test <- !is.na(pred_test) & !is.na(true_test)
  if (sum(valid_test) >= 3) {
    obs_test <- true_test[valid_test]
    pred_t <- pred_test[valid_test]
    mae_test <- mean(abs(obs_test - pred_t))
    rmse_test <- sqrt(mean((obs_test - pred_t)^2))
    r2_test <- 1 - sum((obs_test - pred_t)^2) / sum((obs_test - mean(obs_test))^2)
  } else {
    mae_test <- rmse_test <- r2_test <- NA
  }
  
  test_metrics <- data.frame(
    Threshold = best_threshold,
    MAE_test = mae_test,
    RMSE_test = rmse_test,
    R2_test = r2_test
  )
  
  return(list(
    validation_results = validation_results,
    test_metrics = test_metrics,
    best_threshold = best_threshold
  ))
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

# Print examples of results
print(sos_results$NDVI$validation_results)
print(sos_results$NDVI$test_metrics)

print(eos_results$GDVI$validation_results)
print(eos_results$GDVI$test_metrics)

# ==== Plot composite error scores for all SOS VIs ====
# Combine all SOS validation results for plotting
plot_df <- do.call(rbind, lapply(names(sos_results), function(vi_name) {
  df <- sos_results[[vi_name]]$validation_results
  df$VI <- vi_name
  return(df[, c("Threshold", "ErrorScore", "VI")])
}))

ggplot(plot_df, aes(x = Threshold, y = ErrorScore, color = VI)) +
  geom_line(size = 1) +
  geom_point() +
  ggtitle("SOS VIs - Composite Error Score Across Thresholds") +
  ylab("Composite Error Score (MAE + RMSE + (1 - R²))") +
  xlab("Threshold") +
  theme_minimal()

# ==== Plot composite error scores for all EOS VIs ====
# Combine all EOS validation results for plotting
plot_df_eos <- do.call(rbind, lapply(names(eos_results), function(vi_name) {
  df <- eos_results[[vi_name]]$validation_results
  df$VI <- vi_name
  return(df[, c("Threshold", "ErrorScore", "VI")])
}))

ggplot(plot_df_eos, aes(x = Threshold, y = ErrorScore, color = VI)) +
  geom_line(size = 1) +
  geom_point() +
  ggtitle("EOS VIs - Composite Error Score Across Thresholds") +
  ylab("Composite Error Score (MAE + RMSE + (1 - R²))") +
  xlab("Threshold") +
  theme_minimal()
