############################################################
# Title: Harmonic Curve Fitting for NDVI/GCVI Time Series
# Author: Riasad Bin Mahbub
# Date: 2025-05-15
# Description: Fit harmonic curves to VI time series data,
#              extract key phenology metrics, plot results,
#              and evaluate model performance.
# Paper citation: Field-scale dynamics of planting dates in the US Corn Belt from 2000 to 2020
############################################################

# -----------------------------
# 1. Load Required Packages
# -----------------------------
library(minpack.lm)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Metrics)  # For MAE and R-squared
library(hydroGOF) # For NSE
library(tidyverse)
library(dplyr)
library(lubridate)




# -----------------------------
# 2. Define Feature Clusters (Optional Metadata)
# -----------------------------
feature_clusters <- list(
  Meteorological_Climate = c(
    "Early season precipitation (Jan–Apr) - GRIDMET",#meteo_list[[1]]$ppt
    "Growing Degree Days (Mar–May) - GRIDMET-derived",
    "Growing season mean temperature (Jun–Aug) - GRIDMET",#meteo_list[[1]]$tmean
    "Mean soil temperature (Apr–Jun) - GLDAS Monthly",
    "Precipitation (Apr–Jun) - GRIDMET Monthly", #meteo_list[[1]]$ppt
    "Mean minimum temperature (Apr–Jun) - GRIDMET Monthly", #meteo_list[[1]]$tmean
    "Mean maximum temperature (Apr–Jun) - GRIDMET Monthly",#meteo_list[[1]]$tmean
    "Mean temperature (Apr–Jun) - GRIDMET Monthly",#meteo_list[[1]]$tmean
    "Climate water deficit (Apr–Jun) - TerraClimate Monthly", #meteo_list[[1]]$vpd
    "Soil moisture (Apr–Jun) - TerraClimate Monthly" #meteo_list[[1]]$Es
  ),
  Harmonic_Landsat = c(
    "Harmonic constant (GCVI, NIR, SWIR1, SWIR2)",
    "Harmonic cosine coefficients a1 (GCVI, NIR, SWIR1, SWIR2)",
    "Harmonic cosine coefficients a2 (GCVI, NIR, SWIR1, SWIR2)",
    "Harmonic sine coefficients b1 (GCVI, NIR, SWIR1, SWIR2)",
    "Harmonic sine coefficients b2 (GCVI, NIR, SWIR1, SWIR2)"
  ),
  Vegetation_Phenology = c(
    "Day and value of max observed GCVI - Landsat",
    "Day and value of max GCVI from harmonic fit - Landsat-derived"
  ),
  Spatial_Metadata = c(
    "Year",
    "Latitude",
    "Longitude"
  )
)
meteo_list[[1]]$LST_1KM
meteo_list[[1]]$Lai
meteo_list[[1]]$Ec
meteo_list[[1]]$Ei
meteo_list[[1]]$Es
meteo_list[[1]]$srad




# -----------------------------
# 3. Harmonic Curve Fitting Function
# -----------------------------
fit_harmonic_deines <- function(df, value_col = "kNDVI", omega = 1.5) {
  df <- df %>% 
    dplyr::mutate(DOY = yday(Date),
           t = DOY / 365) %>%
    dplyr::filter(!is.na(.data[[value_col]]))
  
  if (nrow(df) < 10) return(NULL)
  
  t <- df$t
  y <- df[[value_col]]
  
  harmonic_model <- function(t, c, a1, b1, a2, b2) {
    c + 
      a1 * cos(2 * pi * omega * t) + b1 * sin(2 * pi * omega * t) +
      a2 * cos(4 * pi * omega * t) + b2 * sin(4 * pi * omega * t)
  }
  
  tryCatch({
    fit <- nlsLM(y ~ harmonic_model(t, c, a1, b1, a2, b2),
                 start = list(c = mean(y), a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1),
                 control = nls.lm.control(maxiter = 500))
    
    # Predict over full year
    t_seq <- seq(0, 1, length.out = 365)
    pred <- predict(fit, newdata = data.frame(t = t_seq))
    doy_seq <- round(t_seq * 365)
    
    # Extract max from predicted and observed
    max_fit_idx <- which.max(pred)
    doy_max_fit <- doy_seq[max_fit_idx]
    val_max_fit <- pred[max_fit_idx]
    
    max_obs_idx <- which.max(y)
    doy_max_obs <- df$DOY[max_obs_idx]
    val_max_obs <- y[max_obs_idx]
    
    # Extract predictions on observed t
    y_hat <- predict(fit, newdata = data.frame(t = t))
    
    # Calculate performance metrics
    r2 <- cor(y, y_hat)^2
    mae_val <- mae(y, y_hat)
    bias_val <- mean(y_hat - y)
    nse_val <- NSE(y_hat, y)
    
    return(list(
      metrics = data.frame(
        DOY_max_obs = doy_max_obs,
        Value_max_obs = val_max_obs,
        DOY_max_fit = doy_max_fit,
        Value_max_fit = val_max_fit,
        Intercept_c = coef(fit)["c"],
        a1 = coef(fit)["a1"], b1 = coef(fit)["b1"],
        a2 = coef(fit)["a2"], b2 = coef(fit)["b2"],
        R2 = r2, MAE = mae_val, Bias = bias_val, NSE = nse_val
      ),
      fit_curve = data.frame(DOY = doy_seq, Predicted = pred),
      observed = df %>% select(DOY, Observed = !!sym(value_col)),
      coefs = coef(fit)
    ))
    
  }, error = function(e) {
    return(NULL)
  })
}

# -----------------------------
# 4. Apply to a Single Example
# -----------------------------
result_gcvi <- fit_harmonic_deines(vi_list_gt20[[2]], value_col = "GCVI")
result_swir1 <- fit_harmonic_deines(vi_list_gt20[[2]], value_col = "swir1")
result_swir2 <- fit_harmonic_deines(vi_list_gt20[[2]], value_col = "swir2")
result_NIR <- fit_harmonic_deines(vi_list_gt20[[2]], value_col = "nir")

# -----------------------------
# 5. Plot Fit vs Observed
# -----------------------------
fit_df <- result_gcvi$fit_curve
obs_df <- result_gcvi$observed
coefs <- result_gcvi$coefs
metrics <- result_gcvi$metrics

coef_text <- sprintf(
  "c = %.3f\na1 = %.3f\nb1 = %.3f\na2 = %.3f\nb2 = %.3f\nR² = %.3f\nMAE = %.3f\nBias = %.3f\nNSE = %.3f",
  coefs["c"], coefs["a1"], coefs["b1"], coefs["a2"], coefs["b2"],
  metrics$R2, metrics$MAE, metrics$Bias, metrics$NSE
)

ggplot() +
  geom_point(data = obs_df, aes(x = DOY, y = Observed), color = "blue", alpha = 0.6) +
  geom_line(data = fit_df, aes(x = DOY, y = Predicted), color = "red", size = 1.2) +
  labs(title = "NDVI Harmonic Fit vs Observed",
       x = "Day of Year (DOY)", y = "NDVI") +
  theme_minimal(base_size = 14) +
  annotate("text", x = 20, y = max(obs_df$Observed, na.rm = TRUE), 
           label = coef_text, hjust = 0, vjust = 1, size = 4, color = "black")

# Reconstruct individual components
t_seq <- seq(0, 1, length.out = 365)
doy_seq <- round(t_seq * 365)
a1_term <- coefs["a1"] * cos(2 * pi * omega * t_seq)
b1_term <- coefs["b1"] * sin(2 * pi * omega * t_seq)
a2_term <- coefs["a2"] * cos(4 * pi * omega * t_seq)
b2_term <- coefs["b2"] * sin(4 * pi * omega * t_seq)
baseline <- rep(coefs["c"], length(t_seq))

df_components <- data.frame(
  DOY = doy_seq,
  c = baseline,
  a1 = a1_term,
  b1 = b1_term,
  a2 = a2_term,
  b2 = b2_term,
  total = baseline + a1_term + b1_term + a2_term + b2_term
)

# Plot individual components
df_long <- df_components %>%
  pivot_longer(cols = c(c, a1, b1, a2, b2, total),
               names_to = "Component", values_to = "Value")

ggplot(df_long, aes(x = DOY, y = Value, color = Component)) +
  geom_line(size = 1.1) +
  labs(title = "Individual Harmonic Components",
       x = "Day of Year", y = "NDVI Contribution") +
  theme_minimal(base_size = 14)

# -----------------------------
# 6.  Apply to All Dataframes
# -----------------------------

# -----------------------------
#  Harmonic Fitting Function
# -----------------------------
fit_harmonic_deines <- function(df, value_col = "kNDVI") {
  df <- df %>% 
    mutate(DOY = yday(Date),
           t = DOY / 365)
  df <- df[!is.na(df[[value_col]]), ]
  if (nrow(df) < 10) return(NULL)
  
  t <- df$t
  y <- df[[value_col]]
  omega <- 1.5
  
  harmonic_model <- function(t, c, a1, b1, a2, b2) {
    c + 
      a1 * cos(2 * pi * omega * 1 * t) + b1 * sin(2 * pi * omega * 1 * t) +
      a2 * cos(2 * pi * omega * 2 * t) + b2 * sin(2 * pi * omega * 2 * t)
  }
  
  tryCatch({
    fit <- nlsLM(y ~ harmonic_model(t, c, a1, b1, a2, b2),
                 start = list(c = mean(y), a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1),
                 control = nls.lm.control(maxiter = 500))
    
    t_seq <- seq(0, 1, length.out = 365)
    pred <- predict(fit, newdata = data.frame(t = t_seq))
    max_idx <- which.max(pred)
    doy_max_fit <- round(t_seq[max_idx] * 365)
    val_max_fit <- pred[max_idx]
    
    max_obs_idx <- which.max(y)
    doy_max_obs <- df$DOY[max_obs_idx]
    val_max_obs <- y[max_obs_idx]
    
    y_pred <- predict(fit, newdata = data.frame(t = t))
    R2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
    MAE <- mean(abs(y - y_pred))
    Bias <- mean(y_pred - y)
    
    coefs <- coef(fit)
    return(data.frame(
      DOY_max_obs = doy_max_obs,
      Value_max_obs = val_max_obs,
      DOY_max_fit = doy_max_fit,
      Value_max_fit = val_max_fit,
      c = coefs["c"],
      a1 = coefs["a1"], b1 = coefs["b1"],
      a2 = coefs["a2"], b2 = coefs["b2"],
      R2 = R2,
      MAE = MAE,
      Bias = Bias
    ))
  }, error = function(e) {
    return(NULL)
  })
}

# -----------------------------
# Set up Directory to Save Plots
# -----------------------------
save_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/Harmonic_Deines"
dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Initialize Results List
# -----------------------------
results_list <- list()


# -----------------------------
# Loop through vi_list_gt20
# -----------------------------
for (i in seq_along(vi_list_gt20)) {
  df <- vi_list_gt20[[i]]
  field_id <- names(vi_list_gt20)[i]
  
  res <- fit_harmonic_deines(df, value_col = "kNDVI")
  
  if (!is.null(res)) {
    res$ID <- field_id
    results_list[[field_id]] <- res
    
    # Prepare data for plotting
    df_plot <- df %>%
      mutate(DOY = yday(Date), t = DOY / 365) %>%
      dplyr::filter(!is.na(kNDVI))
    
    t <- df_plot$t
    y <- df_plot$kNDVI
    omega <- 1.5
    
    harmonic_model <- function(t, c, a1, b1, a2, b2) {
      c + 
        a1 * cos(2 * pi * omega * t) + b1 * sin(2 * pi * omega * t) +
        a2 * cos(2 * pi * omega * 2 * t) + b2 * sin(2 * pi * omega * 2 * t)
    }
    
    fit <- nlsLM(y ~ harmonic_model(t, c, a1, b1, a2, b2),
                 start = list(c = mean(y), a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1),
                 control = nls.lm.control(maxiter = 500))
    
    t_seq <- seq(0, 1, length.out = 365)
    doy_seq <- round(t_seq * 365)
    pred_ndvi <- predict(fit, newdata = data.frame(t = t_seq))
    fit_df <- data.frame(DOY = doy_seq, kNDVI = pred_ndvi)
    
    y_pred <- predict(fit, newdata = data.frame(t = t))
    R2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
    MAE <- mean(abs(y - y_pred))
    Bias <- mean(y_pred - y)
    NSE <- R2
    
    coefs <- coef(fit)
    coef_text <- sprintf(
      "c = %.3f\na1 = %.3f\nb1 = %.3f\na2 = %.3f\nb2 = %.3f\nR² = %.2f\nMAE = %.3f\nBias = %.3f\nNSE = %.2f",
      coefs["c"], coefs["a1"], coefs["b1"], coefs["a2"], coefs["b2"],
      R2, MAE, Bias, NSE
    )
    
    # Start plot
    p <- ggplot() +
      geom_point(data = df_plot, aes(x = DOY, y = kNDVI), color = "blue", alpha = 0.6) +
      geom_line(data = fit_df, aes(x = DOY, y = kNDVI), color = "red", size = 1.2) +
      labs(title = paste("kNDVI Harmonic Fit:", field_id),
           x = "DOY", y = "kNDVI") +
      annotate("text", x = 30, y = max(y, na.rm = TRUE) * 0.95,
               label = coef_text, hjust = 0, vjust = 1, size = 4) +
      theme_minimal(base_size = 14)
    
    # Add vertical lines if PDDOY/HDDOY exist
    if ("PDDOY" %in% names(df)) {
      p <- p + geom_vline(xintercept = unique(df$PDDOY), linetype = "dashed", color = "green", size = 1)
    }
    if ("HDDOY" %in% names(df)) {
      p <- p + geom_vline(xintercept = unique(df$HDDOY), linetype = "dashed", color = "orange", size = 1)
    }
    
    # Save plot
    ggsave(filename = file.path(save_dir, paste0("harmonic_fit_", field_id, ".jpeg")),
           plot = p, width = 8, height = 5)
  }
}

# Combine results
coef_df <- bind_rows(results_list) %>% 
  select(ID, everything())
print(coef_df)

# write.csv(coef_df, "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/Harmonic_Deines/harmonic_coefficients.csv", row.names = FALSE)

fit_harmonic_deines <- function(df, value_col = "kNDVI") {
  df <- df %>%
    dplyr::mutate(DOY = yday(Date), t = DOY / 365) %>%
    dplyr::filter(!is.na(.data[[value_col]]))
  
  if (nrow(df) < 10) return(NULL)  # Skip if not enough data
  
  t <- df$t
  y <- df[[value_col]]
  omega <- 1.5
  
  harmonic_model <- function(t, c, a1, b1, a2, b2) {
    c + 
      a1 * cos(2 * pi * omega * t) + b1 * sin(2 * pi * omega * t) +
      a2 * cos(2 * pi * omega * 2 * t) + b2 * sin(2 * pi * omega * 2 * t)
  }
  
  tryCatch({
    fit <- nlsLM(y ~ harmonic_model(t, c, a1, b1, a2, b2),
                 start = list(c = mean(y), a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1),
                 control = nls.lm.control(maxiter = 500))
    
    y_pred <- predict(fit)
    
    # Metrics
    R2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
    NSE <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
    RMSE <- sqrt(mean((y - y_pred)^2))
    MAE <- mean(abs(y - y_pred))
    
    # Predict full curve
    t_seq <- seq(0, 1, length.out = 365)
    pred <- predict(fit, newdata = data.frame(t = t_seq))
    
    max_idx <- which.max(pred)
    doy_max_fit <- round(t_seq[max_idx] * 365)
    val_max_fit <- pred[max_idx]
    doy_max_obs <- df$DOY[which.max(y)]
    val_max_obs <- max(y)
    
    coefs <- coef(fit)
    
    return(data.frame(
      DOY_max_obs = doy_max_obs,
      Value_max_obs = val_max_obs,
      DOY_max_fit = doy_max_fit,
      Value_max_fit = val_max_fit,
      Intercept_c = coefs["c"],
      a1 = coefs["a1"], b1 = coefs["b1"],
      a2 = coefs["a2"], b2 = coefs["b2"],
      R2 = R2,
      NSE = NSE,
      RMSE = RMSE,
      MAE = MAE
    ))
  }, error = function(e) {
    return(NULL)
  })
}


# Example: Apply to first entry
result_gcvi <- fit_harmonic_deines(vi_list_gt20[[1]], value_col = "kNDVI")
print(result_gcvi)

# -----------------------------
# 5. Plot Observed vs Fitted Curve
# -----------------------------
# Prepare data for plotting
df <- vi_list_gt20[[1]] %>%
  dplyr::mutate(DOY = yday(Date), t = DOY / 365) %>%
  dplyr:: filter(!is.na(NDVI))

t <- df$t
y <- df$NDVI
fit <- nlsLM(y ~ harmonic_model(t, c, a1, b1, a2, b2),
             start = list(c = mean(y), a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1),
             control = nls.lm.control(maxiter = 500))

t_seq <- seq(0, 1, length.out = 365)
doy_seq <- round(t_seq * 365)
ndvi_pred <- predict(fit, newdata = data.frame(t = t_seq))
fit_df <- data.frame(DOY = doy_seq, NDVI = ndvi_pred)

# Plot
ggplot() +
  geom_point(data = df, aes(x = DOY, y = NDVI), color = "blue", alpha = 0.6) +
  geom_line(data = fit_df, aes(x = DOY, y = NDVI), color = "red", size = 1.2) +
  labs(title = "NDVI Harmonic Fit vs Observed",
       x = "Day of Year (DOY)", y = "NDVI") +
  theme_minimal(base_size = 14)

# Annotate with coefficients
coefs <- coef(fit)
coef_text <- sprintf("c = %.3f\na1 = %.3f\nb1 = %.3f\na2 = %.3f\nb2 = %.3f",
                     coefs["c"], coefs["a1"], coefs["b1"], coefs["a2"], coefs["b2"])
ggplot() +
  geom_point(data = df, aes(x = DOY, y = NDVI), color = "blue", alpha = 0.6) +
  geom_line(data = fit_df, aes(x = DOY, y = NDVI), color = "red", size = 1.2) +
  labs(title = "NDVI Harmonic Fit vs Observed with Coefficients",
       x = "Day of Year (DOY)", y = "NDVI") +
  theme_minimal(base_size = 14) +
  annotate("text", x = 50, y = 0.95, label = coef_text, hjust = 0, vjust = 1, size = 4)


all_harmonic_results <- lapply(vi_list_gt20, function(df) {
  fit_harmonic_deines(df, value_col = "kNDVI")
})

# Combine results if needed
result_df <- bind_rows(all_harmonic_results, .id = "Field_ID")




fit_harmonic_deines <- function(df, value_col = "kNDVI") {
  df <- df %>%
    dplyr::mutate(DOY = yday(Date), t = DOY / 365) %>%
    dplyr::filter(!is.na(.data[[value_col]]))
  
  if (nrow(df) < 10) return(NULL)  # Skip if not enough data
  
  t <- df$t
  y <- df[[value_col]]
  omega <- 1.5
  
  harmonic_model <- function(t, c, a1, b1, a2, b2) {
    c + 
      a1 * cos(2 * pi * omega * t) + b1 * sin(2 * pi * omega * t) +
      a2 * cos(2 * pi * omega * 2 * t) + b2 * sin(2 * pi * omega * 2 * t)
  }
  
  tryCatch({
    fit <- nlsLM(y ~ harmonic_model(t, c, a1, b1, a2, b2),
                 start = list(c = mean(y), a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1),
                 control = nls.lm.control(maxiter = 500))
    
    y_pred <- predict(fit)
    
    # Metrics
    R2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
    NSE <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
    RMSE <- sqrt(mean((y - y_pred)^2))
    MAE <- mean(abs(y - y_pred))
    
    # Predict full curve
    t_seq <- seq(0, 1, length.out = 365)
    pred <- predict(fit, newdata = data.frame(t = t_seq))
    doy_seq <- round(t_seq * 365)
    
    # Main peak (predicted + observed)
    max_idx <- which.max(pred)
    doy_max_fit <- doy_seq[max_idx]
    val_max_fit <- pred[max_idx]
    
    doy_max_obs <- df$DOY[which.max(y)]
    val_max_obs <- max(y)
    
    # --- Small rise before main peak ---
    # Predicted
    pred_before_main <- pred[doy_seq < doy_max_fit]
    doy_before_main  <- doy_seq[doy_seq < doy_max_fit]
    if (length(pred_before_main) > 0) {
      small_idx_fit <- which.max(pred_before_main)
      doy_small_fit <- doy_before_main[small_idx_fit]
      val_small_fit <- pred_before_main[small_idx_fit]
    } else {
      doy_small_fit <- NA
      val_small_fit <- NA
    }
    
    # Observed
    obs_before_main <- y[df$DOY < doy_max_obs]
    doy_before_main_obs <- df$DOY[df$DOY < doy_max_obs]
    if (length(obs_before_main) > 0) {
      small_idx_obs <- which.max(obs_before_main)
      doy_small_obs <- doy_before_main_obs[small_idx_obs]
      val_small_obs <- obs_before_main[small_idx_obs]
    } else {
      doy_small_obs <- NA
      val_small_obs <- NA
    }
    
    # Coefficients
    coefs <- coef(fit)
    
    return(data.frame(
      DOY_max_obs = doy_max_obs,
      Value_max_obs = val_max_obs,
      DOY_small_obs = doy_small_obs,
      Value_small_obs = val_small_obs,
      DOY_max_fit = doy_max_fit,
      Value_max_fit = val_max_fit,
      DOY_small_fit = doy_small_fit,
      Value_small_fit = val_small_fit,
      Intercept_c = coefs["c"],
      a1 = coefs["a1"], b1 = coefs["b1"],
      a2 = coefs["a2"], b2 = coefs["b2"],
      R2 = R2,
      NSE = NSE,
      RMSE = RMSE,
      MAE = MAE
    ))
  }, error = function(e) {
    return(NULL)
  })
}

# Apply to all datasets
all_harmonic_results <- lapply(vi_list_gt20, function(df) {
  fit_harmonic_deines(df, value_col = "kNDVI")
})

# Combine results
result_df <- bind_rows(all_harmonic_results, .id = "Field_ID")

# Save
write.csv(result_df,
          "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/Harmonic_Deines/harmonic_coefficients.csv",
          row.names = FALSE)


#================================================

# Example: "Isbell_2021.csv" → "Isbell"
field_names <-  gsub("\\.csv$", "", basename(vi_csv_files_gt20))
# Set up result storage
harmonic_results <- list()

for (i in seq_along(vi_list_gt20)) {
  df <- vi_list_gt20[[i]]
  field_name <- field_names[i]
  
  result <- fit_harmonic_deines(df, value_col = "kNDVI")
  
  if (!is.null(result)) {
    result$Field_ID <- field_name
    harmonic_results[[i]] <- result
  }
}

harmonic_df <- bind_rows(harmonic_results)

# Combine all into one data frame
harmonic_df <- bind_rows(harmonic_results)
harmonic_df <- harmonic_df %>%
  select(Field_ID, everything())  # Move Field_ID to first column
# Get mean PDDOY and HDDOY per field

mean(harmonic_df$R2 > 0.6, na.rm = TRUE) * 100
harmonic_df_filtered <- harmonic_df %>% filter(R2 > 0.6)

pddoy_summary <- lapply(seq_along(vi_list_gt20), function(i) {
  df <- vi_list_gt20[[i]]
  field_id <- field_names[i]
  
  tibble(
    Field_ID = field_id,
    mean_PDDOY = mean(df$PDDOY, na.rm = TRUE),
    mean_HDDOY = mean(df$HDDOY, na.rm = TRUE)
  )
}) %>% bind_rows()
# Merge with harmonic_df
harmonic_df <- harmonic_df %>%
  left_join(pddoy_summary, by = "Field_ID")



# meteo_summary_list <- lapply(seq_along(meteo_list), function(i) {
#   df <- meteo_list[[i]]  # Add field I
#   field_id <- gsub("\\.csv$", "", basename(vi_csv_files_gt20[i])) # Ensure Date column is Date class
#   df$Date <- as.Date(df$Date)
#   df$Month <- month(df$Date)
#   df_aprjun <- df %>% dplyr::filter(Month %in% 4:6)# Filter for Apr–Jun (Month 4 to 6)
#   # Summarize key variables (adjust names based on actual column names in df)
#   df_summary <- tibble(
#     Field_ID = field_id,
#     APRJUNEmeanTemperature = mean(df_aprjun$tmean, na.rm = TRUE),
#     APRJUNEPrecipitation    = sum(df_aprjun$ppt, na.rm = TRUE),
#     APRJUNEMinTemp          = mean(df_aprjun$tmin, na.rm = TRUE),
#     APRJUNEMaxTemp          = mean(df_aprjun$tmax, na.rm = TRUE),
#     APRJUNEVPD              = mean(df_aprjun$vpd, na.rm = TRUE),
#     APRJUNESoilMoisture     = mean(df_aprjun$Es, na.rm = TRUE)
#   )
#   return(df_summary)
# })
# Identify which elements are missing the 'dayl' column or have all NA values
missing_dayl <- purrr::map_lgl(merged_list, ~ !("dayl" %in% names(.x)) || all(is.na(.x$dayl)))
which(missing_dayl)
names(merged_list)[missing_dayl]
merged_list <- purrr::keep(merged_list, ~ "dayl" %in% names(.x))
merged_list


meteo_summary_list <- lapply(seq_along(merged_list), function(i) {
  df <- merged_list[[i]]
  field_id <- gsub("\\.csv$", "", basename(vi_csv_files_gt20[i]))
  
  # Extract Field_Year from the first row (assuming it's the same for the entire df)
  field_year <- df$Field_Year[1]
  
  # Convert and add time features
  df$Date <- as.Date(df$Date)
  df$Month <- lubridate::month(df$Date)
  df$DOY <- lubridate::yday(df$Date)
  
  # Find the DOY when ATSAVI crosses the 0.50 threshold
  atsavi_threshold_doy <- df %>%
    dplyr::filter(ATSAVI >= 0.50) %>%
    dplyr::summarise(ATSAVI_0.50_DOY = min(DOY, na.rm = TRUE)) %>%
    dplyr::pull(ATSAVI_0.50_DOY)
  
  if (is.infinite(atsavi_threshold_doy)) {
    atsavi_threshold_doy <- NA
  }
  
  # Filter months 2 to 9 for monthly summaries and slope calculation
  df_aprsep <- df %>% dplyr::filter(Month %in% 2:9)
  
  # Monthly climate and VI summaries
  df_monthly <- df_aprsep %>%
    dplyr::group_by(Month) %>%
    dplyr::summarise(
      mean_tmean    = mean(tmean, na.rm = TRUE),
      sum_ppt       = sum(ppt, na.rm = TRUE),
      mean_tmin     = mean(tmin, na.rm = TRUE),
      mean_tmax     = mean(tmax, na.rm = TRUE),
      mean_vpd      = mean(vpd, na.rm = TRUE),
      mean_gdd      = mean(gdd, na.rm = TRUE),
      meansrad      = mean(srad, na.rm = TRUE),
      #meandayl      = sum(dayl, na.rm = TRUE),
      meanRH        = mean(avgRH, na.rm = TRUE),
      meanNDWI      = mean(NDWI, na.rm = TRUE),
      meanLai       = mean(Lai, na.rm = TRUE),
      meanIAVI      = mean(IAVI, na.rm = TRUE),
      meanGDVI      = mean(GDVI, na.rm = TRUE),
      meanVARI      = mean(VARI, na.rm = TRUE),
      meanRNDVI     = mean(RNDVI, na.rm = TRUE),
      meanMLSWI26   = mean(MLSWI26, na.rm = TRUE),
      .groups       = "drop"
    )
  
  # Calculate LSWI slope vs DOY for each month
  lswi_slopes <- df_aprsep %>%
    dplyr::group_by(Month) %>%
    dplyr::do({
      model <- lm(LSWI ~ DOY, data = .)
      broom::tidy(model)
    }) %>%
    dplyr::filter(term == "DOY") %>%
    dplyr::select(Month, lswi_slope = estimate)
  
  # Pivot monthly means wide
  df_summary <- df_monthly %>%
    tidyr::pivot_wider(
      names_from = Month,
      values_from = c(mean_tmean, sum_ppt, mean_vpd, mean_gdd, meanRH, mean_tmin,
                      mean_tmax, meanNDWI, meanLai, meanIAVI, meanGDVI, meanVARI,
                      meanRNDVI, meanMLSWI26, meansrad
                      #, meandayl
                      ),
      names_glue = "{.value}_M{Month}"
    )
  
  # Pivot LSWI slopes wide
  lswi_slopes_wide <- lswi_slopes %>%
    tidyr::pivot_wider(
      names_from = Month,
      values_from = lswi_slope,
      names_prefix = "lswi_slope_M"
    )
  
  # Combine all parts
  df_final <- dplyr::bind_cols(df_summary, lswi_slopes_wide) %>%
    dplyr::mutate(
      Field_ID = field_id,
      Field_Year = field_year,
      ATSAVI_0.50_DOY = atsavi_threshold_doy
    ) %>%
    dplyr::select(Field_ID, Field_Year, ATSAVI_0.50_DOY, dplyr::everything()) %>%
    tibble::as_tibble()
  
  return(df_final)
})


# This section is for the meteo_summary_list generation, which was the focus of the user's query.
# It assumes 'merged_list' and 'vi_csv_files_gt20' are defined elsewhere in the script or environment.
meteo_summary_list <- lapply(seq_along(merged_list), function(i) {
  df <- merged_list[[i]]
  field_id <- gsub("\\.csv$", "", basename(vi_csv_files_gt20[i]))
  
  df$Date <- as.Date(df$Date)
  df$Month <- month(df$Date)
  df$DOY <- yday(df$Date) # Add DOY column
  
  # Find the DOY when ATSAVI crosses the 0.50 threshold
  atsavi_threshold_doy <- df %>%
    dplyr::filter(ATSAVI >= 0.50) %>%
    dplyr::summarise(ATSAVI_0.50_DOY = min(DOY, na.rm = TRUE)) %>%
    pull(ATSAVI_0.50_DOY)
  
  # If no DOY crosses the threshold, set to NA
  if (is.infinite(atsavi_threshold_doy)) {
    atsavi_threshold_doy <- NA
  }
  
  df_aprjun <- df %>% dplyr::filter(Month %in% 2:9)
  
  # Group by Month and calculate monthly means
  df_monthly <- df_aprjun %>%
    group_by(Month) %>%
    dplyr::summarise(
      mean_tmean = mean(tmean, na.rm = TRUE),
      sum_ppt    = sum(ppt, na.rm = TRUE),
      mean_tmin  = mean(tmin, na.rm = TRUE),
      mean_tmax  = mean(tmax, na.rm = TRUE),
      mean_vpd   = mean(vpd, na.rm = TRUE),
      #mean_Es    = mean(Es, na.rm = TRUE),
      mean_gdd = mean(gdd, na.rm = TRUE),
      #mean_soiltemp = mean (SoilTMP0_10cm_inst, na.rm = TRUE),
      meansrad    = mean(srad, na.rm = TRUE),
      #meandayl    = sum(dayl, na.rm = TRUE),
      meanRH = mean(avgRH, na.rm = TRUE),
      meanNDWI = mean(NDWI, na.rm = TRUE),
      meanLai = mean(Lai, na.rm = TRUE),
      meanIAVI = mean(IAVI, na.rm = TRUE),
      meanGDVI = mean(GDVI, na.rm = TRUE),
      meanVARI = mean(VARI, na.rm = TRUE),
      meanRNDVI = mean(RNDVI, na.rm = TRUE),
      meanMLSWI26 = mean(MLSWI26, na.rm = TRUE),
      # Removed meanATSAVI as requested
      .groups = "drop"
    )
  # Pivot wider to create column per metric per month
  df_summary <- df_monthly %>%
    pivot_wider(
      names_from = Month,
      values_from = c(mean_tmean, sum_ppt, mean_vpd, mean_gdd, meanRH, mean_tmin,
                      mean_tmax, meanNDWI, meanLai, meanIAVI, meanGDVI, meanVARI,
                      meanRNDVI, meanMLSWI26, meansrad#, meandayl
                      ), # Removed meanATSAVI
      names_glue = "{.value}_M{Month}"
    ) %>%
    dplyr::mutate(
      Field_ID = field_id,
      ATSAVI_0.50_DOY = atsavi_threshold_doy # Add the new metric here
    ) %>%
    dplyr::select(Field_ID, ATSAVI_0.50_DOY, everything()) %>% # Reorder to place new metric near Field_ID
    as_tibble()
  
  return(df_summary)
})



meteo_summary_df <- bind_rows(meteo_summary_list)
combined_df <- left_join(harmonic_df, meteo_summary_df, by = "Field_ID")
colnames(combined_df)
library(randomForest)

# 1. Filter valid rows (no NA in response or predictors)
df <- combined_df %>%
  dplyr::select(
    a1, b1, a2, b2, DOY_max_fit, DOY_max_obs, Value_max_obs, Value_max_fit, mean_PDDOY,
    #meandayl_M2, meandayl_M3, meandayl_M4, meandayl_M5, meandayl_M6,
    #mean_Es_M4, mean_Es_M5, mean_Es_M6,
    mean_gdd_M4, mean_gdd_M5, mean_gdd_M6, 
    #mean_gdd_M7,
    meansrad_M2,meansrad_M3, meansrad_M4, meansrad_M5, meansrad_M6, meansrad_M7,
    #meanRH_M6,meanRH_M7,
    mean_tmean_M2, mean_tmean_M3,mean_tmean_M4, mean_tmean_M5, mean_tmean_M6, 
    #mean_tmean_M7,
    mean_tmin_M2, mean_tmin_M3, mean_tmin_M4, mean_tmin_M5, mean_tmin_M6,
    #mean_tmin_M7,
    #mean_tmax_M3,
    mean_tmax_M4, mean_tmax_M5, mean_tmax_M6,
    #mean_tmax_M7, 
    meanNDWI_M2, meanNDWI_M3, meanNDWI_M4, meanNDWI_M5, 
    #meanNDWI_M6,meanNDWI_M7,
    #meanMLSWI26_M3, meanMLSWI26_M4, meanMLSWI26_M5, meanMLSWI26_M6,meanMLSWI26_M7,
    meanRNDVI_M2, meanRNDVI_M3, meanRNDVI_M4, 
    #meanRNDVI_M5, 
    #meanATSAVI_M2, meanATSAVI_M3, meanATSAVI_M3, meanATSAVI_M4, meanATSAVI_M5, 
    #meanRNDVI_M6, meanRNDVI_M7, 
   # meanLai_M2, meanLai_M3, meanLai_M4,
   #meanLai_M5, meanLai_M6,meanLai_M7,
    #meanIAVI_M3,meanIAVI_M4, meanIAVI_M5, meanIAVI_M6,
    #meanGDVI_M3, meanGDVI_M4, meanGDVI_M5, meanGDVI_M6, meanGDVI_M7
    #meanVARI_M3, meanVARI_M4, meanVARI_M5, meanVARI_M6, meanVARI_M7
  ) %>%  # drop Field_ID for modeling
  dplyr::filter(!is.na(mean_PDDOY))%>%
  drop_na()  # This removes rows with NA in any column 

# 2. Create train-test split (80:20)
set.seed(42)
n <- nrow(df)
train_idx <- sample(1:n, size = 0.8 * n)
train <- df[train_idx, ]
test <- df[-train_idx, ]

# 3. Train random forest model
rf_model <- randomForest(mean_PDDOY ~ ., data = train,
                         ntree = 100, importance = TRUE, do.trace = 10)

# 4. Predict on train and test
train$pred_PDDOY <- predict(rf_model, newdata = train)
test$pred_PDDOY <- predict(rf_model, newdata = test)

# 5. Evaluate performance
train_r2 <- summary(lm(mean_PDDOY ~ pred_PDDOY, data = train))$r.squared
train_mae <- mae(train$mean_PDDOY, train$pred_PDDOY)

test_r2 <- summary(lm(mean_PDDOY ~ pred_PDDOY, data = test))$r.squared
test_mae <- mae(test$mean_PDDOY, test$pred_PDDOY)

# 6. Plot predictions (Train)
ggplot(train, aes(x = mean_PDDOY, y = pred_PDDOY)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Training Set Performance",
       x = "Observed PDDOY", y = "Predicted PDDOY") +
  annotate("text", x = min(train$mean_PDDOY), y = max(train$pred_PDDOY),
           label = paste0("R² = ", round(train_r2, 2), 
                          "\nMAE = ", round(train_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

# 7. Plot predictions (Test)
ggplot(test, aes(x = mean_PDDOY, y = pred_PDDOY)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Testing Set Performance",
       x = "Observed PDDOY", y = "Predicted PDDOY") +
  annotate("text", x = min(test$mean_PDDOY), y = max(test$pred_PDDOY),
           label = paste0("R² = ", round(test_r2, 2), 
                          "\nMAE = ", round(test_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

# 8. Variable importance
varImpPlot(rf_model, main = "Random Forest Variable Importance")



#-------------------------------------------------------------------
# Interpretation of the results
#-------------------------------------------------------------------
library(pdp)
partial(rf_model, pred.var = "a1", plot = TRUE, rug = TRUE)
ice_plot <- partial(rf_model, pred.var = "a1", ice = TRUE, center = TRUE)
plotPartial(ice_plot)

library(ranger)
library(tree.interpreter)
rf_ranger <- ranger(mean_PDDOY ~ ., data = train, importance = "permutation")
tidy.RF <- tidyRF(rf_ranger, train[, -which(names(train) == "mean_PDDOY")], train$mean_PDDOY)
# For the first observation:
observation_1 <- train[1, -which(names(train) == "mean_PDDOY")]
contributions <- getTree(rf_model, k = 1, labelVar = TRUE)  # For a single tree

library(inTrees)
tree_list <- RF2List(rf_model)  # Convert to inTrees format
rules <- extractRules(tree_list, train[, -which(names(train) == "mean_PDDOY")])
rules_metric <- getRuleMetric(rules, train[, -which(names(train) == "mean_PDDOY")], train$mean_PDDOY)
presentRules(rules_metric, colnames(train))
plotPartial(ice_plot)

library(randomForestExplainer)
# Explain variable interactions
explain_forest(rf_model, data = train[, -which(names(train) == "mean_PDDOY")])

plot(train$a1, train$mean_PDDOY)
plot(train$a2, train$mean_PDDOY)

plot(train$DOY_max_fit, train$mean_PDDOY)




#--------------------------------------------
#Harvest 
#--------------------------------------------


df <- combined_df %>%
  dplyr::select(
    a1, b1, a2, b2, DOY_max_fit, DOY_max_obs, Value_max_obs, Value_max_fit, mean_HDDOY,
    #mean_vpd_M4, 
    #mean_vpd_M5,mean_vpd_M6,mean_vpd_M7,  mean_vpd_M8, mean_vpd_M9,
    
    #mean_Es_M4, mean_Es_M5, mean_Es_M6,
    #mean_gdd_M4, mean_gdd_M5,
    #mean_gdd_M6,mean_gdd_M7, mean_gdd_M8, mean_gdd_M9,
    #meanRH_M4, meanRH_M5, 
    #meanRH_M6,meanRH_M7,meanRH_M8, meanRH_M9,
    #mean_tmean_M3, mean_tmean_M4,  mean_tmean_M5, 
  #  mean_tmean_M6, mean_tmean_M7,mean_tmean_M8, mean_tmean_M9, 
    #mean_tmin_M3, 
    #mean_tmin_M4, mean_tmin_M5, 
    #mean_tmin_M6,mean_tmin_M7,mean_tmin_M8, mean_tmin_M9, 
    #mean_tmax_M3, 
    #mean_tmax_M4,  mean_tmax_M5, 
    #mean_tmax_M6,mean_tmax_M7,mean_tmax_M8, mean_tmax_M9, 
    #meanNDWI_M3, meanNDWI_M4, 
    #meanNDWI_M5, meanNDWI_M6,meanNDWI_M7,meanNDWI_M8, meanNDWI_M9,
    #meanMLSWI26_M3, meanMLSWI26_M4, 
    #meanMLSWI26_M5, meanMLSWI26_M6,meanMLSWI26_M7,
    #meanRNDVI_M3, meanRNDVI_M4, meanRNDVI_M5, 
    #meanRNDVI_M6, meanRNDVI_M7, meanRNDVI_M8, meanRNDVI_M9, 
   # meanLai_M3, meanLai_M4, meanLai_M5, meanLai_M6,meanLai_M7,meanLai_M8,meanLai_M9,
    #meanIAVI_M3,meanIAVI_M4, meanIAVI_M5, meanIAVI_M6,meanIAVI_M7, 
    #meanGDVI_M3, meanGDVI_M4, meanGDVI_M5, meanGDVI_M6,meanGDVI_M7, 
    #meanVARI_M3, meanVARI_M4, meanVARI_M5, meanVARI_M6, meanVARI_M7
  ) %>%  # drop Field_ID for modeling
  dplyr::filter(!is.na(mean_HDDOY))%>%
  drop_na()  # This removes rows with NA in any column 


df <- combined_df %>%
  dplyr::select(
    a1, b1, a2, b2, DOY_max_fit, DOY_max_obs, Value_max_obs, Value_max_fit, mean_HDDOY,
    #meandayl_M2, meandayl_M3, meandayl_M4, meandayl_M5, meandayl_M6,
    #mean_Es_M4, mean_Es_M5, mean_Es_M6,
    #mean_gdd_M4, mean_gdd_M5, mean_gdd_M6,mean_gdd_M7,
    #meanRH_M4, meanRH_M5, meanRH_M6,meanRH_M7,
    #mean_tmean_M3, mean_tmean_M4, 
    mean_tmean_M5, mean_tmean_M6, mean_tmean_M7,
    #mean_tmin_M3, mean_tmin_M4, 
    mean_tmin_M5, mean_tmin_M6,mean_tmin_M7,
    #mean_tmax_M3, mean_tmax_M4, 
    mean_tmax_M5, mean_tmax_M6,mean_tmax_M7,
    #meanNDWI_M3, 
    #meanNDWI_M4, meanNDWI_M5, meanNDWI_M6,meanNDWI_M7,
    #meanLai_M3, meanLai_M4,  meanLai_M5, 
    meanLai_M6,meanLai_M7,
    meanRNDVI_M3, meanRNDVI_M4, meanRNDVI_M5, 
    lswi_slope_M5, lswi_slope_M6, lswi_slope_M7,
    
    #meanIAVI_M3,meanIAVI_M4, meanIAVI_M5, meanIAVI_M6,meanIAVI_M7, 
    #meanGDVI_M3, meanGDVI_M4, meanGDVI_M5, 
    meanGDVI_M6,meanGDVI_M7, 
    #meanVARI_M3, meanVARI_M4, meanVARI_M5, meanVARI_M6, meanVARI_M7
  ) %>%  # drop Field_ID for modeling
  dplyr::filter(!is.na(mean_HDDOY))%>%
  drop_na()  # This removes rows with NA in any column 
set.seed(42)
n <- nrow(df)
train_idx <- sample(1:n, size = 0.8 * n)
train <- df[train_idx, ]
test <- df[-train_idx, ]

# 3. Train random forest model
rf_model <- randomForest(mean_HDDOY ~ ., data = train,
                         ntree = 100, importance = TRUE, do.trace = 10)

# 4. Predict on train and test
train$pred_HDDOY <- predict(rf_model, newdata = train)
test$pred_HDDOY <- predict(rf_model, newdata = test)

# 5. Evaluate performance
train_r2 <- summary(lm(mean_HDDOY ~ pred_HDDOY, data = train))$r.squared
train_mae <- mae(train$mean_HDDOY, train$pred_HDDOY)

test_r2 <- summary(lm(mean_HDDOY ~ pred_HDDOY, data = test))$r.squared
test_mae <- mae(test$mean_HDDOY, test$pred_HDDOY)

# 6. Plot predictions (Train)
ggplot(train, aes(x = mean_HDDOY, y = pred_HDDOY)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +  # 1:1 line
  labs(title = "Training Set Performance",
       x = "Observed HDDOY", y = "Predicted HDDOY") +
  annotate("text", 
           x = min(train$mean_HDDOY, na.rm = TRUE) + 2, 
           y = max(train$pred_HDDOY, na.rm = TRUE) - 2,
           label = paste0("R² = ", round(train_r2, 2), 
                          "\nMAE = ", round(train_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")


# 7. Plot predictions (Test)
ggplot(test, aes(x = mean_HDDOY, y = pred_HDDOY)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +  # 1:1 line
  labs(title = "Testing Set Performance",
       x = "Observed HDDOY", y = "Predicted HDDOY") +
  annotate("text", 
           x = min(test$mean_HDDOY, na.rm = TRUE) + 2, 
           y = max(test$pred_HDDOY, na.rm = TRUE) - 2,
           label = paste0("R² = ", round(test_r2, 2), 
                          "\nMAE = ", round(test_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")


# 8. Variable importance
varImpPlot(rf_model, main = "Random Forest Variable Importance")

reprtree:::plot.getTree(rf_model)
# attach necessary packages
library(ISLR)           # data sets
library(rpart)          # decision tree methodology
library(rpart.plot)     # decision tree visualization
library(randomForest)   # random forest methodology
library(gbm)            # boosting methodology
cat("Generating the decision tree plot for the Harvest (mean_HDDOY) model...\n")
reprtree:::plot.getTree(rf_model)
getTree(rf_model, 1, labelVar=TRUE)
prp(rf_model)
rpart.plot(salfit)
####Old harmonic equation

# -----------------------------
# Define the harmonic model function
# -----------------------------
# This function defines the harmonic model used for fitting.
harmonic_model <- function(t, c, a1, b1, a2, b2, omega = 1.5) {
  c +
    a1 * cos(2 * pi * omega * 1 * t) + b1 * sin(2 * pi * omega * 1 * t) +
    a2 * cos(2 * pi * omega * 2 * t) + b2 * sin(2 * pi * omega * 2 * t)
}

# -----------------------------
# Function to process a single data frame
# -----------------------------
# This function encapsulates the logic for fitting the harmonic model,
# calculating metrics, and generating the plot for a single field.
process_field_data <- function(df_item, index) {
  # Signal progress for each item processed
  p()
  
  # Determine field_id
  field_id <- if (!is.null(df_item$field_id[1])) as.character(df_item$field_id[1]) else paste0("Field_", index)
  
  # Fit harmonic model (assuming fit_harmonic_deines is defined elsewhere)
  # If fit_harmonic_deines is not defined, you might need to include its definition here
  # For this example, I'm using the nlsLM fit directly as in your original code.
  # If fit_harmonic_deines provides additional functionality, ensure it's available.
  # res <- fit_harmonic_deines(df_item, value_col = "kNDVI") # Uncomment if you have this function
  
  # Prepare plot data
  df_plot <- df_item %>%
    mutate(DOY = yday(Date),
           t = DOY / 365) %>%
    dplyr::filter(!is.na(kNDVI))
  
  # Skip if not enough data for fitting
  if (nrow(df_plot) < 5) { # A minimum number of points is usually required for nlsLM
    warning(paste("Skipping field", field_id, "due to insufficient data points."))
    return(NULL)
  }
  
  t <- df_plot$t
  y <- df_plot$kNDVI
  
  # Try to fit the model, handle potential errors
  fit <- tryCatch({
    nlsLM(y ~ harmonic_model(t, c, a1, b1, a2, b2),
          start = list(c = mean(y), a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1),
          control = nls.lm.control(maxiter = 500))
  }, error = function(e) {
    warning(paste("Error fitting harmonic model for", field_id, ":", e$message))
    return(NULL)
  })
  
  if (is.null(fit)) {
    return(NULL)
  }
  
  # Predicted curve
  t_seq <- seq(0, 1, length.out = 365)
  doy_seq <- round(t_seq * 365)
  pred_ndvi <- predict(fit, newdata = data.frame(t = t_seq))
  fit_df <- data.frame(DOY = doy_seq, kNDVI = pred_ndvi)
  
  # Compute metrics
  y_pred <- predict(fit, newdata = data.frame(t = t))
  R2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
  MAE <- mean(abs(y - y_pred))
  Bias <- mean(y_pred - y)
  NSE <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2) # Same as R2 formula here
  
  coefs <- coef(fit)
  coef_text <- sprintf(
    "c = %.3f\na1 = %.3f\nb1 = %.3f\na2 = %.3f\nb2 = %.3f\nR² = %.2f\nMAE = %.3f\nBias = %.3f\nNSE = %.2f",
    coefs["c"], coefs["a1"], coefs["b1"], coefs["a2"], coefs["b2"],
    R2, MAE, Bias, NSE
  )
  
  # Plot
  p_plot <- ggplot() +
    geom_point(data = df_plot, aes(x = DOY, y = kNDVI), color = "blue", alpha = 0.6) +
    geom_line(data = fit_df, aes(x = DOY, y = kNDVI), color = "red", size = 1.2) +
    labs(title = paste("kNDVI Harmonic Fit:", field_id),
         x = "DOY", y = "kNDVI") +
    annotate("text", x = 30, y = max(y, na.rm = TRUE) * 0.95,
             label = coef_text, hjust = 0, vjust = 1, size = 4) +
    theme_minimal(base_size = 14)
  
  # Save plot
  ggsave(filename = file.path(save_dir, paste0("harmonic_fit_", field_id, ".png")),
         plot = p_plot, width = 8, height = 5)
  
  # Return results for this field
  # Ensure 'res' structure matches your original 'fit_harmonic_deines' output if used
  # For now, creating a data frame with coefficients and metrics
  result_df <- as.data.frame(as.list(coefs)) %>%
    mutate(ID = field_id, R2 = R2, MAE = MAE, Bias = Bias, NSE = NSE)
  
  return(result_df)
}

# --- Tree Visualization ---
# Plot a single tree from the 'rf_model_harvest' using reprtree
# This will plot the first tree (k=1) by default.library(devtools)
# Ensure necessary packages are installed and loaded
options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree', 'ggplot2', 'dplyr', 'tidyr', 'lubridate') # Add ggplot2, dplyr, tidyr, lubridate for the user's data prep
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('munoztd0/reprtree')
}

# Load all required packages, including those for data preparation
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))
cat("Generating the decision tree plot for the Harvest (mean_HDDOY) model...\n")
reprtree:::plot.getTree(rf_model)

