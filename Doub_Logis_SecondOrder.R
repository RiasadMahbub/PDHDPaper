# Indices to evaluate for EOS optimization: 
# MLSWI26, IAVI, VARI, RNDVI, NDVI, EVI, kNDVI, ATSAVI, 
# GDVI, MBWI, TVI, NDWI, TSAVI, LSWI


##Paper: Evaluating the Consistency of Vegetation Phenological Parameters in the Northern Hemisphere from 1982 to 2015
### Paper the values have been taken from: Exploring the Use of DSCOVR/EPIC Satellite Observations to Monitor Vegetation Phenology
# 1. LOAD REQUIRED LIBRARIES
# ==============================================
library(dplyr)        # Data manipulation
library(ggplot2)      # Visualization
library(lubridate)    # Date handling
library(minpack.lm)   # Nonlinear least squares fitting
library(Metrics)      # Model evaluation metrics
library(pracma)       # Peak detection (for 2nd derivative method)
library(future.apply) # Parallel processing
library(progressr)    # Progress tracking

# --- Double logistic function
double_logistic <- function(t, v1, v2, m1, n1, m2, n2) {
  v1 + v2 * (1 / (1 + exp(-m1 * (t - n1))) - 1 / (1 + exp(-m2 * (t - n2))))
}

# --- Derivative of double logistic
double_logistic_derivative <- function(t, v2, m1, n1, m2, n2) {
  term1 <- (exp(-m1 * (t - n1)) * m1) / ((1 + exp(-m1 * (t - n1)))^2)
  term2 <- (exp(-m2 * (t - n2)) * m2) / ((1 + exp(-m2 * (t - n2)))^2)
  v2 * (term1 - term2)
}

# --- Fit function returning SOS
extract_sos_double_logistic <- function(df, vi_column = "NDVI", date_column = "Date") {
  df <- df[!is.na(df[[vi_column]]), ]
  if (nrow(df) < 10) return(NA)
  df$DOY <- yday(df[[date_column]])
  start_vals <- list(
    v1 = min(df[[vi_column]], na.rm = TRUE),
    v2 = max(df[[vi_column]], na.rm = TRUE) - min(df[[vi_column]], na.rm = TRUE),
    m1 = 0.1, n1 = 100,
    m2 = 0.1, n2 = 250
  )
  fit <- try(nlsLM(df[[vi_column]] ~ double_logistic(DOY, v1, v2, m1, n1, m2, n2),
                   data = df, start = start_vals, control = nls.lm.control(maxiter = 500)), silent = TRUE)
  if (inherits(fit, "try-error")) return(NA)
  
  params <- coef(fit)
  doy_seq <- seq(min(df$DOY), max(df$DOY), by = 1)
  derivative_vals <- double_logistic_derivative(
    t = doy_seq,
    v2 = params["v2"],
    m1 = params["m1"], n1 = params["n1"],
    m2 = params["m2"], n2 = params["n2"]
  )
  
  sos_doy <- doy_seq[which.max(derivative_vals)]
  return(sos_doy)
}

# --- Apply to all data
sos_double_results <- sapply(vi_list_gt20, function(df) extract_sos_double_logistic(df, vi_column = "NDVI", date_column = "Date"))
obs_pddoy <- sapply(vi_list_gt20, function(df) df$PDDOY[1])
valid <- !is.na(sos_double_results) & !is.na(obs_pddoy)

# --- Metrics
rmse_val <- rmse(obs_pddoy[valid], sos_double_results[valid])
mae_val  <- mae(obs_pddoy[valid], sos_double_results[valid])
r2_val   <- summary(lm(sos_double_results[valid] ~ obs_pddoy[valid]))$r.squared
bias_val <- mean(sos_double_results[valid] - obs_pddoy[valid])

# --- Output metrics
cat("Double Logistic Method Results:\n")
cat("RMSE:", rmse_val, "\n")
cat("MAE:", mae_val, "\n")
cat("R²:", r2_val, "\n")
cat("Bias:", bias_val, "\n")

# --- Plot: Observed vs Predicted
sos_df <- data.frame(
  observed_SOS = obs_pddoy[valid],
  predicted_SOS = sos_double_results[valid]
)

ggplot(sos_df, aes(x = observed_SOS, y = predicted_SOS)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", col = "red", linetype = "dashed") +
  annotate("text",
           x = max(sos_df$observed_SOS),
           y = min(sos_df$predicted_SOS),
           label = paste0("R² = ", round(r2_val, 3),
                          "\nRMSE = ", round(rmse_val, 2),
                          "\nMAE = ", round(mae_val, 2)),
           hjust = 1, vjust = 0, size = 4, color = "blue") +
  labs(
    title = "Observed vs Predicted SOS (Double Logistic)",
    x = "Observed Planting Date (DOY)",
    y = "Predicted SOS (DOY)"
  ) +
  theme_minimal(base_size = 14)



#=======================================================================
#DOUBLE LOGISTICS WITH SECOND ORDER DERIVATIVE
#=======================================================================
extract_phenology_points <- function(df, id = NULL, plot_path = NULL) {
  library(ggplot2)
  library(lubridate)
  library(minpack.lm)
  library(pracma)
  
  # Ensure proper Date and DOY
  df$Date <- as.POSIXct(df$Date, tz = "UTC")
  df$DOY <- yday(df$Date)
  df <- df[order(df$DOY), ]
  doy <- df$DOY
  ndvi <- df$IAVI
  t <- doy
  y <- ndvi
  fit_data <- data.frame(t = t, y = y)
  
  # Double logistic function
  double_logistic <- function(t, a, b, c, d, e, f) {
    a + b * (1 / (1 + exp(c * (t - d))) + 1 / (1 + exp(e * (t - f))))
  }
  
  # Starting parameters
  start_params <- list(
    a = min(y),
    b = max(y) - min(y),
    c = -0.05,
    d = 100,
    e = 0.05,
    f = 250
  )
  
  tryCatch({
    # Fit model
    fit <- nlsLM(y ~ double_logistic(t, a, b, c, d, e, f),
                 data = fit_data,
                 start = start_params,
                 control = nls.lm.control(maxiter = 500))
    
    # Predict NDVI for each DOY
    t_seq <- seq(1, 365, by = 1)
    pred <- predict(fit, newdata = data.frame(t = t_seq))
    
    # 2nd derivative
    second_deriv <- diff(pred, differences = 2)
    t_deriv <- t_seq[-c(1, length(t_seq))]
    
    # Identify peaks
    peaks <- findpeaks(second_deriv, sortstr = TRUE)
    peak_positions <- t_deriv[peaks[, 2]]
    peak_values <- peaks[, 1]
    peak_info <- data.frame(DOY = peak_positions, Value = peak_values)
    first_half <- peak_info[peak_info$DOY <= 180, ]
    second_half <- peak_info[peak_info$DOY > 180, ]
    
    # Phenology points
    sos <- first_half$DOY[which.max(first_half$Value)]
    maturity <- first_half$DOY[order(first_half$Value, decreasing = TRUE)[2]]
    senescence <- second_half$DOY[order(second_half$Value, decreasing = TRUE)[2]]
    eos <- second_half$DOY[which.max(second_half$Value)]
    
    # Normalize 2nd derivative for plotting
    second_deriv_scaled <- scales::rescale(second_deriv, to = range(pred, na.rm = TRUE))
    
    # Create dataframes for plotting
    pred_df <- data.frame(DOY = t_seq, NDVI = pred)
    deriv_df <- data.frame(DOY = t_deriv, SecondDerivative = second_deriv_scaled)
    
    # Plot
    p <- ggplot() +
      geom_point(data = df, aes(x = DOY, y = NDVI), color = "black", alpha = 0.6) +
      geom_line(data = pred_df, aes(x = DOY, y = NDVI), color = "blue", size = 1) +
      geom_line(data = deriv_df, aes(x = DOY, y = SecondDerivative), color = "purple", linetype = "dotted") +
      geom_vline(xintercept = c(sos, maturity, senescence, eos),
                 linetype = "dashed",
                 color = c("green", "darkgreen", "orange", "red")) +
      annotate("text", x = c(sos, maturity, senescence, eos),
               y = max(df$NDVI, na.rm = TRUE),
               label = c("SOS", "Maturity", "Senescence", "EOS"),
               vjust = -1, hjust = 0.5) +
      labs(title = paste("NDVI Fit + Phenology -", id),
           x = "Day of Year (DOY)", y = "NDVI") +
      theme_minimal()
    
    if (!is.null(plot_path)) {
      ggsave(file.path(plot_path, paste0("Phenology_", id, ".jpg")),
             plot = p, width = 7, height = 5, dpi = 300)
    }
    
    return(data.frame(ID = id,
                      SOS = sos,
                      Maturity = maturity,
                      Senescence = senescence,
                      EOS = eos))
    
  }, error = function(e) {
    return(data.frame(ID = id, SOS = NA, Maturity = NA, Senescence = NA, EOS = NA))
  })
}

#First, extract the field names from the filenames
field_names <- gsub("_\\d{4}\\.csv$", "", basename(vi_csv_files_gt20))



# Set up parallel plan (adjust workers as needed)
plan(multisession, workers = 4)

# Run in parallel without plotting
results <- future_lapply(seq_along(vi_list_gt20), function(i) {
  extract_phenology_points(vi_list_gt20[[i]],
                           id = field_names[i],
                           plot = FALSE)  # Disable plotting
})

# Combine results
phenology_df <- do.call(rbind, results)
phenology_df
# Create a joint mask where both variables are not NA
valid_mask <- !is.na(obs_pddoy) & !is.na(phenology_df$SOS)

# Apply the mask to both vectors
obs_valid <- obs_pddoy[valid_mask]
sos_valid <- phenology_df$SOS[valid_mask]
#------------------------------------------------------------------
#SUMMARY TABLE MODEL COMPARISON
#------------------------------------------------------------------
rmse_val2 <- round(rmse(obs_valid, sos_valid), 2)
mae_val2  <- round(mae(obs_valid, sos_valid), 2)
r2_val2   <- round(summary(lm(sos_valid ~ obs_valid))$r.squared, 2)
bias_val2 <- round(mean(sos_valid - obs_valid), 2)

double_logistic_second_summary <- data.frame(
  Model = "Double Logistic - Second Derivative",
  RMSE  = rmse_val2,
  MAE   = mae_val2,
  R2    = r2_val2,
  Bias  = bias_val2
)

dtmdldeines_summary_df <- bind_rows(dtmdldeines_summary_df, double_logistic_second_summary)
dtmdldeines_summary_df


#=============================================================
#Save the fitting figure 
#=============================================================
# Run in parallel
results <- future_lapply(seq_along(vi_list_gt20), function(i) {
  extract_phenology_points(vi_list_gt20[[i]],
                           id = field_names[i],
                           plot_path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DoubleLogistics/Second")
})

