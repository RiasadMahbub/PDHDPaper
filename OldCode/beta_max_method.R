library(minpack.lm)
library(Metrics)
library(ggplot2)
library(dplyr)
library(lubridate)

# Double logistic function
double_logistic <- function(t, a, b, c, d, e, f) {
  a + b * (1 / (1 + exp(c * (t - d))) + 1 / (1 + exp(e * (t - f))))
}

# Method 1: Use second derivative for SOS and EOS detection
get_sos_eos_second_deriv <- function(df, vi_column = "kNDVI", date_column = "Date") {
  df <- df[!is.na(df[[vi_column]]), ]
  if (nrow(df) < 6) return(c(NA, NA))
  
  doy <- yday(df[[date_column]])
  ndvi <- df[[vi_column]]
  
  tryCatch({
    fit <- nlsLM(
      ndvi ~ double_logistic(doy, a, b, c, d, e, f),
      start = list(
        a = min(ndvi), b = max(ndvi) - min(ndvi),
        c = -0.1, d = 100,
        e = 0.1, f = 250
      ),
      control = nls.lm.control(maxiter = 500)
    )
    
    doy_pred <- 1:365
    fit_vals <- predict(fit, newdata = data.frame(doy = doy_pred))
    
    # First and second derivatives
    first_deriv <- c(NA, diff(fit_vals))
    second_deriv <- c(NA, diff(first_deriv))
    
    # Local maxima in second derivative
    local_maxima <- function(x) {
      which(diff(sign(diff(x))) == -2) + 1
    }
    
    peaks <- local_maxima(second_deriv)
    if (length(peaks) < 4) return(c(NA, NA))
    
    # First half (e.g., < 200), Second half (>= 200)
    first_half <- peaks[peaks < 200]
    second_half <- peaks[peaks >= 200]
    
    if (length(first_half) < 1 || length(second_half) < 1) return(c(NA, NA))
    
    sos <- doy_pred[first_half[1]]
    eos <- doy_pred[second_half[length(second_half)]]
    
    return(c(sos, eos))
  }, error = function(e) return(c(NA, NA)))
}

# Method 2: Use first derivative max/min
get_sos_eos_first_deriv <- function(df, vi_column = "kNDVI", date_column = "Date") {
  df <- df[!is.na(df[[vi_column]]), ]
  if (nrow(df) < 6) return(c(NA, NA))
  
  doy <- yday(df[[date_column]])
  ndvi <- df[[vi_column]]
  
  tryCatch({
    fit <- nlsLM(
      ndvi ~ double_logistic(doy, a, b, c, d, e, f),
      start = list(
        a = min(ndvi), b = max(ndvi) - min(ndvi),
        c = -0.1, d = 100,
        e = 0.1, f = 250
      ),
      control = nls.lm.control(maxiter = 500)
    )
    
    doy_pred <- 1:365
    fit_vals <- predict(fit, newdata = data.frame(doy = doy_pred))
    first_deriv <- c(NA, diff(fit_vals))
    
    sos <- doy_pred[which.max(first_deriv)]
    eos <- doy_pred[which.min(first_deriv)]
    
    return(c(sos, eos))
  }, error = function(e) return(c(NA, NA)))
}

# Apply both methods
results_method1 <- t(sapply(vi_list_gt20, get_sos_eos_second_deriv))
results_method2 <- t(sapply(vi_list_gt20, get_sos_eos_first_deriv))

colnames(results_method1) <- c("SOS_2nd", "EOS_2nd")
colnames(results_method2) <- c("SOS_1st", "EOS_1st")

# Get observed DOYs
obs_plant <- sapply(vi_list_gt20, function(df) df$PDDOY[1])
obs_harvest <- sapply(vi_list_gt20, function(df) df$HDDOY[1])

# Combine
phen_df <- data.frame(
  PDDOY = obs_plant,
  HDDOY = obs_harvest,
  SOS_2nd = results_method1[, "SOS_2nd"],
  EOS_2nd = results_method1[, "EOS_2nd"],
  SOS_1st = results_method2[, "SOS_1st"],
  EOS_1st = results_method2[, "EOS_1st"]
)

# Filter valid rows
valid1 <- complete.cases(phen_df[, c("PDDOY", "SOS_2nd")])
valid2 <- complete.cases(phen_df[, c("PDDOY", "SOS_1st")])

# Evaluation function
evaluate_method <- function(obs, pred) {
  data.frame(
    RMSE = rmse(obs, pred),
    MAE = mae(obs, pred),
    Bias = mean(pred - obs),
    R2 = summary(lm(pred ~ obs))$r.squared
  )
}

# Metrics
metrics_1 <- evaluate_method(phen_df$PDDOY[valid1], phen_df$SOS_2nd[valid1])
metrics_2 <- evaluate_method(phen_df$PDDOY[valid2], phen_df$SOS_1st[valid2])

# Print metrics
cat("Method 1 (Second Derivative):\n"); print(metrics_1)
cat("\nMethod 2 (First Derivative):\n"); print(metrics_2)

# Plot comparison
plot_comparison <- function(df, obs_col, pred_col, method_name) {
  ggplot(df, aes_string(x = obs_col, y = pred_col)) +
    geom_point(color = "darkgreen") +
    geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
    labs(
      title = paste("Observed vs Predicted SOS -", method_name),
      x = "Observed Planting Date (DOY)",
      y = "Predicted SOS (DOY)"
    ) +
    theme_minimal()
}

plot1 <- plot_comparison(phen_df[valid1, ], "PDDOY", "SOS_2nd", "Method 1 (2nd Deriv)")
plot2 <- plot_comparison(phen_df[valid2, ], "PDDOY", "SOS_1st", "Method 2 (1st Deriv)")

# Display plots
print(plot1)
print(plot2)


