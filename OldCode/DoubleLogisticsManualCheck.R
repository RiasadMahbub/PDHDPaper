extract_sos_eos_second_derivative <- function(df, id = NULL, out_dir = NULL, vi_column = "NDVI") {
  library(ggplot2)
  library(lubridate)
  library(minpack.lm)
  library(scales)
  
  df$Date <- as.POSIXct(df$Date, tz = "UTC")
  df$DOY <- yday(df$Date)
  df <- df[!is.na(df[[vi_column]]), ]
  
  if (nrow(df) < 10) {
    warning(paste("Insufficient data for field", id))
    return(data.frame(ID = id, SOS = NA, Maturity = NA, Senescence = NA, EOS = NA, 
                      a = NA, b = NA, c = NA, d = NA, e = NA, f = NA,
                      R2 = NA, RMSE = NA, MAE = NA, NSE = NA))
  }
  
  t <- df$DOY
  y <- df[[vi_column]]
  fit_data <- data.frame(t = t, y = y)
  
  double_logistic <- function(t, a, b, c, d, e, f) {
    a + b * (1 / (1 + exp(c * (t - d))) + 1 / (1 + exp(e * (t - f))))
  }
  
  start_params_list <- list(
    list(a = min(y), b = max(y) - min(y), c = -0.05, d = median(t), e = 0.05, f = 250),
    list(a = min(y), b = max(y) - min(y), c = -0.05, d = 100, e = 0.05, f = 250),
    list(a = min(y), b = max(y) - min(y), c = -0.05, d = 150, e = 0.05, f = 250),
    list(a = mean(y) - sd(y), b = 2 * sd(y), c = -0.1, d = 200, e = 0.1, f = 280)
  )
  
  best_fit <- NULL
  best_r2 <- -Inf
  
  for (start_params in start_params_list) {
    current_fit <- tryCatch({
      fit <- nlsLM(y ~ double_logistic(t, a, b, c, d, e, f),
                   data = fit_data,
                   start = start_params,
                   control = nls.lm.control(maxiter = 500))
      y_pred <- predict(fit)
      current_r2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
      if (current_r2 > best_r2) {
        best_r2 <- current_r2
        best_fit <- fit
      }
      fit
    }, error = function(e) NULL)
  }
  
  if (is.null(best_fit)) {
    warning(paste("All fitting attempts failed for field", id))
    return(data.frame(ID = id, SOS = NA, Maturity = NA, Senescence = NA, EOS = NA,
                      a = NA, b = NA, c = NA, d = NA, e = NA, f = NA,
                      R2 = NA, RMSE = NA, MAE = NA, NSE = NA))
  }
  
  t_seq <- seq(min(t), max(t), by = 1)
  pred <- predict(best_fit, newdata = data.frame(t = t_seq))
  second_derivative <- rep(NA, length(pred))
  second_derivative[2:(length(pred)-1)] <- pred[3:length(pred)] - 2 * pred[2:(length(pred)-1)] + pred[1:(length(pred)-2)]
  t_mid <- t_seq[2:(length(t_seq)-1)]
  sd_vals <- second_derivative[2:(length(pred)-1)]
  
  vi_peak_doy <- t_seq[which.max(pred)]
  first_half <- t_mid < vi_peak_doy
  second_half <- t_mid > vi_peak_doy
  
  find_local_maxima <- function(x, t) {
    idx <- which(diff(sign(diff(x))) == -2) + 1
    t[idx]
  }
  
  find_local_minima <- function(x, t) {
    idx <- which(diff(sign(diff(x))) == 2) + 1
    t[idx]
  }
  
  sos_candidates <- find_local_maxima(sd_vals[first_half], t_mid[first_half])
  eos_candidates <- find_local_minima(sd_vals[second_half], t_mid[second_half])
  
  sos_doy <- if (length(sos_candidates) >= 1) sos_candidates[1] else NA
  maturity_doy <- if (length(sos_candidates) >= 2) sos_candidates[2] else NA
  senescence_doy <- if (length(eos_candidates) >= 1) eos_candidates[1] else NA
  eos_doy <- if (length(eos_candidates) >= 2) eos_candidates[2] else NA
  
  y_pred <- predict(best_fit)
  R2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
  RMSE <- sqrt(mean((y - y_pred)^2))
  MAE <- mean(abs(y - y_pred))
  NSE <- R2
  
  if (!is.null(out_dir)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    df_plot <- data.frame(DOY = t, VI = y)
    pred_df <- data.frame(DOY = t_seq, VI = pred)
    deriv_df <- data.frame(DOY = t_mid, SecondDerivative = scales::rescale(sd_vals, to = range(pred)))
    
    coef_vals <- coef(best_fit)
    coef_text <- paste0(
      "Coefficients:\n",
      "a = ", round(coef_vals["a"], 3), "\n",
      "b = ", round(coef_vals["b"], 3), "\n",
      "c = ", round(coef_vals["c"], 3), "\n",
      "d = ", round(coef_vals["d"], 1), "\n",
      "e = ", round(coef_vals["e"], 3), "\n",
      "f = ", round(coef_vals["f"], 1)
    )
    
    metrics_text <- paste0(
      "R² = ", round(R2, 3), "\n",
      "RMSE = ", round(RMSE, 3), "\n",
      "MAE = ", round(MAE, 3), "\n",
      "NSE = ", round(NSE, 3)
    )
    
    transition_lines <- na.omit(c(sos_doy, maturity_doy, senescence_doy, eos_doy))
    transition_colors <- c("green", "darkgreen", "orange", "red")[!is.na(c(sos_doy, maturity_doy, senescence_doy, eos_doy))]
    
    p <- ggplot() +
      geom_point(data = df_plot, aes(x = DOY, y = VI), color = "black", alpha = 0.6) +
      geom_line(data = pred_df, aes(x = DOY, y = VI), color = "blue", linewidth = 1) +
      geom_line(data = deriv_df, aes(x = DOY, y = SecondDerivative), color = "purple", linetype = "dotted") +
      geom_vline(xintercept = transition_lines, linetype = "dashed", color = transition_colors) +
      labs(title = paste("Phenology for", id), x = "DOY", y = vi_column) +
      theme_minimal()
    
    if (!is.na(sos_doy))       p <- p + annotate("text", x = sos_doy,       y = max(y), label = "SOS",       color = "green",     vjust = -1)
    if (!is.na(maturity_doy))  p <- p + annotate("text", x = maturity_doy,  y = max(y), label = "Maturity",  color = "darkgreen", vjust = -1)
    if (!is.na(senescence_doy))p <- p + annotate("text", x = senescence_doy,y = max(y), label = "Senescence",color = "orange",    vjust = -1)
    if (!is.na(eos_doy))       p <- p + annotate("text", x = eos_doy,       y = max(y), label = "EOS",       color = "red",       vjust = -1)
    
    p <- p +
      annotate("text", x = min(t_seq), y = max(y) * 0.85, label = coef_text, hjust = 0, size = 3, color = "darkblue") +
      annotate("text", x = min(t_seq), y = max(y) * 0.55, label = metrics_text, hjust = 0, size = 3, color = "darkred")
    
    ggsave(filename = file.path(out_dir, paste0("Phenology_", id, ".jpeg")),
           plot = p, width = 7, height = 5, dpi = 300)
  }
  
  coefs <- coef(best_fit)
  return(data.frame(
    ID = id,
    SOS = sos_doy,
    Maturity = maturity_doy,
    Senescence = senescence_doy,
    EOS = eos_doy,
    a = coefs["a"], b = coefs["b"], c = coefs["c"],
    d = coefs["d"], e = coefs["e"], f = coefs["f"],
    R2 = R2, RMSE = RMSE, MAE = MAE, NSE = NSE
  ))
}





out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DoubleLogistics/Second"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

if (is.null(names(vi_list_gt20))) {
  names(vi_list_gt20) <- paste0("Field_", seq_along(vi_list_gt20))
}

results_list <- Map(function(df, id) {
  message("Processing: ", id)
  extract_sos_eos_second_derivative(df, id = id, out_dir = out_dir, vi_column = "NDVI")
}, vi_list_gt20, names(vi_list_gt20))

final_results <- do.call(rbind, results_list)
print(final_results)


# install.packages("remotes")
remotes::install_github("eco-hydro/phenofit")

