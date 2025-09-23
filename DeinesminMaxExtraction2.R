# Load necessary libraries.
# The 'dplyr', 'minpack.lm', and 'ggplot2' libraries are required for this script to run.
# You may also need 'lubridate' for the yday() function.
library(dplyr)
library(minpack.lm)
library(lubridate) # For yday() function
library(ggplot2) # For plotting

#' Fits a harmonic model to time-series data and extracts key phenological dates.
#'
#' This function takes a data frame with 'Date' and a value column (e.g., 'kNDVI')
#' and fits a harmonic regression model. It then extracts the main peak, the
#' local minimum before the peak, and the local maximum before the minimum for
#' both the observed and fitted data.
#'
#' @param df A data frame with a 'Date' column and a column for the value to fit.
#' @param value_col A string specifying the name of the column with the values.
#' @return A data frame with summary metrics and phenological dates, or NULL if
#'         the model fitting fails or there is insufficient data.
#' Fit harmonic model with flexible omega selection
fit_harmonic_deines <- function(df, value_col = "kNDVI", omega_candidates = c(1, 1.2, 1.5)) {
  
  # Prepare data
  df <- df %>%
    mutate(DOY = yday(Date), t = DOY / 365) %>%
    dplyr::filter(!is.na(.data[[value_col]]))
  
  if (nrow(df) < 10) return(NULL)
  
  t <- df$t
  y <- df[[value_col]]
  
  best_fit <- NULL
  best_R2 <- -Inf
  best_omega <- NA
  
  # Try different omega values
  for (omega in omega_candidates) {
    
    harmonic_model <- function(t, c, a1, b1, a2, b2) {
      c +
        a1 * cos(2 * pi * omega * t) + b1 * sin(2 * pi * omega * t) +
        a2 * cos(2 * pi * omega * 2 * t) + b2 * sin(2 * pi * omega * 2 * t)
    }
    
    fit <- tryCatch({
      nlsLM(
        y ~ harmonic_model(t, c, a1, b1, a2, b2),
        start = list(
          c = mean(y),
          a1 = (max(y) - min(y)) / 2,
          b1 = 0,
          a2 = (max(y) - min(y)) / 4,
          b2 = 0
        ),
        control = nls.lm.control(maxiter = 500)
      )
    }, error = function(e) NULL)
    
    if (!is.null(fit)) {
      y_pred <- predict(fit)
      R2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
      if (R2 > best_R2) {
        best_R2 <- R2
        best_fit <- fit
        best_omega <- omega
      }
    }
  }
  
  if (is.null(best_fit)) return(NULL)
  
  coefs <- coef(best_fit)
  t_seq <- seq(0, 1, length.out = 365)
  doy_seq <- round(t_seq * 365)
  pred <- predict(best_fit, newdata = data.frame(t = t_seq))
  
  max_idx <- which.max(pred)
  doy_max_fit <- doy_seq[max_idx]
  val_max_fit <- pred[max_idx]
  
  doy_max_obs <- df$DOY[which.max(y)]
  val_max_obs <- max(y)
  
  # local minima & maxima before peak
  doy_min_fit <- val_min_fit <- doy_max_before_min_fit <- val_max_before_min_fit <- NA
  for (i in (max_idx - 1):2) {
    if (pred[i] < pred[i+1] && pred[i] < pred[i-1]) {
      doy_min_fit <- doy_seq[i]
      val_min_fit <- pred[i]
      for (j in (i - 1):2) {
        if (pred[j] > pred[j+1] && pred[j] > pred[j-1]) {
          doy_max_before_min_fit <- doy_seq[j]
          val_max_before_min_fit <- pred[j]
          break
        }
      }
      break
    }
  }
  
  doy_min_obs <- val_min_obs <- doy_max_before_min_obs <- val_max_before_min_obs <- NA
  obs_max_idx <- which.max(y)
  for (i in (obs_max_idx - 1):2) {
    if (y[i] < y[i+1] && y[i] < y[i-1]) {
      doy_min_obs <- df$DOY[i]
      val_min_obs <- y[i]
      for (j in (i - 1):2) {
        if (y[j] > y[j+1] && y[j] > y[j-1]) {
          doy_max_before_min_obs <- df$DOY[j]
          val_max_before_min_obs <- y[j]
          break
        }
      }
      break
    }
  }
  
  PDDOY_val <- if ("PDDOY" %in% names(df)) unique(df$PDDOY) else NA
  HDDOY_val <- if ("HDDOY" %in% names(df)) unique(df$HDDOY) else NA
  
  data.frame(
    DOY_max_obs = doy_max_obs,
    Value_max_obs = val_max_obs,
    DOY_min_obs = doy_min_obs,
    Value_min_obs = val_min_obs,
    DOY_max_before_min_obs = doy_max_before_min_obs,
    Value_max_before_min_obs = val_max_before_min_obs,
    DOY_max_fit = doy_max_fit,
    Value_max_fit = val_max_fit,
    DOY_min_fit = doy_min_fit,
    Value_min_fit = val_min_fit,
    DOY_max_before_min_fit = doy_max_before_min_fit,
    Value_max_before_min_fit = val_max_before_min_fit,
    Intercept_c = coefs["c"],
    a1 = coefs["a1"], b1 = coefs["b1"],
    a2 = coefs["a2"], b2 = coefs["b2"],
    R2 = best_R2,
    PDDOY = PDDOY_val,
    HDDOY = HDDOY_val,
    Omega = best_omega
  )
}

# -----------------------------
# Example usage and plotting
# -----------------------------
save_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/Harmonic_Deines"
dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

results_list <- list()
for (i in seq_along(vi_list_gt20)) {
  df <- vi_list_gt20[[i]]
  field_id <- names(vi_list_gt20)[i]
  
  res <- fit_harmonic_deines(df, value_col = "kNDVI")
  if (!is.null(res)) {
    res$ID <- field_id
    results_list[[field_id]] <- res
    
    # Prepare observed data for plotting
    df_plot <- df %>% mutate(DOY = yday(Date)) %>% dplyr::filter(!is.na(kNDVI))
    
    # Compute fitted values directly using the best omega and coefficients
    DOY_seq <- seq(1, 365)
    fit_values <- res$Intercept_c +
      res$a1 * cos(2 * pi * res$Omega * DOY_seq / 365) +
      res$b1 * sin(2 * pi * res$Omega * DOY_seq / 365) +
      res$a2 * cos(4 * pi * res$Omega * DOY_seq / 365) +
      res$b2 * sin(4 * pi * res$Omega * DOY_seq / 365)
    
    fit_df <- data.frame(DOY = DOY_seq, kNDVI = fit_values)
    
    # Plot
    p <- ggplot() +
      geom_point(data = df_plot, aes(x = DOY, y = kNDVI), color = "blue", alpha = 0.6) +
      geom_line(data = fit_df, aes(x = DOY, y = kNDVI), color = "red", size = 1.2) +
      labs(title = paste("kNDVI Harmonic Fit:", field_id),
           x = "DOY", y = "kNDVI") +
      theme_minimal()
    
    # Save the plot
    ggsave(filename = file.path(save_dir, paste0(field_id, ".jpeg")),
           plot = p, width = 8, height = 5)
  }
}
# This is an empty list to store your results
results_list <- list()

# This loop iterates through your list of data frames
for (i in seq_along(vi_list_gt20)) {
  # Get the current data frame and its field ID
  df <- vi_list_gt20[[i]]
  field_id <- names(vi_list_gt20)[i]
  
  # Attempt to fit the harmonic Deines model
  res <- fit_harmonic_deines(df, value_col = "kNDVI")
  
  # Check if the model fit was successful
  if (!is.null(res)) {
    # Store the results with the field ID
    res$ID <- field_id
    results_list[[field_id]] <- res
    
    # Prepare the observed data for plotting
    df_plot <- df %>% 
      mutate(DOY = yday(Date)) %>% 
      dplyr::filter(!is.na(kNDVI))
    
    # Compute the fitted values for the full year (DOY 1 to 365)
    # This uses the coefficients from your successful model fit
    fit_df <- data.frame(DOY = seq(1, 365))
    fit_df$kNDVI <- res$Intercept_c +
      res$a1 * cos(2 * pi * res$Omega * fit_df$DOY / 365) +
      res$b1 * sin(2 * pi * res$Omega * fit_df$DOY / 365) +
      res$a2 * cos(4 * pi * res$Omega * fit_df$DOY / 365) +
      res$b2 * sin(4 * pi * res$Omega * fit_df$DOY / 365)
    
    # Compute metrics (R², MAE, Bias) for the observed data points
    y_pred <- res$Intercept_c +
      res$a1 * cos(2 * pi * res$Omega * df_plot$DOY / 365) +
      res$b1 * sin(2 * pi * res$Omega * df_plot$DOY / 365) +
      res$a2 * cos(4 * pi * res$Omega * df_plot$DOY / 365) +
      res$b2 * sin(4 * pi * res$Omega * df_plot$DOY / 365)
    
    y <- df_plot$kNDVI
    R2 <- 1 - sum((y - y_pred)^2, na.rm = TRUE) / sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
    MAE <- mean(abs(y - y_pred), na.rm = TRUE)
    Bias <- mean(y_pred - y, na.rm = TRUE)
    NSE <- R2 # NSE is equivalent to R² for this model
    
    # Create the text for the plot's annotation
    coef_text <- sprintf(
      "c = %.3f\na1 = %.3f\nb1 = %.3f\na2 = %.3f\nb2 = %.3f\nR² = %.2f\nMAE = %.3f\nBias = %.3f\nNSE = %.2f",
      res$Intercept_c, res$a1, res$b1, res$a2, res$b2,
      R2, MAE, Bias, NSE
    )
    
    # Prepare data for vertical phenology lines
    legend_data <- data.frame(
      DOY = c(res$DOY_max_fit, res$DOY_min_fit, res$DOY_max_before_min_fit),
      Label = c("Global Maxima", "Local Minima before Max", "Local Maxima before Min")
    )
    if (!is.na(res$PDDOY)) {
      legend_data <- rbind(legend_data, data.frame(DOY = res$PDDOY, Label = "Planting Date"))
    }
    if (!is.na(res$HDDOY)) {
      legend_data <- rbind(legend_data, data.frame(DOY = res$HDDOY, Label = "Harvesting Date"))
    }
    
    # Define colors and linetypes
    colors <- c("Global Maxima" = "red",
                "Local Minima before Max" = "orange",
                "Local Maxima before Min" = "green",
                "Planting Date" = "purple",
                "Harvesting Date" = "brown")
    linetypes <- c("Global Maxima" = "solid",
                   "Local Minima before Max" = "dashed",
                   "Local Maxima before Min" = "dashed",
                   "Planting Date" = "dotted",
                   "Harvesting Date" = "dotted")
    
    # # Plot the data, fitted curve, and phenology dates
    # p <- ggplot() +
    #   geom_point(data = df_plot, aes(x = DOY, y = kNDVI), color = "blue", alpha = 0.6) +
    #   geom_line(data = fit_df, aes(x = DOY, y = kNDVI), color = "red", size = 1.2) +
    #   geom_vline(data = legend_data, aes(xintercept = DOY, color = Label, linetype = Label), size = 1, alpha = 0.7) +
    #   annotate("text", x = 30, y = max(y, na.rm = TRUE) * 0.95, label = coef_text, hjust = 0, vjust = 1, size = 4) +
    #   scale_color_manual(values = colors) +
    #   scale_linetype_manual(values = linetypes) +
    #   labs(title = paste("kNDVI Harmonic Fit:", field_id), x = "DOY", y = "kNDVI") +
    #   theme_minimal(base_size = 9.5) +
    #   theme(legend.position = c(0.99, 0.98), legend.justification = c(1, 1),
    #         legend.background = element_rect(fill="white", linetype="solid", color="black"))
    # 
    # # Save the plot
    # ggsave(filename = file.path(save_dir, paste0(field_id, ".jpeg")), plot = p, width = 8, height = 5)
  }
} # This is the missing bracket that closes the for loop.


# The final result data frame is created outside the loop
deinesharmonicminmaxdf <- bind_rows(results_list, .id = "Field_ID")
deinesharmonicminmaxdf$Field_Year<-deinesharmonicminmaxdf$Field_ID
deinesharmonicminmaxdf$Laglocalmaxglomax <- deinesharmonicminmaxdf$DOY_max_fit - deinesharmonicminmaxdf$DOY_max_before_min_fit
deinesharmonicminmaxdf$Laglocalminglomax <- deinesharmonicminmaxdf$DOY_max_fit - deinesharmonicminmaxdf$DOY_max_before_min_fit
deinesharmonicminmaxdf$Laglocalmaxlocalmin <- deinesharmonicminmaxdf$DOY_min_fit - deinesharmonicminmaxdf$DOY_max_before_min_fit
mean_val <- mean(deinesharmonicminmaxdf$DOY_min_fit[deinesharmonicminmaxdf$DOY_min_fit >= 60], na.rm = TRUE)
