##Paper: Evaluating the Consistency of Vegetation Phenological Parameters in the Northern Hemisphere from 1982 to 2015
# Load necessary libraries
library(zoo)
library(dplyr)
library(lubridate)
library(minpack.lm)
# Required libraries
library(minpack.lm)  # for nlsLM
library(dplyr)
library(Metrics)
# 
# # Apply DOY filtering to each element in vi_list_gt20
# vi_list_gt20 <- lapply(vi_list_gt20, function(df) {
#   df$Date <- as.POSIXct(df$Date, tz = "UTC")  # Ensure proper date format
#   df$DOY <- lubridate::yday(df$Date)           # Compute DOY
#   # Filter rows where DOY is between 60 and 315
#   df <- df[df$DOY > 60 & df$DOY < 315, ]
#   return(df)
# })

# Define double logistic function if not already defined
double_logistic <- function(DOY, a, b, c, d, e, f) {
  a / (1 + exp((b - DOY) / c)) + e / (1 + exp((DOY - d) / f))
}

# Ensure DOY is added to each dataframe in the list
vi_list_gt20 <- lapply(vi_list_gt20, function(df) {
  df$Date <- as.POSIXct(df$Date, tz = "UTC")  # Ensure POSIXct format
  df$DOY <- yday(df$Date)                     # Extract DOY
  return(df)
})

# Check DOY of the first element (for validation/debug)
vi_list_gt20[[1]]$DOY 


# Step 1: Data prep
ndvi <- vi_list_gt20[[372]]$NDVI
doy <- vi_list_gt20[[372]]$DOY
df <- data.frame(t = doy, y = ndvi)

# Step 2: Define double logistic function
double_logistic <- function(t, a, b, c, d, e, f) {
  a + b * (1 / (1 + exp(c * (t - d))) + 1 / (1 + exp(e * (t - f))))
}

# Step 3: Fit the model using nlsLM
fit <- nlsLM(y ~ double_logistic(t, a, b, c, d, e, f),
             data = df,
             start = list(a = min(ndvi), 
                          b = max(ndvi) - min(ndvi), 
                          c = -0.05, 
                          d = 160, 
                          e = 0.05, 
                          f = 250),
             control = nls.lm.control(maxiter = 500))


# Step 4: Predict smooth NDVI and compute derivative
t_seq <- seq(min(doy), max(doy), by = 1)
pred_ndvi <- predict(fit, newdata = data.frame(t = t_seq))

# Numerical derivative using diff
dy_dt <- diff(pred_ndvi) / diff(t_seq)
t_mid <- t_seq[-1]  # since diff reduces length by 1

# Step 5: Identify SOS and EOS
sos_doy <- t_mid[which.max(dy_dt)]
eos_doy <- t_mid[which.min(dy_dt)]

# Output
cat("SOS (Start of Season) DOY:", sos_doy, "\n")
cat("EOS (End of Season) DOY:", eos_doy, "\n")

# Optional: Plot
plot(doy, ndvi, pch = 16, col = "blue", main = "NDVI and Fitted Curve")
lines(t_seq, pred_ndvi, col = "darkgreen", lwd = 2)
abline(v = sos_doy, col = "red", lty = 2)
abline(v = eos_doy, col = "purple", lty = 2)
legend("topright", legend = c("SOS", "EOS"),
       col = c("red", "purple"), lty = 2, bty = "n")




####################################################################
#############APPLY the FUNCTION ACROSS DIFFERENT DATSET#############
####################################################################

# Define double logistic function
double_logistic <- function(t, a, b, c, d, e, f) {
  a + b * (1 / (1 + exp(c * (t - d))) + 1 / (1 + exp(e * (t - f))))
}

# Function to extract SOS and EOS from one dataframe

extract_sos_eos <- function(df) {
  # Ensure Date and DOY columns
  df$Date <- as.POSIXct(df$Date, tz = "UTC")
  df$DOY <- yday(df$Date)
  ndvi <- df$kNDVI
  doy <- df$DOY
  t <- doy
  y <- ndvi
  
  # Create data frame for fitting
  fit_data <- data.frame(t = t, y = y)
  
  # Define four different sets of starting parameters for 'd'
  start_params_list <- list(
    list(a = min(ndvi), 
         b = max(ndvi) - min(ndvi), 
         c = -0.05, 
         d = median(doy),  # central season
         e = 0.05, 
         f = 250),

    list(a = min(ndvi), 
         b = max(ndvi) - min(ndvi), 
         c = -0.05, 
         d = 25,           # early season
         e = 0.05, 
         f = 250),
    
    list(a = min(ndvi), 
         b = max(ndvi) - min(ndvi), 
         c = -0.05, 
         d = 100,          # moderate early season
         e = 0.05, 
         f = 250),
    
    list(a = min(ndvi), 
         b = max(ndvi) - min(ndvi), 
         c = -0.05, 
         d = 120,          # late early season
         e = 0.05, 
         f = 250),
    list(a = min(ndvi), 
         b = max(ndvi) - min(ndvi), 
         c = -0.05, 
         d = 160,          # late early season
         e = 0.05, 
         f = 250)
  )
  # Try fitting with each set of parameters
  for (start_params in start_params_list) {
    tryCatch({
      fit <- nlsLM(y ~ double_logistic(t, a, b, c, d, e, f),
                   data = fit_data,
                   start = start_params,
                   control = nls.lm.control(maxiter = 500))
      
      # Predict and compute derivative
      t_seq <- seq(min(t), max(t), by = 1)
      pred_ndvi <- predict(fit, newdata = data.frame(t = t_seq))
      dy_dt <- diff(pred_ndvi) / diff(t_seq)
      t_mid <- t_seq[-1]
      sos_doy <- t_mid[which.max(dy_dt)]
      eos_doy <- t_mid[which.min(dy_dt)]
    
      return(data.frame(SOS = sos_doy, EOS = eos_doy))
    }, error = function(e) {
      # Continue to next attempt if this one fails
    })
  }
  # If all attempts fail, return NA
  return(data.frame(SOS = NA, EOS = NA))
}

sos_eos_list <- lapply(vi_list_gt20, extract_sos_eos)
sos_eos_df <- bind_rows(sos_eos_list, .id = "FieldID")  # .id adds index# Combine into one data frame
sos_eos_df[is.na(sos_eos_df$SOS) & is.na(sos_eos_df$EOS), ]# Print rows where both SOS and EOS are NA


####################################################################
#############APPLY the FUNCTION ACROSS DIFFERENT DATSET#############
#############SAVE EACH IMAGE########################################
####################################################################
extract_sos_eos <- function(df, id = NULL, out_dir = NULL) {
  library(ggplot2)
  library(lubridate)
  library(minpack.lm)
  
  # Ensure Date and DOY columns
  df$Date <- as.POSIXct(df$Date, tz = "UTC")
  df$DOY <- yday(df$Date)
  ndvi <- df$kNDVI
  doy <- df$DOY
  t <- doy
  y <- ndvi
  
  fit_data <- data.frame(t = t, y = y)
  
  # Define multiple sets of start params
  start_params_list <- list(
    list(a = min(ndvi), b = max(ndvi) - min(ndvi), c = -0.05, d = median(doy), e = 0.05, f = 250),
    list(a = min(ndvi), b = max(ndvi) - min(ndvi), c = -0.05, d = 25, e = 0.05, f = 250),
    list(a = min(ndvi), b = max(ndvi) - min(ndvi), c = -0.05, d = 100, e = 0.05, f = 250),
    list(a = min(ndvi), b = max(ndvi) - min(ndvi), c = -0.05, d = 120, e = 0.05, f = 250),
    list(a = min(ndvi), b = max(ndvi) - min(ndvi), c = -0.05, d = 160, e = 0.05, f = 250)
  )
  
  double_logistic <- function(t, a, b, c, d, e, f) {
    a + b * (1 / (1 + exp(c * (t - d))) + 1 / (1 + exp(e * (t - f))))
  }
  
  # Try fitting
  for (start_params in start_params_list) {
    tryCatch({
      fit <- nlsLM(y ~ double_logistic(t, a, b, c, d, e, f),
                   data = fit_data,
                   start = start_params,
                   control = nls.lm.control(maxiter = 500))
      
      # Predict and compute derivative
      t_seq <- seq(min(t), max(t), by = 1)
      pred_ndvi <- predict(fit, newdata = data.frame(t = t_seq))
      dy_dt <- diff(pred_ndvi) / diff(t_seq)
      t_mid <- t_seq[-1]
      sos_doy <- t_mid[which.max(dy_dt)]
      eos_doy <- t_mid[which.min(dy_dt)]
      
      # --- Plotting ---
      pred_df <- data.frame(DOY = t_seq, NDVI = pred_ndvi)
      
      p <- ggplot() +
        geom_point(data = df, aes(x = DOY, y = NDVI), color = "black", size = 2) +
        geom_line(data = pred_df, aes(x = DOY, y = NDVI), color = "blue", size = 1) +
        geom_vline(xintercept = sos_doy, color = "green", linetype = "dashed", size = 1.2) +
        geom_vline(xintercept = eos_doy, color = "red", linetype = "dashed", size = 1.2) +
        ggtitle(paste0("NDVI Fit - ", ifelse(is.null(id), "Unknown", id))) +
        theme_minimal()
      if (!is.null(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        fname <- file.path(out_dir, paste0("fit_", ifelse(is.null(id), "unknown", id), ".jpeg"))
        ggsave(filename = fname, plot = p, width = 7, height = 5, dpi = 300)
      }
      return(data.frame(SOS = sos_doy, EOS = eos_doy))
    }, error = function(e) {
      # Continue to next if fails
    })
  }
  
  return(data.frame(SOS = NA, EOS = NA))
}


results <- lapply(seq_along(vi_list_gt20), function(i) {
  extract_sos_eos(vi_list_gt20[[i]],
                  id = paste0("field_", i),
                  out_dir = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ROC_Curvature_Method")
})
sos_eos_df <- do.call(rbind, results)

sos_eos_df
sos_eos_df


# Combine SOS/EOS results into a single data frame
sos_eos_df <- bind_rows(sos_eos_results, .id = "id")
sos_eos_df$file_name <- basename(vi_csv_files_gt20)
# Extract observed SOS (PDDOY) from each data frame
observed_sos <- sapply(vi_list_gt20, function(df) df$PDDOY[1])  # Assuming first row has PDDOY
# Add observed SOS and compute DOY of predicted SOS
sos_eos_df$observed_SOS <- observed_sos
sos_eos_df$SOS_doy <- sos_eos_df$SOS  # already in DOY format

# Fit linear model: predicted vs. observed
model <- lm(SOS ~ observed_SOS, data = sos_eos_df)
# Load metrics if not already loaded

# Evaluate model performance
rmse_value <- Metrics::rmse(sos_eos_df$observed_SOS, sos_eos_df$SOS)
mae_value <- mae(sos_eos_df$observed_SOS, sos_eos_df$SOS)
r2_value <- summary(model)$r.squared

ggplot(sos_eos_df, aes(x = observed_SOS, y = SOS_doy)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # 1:1 line
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  annotate("text", x = min(sos_eos_df$observed_SOS, na.rm = TRUE) + 5, 
           y = max(sos_eos_df$SOS_doy, na.rm = TRUE) - 5,
           label = sprintf("RMSE = %.2f\nMAE = %.2f\nR² = %.2f", rmse_value, mae_value, r2_value),
           hjust = 0, size = 4) +
  labs(x = "Observed SOS (PDDOY)", y = "Predicted SOS (from double logistic)", 
       title = "Observed vs. Predicted Start of Season (SOS)") +
  theme_minimal()

ggsave("Observed_vs_Predicted_SOS_DoubleLogistic.png", width = 7, height = 6, dpi = 300)
cat(sprintf("Model performance:\n  RMSE: %.2f\n  MAE: %.2f\n  R²: %.2f\n", 
            rmse_value, mae_value, r2_value))



