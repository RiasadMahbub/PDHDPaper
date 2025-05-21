#Dynamic threshold method detects start of season (SOS) (or PD) when a smoothed VI (e.g., NDVI) curve crosses a fixed value, e.g., 0.17.
#3.1.1. Threshold-based method
#A review of vegetation phenological metrics extraction using time-series, multispectral satellite data
library(ggplot2)
library(dplyr)
library(caret)  # For RMSE and MAE functions
library(Metrics)  # For R squared (R2)
library(lubridate)

####Input features
### VI values 
vi_list_gt20[[1]]$NDVI
vi_list_gt20[[1]]$Date
plot(vi_list_gt20[[1]]$Date, vi_list_gt20[[1]]$NDVI)

extract_sos_eos <- function(df, vi_column = "kNDVI", date_column = "Date", VIthd = 0.2) {
  vi <- df[[vi_column]]
  date <- df[[date_column]]
  if (length(vi) < 5 || all(is.na(vi))) return(data.frame(SOS = NA, EOS = NA))
  vi_smooth <- stats::filter(vi, rep(1/3, 3), sides = 2)# Smooth the time series (optional but helps with noise
  c_idx <- which.max(vi_smooth)  # Identify peak (maximum value)
  c <- vi_smooth[c_idx]
  c_date <- date[c_idx]
  vi_left <- vi_smooth[1:c_idx]# Get left and right sides
  vi_right <- vi_smooth[c_idx:length(vi_smooth)]
  date_left <- date[1:c_idx]
  date_right <- date[c_idx:length(date)]
  a <- min(vi_left, na.rm = TRUE)# Left min (a), Right min (b)
  b <- min(vi_right, na.rm = TRUE)
  g1 <- c - a # Compute g1 and g2
  g2 <- c - b
  sos_threshold <- a + VIthd * g1# Compute thresholds
  eos_threshold <- b + VIthd * g2
  sos_idx <- which(vi_left >= sos_threshold)[1]# Find SOS: first date left of peak where vi >= threshold
  sos <- if (!is.na(sos_idx)) date_left[sos_idx] else NA
  eos_idx <- which(vi_right <= eos_threshold)[1]# Find EOS: first date right of peak where vi <= threshold
  eos <- if (!is.na(eos_idx)) date_right[eos_idx] else NA
  return(data.frame(SOS = sos, EOS = eos))
}

# Apply this function across vi_list_gt20
sos_eos_results <- lapply(vi_list_gt20, extract_sos_eos)

# Combine results with file names
sos_eos_df <- bind_rows(sos_eos_results, .id = "id")
sos_eos_df$file_name <- basename(vi_csv_files_gt20)

# Assuming 'vi_list_gt20' is a list of data frames, each containing the 'PDDOY' column
# Extract the observed SOS (PDDOY) for each file (assuming the first entry in each data frame is the observed SOS)
observed_sos <- sapply(vi_list_gt20, function(df) df$PDDOY[1])  # Adjust this line if the observed SOS is stored differently

# Assuming 'sos_eos_df' contains the predicted SOS values in the 'SOS' column
# Add observed SOS to the 'sos_eos_df' data frame
sos_eos_df$observed_SOS <- observed_sos
sos_eos_df$SOS_doy<-yday(sos_eos_df$SOS)
# Plot the comparison between observed and predicted SOS

# Assuming sos_eos_df has 'observed_SOS' and 'SOS_doy' columns
model <- lm(SOS_doy ~ observed_SOS, data = sos_eos_df)# Fit the linear model
sos_eos_df$predicted_SOS <- predict(model, sos_eos_df)# Calculate predictions

# Calculate RMSE, MAE, and R^2
rmse_value <- rmse(sos_eos_df$SOS_doy, sos_eos_df$predicted_SOS)
mae_value <- mae(sos_eos_df$SOS_doy, sos_eos_df$predicted_SOS)
r2_value <- summary(model)$r.squared

# Plot with ggplot2
ggplot(sos_eos_df, aes(x = observed_SOS, y = SOS_doy)) +
  geom_point() +  # Scatter plot of observed vs predicted SOS
  labs(title = "Observed SOS vs Predicted SOS",
       x = "Observed Planting Day (Day of the year)",
       y = "Observed Planting Day (Day of the year)") +
  theme_minimal() +  # Use a minimal theme
  geom_smooth(method = "lm", col = "red", linetype = "dashed") +  # Add linear regression line
  annotate("text", x = max(sos_eos_df$observed_SOS), y = max(sos_eos_df$SOS_doy), 
           label = paste("R² = ", round(r2_value, 3), "\nRMSE = ", round(rmse_value, 2), 
                         "\nMAE = ", round(mae_value, 2)), 
           hjust = 1, vjust = 1, size = 4, color = "blue")


##### optimization part########
## use different optimization values which are threshold 0.1 to 0.9 and calculate the rmse, mae, r2, and bias####
### Create a table using NDVI, four columns are rmse, mae, r2, bias
### THe rows would be 0.1 to 0.9 increase by 1
### another table same method but use kNDVI

# Optimization function
# Define a function to extract SOS using dynamic threshold
extract_sos <- function(df, vi_column = "NDVI", date_column = "Date", VIthd = 0.2) {
  vi <- df[[vi_column]]
  date <- df[[date_column]]
  if (length(vi) < 5 || all(is.na(vi))) return(NA)
  vi_smooth <- stats::filter(vi, rep(1/3, 3), sides = 2)
  c_idx <- which.max(vi_smooth)
  c <- vi_smooth[c_idx]
  vi_left <- vi_smooth[1:c_idx]
  date_left <- date[1:c_idx]
  a <- min(vi_left, na.rm = TRUE)
  g1 <- c - a
  sos_threshold <- a + VIthd * g1
  sos_idx <- which(vi_left >= sos_threshold)[1]
  if (is.na(sos_idx)) return(NA)
  return(yday(date_left[sos_idx]))
}

# Optimization function
optimize_threshold <- function(vi_list, vi_column = "NDVI") {
  thresholds <- seq(0.1, 0.9, by = 0.1)
  results <- data.frame(threshold = thresholds, RMSE = NA, MAE = NA, R2 = NA, Bias = NA)
  
  observed <- sapply(vi_list, function(df) df$PDDOY[1])
  
  for (i in seq_along(thresholds)) {
    thd <- thresholds[i]
    predicted <- sapply(vi_list, function(df) extract_sos(df, vi_column = vi_column, VIthd = thd))
    valid_idx <- which(!is.na(predicted) & !is.na(observed))
    
    if (length(valid_idx) > 2) {
      obs <- observed[valid_idx]
      pred <- predicted[valid_idx]
      model <- lm(pred ~ obs)
      results$RMSE[i] <- rmse(obs, pred)
      results$MAE[i] <- mae(obs, pred)
      results$R2[i] <- summary(model)$r.squared
      results$Bias[i] <- mean(pred - obs)
    }
  }
  return(results)
}

# Run optimization for both NDVI and kNDVI
ndvi_results <- optimize_threshold(vi_list_gt20, vi_column = "NDVI")
kndvi_results <- optimize_threshold(vi_list_gt20, vi_column = "kNDVI")

# View results
print("NDVI Optimization Results:")
print(ndvi_results)

print("kNDVI Optimization Results:")
print(kndvi_results)

################################################
##############PLOT THE OPTIMIZATION#############
################################################
# Set output directory
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DTM/Optimization"

# Function to create and save optimization plots
plot_optimization_metrics <- function(results_df, vi_name, output_dir) {
  metrics <- c("RMSE", "MAE", "R2", "Bias")
  
  for (metric in metrics) {
    p <- ggplot(results_df, aes(x = threshold, y = .data[[metric]])) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(title = paste(vi_name, "-", metric, "vs Threshold"),
           x = "Threshold",
           y = metric) +
      theme_minimal(base_size = 14)
    
    # Save the plot
    ggsave(filename = paste0(output_dir, "/", vi_name, "_", metric, "_optimization.png"),
           plot = p, width = 6, height = 4, dpi = 300)
  }
}

# Run plotting and saving for both NDVI and kNDVI
plot_optimization_metrics(ndvi_results, "NDVI", output_dir)
plot_optimization_metrics(kndvi_results, "kNDVI", output_dir)



# Compute performance metrics
obs <- sos_eos_df$observed_SOS
pred <- sos_eos_df$SOS_doy
model <- lm(pred ~ obs)

r2_value <- summary(model)$r.squared
rmse_value <- rmse(obs, pred)
mae_value <- mae(obs, pred)
bias_value <- mean(pred - obs)


optimize_threshold_with_plot <- function(vi_list, vi_column = "NDVI", output_dir) {
  thresholds <- seq(0.1, 0.9, by = 0.1)
  results <- data.frame(threshold = thresholds, RMSE = NA, MAE = NA, R2 = NA, Bias = NA)
  
  observed <- sapply(vi_list, function(df) df$PDDOY[1])
  
  for (i in seq_along(thresholds)) {
    thd <- thresholds[i]
    predicted <- sapply(vi_list, function(df) extract_sos(df, vi_column = vi_column, VIthd = thd))
    valid_idx <- which(!is.na(predicted) & !is.na(observed))
    
    if (length(valid_idx) > 2) {
      obs <- observed[valid_idx]
      pred <- predicted[valid_idx]
      model <- lm(pred ~ obs)
      r2_value <- summary(model)$r.squared
      rmse_value <- rmse(obs, pred)
      mae_value <- mae(obs, pred)
      bias_value <- mean(pred - obs)
      
      # Store metrics
      results$RMSE[i] <- rmse_value
      results$MAE[i] <- mae_value
      results$R2[i] <- r2_value
      results$Bias[i] <- bias_value
      
      # Prepare dataframe for plotting
      plot_df <- data.frame(observed = obs, predicted = pred)
      
      # Plot
      p <- ggplot(plot_df, aes(x = observed, y = predicted)) +
        geom_point(color = "darkgreen", size = 2) +
        geom_smooth(method = "lm", col = "red", linetype = "dashed") +
        labs(title = paste0("Observed vs Predicted SOS (", vi_column, ", Thd = ", thd, ")"),
             x = "Observed Planting Date (DOY)",
             y = "Predicted Planting Date (DOY)") +
        theme_minimal(base_size = 14) +
        annotate("text",
                 x = max(plot_df$observed, na.rm = TRUE),
                 y = min(plot_df$predicted, na.rm = TRUE),
                 hjust = 1.1, vjust = -0.1, size = 5, color = "blue",
                 label = paste0("R² = ", round(r2_value, 3),
                                "\nRMSE = ", round(rmse_value, 2),
                                "\nMAE = ", round(mae_value, 2),
                                "\nBias = ", round(bias_value, 2)))
      
      # Save plot
      filename <- paste0(output_dir, "/", vi_column, "_Thd_", thd, ".png")
      ggsave(filename, plot = p, width = 7, height = 5, dpi = 300)
    }
  }
  return(results)
}

# Set output directory
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DTM/Optimization"

# Run optimization and save plots for each band
ndvi_results <- optimize_threshold_with_plot(vi_list_gt20, vi_column = "NDVI", output_dir = output_dir)
kndvi_results <- optimize_threshold_with_plot(vi_list_gt20, vi_column = "kNDVI", output_dir = output_dir)

# Print summary
print("NDVI Optimization Results:")
print(ndvi_results)

print("kNDVI Optimization Results:")
print(kndvi_results)

