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
observed_eos <- sapply(vi_list_gt20, function(df) df$HDDOY[1])  # Adjust this line if the observed SOS is stored differently

# Assuming 'sos_eos_df' contains the predicted SOS values in the 'SOS' column
sos_eos_df$observed_SOS <- observed_sos
sos_eos_df$SOS_doy<-yday(sos_eos_df$SOS)
# Plot the comparison between observed and predicted SOS

# Assuming sos_eos_df has 'observed_SOS' and 'SOS_doy' columns
model <- lm(SOS_doy ~ observed_SOS, data = sos_eos_df)# Fit the linear mhttp://127.0.0.1:16709/graphics/plot_zoom_png?width=1572&height=805odel
# sos_eos_df$predicted_SOS <- predict(model, sos_eos_df)# Calculate predictions

# Calculate RMSE, MAE, and R^2
rmse_value <- rmse(sos_eos_df$observed_SOS, sos_eos_df$SOS_doy)
mae_value <- mae(sos_eos_df$observed_SOS, sos_eos_df$SOS_doy)
r2_value <- summary(model)$r.squared

# Plot with ggplot2
ggplot(sos_eos_df, aes(x = observed_SOS, y = SOS_doy)) +
  geom_point() +  # Scatter plot of observed vs predicted SOS
  labs(title = "Observed SOS vs Predicted SOS",
       x = "Observed Planting Day (Day of the year)",
       y = "Predicted Planting Day (Day of the year)") +
  theme_minimal() +  # Use a minimal theme
  geom_smooth(method = "lm", col = "red", linetype = "dashed") +  # Add linear regression line
  annotate("text", 
           x = 70, y = 210,  # Fixed position
           label = paste("R² = ", round(r2_value, 3),
                         "\nRMSE = ", round(rmse_value, 2), 
                         "\nMAE = ", round(mae_value, 2)),
           hjust = 0, vjust = 1, size = 4, color = "blue")


#------------------------------------------------------------------
#Harvest
#----------------------------------------------------------------------
# Assuming 'sos_eos_df' contains the predicted SOS values in the 'SOS' column

observed_eos <- sapply(vi_list_gt20, function(df) {
  val <- df$HDDOY[1]
  if (is.null(val)) return(NA) else return(val)
})
observed_eos <- sapply(vi_list_gt20, function(df) {
  val <- df$HDDOY[1]
  if (is.null(val) || is.na(val) || val >= 360) return(NA) else return(val)
})
sos_eos_df$observed_EOS <- observed_eos
sos_eos_df$EOS_doy<-yday(sos_eos_df$EOS)
sos_eos_df$observed_EOS <- as.numeric(observed_eos)
# Plot the comparison between observed and predicted SOS

# Assuming sos_eos_df has 'observed_SOS' and 'SOS_doy' columns
model <- lm(EOS_doy ~ observed_EOS, data = sos_eos_df)# Fit the linear model
# sos_eos_df$predicted_SOS <- predict(model, sos_eos_df)# Calculate predictions

# Calculate RMSE, MAE, and R^2
# Filter valid rows for harvest (EOS)
sos_eos_df_validHarvest <- sos_eos_df %>%
  dplyr::filter(!is.na(observed_EOS), !is.na(EOS_doy))

# Fit model using filtered data
model <- lm(EOS_doy ~ observed_EOS, data = sos_eos_df_validHarvest)

# Compute metrics
rmse_value <- rmse(sos_eos_df_validHarvest$observed_EOS, sos_eos_df_validHarvest$EOS_doy)
mae_value <- mae(sos_eos_df_validHarvest$observed_EOS, sos_eos_df_validHarvest$EOS_doy)
r2_value <- summary(model)$r.squared

# Plot with ggplot2
ggplot(sos_eos_df, aes(x = observed_EOS, y = EOS_doy)) +
  geom_point() +
  labs(title = "Observed EOS vs Predicted EOS",
       x = "Observed Harvest Day (Day of the year)",
       y = "Predicted Harvest Day (Day of the year)") +
  theme_minimal() +
  geom_smooth(method = "lm", col = "red", linetype = "dashed") +
  annotate("text", 
           x = max(sos_eos_df$observed_EOS, na.rm = TRUE) + 5,  # Shift 5 days to the right
           y = max(sos_eos_df$EOS_doy, na.rm = TRUE) + 5,
           label = paste("R² = ", round(r2_value, 3), 
                         "\nRMSE = ", round(rmse_value, 2), 
                         "\nMAE = ", round(mae_value, 2)),
           hjust = 1, vjust =3, size = 4, color = "blue")

# Step 1: Compute residuals
sos_eos_df <- sos_eos_df %>%
  mutate(residual = EOS_doy - observed_EOS,
         abs_residual = abs(residual))  # absolute residual for sorting

# Step 2: Sort by largest absolute residuals
high_residuals <- sos_eos_df %>%
  arrange(desc(abs_residual)) %>%
  select(id, observed_EOS, EOS_doy, residual, abs_residual)

# Step 3 (Optional): Show top 10 high residuals
head(high_residuals, 10)
threshold <- 10  # or whatever threshold makes sense
high_residuals_filtered <- sos_eos_df %>%
  dplyr::filter(abs_residual > threshold) %>%
  arrange(desc(abs_residual)) %>%
  select(id, observed_EOS, EOS_doy, residual, abs_residual)
high_residuals_filtered
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
  thresholds <- seq(0.1, 1, by = 0.1)
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
# Filter list elements that have the 'HDDOY' column

# Run optimization for SOS (Planting Date)
mlswi26_results <- optimize_threshold(vi_list_gt20, vi_column = "MLSWI26")
iavi_results    <- optimize_threshold(vi_list_gt20, vi_column = "IAVI")
vari_results    <- optimize_threshold(vi_list_gt20, vi_column = "VARI")
rndvi_results   <- optimize_threshold(vi_list_gt20, vi_column = "RNDVI")
ndvi_results    <- optimize_threshold(vi_list_gt20, vi_column = "NDVI")
evi_results     <- optimize_threshold(vi_list_gt20, vi_column = "EVI")
kndvi_results   <- optimize_threshold(vi_list_gt20, vi_column = "kNDVI")
atsavi_results  <- optimize_threshold(vi_list_gt20, vi_column = "ATSAVI")
gdvi_results    <- optimize_threshold(vi_list_gt20, vi_column = "GDVI")
mbwi_results    <- optimize_threshold(vi_list_gt20, vi_column = "MBWI")
tvi_results     <- optimize_threshold(vi_list_gt20, vi_column = "TVI")
ndwi_results    <- optimize_threshold(vi_list_gt20, vi_column = "NDWI")
tsavi_results   <- optimize_threshold(vi_list_gt20, vi_column = "TSAVI")
lswi_results    <- optimize_threshold(vi_list_gt20, vi_column = "LSWI")

# Format results: 2 significant digits, no scientific notation
rndvi_results_fmt <- rndvi_results
rndvi_results_fmt[, -1] <- lapply(rndvi_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

ndvi_results_fmt <- ndvi_results
ndvi_results_fmt[, -1] <- lapply(ndvi_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

evi_results_fmt <- evi_results
evi_results_fmt[, -1] <- lapply(evi_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

kndvi_results_fmt <- kndvi_results
kndvi_results_fmt[, -1] <- lapply(kndvi_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

atsavi_results_fmt <- atsavi_results
atsavi_results_fmt[, -1] <- lapply(atsavi_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

gdvi_results_fmt <- gdvi_results
gdvi_results_fmt[, -1] <- lapply(gdvi_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

mbwi_results_fmt <- mbwi_results
mbwi_results_fmt[, -1] <- lapply(mbwi_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

tvi_results_fmt <- tvi_results
tvi_results_fmt[, -1] <- lapply(tvi_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

ndwi_results_fmt <- ndwi_results
ndwi_results_fmt[, -1] <- lapply(ndwi_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

tsavi_results_fmt <- tsavi_results
tsavi_results_fmt[, -1] <- lapply(tsavi_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

lswi_results_fmt <- lswi_results
lswi_results_fmt[, -1] <- lapply(lswi_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))



cat("\nRNDVI SOS Optimization Results:\n")
print(rndvi_results_fmt, row.names = FALSE)

cat("\nNDVI SOS Optimization Results:\n")
print(ndvi_results_fmt, row.names = FALSE)

cat("\nEVI SOS Optimization Results:\n")
print(evi_results_fmt, row.names = FALSE)

cat("\nkNDVI SOS Optimization Results:\n")
print(kndvi_results_fmt, row.names = FALSE)

cat("\nATSAVI SOS Optimization Results:\n")
print(atsavi_results_fmt, row.names = FALSE)

cat("\nGDVI SOS Optimization Results:\n")
print(gdvi_results_fmt, row.names = FALSE)

cat("\nMBWI SOS Optimization Results:\n")
print(mbwi_results_fmt, row.names = FALSE)

cat("\nTVI SOS Optimization Results:\n")
print(tvi_results_fmt, row.names = FALSE)

cat("\nNDWI SOS Optimization Results:\n")
print(ndwi_results_fmt, row.names = FALSE)

cat("\nTSAVI SOS Optimization Results:\n")
print(tsavi_results_fmt, row.names = FALSE)

cat("\nLSWI SOS Optimization Results:\n")
print(lswi_results_fmt, row.names = FALSE)



#----------------------------------------------------
#Optimization for the harvest 
#----------------------------------------------------
# Define a function to extract EOS using dynamic threshold
extract_eos <- function(df, vi_column = "NDVI", date_column = "Date", VIthd = 0.2) {
  vi <- df[[vi_column]]
  date <- df[[date_column]]
  if (length(vi) < 5 || all(is.na(vi))) return(NA)
  
  vi_smooth <- stats::filter(vi, rep(1/3, 3), sides = 2)
  c_idx <- which.max(vi_smooth)
  c <- vi_smooth[c_idx]
  vi_right <- vi_smooth[c_idx:length(vi_smooth)]
  date_right <- date[c_idx:length(date)]
  b <- min(vi_right, na.rm = TRUE)
  g2 <- c - b
  eos_threshold <- b + VIthd * g2
  eos_idx <- which(vi_right <= eos_threshold)[1]
  if (is.na(eos_idx)) return(NA)
  return(yday(date_right[eos_idx]))
}

# Optimization function for EOS (Harvest Date)
optimize_threshold_eos <- function(vi_list, vi_column = "NDVI") {
  thresholds <- seq(0.1, 1, by = 0.1)
  results <- data.frame(threshold = thresholds, RMSE = NA, MAE = NA, R2 = NA, Bias = NA)
  
  observed <- sapply(vi_list, function(df) df$HDDOY[1])
  
  for (i in seq_along(thresholds)) {
    thd <- thresholds[i]
    predicted <- sapply(vi_list, function(df) extract_eos(df, vi_column = vi_column, VIthd = thd))
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

# Run optimization for EOS (Harvest Date)
vi_list_with_HDDOY <- Filter(function(df) "HDDOY" %in% names(df), vi_list_gt20)
iavi_eos_results    <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "IAVI")
vari_eos_results    <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "VARI")
rndvi_eos_results   <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "RNDVI")
ndvi_eos_results    <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "NDVI")
evi_eos_results     <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "EVI")
kndvi_eos_results   <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "kNDVI")
atsavi_eos_results  <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "ATSAVI")
gdvi_eos_results    <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "GDVI")
mbwi_eos_results    <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "MBWI")
tvi_eos_results     <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "TVI")
ndwi_eos_results    <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "NDWI")
tsavi_eos_results   <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "TSAVI")
lswi_eos_results    <- optimize_threshold_eos(vi_list_with_HDDOY, vi_column = "LSWI")


iavi_eos_results_fmt <- iavi_eos_results
iavi_eos_results_fmt[, -1] <- lapply(iavi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

vari_eos_results_fmt <- vari_eos_results
vari_eos_results_fmt[, -1] <- lapply(vari_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

rndvi_eos_results_fmt <- rndvi_eos_results
rndvi_eos_results_fmt[, -1] <- lapply(rndvi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

ndvi_eos_results_fmt <- ndvi_eos_results
ndvi_eos_results_fmt[, -1] <- lapply(ndvi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

evi_eos_results_fmt <- evi_eos_results
evi_eos_results_fmt[, -1] <- lapply(evi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

kndvi_eos_results_fmt <- kndvi_eos_results
kndvi_eos_results_fmt[, -1] <- lapply(kndvi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

atsavi_eos_results_fmt <- atsavi_eos_results
atsavi_eos_results_fmt[, -1] <- lapply(atsavi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

gdvi_eos_results_fmt <- gdvi_eos_results
gdvi_eos_results_fmt[, -1] <- lapply(gdvi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

mbwi_eos_results_fmt <- mbwi_eos_results
mbwi_eos_results_fmt[, -1] <- lapply(mbwi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

tvi_eos_results_fmt <- tvi_eos_results
tvi_eos_results_fmt[, -1] <- lapply(tvi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

ndwi_eos_results_fmt <- ndwi_eos_results
ndwi_eos_results_fmt[, -1] <- lapply(ndwi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

tsavi_eos_results_fmt <- tsavi_eos_results
tsavi_eos_results_fmt[, -1] <- lapply(tsavi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))

lswi_eos_results_fmt <- lswi_eos_results
lswi_eos_results_fmt[, -1] <- lapply(lswi_eos_results_fmt[, -1], function(x) format(signif(x, 2), scientific = FALSE))


# View formatted results
cat("MLSWI26 EOS Optimization Results:\n")
print(mlswi26_eos_results_fmt, row.names = FALSE)

cat("\nIAVI EOS Optimization Results:\n")
print(iavi_eos_results_fmt, row.names = FALSE)

cat("\nVARI EOS Optimization Results:\n")
print(vari_eos_results_fmt, row.names = FALSE)

cat("\nRNDVI EOS Optimization Results:\n")
print(rndvi_eos_results_fmt, row.names = FALSE)

cat("\nNDVI EOS Optimization Results:\n")
print(ndvi_eos_results_fmt, row.names = FALSE)

cat("\nEVI EOS Optimization Results:\n")
print(evi_eos_results_fmt, row.names = FALSE)

cat("\nkNDVI EOS Optimization Results:\n")
print(kndvi_eos_results_fmt, row.names = FALSE)

cat("\nATSAVI EOS Optimization Results:\n")
print(atsavi_eos_results_fmt, row.names = FALSE)

cat("\nGDVI EOS Optimization Results:\n")
print(gdvi_eos_results_fmt, row.names = FALSE)

cat("\nMBWI EOS Optimization Results:\n")
print(mbwi_eos_results_fmt, row.names = FALSE)

cat("\nTVI EOS Optimization Results:\n")
print(tvi_eos_results_fmt, row.names = FALSE)

cat("\nNDWI EOS Optimization Results:\n")
print(ndwi_eos_results_fmt, row.names = FALSE)

cat("\nTSAVI EOS Optimization Results:\n")
print(tsavi_eos_results_fmt, row.names = FALSE)

cat("\nLSWI EOS Optimization Results:\n")
print(lswi_eos_results_fmt, row.names = FALSE)

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

#-----------------------------------------\
#Save the performance metrics

# Function to get metrics for a specific threshold from results
get_metrics_at_threshold <- function(results_df, threshold) {
  row <- results_df[which(abs(results_df$threshold - threshold) < 1e-5), ]
  if (nrow(row) == 0) {
    return(c(NA, NA, NA, NA))
  }
  return(c(round(row$RMSE, 2), round(row$MAE, 2), round(row$R2, 4), round(row$Bias, 2)))
}

# Extract performance for DTM models at 20% and 50% thresholds
DTM_NDVI_20  <- get_metrics_at_threshold(ndvi_results, 0.2)
DTM_NDVI_50  <- get_metrics_at_threshold(ndvi_results, 0.5)
DTM_kNDVI_20 <- get_metrics_at_threshold(kndvi_results, 0.2)
DTM_kNDVI_50 <- get_metrics_at_threshold(kndvi_results, 0.5)

# Create summary dataframe
dtmdldeines_summary_df <- data.frame(
  Model = c("DTM_NDVI_20", "DTM_NDVI_50", "DTM_kNDVI_20", "DTM_kNDVI_50"),
  RMSE  = c(DTM_NDVI_20[1], DTM_NDVI_50[1], DTM_kNDVI_20[1], DTM_kNDVI_50[1]),
  MAE   = c(DTM_NDVI_20[2], DTM_NDVI_50[2], DTM_kNDVI_20[2], DTM_kNDVI_50[2]),
  R2    = c(DTM_NDVI_20[3], DTM_NDVI_50[3], DTM_kNDVI_20[3], DTM_kNDVI_50[3]),
  Bias  = c(DTM_NDVI_20[4], DTM_NDVI_50[4], DTM_kNDVI_20[4], DTM_kNDVI_50[4])
)

# View the resulting table
print(dtmdldeines_summary_df)

