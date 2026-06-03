##Paper: Evaluating the Consistency of Vegetation Phenological Parameters in the Northern Hemisphere from 1982 to 2015
# ==============================================
# PHENOLOGY EXTRACTION PIPELINE
# ==============================================
#
# This script performs the following steps:
#
# 1. LOAD REQUIRED LIBRARIES
#    - Data manipulation (dplyr, lubridate)
#    - Curve fitting (minpack.lm)
#    - Visualization (ggplot2)
#    - Metrics calculation (Metrics)
#
# 2. DEFINE CORE FUNCTIONS
#    - Double logistic function and its derivative
#    - Phenology parameter extraction functions
#    - Visualization functions
#
# 3. DATA PREPARATION
#    - Add DOY (Day of Year) to time series
#    - Filter data to growing season
#
# 4. PHENOLOGY EXTRACTION
#    - Fit double logistic curve to NDVI/kNDVI time series
#    - Calculate derivatives to identify key phenological dates
#    - Handle multiple starting parameters for robust fitting
#
# 5. RESULTS PROCESSING
#    - Combine results across all fields
#    - Calculate performance metrics
#    - Generate validation plots
#
# 6. OUTPUTS
#    - CSV files with extracted phenology dates
#    - Diagnostic plots for each field
#    - Validation plots (observed vs predicted)


# Indices to evaluate for EOS optimization: 
#MLSWI26, IAVI, VARI, RNDVI, NDVI, EVI, kNDVI, ATSAVI, 
# GDVI, MBWI, TVI, NDWI, TSAVI, LSWI


# ==============================================
# Load necessary libraries
library(zoo)          # For time series operations
library(dplyr)        # For data manipulation
library(lubridate)    # For date handling
library(minpack.lm)   # For nonlinear least squares fitting
library(Metrics)      # For model evaluation metrics
library(ggplot2)      # For visualization
library(future.apply)
library(progressr)

# ==============================================
# CORE FUNCTION DEFINITIONS
# ==============================================

#' Double logistic function for phenology curve fitting
#' @param t Day of Year (DOY)
#' @param a Baseline NDVI value
#' @param b Amplitude of seasonal increase
#' @param c Rate of spring increase
#' @param d DOY of spring midpoint
#' @param e Rate of autumn decrease
#' @param f DOY of autumn midpoint

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


#===================================================================
#APPLY the FUNCTION ACROSS DIFFERENT DATSET
#===================================================================
extract_sos_eos <- function(df, id = NULL, out_dir = NULL, vi_column = "kNDVI") {
  df$Date <- as.POSIXct(df$Date, tz = "UTC")
  df$DOY <- yday(df$Date)
  df <- df[!is.na(df[[vi_column]]), ]
  
  if (nrow(df) < 10) {
    warning(paste("Insufficient data for field", id))
    return(data.frame(ID = id, SOS = NA, EOS = NA, 
                      a = NA, b = NA, c = NA, d = NA, e = NA, f = NA,
                      R2 = NA, RMSE = NA, MAE = NA, NSE = NA))
  }
  
  t <- df$DOY
  y <- df[[vi_column]]
  fit_data <- data.frame(t = t, y = y)
  
  # Fixed: Define double logistic function with correct parentheses
  double_logistic <- function(t, a, b, c, d, e, f) {
    a + b * (1 / (1 + exp(c * (t - d))) + 1 / (1 + exp(e * (t - f))))
  }
  
  # Starting parameters
  start_params_list <- list(
    list(a = min(y), b = max(y) - min(y), c = -0.05, d = median(t), e = 0.05, f = 250),
    list(a = min(y), b = max(y) - min(y), c = -0.05, d = 100, e = 0.05, f = 250),
    list(a = min(y), b = max(y) - min(y), c = -0.05, d = 150, e = 0.05, f = 250),
    list(a = mean(y) - sd(y), b = 2*sd(y), c = -0.1, d = 200, e = 0.1, f = 280)
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
    return(data.frame(ID = id, SOS = NA, EOS = NA, 
                      a = NA, b = NA, c = NA, d = NA, e = NA, f = NA,
                      R2 = NA, RMSE = NA, MAE = NA, NSE = NA))
  }
  
  # Generate predictions
  t_seq <- seq(min(t), max(t), by = 1)
  pred <- predict(best_fit, newdata = data.frame(t = t_seq))
  
  # Calculate derivatives
  dy_dt <- diff(pred) / diff(t_seq)
  t_mid <- t_seq[-1]
  sos_doy <- t_mid[which.max(dy_dt)]
  eos_doy <- t_mid[which.min(dy_dt)]
  
  # Calculate metrics
  y_pred <- predict(best_fit)
  R2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
  RMSE <- sqrt(mean((y - y_pred)^2))
  MAE <- mean(abs(y - y_pred))
  NSE <- R2  # Identical in this case
  
  # (Skip plotting since out_dir = NULL)
  
  coefs <- coef(best_fit)
  return(data.frame(
    ID = id,
    SOS = sos_doy,
    EOS = eos_doy,
    a = coefs["a"], b = coefs["b"], c = coefs["c"],
    d = coefs["d"], e = coefs["e"], f = coefs["f"],
    R2 = R2, RMSE = RMSE, MAE = MAE, NSE = NSE
  ))
}
# ==============================================
# 4. PARALLEL PROCESSING EXECUTION
# ==============================================
handlers(global = TRUE)
handlers("txtprogressbar")  # or use "progress" for a nicer CLI bar
plan(multisession, workers = 4)  # Adjust number of workers if needed

# # Enable progress reporting (saving the figures)
# with_progress({
#   p <- progressor(along = vi_list_gt20)  # Automatically handles % done
#   
#   # Run in parallel with progress bar
#   results <- future_lapply(seq_along(vi_list_gt20), function(i) {
#     p(message = paste("Processing", i))
#     extract_sos_eos(
#       vi_list_gt20[[i]],
#       id = paste0("field_", i),  # or use actual field name if available
#       out_dir = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DoubleLogistics/First"
#     )
#   })
# })

# Run without saving figures
# Apply function in parallel with no plot saving
with_progress({
  p <- progressor(along = vi_list_gt20)
  
  results <- future_lapply(seq_along(vi_list_gt20), function(i) {
    tryCatch({
      p(sprintf("Processing field %d/%d", i, length(vi_list_gt20)))
      extract_sos_eos(
        df = vi_list_gt20[[i]],
        id = paste0("field_", i),
        out_dir = NULL,           # <--- disables plot saving
        vi_column = "kNDVI"
      )
    }, error = function(e) {
      warning(sprintf("Field %d failed: %s", i, e$message))
      return(NULL)
    })
  })
})


plan(sequential)  # Clean up after


#=========================================================
# COMBINE RESULTS
#=========================================================
# Combine results
sos_eos_df <- do.call(rbind, results)
sos_eos_df
# Add observed values
sos_eos_df$observed_SOS <- sapply(vi_list_gt20, function(df) df$PDDOY[1])
sos_eos_df$observed_EOS <- sapply(vi_list_gt20, function(df) df$HDDOY[1])
# Handle missing values
sos_eos_df$observed_EOS[sos_eos_df$observed_EOS == "NULL"] <- NA
sos_eos_df$observed_EOS <- as.numeric(sos_eos_df$observed_EOS)
# Handle missing values
sos_eos_df$observed_EOS[sos_eos_df$observed_EOS == "NULL"] <- NA
sos_eos_df$observed_EOS <- as.numeric(sos_eos_df$observed_EOS)

# Extract field names
field_names <- sapply(vi_list_gt20, function(df) {
  unique_name <- unique(na.omit(df$FIELD_NAME))
  if (length(unique_name) > 0) unique_name[1] else NA
})

# Extract year from first non-NA Date
field_years <- sapply(vi_list_gt20, function(df) {
  valid_dates <- na.omit(df$Date)
  if (length(valid_dates) > 0) format(valid_dates[1], "%Y") else NA
})

# Combine into Field_Year
sos_eos_df$Field_Year <- paste(field_names, field_years, sep = "_")
sos_eos_df


# ==============================================
# 5. PERFORMANCE EVALUATION
# ==============================================
# Function to calculate evaluation metrics
calculate_metrics <- function(observed, predicted) {
  valid_df <- na.omit(data.frame(observed, predicted))
  list(
    RMSE = round(rmse(valid_df$observed, valid_df$predicted), 2),
    MAE = round(mae(valid_df$observed, valid_df$predicted), 2),
    R2 = round(summary(lm(predicted ~ observed, data = valid_df))$r.squared, 2),
    Bias = round(bias(valid_df$observed, valid_df$predicted), 2)
  )
}

# Calculate metrics for SOS and EOS
sos_metrics <- calculate_metrics(sos_eos_df$observed_SOS, sos_eos_df$SOS)
eos_metrics <- calculate_metrics(sos_eos_df$observed_EOS, sos_eos_df$EOS)

# ==============================================
# 6. VISUALIZATION
# ==============================================
# SOS Validation Plot
ggplot(sos_eos_df, aes(x = observed_SOS, y = SOS)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = quantile(sos_eos_df$observed_SOS, 0.1, na.rm = TRUE),
           y = quantile(sos_eos_df$SOS, 0.9, na.rm = TRUE),
           label = sprintf("RMSE: %.1f\nMAE: %.1f\nR²: %.2f",
                           sos_metrics$RMSE, sos_metrics$MAE, sos_metrics$R2)) +
  labs(x = "Observed SOS (DOY)", y = "Predicted SOS (DOY)") +
  theme_bw()

# EOS Validation Plot
ggplot(na.omit(sos_eos_df), aes(x = observed_EOS, y = EOS)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "orange") +
  annotate("text", x = quantile(sos_eos_df$observed_EOS, 0.1, na.rm = TRUE),
           y = quantile(sos_eos_df$EOS, 0.9, na.rm = TRUE),
           label = sprintf("RMSE: %.1f\nMAE: %.1f\nR²: %.2f",
                           eos_metrics$RMSE, eos_metrics$MAE, eos_metrics$R2)) +
  labs(x = "Observed EOS (DOY)", y = "Predicted EOS (DOY)") +
  theme_bw()


#------------------------------------------------
#GDD SOS 
#-------------------------------------------------
#Extract GDD at SOS from meteo_list
# Get field names from VI file paths
#------------------------------------------------
# GDD at SOS extraction
#------------------------------------------------
get_cumGDD_at_SOS <- function(field_year, sos_day) {
  # Check if this field_year exists in the list names
  if (!field_year %in% names(vi_list_gt20)) {
    warning(paste("Field_Year", field_year, "not found in vi_list_gt20"))
    return(NA)
  }
  
  df <- vi_list_gt20[[field_year]]
  
  # Make sure SOS is within the row index range of df
  if (sos_day > nrow(df) | sos_day < 1) {
    warning(paste("SOS DOY", sos_day, "is out of range for", field_year))
    return(NA)
  }
  
  # Return cumulative GDD at SOS DOY row
  return(df$cumulative_gdd_from_pddoy[sos_day])
}

sos_eos_df$cumulative_GDD_at_SOS <- mapply(get_cumGDD_at_SOS, 
                                           sos_eos_df$Field_Year, 
                                           sos_eos_df$SOS)

mean_value <- mean(sos_eos_df$cumulative_GDD_at_SOS, na.rm = TRUE)
sd_value <- sd(sos_eos_df$cumulative_GDD_at_SOS, na.rm = TRUE)

mean_value
sd_value
hist(as.numeric(sos_eos_df$cumulative_GDD_at_SOS, na.rm = TRUE))
hist(as.numeric(na.omit(sos_eos_df$cumulative_GDD_at_SOS)),
     main = "Histogram of Cumulative GDD at SOS",
     xlab = "Cumulative GDD at SOS",
     col = "lightblue",
     border = "black")


#==============================================
#calculate the lag ========================
#==============================================
# Required libraries
library(dplyr)
library(purrr)

# Function to estimate days after SOS to reach 892 cumulative GDD
# Function to compute lag between 892 GDD DOY and SOS GDD "start" DOY
calculate_gdd_lag_precise <- function(df, gdd_target = 1063.499) {
  # Remove rows where cumulative GDD is NA (before SOS/PDDOY)
  df_valid <- df[!is.na(df$cumulative_gdd_from_pddoy), ]
  
  # If no valid data, return NA
  if (nrow(df_valid) == 0) return(NA_real_)
  
  # Find index where cumulative GDD reaches the target
  reach_idx <- which(df_valid$cumulative_gdd_from_pddoy >= gdd_target)[1]
  
  if (is.na(reach_idx)) return(NA_real_)  # Target never reached
  
  # Get DOY of reaching the target
  doy_at_target <- df_valid$doy[reach_idx]
  
  # Back-calculate to find when the GDD accumulation started
  running_gdd <- gdd_target
  for (i in reach_idx:1) {
    running_gdd <- running_gdd - df_valid$gdd[i]
    if (running_gdd <= 0) {
      doy_at_start <- df_valid$doy[i]
      lag_days <- doy_at_target - doy_at_start
      return(lag_days)
    }
  }
  
  return(NA_real_)  # fallback
}

# 1. Combine all VI data
vi_data_all <- dplyr::bind_rows(vi_list_gt20)
# 2. Split by Field_Year and name each list element
grouped <- vi_data_all %>%
  dplyr::group_by(Field_Year) %>%
  dplyr::group_split()
field_names <- vi_data_all %>%
  dplyr::group_by(Field_Year) %>%
  dplyr::group_keys() %>%
  dplyr::pull(Field_Year)
vi_data_by_field <- rlang::set_names(grouped, field_names)

# 3. Identify Field_Years that exist in both sos_eos_df and VI data
valid_ids <- base::intersect(sos_eos_df$Field_Year, names(vi_data_by_field))

# 4. Filter sos_eos_df to valid Field_Years
sos_filtered <- sos_eos_df %>%
  dplyr::filter(Field_Year %in% valid_ids)
sos_filtered
# 5. Calculate gdd_lags for valid Field_Years
# Initialize an empty list to store the lag results
gdd_lags_list <- list()

# Loop through each valid Field_Year
for (field_year in valid_ids) {
  # Get the VI data for the current Field_Year
  current_vi_data <- vi_data_by_field[[field_year]]
  
  # Get the SOS DOY for the current Field_Year from sos_filtered
  # This SOS DOY will be used as the starting point for cumulative GDD calculation
  # We need to find the specific row in sos_filtered for the current field_year
  sos_doy_for_field <- sos_filtered %>%
    dplyr::filter(Field_Year == field_year) %>%
    dplyr::pull(SOS)

  # Apply the lag calculation function
  lag_result <- calculate_gdd_lag_precise(current_vi_data)
  gdd_lags_list[[field_year]] <- lag_result
}

# Convert the list of lags to a named vector
gdd_lags <- unlist(gdd_lags_list)
# Ensure gdd_lags is ordered by Field_Year to match sos_filtered
gdd_lags <- gdd_lags[sos_filtered$Field_Year]


# 6. Create lag data frame
lag_df <- data.frame(
  Field_Year = names(gdd_lags),
  Lag_Days = gdd_lags
)

# 7. Join lag results with main SOS/EOS data
sos_eos_with_lag <- sos_eos_df %>%
  dplyr::left_join(lag_df, by = "Field_Year")

# 8. Calculate the adjusted SOS using GDD lag
sos_eos_with_lag <- sos_eos_with_lag %>%
  dplyr::mutate(SOSPD2 = SOS - Lag_Days)

# 9. View result
print(sos_eos_with_lag)


sos_eos_with_lag$SOSPD2<-sos_eos_with_lag$SOS-sos_eos_with_lag$Lag_Days
sos_eos_with_lag$lagobserved <-sos_eos_with_lag$SOS-sos_eos_with_lag$observed_SOS
# Calculate metrics for SOS and EOS
sos_metrics2 <- calculate_metrics(sos_eos_with_lag$observed_SOS, sos_eos_with_lag$SOSPD2)


# Check which fields are failing and why
check_gdd_status <- map(sos_filtered$Field_Year, ~ {
  df <- vi_data_by_field[[.x]]
  if (is.null(df)) return(data.frame(Field_Year = .x, Max_GDD = NA, Has_Column = FALSE))
  max_gdd <- max(df$cumulative_gdd_from_pddoy, na.rm = TRUE)
  has_column <- "cumulative_gdd_from_pddoy" %in% colnames(df)
  data.frame(Field_Year = .x, Max_GDD = max_gdd, Has_Column = has_column)
}) %>% bind_rows()

print(check_gdd_status)


# ==============================================
# 6. VISUALIZATION
# ==============================================
# SOS Validation Plot
ggplot(sos_eos_with_lag, aes(x = observed_SOS, y = SOSPD2)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "red") +
  annotate("text", x = quantile(sos_eos_df$observed_SOS, 0.1, na.rm = TRUE),
           y = quantile(sos_eos_df$SOS, 0.9, na.rm = TRUE),
           label = sprintf("RMSE: %.1f\nMAE: %.1f\nR²: %.2f",
                           sos_metrics2$RMSE, sos_metrics2$MAE, sos_metrics2$R2)) +
  labs(x = "Observed SOS (DOY)", y = "Predicted SOS (DOY)") +
  theme_bw()



required_cols <- c("doy", "cumulative_gdd_from_pddoy", "gdd")

# Loop through each valid Field_Year
for (field_year in valid_ids) {
  current_vi_data <- vi_data_by_field[[field_year]]
  # Check if required columns exist
  if (!all(required_cols %in% names(current_vi_data))) {
    warning(paste("Missing columns in:", field_year))
    gdd_lags_list[[field_year]] <- NA_real_
    next
  }
  
  # Proceed only if GDD reaches target
  max_gdd <- max(current_vi_data$cumulative_gdd_from_pddoy, na.rm = TRUE)
  if (max_gdd < 892) {
    warning(paste("GDD never reaches target for:", field_year))
    gdd_lags_list[[field_year]] <- NA_real_
    next
  }
  
  lag_result <- calculate_gdd_lag_precise(current_vi_data)
  gdd_lags_list[[field_year]] <- lag_result
}


#---------------------------------------
#--Planting EVALUATION
#---------------------------------------
sos_eos_df
model <- lm(SOS ~ observed_SOS, data = sos_eos_df)# Fit linear model: predicted vs. observed
# Evaluate model performance
rmse_value <- Metrics::rmse(sos_eos_df$observed_SOS, sos_eos_df$SOS)
mae_value <- Metrics::mae(sos_eos_df$observed_SOS, sos_eos_df$SOS)
r2_value <- summary(model)$r.squared

# Remove NAs for evaluation
valid_df <- na.omit(data.frame(
  observed = sos_eos_df$observed_SOS,
  predicted = sos_eos_df$SOS
))

# Evaluate model performance
rmse_value  <- round(Metrics::rmse(valid_df$observed, valid_df$predicted), 2)
mae_value   <- round(Metrics::mae(valid_df$observed, valid_df$predicted), 2)
r2_value    <- round(summary(lm(predicted ~ observed, data = valid_df))$r.squared, 2)
bias_value  <- round(Metrics::bias(valid_df$observed, valid_df$predicted), 2)


# Print results
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("R²:", r2_value, "\n")

ggplot(sos_eos_df, aes(x = observed_SOS, y = SOS)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # 1:1 line
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  annotate("text", x = max(sos_eos_df$observed_SOS, na.rm = TRUE) - 25, 
           y = min(sos_eos_df$SOS, na.rm = TRUE) + 10,
           label = sprintf("RMSE = %.2f\nMAE = %.2f\nR² = %.2f", rmse_value, mae_value, r2_value),
           hjust = 0, size = 4) +
  labs(x = "Observed SOS (PDDOY)", y = "Predicted SOS (from double logistic)", 
       title = "Observed vs. Predicted Start of Season (SOS)") +
  theme_minimal()


ggsave("Observed_vs_Predicted_SOS_DoubleLogistic.png", width = 7, height = 6, dpi = 300)
cat(sprintf("Model performance:\n  RMSE: %.2f\n  MAE: %.2f\n  R²: %.2f\n", 
            rmse_value, mae_value, r2_value))

sos_eos_with_lag$lagobserved <-sos_eos_with_lag$SOS-sos_eos_with_lag$observed_SOS
ggplot(sos_eos_with_lag, aes(x = lagobserved, y = Lag_Days)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # 1:1 line
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  labs(x = "Observed SOS (PDDOY)", y = "Predicted SOS (from double logistic)", 
       title = "Observed vs. Predicted Start of Season (SOS)") +
  theme_minimal()


#------------------------------------------------------------------
#SUMMARY TABLE MODEL COMPARISON
#------------------------------------------------------------------

# Create a one-row data frame with the double logistic metrics
double_logistic_summary <- data.frame(
  Model = "Double Logistic - First Order",
  RMSE = rmse_value,
  MAE = mae_value,
  R2 = r2_value,
  Bias = bias_value
)

# Ensure column names match existing summary table (adjust if needed)
# Combine with existing summary df
dtmdldeines_summary_df <- bind_rows(dtmdldeines_summary_df, double_logistic_summary)

# View the updated summary table
print(dtmdldeines_summary_df)


#------------------------------------------------------------------
# Harvest
#------------------------------------------------------------------
sos_eos_df$observed_EOS[sos_eos_df$observed_EOS == "NULL"] <- NA
sos_eos_df$observed_EOS <- as.numeric(sos_eos_df$observed_EOS)

# Remove NAs for evaluation
valid_df <- na.omit(data.frame(
  observed = sos_eos_df$observed_EOS,
  predicted = sos_eos_df$EOS
))

# Evaluate model performance
rmse_value  <- round(Metrics::rmse(valid_df$observed, valid_df$predicted), 2)
mae_value   <- round(Metrics::mae(valid_df$observed, valid_df$predicted), 2)
r2_value    <- round(summary(lm(predicted ~ observed, data = valid_df))$r.squared, 2)
bias_value  <- round(Metrics::bias(valid_df$observed, valid_df$predicted), 2)


# Print results
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("R²:", r2_value, "\n")

ggplot(sos_eos_df, aes(x = observed_EOS, y = EOS)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # 1:1 line
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  annotate("text", x = max(sos_eos_df$EOS, na.rm = TRUE) -20, 
           y = max(sos_eos_df$EOS, na.rm = TRUE) - 10,
           label = sprintf("RMSE = %.2f\nMAE = %.2f\nR² = %.2f", rmse_value, mae_value, r2_value),
           hjust = 0, size = 4) +
  labs(x = "Observed EOS (HDDOY)", y = "Predicted EOS (from double logistic)", 
       title = "Observed vs. Predicted Start of Season (EOS)") +
  theme_minimal()


ggsave("Observed_vs_Predicted_SOS_DoubleLogistic.png", width = 7, height = 6, dpi = 300)
cat(sprintf("Model performance:\n  RMSE: %.2f\n  MAE: %.2f\n  R²: %.2f\n", 
            rmse_value, mae_value, r2_value))


#------------------------------------------------------------------
# TRY different vegetation indices
#------------------------------------------------------------------



# Load required libraries
library(future)
library(furrr)
library(progressr)
library(dplyr)

# Setup output directory
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/DoubleLogistics/First"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Enable progress handlers
handlers(global = TRUE)
handlers("txtprogressbar")  # Use "progress" for richer output in some environments

# Set parallel plan
plan(multisession, workers = 4)  # Adjust number based on your system

# Run in parallel with progress
with_progress({
  p <- progressor(along = vi_list_gt20)
  
  results <- future_lapply(seq_along(vi_list_gt20), function(i) {
    tryCatch({
      p(message = sprintf("Processing field %d/%d", i, length(vi_list_gt20)))
      extract_sos_eos(
        df = vi_list_gt20[[i]],
        id = paste0("field_", i),
        out_dir = output_dir,
        vi_column = "kNDVI"  # Change to "NDVI" if needed
      )
    }, error = function(e) {
      warning(sprintf("Field %d failed: %s", i, e$message))
      return(NULL)
    })
  })
})

# Combine results into data frame
results_df <- bind_rows(results)
results_df

# Optional: Save result table to CSV
write.csv(results_df, file.path(output_dir, "DoubleLogistic_Fitting_Results.csv"), row.names = FALSE)

# Cleanup
plan(sequential)
results_df

#------------------------------------------------------------------
#END of the code
# 
# ####################################
# # Define double logistic function
# double_logistic <- function(t, a, b, c, d, e, f) {
#   a + b * (1 / (1 + exp(c * (t - d))) + 1 / (1 + exp(e * (t - f))))
# }
# 
# extract_sos_eos <- function(df) {
#   # Ensure Date and DOY columns
#   df$Date <- as.POSIXct(df$Date, tz = "UTC")
#   df$DOY <- yday(df$Date)
#   ndvi <- df$kNDVI
#   doy <- df$DOY
#   t <- doy
#   y <- ndvi
#   
#   # Create data frame for fitting
#   fit_data <- data.frame(t = t, y = y)
#   
#   # Define four different sets of starting parameters for 'd'
#   start_params_list <- list(
#     list(a = min(ndvi), 
#          b = max(ndvi) - min(ndvi), 
#          c = -0.05, 
#          d = median(doy),  # central season
#          e = 0.05, 
#          f = 250),
#     
#     list(a = min(ndvi), 
#          b = max(ndvi) - min(ndvi), 
#          c = -0.05, 
#          d = 25,           # early season
#          e = 0.05, 
#          f = 250),
#     
#     list(a = min(ndvi), 
#          b = max(ndvi) - min(ndvi), 
#          c = -0.05, 
#          d = 100,          # moderate early season
#          e = 0.05, 
#          f = 250),
#     
#     list(a = min(ndvi), 
#          b = max(ndvi) - min(ndvi), 
#          c = -0.05, 
#          d = 120,          # late early season
#          e = 0.05, 
#          f = 250),
#     list(a = min(ndvi), 
#          b = max(ndvi) - min(ndvi), 
#          c = -0.05, 
#          d = 160,          # late early season
#          e = 0.05, 
#          f = 250)
#   )
#   # Try fitting with each set of parameters
#   for (start_params in start_params_list) {
#     tryCatch({
#       fit <- nlsLM(y ~ double_logistic(t, a, b, c, d, e, f),
#                    data = fit_data,
#                    start = start_params,
#                    control = nls.lm.control(maxiter = 500))
#       
#       # Predict and compute derivative
#       t_seq <- seq(min(t), max(t), by = 1)
#       pred_ndvi <- predict(fit, newdata = data.frame(t = t_seq))
#       dy_dt <- diff(pred_ndvi) / diff(t_seq)
#       t_mid <- t_seq[-1]
#       sos_doy <- t_mid[which.max(dy_dt)]
#       eos_doy <- t_mid[which.min(dy_dt)]
#       
#       return(data.frame(SOS = sos_doy, EOS = eos_doy))
#     }, error = function(e) {
#       # Continue to next attempt if this one fails
#     })
#   }
#   # If all attempts fail, return NA
#   return(data.frame(SOS = NA, EOS = NA))
# }
# 
# sos_eos_list <- lapply(vi_list_gt20, extract_sos_eos)
# sos_eos_df <- bind_rows(sos_eos_list, .id = "FieldID")  # .id adds index# Combine into one data frame
# sos_eos_df[is.na(sos_eos_df$SOS) & is.na(sos_eos_df$EOS), ]# Print rows where both SOS and EOS are NA
# 
# 
# 
# 
# 




