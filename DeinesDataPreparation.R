#---------------------------------------------------------
#CALCULATE GCVI INDEX
#---------------------------------------------------------
# Remove Dewitt_2_2023 from vi_list_gt20
vi_list_gt20 <- vi_list_gt20[!names(vi_list_gt20) %in% "Dewitt_2_2023"]
vi_list_gt20 <- lapply(vi_list_gt20, function(df) {
  df %>%
    mutate(GCVI = (nir/green) - 1)
})


library(dplyr)
library(minpack.lm) # For nlsLM
library(lubridate) # For yday
library(purrr) # For map
library(tibble) # For tibble

# The core function, modified to take a band name and rename output columns
fit_harmonic_deines <- function(df, value_col = "kNDVI") {
  # Add the DOY and a scaled time variable 't' (Day of Year / 365)
  df <- df %>%
    dplyr::mutate(DOY = yday(Date), t = DOY / 365) %>%
    dplyr::filter(!is.na(.data[[value_col]]))
  
  # Skip if there's not enough data for fitting
  if (nrow(df) < 10) return(NULL) 
  
  t <- df$t
  y <- df[[value_col]]
  
  # Set the harmonic frequency. omega=1.5 is a common choice.
  omega <- 1.5
  
  # Define the harmonic model function
  harmonic_model <- function(t, c, a1, b1, a2, b2) {
    c + 
      a1 * cos(2 * pi * omega * t) + b1 * sin(2 * pi * omega * t) +
      a2 * cos(2 * pi * omega * 2 * t) + b2 * sin(2 * pi * omega * 2 * t)
  }
  
  tryCatch({
    # Fit the non-linear least squares model using nlsLM for robustness
    fit <- nlsLM(y ~ harmonic_model(t, c, a1, b1, a2, b2),
                 start = list(c = mean(y), a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1),
                 control = nls.lm.control(maxiter = 500))
    
    # Generate predictions from the fitted model
    y_pred <- predict(fit)
    
    # Calculate performance metrics
    R2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)
    NSE <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2) # NSE is identical to R2 for linear models
    RMSE <- sqrt(mean((y - y_pred)^2))
    MAE <- mean(abs(y - y_pred))
    
    # Predict the full yearly curve (365 points) from the model
    t_seq <- seq(0, 1, length.out = 365)
    pred <- predict(fit, newdata = data.frame(t = t_seq))
    
    # Find the day of year (DOY) and value of the fitted and observed peaks
    max_idx <- which.max(pred)
    doy_max_fit <- round(t_seq[max_idx] * 365)
    val_max_fit <- pred[max_idx]
    doy_max_obs <- df$DOY[which.max(y)]
    val_max_obs <- max(y)
    
    # Extract the model coefficients
    coefs <- coef(fit)
    
    # Create the results data frame
    results <- data.frame(
      Field_ID = df$Field_Year[1], # Use the Field_Year column from the data itself
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
    )
    
    # Rename columns with the band name prefix, excluding Field_ID
    names(results)[-1] <- paste0(value_col, "_", names(results)[-1])
    return(results)
    
  }, error = function(e) {
    # Return NULL if the model fitting fails
    return(NULL)
  })
}

# The previous line that created field_names is no longer needed
# field_names <- gsub("\\.csv$", "", basename(vi_csv_files_gt20))

# Define the list of bands to process
bands_to_process <- c("kNDVI", "GCVI", "swir1", "swir2", "nir")

# Initialize an empty list to store all results
all_harmonic_results <- list()

# Loop through each band and process all fields
for (band in bands_to_process) {
  cat("Processing band:", band, "\n")
  
  # Apply the function to each data frame in the list for the current band
  band_results <- lapply(vi_list_gt20, function(df) {
    fit_harmonic_deines(df, value_col = band)
  })
  
  # Combine results and filter out NULLs
  combined_results <- bind_rows(band_results) %>%
    dplyr::filter(!is.na(Field_ID)) # Field_ID is now added within the function
  
  all_harmonic_results[[band]] <- combined_results
}

# Use purrr::reduce and dplyr::full_join to merge all results by Field_ID
# This will create a single wide data frame with one row per field
harmonic_df <- purrr::reduce(all_harmonic_results, dplyr::full_join, by = "Field_ID")

# The final data frame now has the structure you requested
print(colnames(harmonic_df))
print(nrow(harmonic_df))

# Example of a summary calculation from the final combined dataframe
# You can now filter and analyze results for any band.
print(harmonic_df %>%
        summarise(mean_R2 = mean(kNDVI_R2, na.rm = TRUE)))

harmonic_df
unique(harmonic_df$Field_ID)


#---------------------------------------------
#METEO-----------------------------------
#---------------------------------------------
# This section is for the meteo_summary_list generation, which was the focus of the user's query.
# It assumes 'merged_list' and 'vi_csv_files_gt20' are defined elsewhere in the script or environment.

# New function to get meteorological summary for each field
get_meteorological_summary <- function(df) {
  # Get the field ID from the Field_Year column
  field_id <- df$Field_Year[1]
  
  # Ensure Date column is in Date format and add Month
  df$Date <- as.Date(df$Date)
  df$Month <- month(df$Date)
  
  # April-June metrics
  df_apr_jun <- df %>% dplyr::filter(Month %in% 4:6)
  
  # Jan-April metrics for early season ppt
  df_jan_apr <- df %>% dplyr::filter(Month %in% 1:4)
  
  # Mar-May metrics for GDD
  df_mar_may <- df %>% dplyr::filter(Month %in% 3:5)
  
  # June-Aug metrics for growing season mean temperature
  df_jun_aug <- df %>% dplyr::filter(Month %in% 6:8)
  
  # April, May, and June for specific monthly metrics
  df_apr <- df %>% dplyr::filter(Month == 4)
  df_may <- df %>% dplyr::filter(Month == 5)
  df_jun <- df %>% dplyr::filter(Month == 6)
  
  # Calculate the metrics for each period
  summary_stats <- tibble(
    Field_ID = field_id,
    ppt_Apr_Jun = mean(df_apr_jun$ppt, na.rm = TRUE),
    tmean_Apr_Jun = mean(df_apr_jun$tmean, na.rm = TRUE),
    tmax_Apr_Jun = mean(df_apr_jun$tmax, na.rm = TRUE),
    vpd_Apr_Jun = mean(df_apr_jun$vpd, na.rm = TRUE),
    soiltemp_Apr_Jun = mean(df_apr_jun$SoilTMP0_10cm_inst, na.rm = TRUE),
    ppt_Jan_Apr_early_season = sum(df_jan_apr$ppt, na.rm = TRUE),
    gdd_Mar_May = mean(df_mar_may$gdd, na.rm = TRUE),
    tmean_Jun_Aug_growing_season = mean(df_jun_aug$tmean, na.rm = TRUE),
    # New monthly metrics
    tmin_Apr = mean(df_apr$tmin, na.rm = TRUE),
    tmin_May = mean(df_may$tmin, na.rm = TRUE),
    vpd_Jun = mean(df_jun$vpd, na.rm = TRUE),
    vpd_May = mean(df_may$vpd, na.rm = TRUE),
    # Additional monthly metrics from the image
    tmax_Apr = mean(df_apr$tmax, na.rm = TRUE),
    ppt_Apr = mean(df_apr$ppt, na.rm = TRUE),
    ppt_May = mean(df_may$ppt, na.rm = TRUE),
    soiltemp_May = mean(df_may$SoilTMP0_10cm_inst, na.rm = TRUE),
    tmax_May = mean(df_may$tmax, na.rm = TRUE),
    PDDOY = df$PDDOY[1], # Capture PDDOY from the first row since it's constant
    HDDOY = df$HDDOY[1] # Capture HDDOY from the first row since it's constant
  )
  
  return(summary_stats)
}



# Now, calculate the meteorological summary for each field
meteo_summary_list <- lapply(vi_list_gt20, get_meteorological_summary)

# Combine the list of summaries into a single data frame
meteo_df <- bind_rows(meteo_summary_list)

# Join the meteorological data with the harmonic fitting data
deines_results_df <- dplyr::full_join(harmonic_df, meteo_df, by = "Field_ID")

# The final data frame now has the structure you requested
print(colnames(deines_results_df))
print(nrow(deines_results_df))
print(deines_results_df)

