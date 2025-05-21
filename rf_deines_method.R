############################################################
# Title: Harmonic Curve Fitting for NDVI/GCVI Time Series
# Author: Riasad Bin Mahbub
# Date: 2025-05-15
# Description: Fit harmonic curves to VI time series data,
#              extract key phenology metrics, and plot.
# Paper citation: Field-scale dynamics of planting dates in the US Corn Belt from 2000 to 2020
############################################################

# -----------------------------
# 1. Load Required Packages
# -----------------------------
library(minpack.lm)
library(dplyr)
library(lubridate)
library(ggplot2)

# -----------------------------
# 2. Define Feature Clusters (Optional Metadata)
# -----------------------------
# Define feature clusters used in the Random Forest model (rf_deines_method)
feature_clusters <- list(
  # 1. Meteorological / Climate Features
  Meteorological_Climate = c(
    "Early season precipitation (Jan–Apr) - GRIDMET",
    "Growing Degree Days (Mar–May) - GRIDMET-derived",
    "Growing season mean temperature (Jun–Aug) - GRIDMET",
    "Mean soil temperature (Apr–Jun) - GLDAS Monthly",
    "Precipitation (Apr–Jun) - GRIDMET Monthly",
    "Mean minimum temperature (Apr–Jun) - GRIDMET Monthly",
    "Mean maximum temperature (Apr–Jun) - GRIDMET Monthly",
    "Mean temperature (Apr–Jun) - GRIDMET Monthly",
    "Climate water deficit (Apr–Jun) - TerraClimate Monthly",
    "Soil moisture (Apr–Jun) - TerraClimate Monthly"
  ),
  # 2. Harmonic Features (from Landsat)
  Harmonic_Landsat = c(
    "Harmonic constant (GCVI, NIR, SWIR1, SWIR2)",
    "Harmonic cosine coefficients a1 (GCVI, NIR, SWIR1, SWIR2)",
    "Harmonic cosine coefficients a2 (GCVI, NIR, SWIR1, SWIR2)",
    "Harmonic sine coefficients b1 (GCVI, NIR, SWIR1, SWIR2)",
    "Harmonic sine coefficients b2 (GCVI, NIR, SWIR1, SWIR2)"
  ),
  # 3. Vegetation Index Phenology
  Vegetation_Phenology = c(
    "Day and value of max observed GCVI - Landsat",
    "Day and value of max GCVI from harmonic fit - Landsat-derived"
  ),
  # 4. Static Spatial Variables
  Spatial_Metadata = c(
    "Year",
    "Latitude",
    "Longitude"
  )
)


vi_list_gt20[[1]]$kNDVI
vi_list_gt20[[1]]$nir
vi_list_gt20[[1]]$swir1
vi_list_gt20[[1]]$swir2


# -----------------------------
# 4. Apply to a Single Example
# -----------------------------
# Define the harmonic fitting function
fit_harmonic_deines <- function(df, value_col = "GCVI") {
  df <- df %>% 
    mutate(DOY = yday(Date),
           t = DOY / 365)
  # Remove NAs
  df <- df[!is.na(df[[value_col]]), ]
  if (nrow(df) < 10) return(NULL)  # skip sparse data
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
    # Predict fitted curve over smooth time
    t_seq <- seq(0, 1, length.out = 365)
    pred <- predict(fit, newdata = data.frame(t = t_seq))
    # Extract max from fit
    max_idx <- which.max(pred)
    doy_max_fit <- round(t_seq[max_idx] * 365)
    val_max_fit <- pred[max_idx]
    # Extract max from observed
    max_obs_idx <- which.max(y)
    doy_max_obs <- df$DOY[max_obs_idx]
    val_max_obs <- y[max_obs_idx]
    coefs <- coef(fit)
    return(data.frame(
      DOY_max_obs = doy_max_obs,
      Value_max_obs = val_max_obs,
      DOY_max_fit = doy_max_fit,
      Value_max_fit = val_max_fit,
      Intercept_c = coefs["c"],
      a1 = coefs["a1"], b1 = coefs["b1"],
      a2 = coefs["a2"], b2 = coefs["b2"]
    ))
  }, error = function(e) {
    return(NULL)
  })
}

# Now apply to vi_list_gt20[[1]]
result_gcvi <- fit_harmonic_deines(vi_list_gt20[[1]], value_col = "NDVI")
# View result
print(result_gcvi)

# Extract the original data
df <- vi_list_gt20[[1]] %>%
  mutate(DOY = yday(Date),
         t = DOY / 365) %>%
  dplyr::filter(!is.na(NDVI))
# Step 2: Assign values to local variables
t <- df$t
y <- df$NDVI
omega <- 1.5

# Step 3: Fit the harmonic model
harmonic_model <- function(t, c, a1, b1, a2, b2) {
  c + 
    a1 * cos(2 * pi * omega * 1 * t) + b1 * sin(2 * pi * omega * 1 * t) +
    a2 * cos(2 * pi * omega * 2 * t) + b2 * sin(2 * pi * omega * 2 * t)
}

fit <- nlsLM(y ~ harmonic_model(t, c, a1, b1, a2, b2),
             start = list(c = mean(y), a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1),
             control = nls.lm.control(maxiter = 500))

# Step 4: Predict over full year
t_seq <- seq(0, 1, length.out = 365)
doy_seq <- round(t_seq * 365)
ndvi_pred <- predict(fit, newdata = data.frame(t = t_seq))
# Step 5: Create prediction dataframe
fit_df <- data.frame(DOY = doy_seq, NDVI = ndvi_pred)
# Extract and format coefficients
coefs <- coef(fit)
coef_text <- sprintf("c = %.3f\na1 = %.3f\nb1 = %.3f\na2 = %.3f\nb2 = %.3f",
                     coefs["c"], coefs["a1"], coefs["b1"], coefs["a2"], coefs["b2"])
# Plot again with annotation
ggplot() +
  geom_point(data = df, aes(x = DOY, y = NDVI), color = "blue", alpha = 0.6) +
  geom_line(data = fit_df, aes(x = DOY, y = NDVI), color = "red", size = 1.2) +
  labs(title = "NDVI Harmonic Fit vs Observed",
       x = "Day of Year (DOY)", y = "NDVI") +
  theme_minimal(base_size = 14) +
  annotate("text", x = 50, y = 0.95, label = coef_text, hjust = 0, vjust = 1, size = 4, color = "black")

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
    coefs <- coef(fit)
    return(data.frame(
      DOY_max_obs = doy_max_obs,
      Value_max_obs = val_max_obs,
      DOY_max_fit = doy_max_fit,
      Value_max_fit = val_max_fit,
      c = coefs["c"],
      a1 = coefs["a1"], b1 = coefs["b1"],
      a2 = coefs["a2"], b2 = coefs["b2"]
    ))
  }, error = function(e) {
    return(NULL)
  })
}

# Directory to save plots
save_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/Harmonic_Deines"
dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

# Initialize result list
results_list <- list()

# Loop through vi_list_gt20
for (i in seq_along(vi_list_gt20)) {
  df <- vi_list_gt20[[i]]
  field_id <- if (!is.null(df$field_id[1])) as.character(df$field_id[1]) else paste0("Field_", i)
  # Fit harmonic model
  res <- fit_harmonic_deines(df, value_col = "kNDVI")
  if (!is.null(res)) {
    res$ID <- field_id
    results_list[[field_id]] <- res
    # Plot and save
    df_plot <- df %>%
      mutate(DOY = yday(Date),
             t = DOY / 365) %>%
      dplyr::filter(!is.na(kNDVI))
    t <- df_plot$t
    y <- df_plot$kNDVI
    omega <- 1.5
    harmonic_model <- function(t, c, a1, b1, a2, b2) {
      c + 
        a1 * cos(2 * pi * omega * 1 * t) + b1 * sin(2 * pi * omega * 1 * t) +
        a2 * cos(2 * pi * omega * 2 * t) + b2 * sin(2 * pi * omega * 2 * t)
    }
    
    fit <- nlsLM(y ~ harmonic_model(t, c, a1, b1, a2, b2),
                 start = list(c = mean(y), a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1),
                 control = nls.lm.control(maxiter = 500))
    t_seq <- seq(0, 1, length.out = 365)
    doy_seq <- round(t_seq * 365)
    pred_ndvi <- predict(fit, newdata = data.frame(t = t_seq))
    fit_df <- data.frame(DOY = doy_seq, kNDVI = pred_ndvi)
    coefs <- coef(fit)
    coef_text <- sprintf("c = %.3f\na1 = %.3f\nb1 = %.3f\na2 = %.3f\nb2 = %.3f",
                         coefs["c"], coefs["a1"], coefs["b1"], coefs["a2"], coefs["b2"])
    
    p <- ggplot() +
      geom_point(data = df_plot, aes(x = DOY, y = kNDVI), color = "blue", alpha = 0.6) +
      geom_line(data = fit_df, aes(x = DOY, y = kNDVI), color = "red", size = 1.2) +
      labs(title = paste("kNDVI Harmonic Fit:", field_id),
           x = "DOY", y = "kNDVI") +
      annotate("text", x = 30, y = max(y, na.rm = TRUE) * 0.95,
               label = coef_text, hjust = 0, vjust = 1, size = 4) +
      theme_minimal(base_size = 14)
    
    ggsave(filename = file.path(save_dir, paste0("harmonic_fit_", field_id, ".png")),
           plot = p, width = 8, height = 5)
  }
}

# Combine all results into a single dataframe
coef_df <- bind_rows(results_list) %>% 
  select(ID, everything())  # move ID to front

# View or save to CSV
print(coef_df)
# write.csv(coef_df, "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/Harmonic_Deines/harmonic_coefficients.csv", row.names = FALSE)




fit_harmonic_deines <- function(df, value_col = "kNDVI") {
  df <- df %>%
    mutate(DOY = yday(Date), t = DOY / 365) %>%
    filter(!is.na(.data[[value_col]]))
  
  if (nrow(df) < 10) return(NULL)  # Skip if not enough data
  
  t <- df$t
  y <- df[[value_col]]
  omega <- 1.5
  
  # Harmonic function model
  harmonic_model <- function(t, c, a1, b1, a2, b2) {
    c + 
      a1 * cos(2 * pi * omega * t) + b1 * sin(2 * pi * omega * t) +
      a2 * cos(2 * pi * omega * 2 * t) + b2 * sin(2 * pi * omega * 2 * t)
  }
  
  # Fit model using non-linear least squares
  tryCatch({
    fit <- nlsLM(y ~ harmonic_model(t, c, a1, b1, a2, b2),
                 start = list(c = mean(y), a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1),
                 control = nls.lm.control(maxiter = 500))
    
    # Predict full-year curve
    t_seq <- seq(0, 1, length.out = 365)
    pred <- predict(fit, newdata = data.frame(t = t_seq))
    
    # Extract max values
    max_idx <- which.max(pred)
    doy_max_fit <- round(t_seq[max_idx] * 365)
    val_max_fit <- pred[max_idx]
    doy_max_obs <- df$DOY[which.max(y)]
    val_max_obs <- max(y)
    
    # Return summary
    coefs <- coef(fit)
    return(data.frame(
      DOY_max_obs = doy_max_obs,
      Value_max_obs = val_max_obs,
      DOY_max_fit = doy_max_fit,
      Value_max_fit = val_max_fit,
      Intercept_c = coefs["c"],
      a1 = coefs["a1"], b1 = coefs["b1"],
      a2 = coefs["a2"], b2 = coefs["b2"]
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
  mutate(DOY = yday(Date), t = DOY / 365) %>%
  filter(!is.na(NDVI))

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
