# This script uses parallel processing (furrr) to search for the optimal time window
# size (30 to 60 days) and end day (DOY 46 to 250) that produces the most highly 
# correlated cumulative features for predicting PDDOY.

# --- 0. SETUP AND PARALLELIZATION ---
library(dplyr)
library(lubridate)
library(furrr)
library(future)
library(progressr)
library(ggplot2)
library(tidyr)

# Set up parallel processing (using available cores minus 1)
plan(multisession, workers = availableCores() - 1)
cat(sprintf("✅ Running parallel search using %d workers.\n", availableCores() - 1))

# Solar radiation conversion factor: W/m^2 (daily avg) to MJ/m^2/day (daily total)
conversion_factor <- 0.0864

# List of soil/VI indices used in your original feature creation
soil_indices <- c("AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "BCC", "BNDVI", "BWDRVI", "CIG", "CVI",
                  "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI", "ENDVI", "EVI", "EVI2", "EVIv", 
                  "ExG", "ExGR", "ExR", "FCVI", "GARI", "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", 
                  "GOSAVI", "GRNDVI", "GRVI", "GSAVI", "GVMI", "IAVI", "IKAW", "IPVI", "MCARI1", "MCARI2", 
                  "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2", "NDDI", "NDII", 
                  "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv", "NLI", "NMDI", "NRFIg", "NRFIr", "NormG", 
                  "NormNIR", "NormR", "OCVI", "OSAVI", "RCC", "RDVI", "RGBVI", "RGRI", "RI", "SARVI", "SAVI", 
                  "SAVI2", "SEVI", "SI", "SLAVI", "SR", "SR2", "TDVI", "TGI", "TSAVI", "TVI", "TriVI", "VARI", 
                  "VIG", "WDRVI", "WDVI", "bNIRv", "sNIRvLSWI", "sNIRvNDPI", "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", 
                  "sNIRvSWIR", "ANDWI", "AWEInsh", "AWEIsh", "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", 
                  "MuWIR", "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI", "RNDVI", "SWM", 
                  "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS", "BaI", "DBSI", "EMBI", "MBI", "NDSoI", 
                  "NSDS", "NSDSI1", "NSDSI2", "NSDSI3", "RI4XS", "kIPVI", "kNDVI", "kRVI", "nir")

# --- 1. FUNCTION TO CALCULATE METRICS AND CORRELATION ---

#' Calculates cumulative features for a fixed window and finds the correlation with PDDOY.
#' @param params A list/row containing 'end_doy' and 'window_size'.
#' @param merged_list The list of field data frames.
#' @param df_pdoy The reference data frame containing Field_Year and PDDOY.
#' @param soil_indices List of vegetation/soil indices to summarize.
#' @param conversion_factor Factor to convert srad (W/m^2) to energy (MJ/m^2).
calculate_feature_correlation_combined <- function(params, merged_list, df_pdoy, soil_indices, conversion_factor) {
  end_doy <- params$end_doy
  window_size <- params$window_size
  
  # Define the dynamic window
  start_doy <- end_doy - window_size
  
  # Check for invalid windows (too early in the year)
  if (start_doy <= 0) return(NULL)
  
  # --- 1. Aggregate features for all fields in the current window ---
  meteo_summary_list_window <- lapply(names(merged_list), function(field_id) {
    df <- merged_list[[field_id]]
    df$Date <- as.Date(df$Date)
    df$DOY <- yday(df$Date)
    
    # Filter by the current search window
    df_window <- df %>%
      dplyr::filter(DOY >= start_doy & DOY <= end_doy)
    
    # Check for empty window and return NULL if empty
    if (nrow(df_window) < 10) return(NULL) # Require at least 10 data points
    
    # Compute summaries
    df_summary <- df_window %>%
      dplyr::summarise(
        cum_tmean     = sum(tmean, na.rm = TRUE),
        cum_gdd       = sum(gdd, na.rm = TRUE),
        # CORRECTED CUMULATIVE SOLAR RADIATION
        cum_meansrad  = sum(srad * conversion_factor, na.rm = TRUE), 
        cum_vpd       = sum(vpd, na.rm = TRUE),
        cum_tmin      = sum(tmin, na.rm = TRUE),
        cum_tmax      = sum(tmax, na.rm = TRUE),
        cum_RH        = sum(avgRH, na.rm = TRUE),
        cum_kndvi  = sum(kNDVI, na.rm = TRUE),
        cum_soiltemp  = sum(SoilTMP0_10cm_inst, na.rm = TRUE),
        avgsoilorg    = mean(SOC_avg_0_30cm, na.rm = TRUE),
        avgsoilclay   = mean(Clay_avg_0_30cm, na.rm = TRUE),
        
        # Calculate means for soil/VI indices
        soil_means = across(all_of(soil_indices), ~mean(.x, na.rm = TRUE), .names = "mean_{.col}")
      ) %>%
      dplyr::mutate(Field_Year = field_id) %>%
      # Ensure all summarized columns are present for correlation later
      tidyr::unnest(cols = c(soil_means))
    
    return(df_summary)
  })
  
  # --- 2. Combine and Correlate ---
  
  # Remove NULL results and bind rows
  meteo_summary_df_window <- bind_rows(meteo_summary_list_window)
  
  if (nrow(meteo_summary_df_window) < 10) return(NULL) # Minimum required rows for correlation
  
  # Join with PDDOY data
  joined_df <- meteo_summary_df_window %>%
    dplyr::inner_join(df_pdoy %>% dplyr::select(Field_Year, PDDOY), by = "Field_Year") %>%
    dplyr::select(-Field_Year) %>%
    drop_na() # Remove NAs for correlation calculation
  
  if (nrow(joined_df) < 10) return(NULL)
  
  # Separate target (PDDOY) from predictors (all other features)
  pdoy_target <- joined_df$PDDOY
  predictors <- joined_df %>% dplyr::select(-PDDOY)
  
  # Calculate correlation matrix
  cor_matrix <- cor(predictors, pdoy_target, use = "pairwise.complete.obs")
  
  # Calculate mean absolute correlation
  mean_abs_r <- mean(abs(cor_matrix), na.rm = TRUE)
  
  return(data.frame(
    DOY_End = end_doy,
    DOY_Start = start_doy,
    Window_Size = window_size, # <-- Added Window Size
    Mean_Abs_R = mean_abs_r,
    stringsAsFactors = FALSE
  ))
}

# --- 2. PREPARATION AND EXECUTION ---

# Get the target PDDOY data from your previous 'df' (assuming 'df' exists)
df_join_target <- df %>% dplyr::select(Field_Year, PDDOY) %>% drop_na()

# Define search space for window size (30 to 60 days)
window_search_space <- 30:60

# Define search space for End DOY (46 to 250)
doy_search_space <- 46:250 

# Create a data frame of all combinations to iterate over
search_combinations <- expand.grid(
  end_doy = doy_search_space,
  window_size = window_search_space
) %>% 
  # Filter out combinations where start_doy <= 0 (though the function handles this, it reduces unnecessary work)
  dplyr::filter(end_doy > window_size)


# Enable progress bar
handlers(global = TRUE)

cat(sprintf("🚀 Starting parallel search across %d combinations of DOY and Window Size...\n", 
            nrow(search_combinations)))

progressr::with_progress({
  p <- progressor(steps = nrow(search_combinations))
  
  # Run the calculation in parallel for every combination
  correlation_results_list <- future_map(
    1:nrow(search_combinations), 
    ~{
      p() # Update progress
      calculate_feature_correlation_combined(
        params = search_combinations[.x, ], 
        merged_list = merged_list, 
        df_pdoy = df_join_target, 
        soil_indices = soil_indices,
        conversion_factor = conversion_factor
      )
    }, 
    .options = furrr_options(seed = TRUE)
  )
})

# --- 3. AGGREGATION AND PLOT ---

# Stop parallel workers
plan(sequential) 

# Combine results (using bind_rows on the list of data frames)
correlation_results_df <- bind_rows(correlation_results_list) %>%
  drop_na()

# Find the optimal window (size and DOY)
optimal_window <- correlation_results_df %>%
  dplyr::arrange(desc(Mean_Abs_R)) %>%
  dplyr::slice(1)

# Group by window size to find the best mean R for each size
optimal_size_summary <- correlation_results_df %>%
  dplyr::group_by(Window_Size) %>%
  dplyr::summarise(
    Max_R_for_Size = max(Mean_Abs_R, na.rm = TRUE),
    Optimal_End_DOY = DOY_End[which.max(Mean_Abs_R)]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Max_R_for_Size))

# Get the results for the single best window size
best_window_size_df <- correlation_results_df %>%
  dplyr::filter(Window_Size == optimal_window$Window_Size)


# Print results
cat("\n======================================================\n")
cat("          OPTIMAL WINDOW SEARCH RESULTS\n")
cat("======================================================\n")
cat(sprintf("Best Window Size: %d days\n", optimal_window$Window_Size))
cat(sprintf("Best End DOY: %d\n", optimal_window$DOY_End))
cat(sprintf("Optimal 55-day Window: DOY %d to DOY %d\n", 
            optimal_window$DOY_Start, optimal_window$DOY_End))
cat(sprintf("Best Mean |R|: %.4f\n", optimal_window$Mean_Abs_R))
cat("======================================================\n")

# --- Plot 1: Optimal Window Size ---

p_optimal_size <- ggplot(optimal_size_summary, aes(x = Window_Size, y = Max_R_for_Size)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_point(data = optimal_size_summary %>% dplyr::slice(1), 
             aes(x = Window_Size, y = Max_R_for_Size), 
             color = "#D55E00", size = 3) +
  labs(
    title = "Best Correlation (|R|) Achieved vs. Window Size",
    subtitle = paste0("Max Correlation is ", sprintf("%.4f", optimal_window$Mean_Abs_R), 
                      " at Size ", optimal_window$Window_Size, " days."),
    x = "Window Size (Days)",
    y = "Maximum Mean Absolute Correlation (|R|) Achieved"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_optimal_size)

# --- Plot 2: Correlation Curve for the BEST Window Size ---

p_optimal_curve <- ggplot(best_window_size_df, aes(x = DOY_End, y = Mean_Abs_R)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_point(data = optimal_window, 
             aes(x = DOY_End, y = Mean_Abs_R), 
             color = "#D55E00", size = 3) +
  geom_vline(xintercept = optimal_window$DOY_End, linetype = "dashed", color = "#D55E00") +
  annotate("text", 
           x = optimal_window$DOY_End, 
           y = max(best_window_size_df$Mean_Abs_R) * 0.95, 
           label = paste0("Optimal End DOY: ", optimal_window$DOY_End), 
           color = "#D55E00", 
           vjust = -1,
           size = 5) +
  labs(
    title = paste0("Correlation Curve for Optimal Window Size (", optimal_window$Window_Size, " days)"),
    subtitle = "Mean Feature Correlation with PDDOY vs. Window End Day",
    x = "End Day of Window (DOY)",
    y = "Mean Absolute Correlation (|R|) with PDDOY"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_optimal_curve)


# --- Save Plot 2 ---
ggsave(
  filename = "optimekndvigdd_curve.jpeg",
  plot = p_optimal_curve,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

