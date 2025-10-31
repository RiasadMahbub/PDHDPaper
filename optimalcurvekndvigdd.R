# This script uses parallel processing (furrr) to search for the optimal time window
# (size and end DOY) that produces the MINIMUM (MOST NEGATIVE) correlation (R)
# between CUMULATIVE kNDVI (cum_kndvi) and CUMULATIVE GDD (cum_gdd).

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

# soil_indices list is now only included for completeness but not used in the correlation function
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

#' Calculates cumulative GDD and kNDVI for a fixed window and finds their correlation (R).
#' @param params A list/row containing 'end_doy' and 'window_size'.
#' @param merged_list The list of field data frames.
calculate_kndvi_gdd_correlation <- function(params, merged_list) {
  end_doy <- params$end_doy
  window_size <- params$window_size
  
  # Define the dynamic window
  start_doy <- end_doy - window_size
  
  # Check for invalid windows (too early in the year)
  if (start_doy <= 0) return(NULL)
  
  # --- 1. Aggregate features (only GDD and kNDVI) ---
  meteo_summary_list_window <- lapply(names(merged_list)[!names(merged_list) %in% EXCLUDED_FIELD_YEARS], function(field_id) {
    df <- merged_list[[field_id]]
    df$Date <- as.Date(df$Date)
    df$DOY <- yday(df$Date)
    
    # Filter by the current search window
    df_window <- df %>%
      dplyr::filter(DOY >= start_doy & DOY <= end_doy)
    
    # Check for empty window and return NULL if empty
    if (nrow(df_window) < 10) return(NULL) 
    
    # Compute summaries for GDD and kNDVI
    df_summary <- df_window %>%
      dplyr::summarise(
        cum_gdd = sum(gdd, na.rm = TRUE),
        cum_kndvi = sum(kNDVI, na.rm = TRUE)
      ) %>%
      dplyr::mutate(Field_Year = field_id)
    
    return(df_summary)
  })
  
  # --- 2. Combine and Correlate ---
  
  # Remove NULL results and bind rows
  meteo_summary_df_window <- bind_rows(meteo_summary_list_window)
  
  if (nrow(meteo_summary_df_window) < 10) return(NULL)
  
  # Calculate the specific correlation (R) between the two cumulative features
  R_value <- cor(meteo_summary_df_window$cum_gdd, 
                 meteo_summary_df_window$cum_kndvi, 
                 use = "pairwise.complete.obs")
  
  return(data.frame(
    DOY_End = end_doy,
    DOY_Start = start_doy,
    Window_Size = window_size, 
    R_Value = R_value, # Now returning the actual R (can be negative)
    stringsAsFactors = FALSE
  ))
}

# --- 2. PREPARATION AND EXECUTION ---

# Define the Field Years to exclude from the feature search (assumed test set to prevent leakage)
EXCLUDED_FIELD_YEARS <- c(
  "Kelly_05_2024", "Mid_Bransford_2019", "F_20582_69_Res_N_2015", "Shanes_2019",            
  "F_8245_23_Cr_2021", "East_09_2022", "F_8313_59_7_2020", "F_8259_3_M3_2022",       
  "F_8266_33_AP_1_2017", "Baker_40_2023", "HS_NE_3_2023", "Way_01_2022",            
  "Way_01_2023", "East_05_2019", "Flor_12_2023", "Haley_2016",             
  "Bott_of_Res_2020", "Seed_Rice_2018", "Kelly_08_2019", "F_8250_12_M8_2018",      
  "Walls_12_2020", "Walls_12_2023", "Walls_01_2023", "HS_NE_2_3023",           
  "East_13_2024", "The_90_2023", "P2_Farm_01_2019", "Way_05_2019",            
  "Berry_S_3_2023", "F_8320_66_6_2018", "East_12_2022", "F_8256_5_M12_2024",      
  "F_20578_65_Wg_N_2018", "F_8253_37_KD_Shack_2024", "F_8308_54_2_2020", "Walls_09_2024",          
  "Dewitt_1_2023", "Walls_11_2023", "Walls_05_2023", "East_13_2022",           
  "East_Harvey_2019", "F_8309_55_3_2020", "F_8292_29_Pn_3_2021", "Wst_Bransford_2024",     
  "Carr_North_2022", "Kelly_08_2024", "East_04_2024", "Fairley_NW_3_2023",      
  "F_8256_5_M12_2019", "Shanes_2022", "East_09_2020", "Way_02_2020",            
  "Lyntz_2022", "Walls_13_2024", "Carr_North_2019", "F_1_Farm_8_2021",        
  "F_8249_39_BD_E_2018", "Wildy_W_8_2023", "Bransford_Est_2019", "Seed_Rice_2015",         
  "Baker_40_2022", "Cotton_Patch_2020", "Kelly_04_2023", "F_8320_66_6_2015",       
  "F_8292_29_Pn_3_2023", "East_04_2023", "T18M_Farm_05_2022", "F_8309_55_3_2016",       
  "Kelly_07_2023", "F_8307_53_1_2020", "East_10_2020", "East_08_2020",           
  "EM_9E_2023", "Cattlet_02_2021", "New_Ground_2021", "MP_13_2023",             
  "N4_Farm_05_2021", "Clark14_2021", "East_04_2019", "F_202_2020",             
  "Cattlet_01_2016", "East_01_2019", "F_20581_68_MF_2019", "Hardy_7_2023",           
  "F_8262_13_Sm_1_2020", "Field12west_2024", "East_11_2020", "F_8250_12_M8_2020",      
  "Fairley_NE_1_2023", "F_8266_33_AP_1_2016", "East_08_2023", "Walls_11_2022",          
  "East_Harvey_2016", "F_16NW_2022", "Kelly_01_2021", "EC_1_2023",              
  "West_Joe_T_2023", "Baker_20_2024", "Field_2_Farm_8_2022", "F_8293_32_Pn_House_2020",
  "F_8263_14_Sm_2_2017", "FieldF_2024", "Way_02_2022", "East_12_2024",           
  "Lyntz_2021", "East_02_2023", "Walls_02_2023", "F_8245_23_Cr_2023",      
  "West_Joe_T_2017", "Pops_2022", "Flat_2020", "Walls_05_2022",          
  "Mid_Bransford_2015", "Baker_50_2024", "Wst_Bransford_2021", "F_202_2018",             
  "Cattlet_01_2017", "Store_1_2023", "F_20581_68_MF_2018", "East_02_2024",           
  "Shanes_2016", "Kelly_01_2022", "F_8316_62_2_2015", "Kelly_08_2023",          
  "Kelly_08_2021", "F_8274_20_M14_2016", "Kelly_08_2022", "Kelly_02_2023",          
  "Way_03_2022", "Judys_2019", "Carr_North_2020", "Shanes_2015"
)
# The target PDDOY data is no longer needed for correlation, but we keep the structure
# for the search parameters.

# Define search space for window size (30 to 60 days)
window_search_space <- 30:60

# Define search space for End DOY (46 to 250)
doy_search_space <- 46:250 

# Create a data frame of all combinations to iterate over
search_combinations <- expand.grid(
  end_doy = doy_search_space,
  window_size = window_search_space
) %>% 
  # Filter out combinations where start_doy <= 0 
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
      calculate_kndvi_gdd_correlation(
        params = search_combinations[.x, ], 
        merged_list = merged_list
        # Removed unnecessary arguments: df_pdoy, soil_indices, conversion_factor
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

# Find the optimal window (size and DOY) where R is MINIMUM (most negative)
optimal_window <- correlation_results_df %>%
  dplyr::arrange(R_Value) %>% 
  dplyr::slice(1)

# Group by window size to find the best (most negative) R for each size
optimal_size_summary <- correlation_results_df %>%
  dplyr::group_by(Window_Size) %>%
  dplyr::summarise(
    Min_R_for_Size = min(R_Value, na.rm = TRUE),
    Optimal_End_DOY = DOY_End[which.min(R_Value)]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Min_R_for_Size) 

# Get the results for the single best window size
best_window_size_df <- correlation_results_df %>%
  dplyr::filter(Window_Size == optimal_window$Window_Size)


# Print results
cat("\n======================================================\n")
cat("       OPTIMAL NEGATIVE CORRELATION SEARCH\n")
cat("======================================================\n")
cat(sprintf("Metric: R(cum_kndvi, cum_gdd)\n"))
cat(sprintf("Most Negative R: %.4f\n", optimal_window$R_Value))
cat(sprintf("Best Window Size: %d days\n", optimal_window$Window_Size))
cat(sprintf("Best End DOY: %d\n", optimal_window$DOY_End))
cat(sprintf("Optimal Window Period: DOY %d to DOY %d\n", 
            optimal_window$DOY_Start, optimal_window$DOY_End))
cat("======================================================\n")

# --- Plot 1: Optimal Window Size ---

p_optimal_size <- ggplot(optimal_size_summary, aes(x = Window_Size, y = Min_R_for_Size)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_point(data = optimal_size_summary %>% dplyr::slice(1), 
             aes(x = Window_Size, y = Min_R_for_Size), 
             color = "#D55E00", size = 3) +
  labs(
    title = "Minimum Correlation (R) Achieved vs. Window Size",
    subtitle = paste0("Most Negative R is ", sprintf("%.4f", optimal_window$R_Value), 
                      " at Size ", optimal_window$Window_Size, " days."),
    x = "Window Size (Days)",
    y = "Minimum Correlation (R) Achieved"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_optimal_size)

# --- Save Plot 1 ---
ggsave(
  filename = "optimekndvigdd_size.jpeg",
  plot = p_optimal_size,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

# --- Plot 2: Correlation Curve for the BEST Window Size ---

p_optimal_curve <- ggplot(best_window_size_df, aes(x = DOY_End, y = R_Value)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_point(data = optimal_window, 
             aes(x = DOY_End, y = R_Value), 
             color = "#D55E00", size = 3) +
  geom_vline(xintercept = optimal_window$DOY_End, linetype = "dashed", color = "#D55E00") +
  annotate("text", 
           x = optimal_window$DOY_End, 
           y = max(best_window_size_df$R_Value) * 0.95, 
           label = paste0("Optimal End DOY: ", optimal_window$DOY_End), 
           color = "#D55E00", 
           vjust = -1,
           size = 5) +
  labs(
    title = paste0("Correlation Curve for Optimal Window Size (", optimal_window$Window_Size, " days)"),
    subtitle = "R(cum_kndvi, cum_gdd) vs. Window End Day",
    x = "End Day of Window (DOY)",
    y = "Correlation (R)"
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

# Check the total count and the count of fields actually used in the analysis.
# This relies on 'merged_list' and 'EXCLUDED_FIELD_YEARS' being defined in the environment.

# 1. Determine which fields from the exclusion list are present in merged_list
present_excluded_fields <- intersect(names(merged_list), EXCLUDED_FIELD_YEARS)

# 2. Calculate the expected number of fields for analysis
total_fields <- length(names(merged_list))
expected_analysis_count <- total_fields - length(present_excluded_fields)

cat("\n======================================================\n")
cat(sprintf("Total fields found in 'merged_list': %d\n", total_fields))
cat(sprintf("Fields successfully removed (intersection count): %d\n", length(present_excluded_fields)))
cat(sprintf("Expected count of fields for analysis: %d\n", expected_analysis_count))
cat("======================================================\n")


# 3. Perform the definitive check: Intersect the list *used* by the function 
#    with the *excluded* list. The result should be zero.

# The list of field IDs the function actually processes:
fields_being_processed <- names(merged_list)[!names(merged_list) %in% EXCLUDED_FIELD_YEARS]

# Find any overlap (should be 0)
excluded_and_processed <- intersect(fields_being_processed, EXCLUDED_FIELD_YEARS)

if (length(excluded_and_processed) == 0) {
  cat("\n✅ Verification successful: Zero excluded Field_Year entries are present in the list used for correlation.\n")
} else {
  cat("\n⚠️ Verification FAILED: The following excluded fields were found in the analysis list:\n")
  print(excluded_and_processed)
}

