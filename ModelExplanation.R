# -----------------------------------------------------------------------------
# 1.1 Filter for Planting and Harvest Data
#
# Create a new list 'vi_list_gt20_PDHD' containing only the dataframes from
# 'vi_list_gt20' that have both 'PDDOY' and 'HDDOY' columns.
# -----------------------------------------------------------------------------

vi_list_gt20_PDHD <- purrr::keep(vi_list_gt20, ~all(c("PDDOY", "HDDOY") %in% names(.x)))

print("Original list length:")
print(length(vi_list_gt20))
print("Filtered list length (with PDDOY and HDDOY):")
print(length(vi_list_gt20_PDHD))


# -----------------------------------------------------------------------------
# 1.2 Calculate Growing Season Length (GSL) and add it as a new column
#
# Iterate through the filtered list, calculate GSL for each dataframe, and add
# it as a new column. This prepares the data for the next step.
# -----------------------------------------------------------------------------

vi_list_gt20_PDHD_GSL <- purrr::map(vi_list_gt20_PDHD, function(df) {
  # Calculate GSL as HDDOY - PDDOY + 1 to include both start and end days
  gsl_value <- df$HDDOY[[1]] - df$PDDOY[[1]] + 1
  df %>%
    mutate(GSL = gsl_value)
})

print("First few rows of a dataframe in the new list with GSL:")
print(head(vi_list_gt20_PDHD_GSL[[1]]))

# Remove the problematic dataframe before processing.
# You can uncomment and run this line if the "Greenfield_NW19_2024" dataframe
# is causing an error due to missing or invalid data.
# vi_list_gt20_PDHD_GSL[["Greenfield_NW19_2024"]] <- NULL
# Filter to keep only those data frames that contain the 'dayl' column
vi_list_gt20_PDHD_GSL <- purrr::keep(vi_list_gt20_PDHD_GSL, ~ "dayl" %in% names(.x))
# -----------------------------------------------------------------------------
# 2. Function to Process Each Dataframe
#
# This function encapsulates the logic for setting values outside the growing
# season to NA and then accumulating the specified variables.
# -----------------------------------------------------------------------------

process_dataframe <- function(df) {
  
  # Check if PDDOY and HDDOY columns exist in the dataframe
  if (!all(c("PDDOY", "HDDOY") %in% names(df))) {
    message(paste("Skipping", unique(df$Field_Year), "- PDDOY or HDDOY columns not found."))
    return(tibble(
      Field_Year = unique(df$Field_Year),
      GSL = NA,
      accumulated_dayl = NA,
      accumulated_tmean = NA,
      accumulated_vpd = NA,
      accumulated_avgRH = NA,
      accumulated_srad = NA,
      #accumulated_SoilTMP0_10cm_inst = NA,
      accumulated_tmax = NA,
      accumulated_tmin = NA
    ))
  }
  
  pd_doy <- df$PDDOY[[1]]
  hd_doy <- df$HDDOY[[1]]
  gsl <- df$GSL[[1]]
  
  # Add a check for NA values in PDDOY and HDDOY before the logical test
  if (is.na(pd_doy) || is.na(hd_doy)) {
    message(paste("Skipping", unique(df$Field_Year), "- PDDOY or HDDOY value is NA."))
    return(tibble(
      Field_Year = unique(df$Field_Year),
      GSL = NA,
      accumulated_dayl = NA,
      accumulated_tmean = NA,
      accumulated_vpd = NA,
      accumulated_avgRH = NA,
      accumulated_srad = NA,
      #accumulated_SoilTMP0_10cm_inst = NA,
      accumulated_tmax = NA,
      accumulated_tmin = NA
    ))
  }
  
  # Handle the case where planting DOY is after harvest DOY
  if (pd_doy > hd_doy) {
    message(paste("Skipping", unique(df$Field_Year), "- PDDOY is after HDDOY."))
    return(tibble(
      Field_Year = unique(df$Field_Year),
      GSL = NA,
      accumulated_dayl = NA,
      accumulated_tmean = NA,
      accumulated_vpd = NA,
      accumulated_avgRH = NA,
      accumulated_srad = NA,
      #accumulated_SoilTMP0_10cm_inst = NA,
      accumulated_tmax = NA,
      accumulated_tmin = NA
    ))
  }
  
  # Set values outside the growing season to NA for accumulation
  df_na <- df %>%
    mutate(
      tmean = ifelse(DOY >= pd_doy & DOY <= hd_doy, tmean, NA),
      tmin = ifelse(DOY >= pd_doy & DOY <= hd_doy, tmin, NA),
      tmax = ifelse(DOY >= pd_doy & DOY <= hd_doy, tmax, NA),
      dayl = ifelse(DOY >= pd_doy & DOY <= hd_doy, dayl, NA),
      vpd = ifelse(DOY >= pd_doy & DOY <= hd_doy, vpd, NA),
      avgRH = ifelse(DOY >= pd_doy & DOY <= hd_doy, avgRH, NA),
      srad = ifelse(DOY >= pd_doy & DOY <= hd_doy, srad, NA),
      #SoilTMP0_10cm_inst = ifelse(DOY >= pd_doy & DOY <= hd_doy, SoilTMP0_10cm_inst, NA)
    )
  
  # Perform accumulations for each variable
  summary_stats <- df_na %>%
    summarise(
      Field_Year = first(Field_Year),
      GSL = gsl,
      accumulated_dayl = sum(dayl, na.rm = TRUE),
      accumulated_tmean = sum(tmean, na.rm = TRUE),
      accumulated_vpd = sum(vpd, na.rm = TRUE),
      accumulated_avgRH = sum(avgRH, na.rm = TRUE),
      accumulated_srad = sum(srad, na.rm = TRUE),
      #accumulated_SoilTMP0_10cm_inst = sum(SoilTMP0_10cm_inst, na.rm = TRUE),
      accumulated_tmax = sum(tmax, na.rm = TRUE),
      accumulated_tmin = sum(tmin, na.rm = TRUE)
    )
  
  return(summary_stats)
}

# -----------------------------------------------------------------------------
# 3. Apply the Function to the Filtered List and Combine Results
# -----------------------------------------------------------------------------

# Use purrr::map_dfr to apply the function to the filtered list
# and combine the results into a single dataframe.
final_results <- map_dfr(vi_list_gt20_PDHD_GSL, process_dataframe)

# Print the final results
print(final_results)


# Left join: keeps all rows from meteo_summary_df, adds columns from combined_df
final_results <- final_results %>%
  left_join(combined_df, by = "Field_Year")
# -----------------------------------------------------------------------------
# 4. Optional: Further Analysis (e.g., plotting)
# -----------------------------------------------------------------------------
final_results$b1
final_results$b2

final_results$DOY_max_fit
final_results$DOY_max_obs

final_results$a1
final_results$a2

final_results$Intercept_c
# Example: Plot GSL vs. accumulated temperature
# You can uncomment and run this section to see a simple plot.
# library(ggplot2)
#
# Define your output directory
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"

# Ensure the directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Extract all accumulated variable names (everything except Field_Year and GSL)
accum_vars <- setdiff(colnames(final_results), c("Field_Year", "GSL"))

# Loop through each accumulated variable and plot vs GSL
# Loop through each accumulated variable
for (var in accum_vars) {
  for (color_var in color_vars) {
    
    # Make sure the color variable is treated as a factor or numeric accordingly
    color_data <- final_results[[color_var]]
    
    p <- ggplot(final_results, aes_string(x = var, y = "GSL", color = color_var)) +
      geom_point(size = 2, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
      scale_color_viridis_c(option = "D", name = color_var) +
      labs(title = paste("GSL vs", var, "colored by", color_var),
           x = var,
           y = "Growing Season Length (GSL)") +
      theme_minimal(base_size = 14)
    
    # File name
    file_name <- paste0("GSL_vs_", var, "_by_", color_var, ".png")
    file_path <- file.path(output_dir, file_name)
    
    # Save plot
    ggsave(file_path, plot = p, width = 7, height = 5, dpi = 300)
  }
}

#--------------------------------------------------------------------
#Plot LSWI
#--------------------------------------------------------------------
vi_list_gt20_PDHD[[1]]$doy
vi_list_gt20_PDHD[[1]]$HDDOY
vi_list_gt20_PDHD[[1]]$PDDOY
vi_list_gt20_PDHD[[1]]$LSWI
vi_list_gt20_PDHD[[1]]$WI1
library(ggplot2)

# Output folder path
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/LSWI_WT"

# Create folder if it doesn't exist
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Loop through each dataframe
for (i in seq_along(vi_list_gt20_PDHD)) {
  df <- vi_list_gt20_PDHD[[i]]
  
  # Skip if required columns are missing
  if (!all(c("doy", "LSWI", "WI1", "PDDOY", "HDDOY") %in% colnames(df))) next
  
  # Reshape data to long format for ggplot
  df_long <- tidyr::pivot_longer(df, cols = c("LSWI", "WI1"), names_to = "Index", values_to = "Value")
  
  # Create the plot
  p <- ggplot(df_long, aes(x = doy, y = Value, color = Index)) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = PDDOY), color = "green", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = HDDOY), color = "red", linetype = "dashed", size = 1) +
    labs(title = paste0("LSWI and WI1 vs DOY - Field ", i),
         x = "Day of Year (DOY)",
         y = "Index Value",
         color = "Index") +
    scale_color_manual(values = c("LSWI" = "steelblue", "WT" = "orange")) +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = file.path(output_dir, paste0("LSWI_WT_DOY_Field_", i, ".jpeg")),
         plot = p, width = 7, height = 5)
}


#### 
head(sos_eos_df$Field_Year)
head(final_results$Field_Year)

accumulatedsoseos <- inner_join(sos_eos_with_lag, final_results, by = "Field_Year")
accumulatedsoseos$GSL
accumulatedsoseos$soseosgsl<-(accumulatedsoseos$EOS-accumulatedsoseos$SOSPD2)

ggplot(accumulatedsoseos, aes(x = GSL, y = accumulated_tmean, color = lagobserved)) +
  geom_point(size = 4) +  # Increased point size
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # Regression line in black
  scale_color_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0, 
    name = "Lag Observed"
  ) +
  labs(
    title = "Scatter plot of GSL vs accumulated Tmean",
    x = "GSL",
    y = "Accumulated Tmean"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11)
  )
