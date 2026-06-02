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
df$Field_Year
df$lagtrs

accumulatedsoseos <- inner_join(df, final_results, by = "Field_Year")
if("GSL.x" %in% colnames(accumulatedsoseos) & "GSL.y" %in% colnames(accumulatedsoseos)) {
  accumulatedsoseos$GSL <- ifelse(!is.na(accumulatedsoseos$GSL.x),
                                  accumulatedsoseos$GSL.x,
                                  accumulatedsoseos$GSL.y)
  accumulatedsoseos <- accumulatedsoseos %>%
    select(-GSL.x, -GSL.y)
}

accumulatedsoseos$GSL
accumulatedsoseos$soseosgsl<-(accumulatedsoseos$EOS-accumulatedsoseos$SOSPD2)
accumulatedsoseos$lagtrsmindeines<-(accumulatedsoseos$SD.SD-accumulatedsoseos$Greenup.Greenup)
accumulatedsoseos$PDUD<-(accumulatedsoseos$Greenup.Greenup-accumulatedsoseos$PDDOY)
hist(accumulatedsoseos$PDUD)
# Check duplicate column names
dup_cols <- colnames(accumulatedsoseos)[duplicated(colnames(accumulatedsoseos))]
dup_cols

ggplot(accumulatedsoseos, aes(x = DOY_min_fit, y = PDDOY, color =Greenup.Greenup)) +
  geom_point(size = 4) +  # Increased point size
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # Regression line in black

  labs(
    title = "Scatter plot of GSL vs accumulated Tmean",
    x = "Lag",
    y = "GSL"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11)
  )

ggplot(accumulatedsoseos, aes(x = avgsoilclay, y = GSL )) +
  geom_point(size = 4) +  # Increased point size
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # Regression line in black
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Lag Greenup")+
  labs(
    title = "Scatter plot of GSL vs accumulated Tmean",
    x = "Lag",
    y = "GSL"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11)
  )


df$lagrs
colnames(df)


# Load required libraries
library(dplyr)
library(corrplot)

# -------------------------------
# 1. Prepare feature groups
# -------------------------------
all_cols <- colnames(df)

# Remove response variable 'lagtrs' from predictors
predictors <- setdiff(all_cols, "lagtrs")

# Split into three groups of 23 (adjust last group if needed)
group1 <- predictors[1:23]
group2 <- predictors[24:46]
group3 <- predictors[47:length(predictors)]

# -------------------------------
# 2. Function to compute correlation with lagtrs
# -------------------------------
# -------------------------------
# 2. Function to compute correlation with lagtrs (numeric only)
# -------------------------------
corr_with_lagtrs <- function(df, vars) {
  cor_df <- df %>%
    select(all_of(vars), lagtrs) %>%
    select(where(is.numeric))   # keep only numeric columns
  cor_matrix <- cor(cor_df, use = "complete.obs")
  return(cor_matrix)
}

# Compute correlations for each group
cor1 <- corr_with_lagtrs(df, group1)
cor2 <- corr_with_lagtrs(df, group2)
cor3 <- corr_with_lagtrs(df, group3)

# -------------------------------
# 3. Save correlation plots
# -------------------------------
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/Multicollinearity"

# Plot function
save_corr_plot <- function(cor_matrix, filename, title_text) {
  png(filename, width = 2000, height = 1500, res = 150)
  corrplot(cor_matrix, method = "color", type = "upper",
           tl.cex = 1, tl.col = "black",
           addCoef.col = "black", number.cex = 0.8,
           title = title_text, mar=c(0,0,3,0))
  dev.off()
}

# Save each plot
save_corr_plot(cor1, file.path(output_dir, "Multicollinearity_Group1.png"), "Multicollinearity: Group 1")
save_corr_plot(cor2, file.path(output_dir, "Multicollinearity_Group2.png"), "Multicollinearity: Group 2")
save_corr_plot(cor3, file.path(output_dir, "Multicollinearity_Group3.png"), "Multicollinearity: Group 3")

library(dplyr)
library(car)
library(caret) # for findCorrelation
# Load required libraries
library(dplyr)
library(caret)
library(car)
# -------------------------------
# Load required libraries
# -------------------------------
library(dplyr)
library(caret)
library(car)

# -------------------------------
# Remove specific columns if they exist
# -------------------------------
df_vif <- df %>% 
  select(-any_of(c("PDDOY", "HDDOY", "GSL", "lagder")))
# Columns already removed from df_vif
removed_cols <- c("PDDOY", "HDDOY", "GSL", "lagder")

# Clean each group by removing non-existent columns
group1_clean <- setdiff(group1, removed_cols)
group2_clean <- setdiff(group2, removed_cols)
group3_clean <- setdiff(group3, removed_cols)

# -------------------------------
# Function to prepare numeric dataset for a group
# -------------------------------
# -------------------------------
# 1. Include lagtrs in numeric datasets for VIF/linear model
# -------------------------------
prepare_numeric_for_vif <- function(df, vars, response = "lagtrs") {
  df %>%
    select(any_of(c(vars, response))) %>%   # include response
    select(where(is.numeric)) %>%
    drop_na()
}

# Prepare numeric datasets including lagtrs
df1_num <- prepare_numeric_for_vif(df_vif, group1_clean)
df2_num <- prepare_numeric_for_vif(df_vif, group2_clean)
df3_num <- prepare_numeric_for_vif(df_vif, group3_clean)

# -------------------------------
# Function to remove highly correlated columns
# -------------------------------
prepare_vif <- function(df_num, cor_cutoff = 0.999) {
  # Remove zero-variance columns
  df_num <- df_num[, sapply(df_num, function(x) var(x, na.rm = TRUE) != 0)]
  
  if(ncol(df_num) > 1) {
    cor_matrix <- cor(df_num, use = "complete.obs")
    to_remove <- findCorrelation(cor_matrix, cutoff = cor_cutoff)
    
    # Keep at least one column
    if(length(to_remove) >= ncol(df_num)) to_remove <- to_remove[-1]
    
    if(length(to_remove) > 0) df_num <- df_num[, -to_remove, drop = FALSE]
  }
  return(df_num)
}

# Prepare numeric datasets after correlation filtering
df1_vif <- prepare_vif(df1_num)
df2_vif <- prepare_vif(df2_num)
df3_vif <- prepare_vif(df3_num)

# -------------------------------
# Function to fit linear model safely
# -------------------------------
fit_vif <- function(df_vif, response = "lagtrs") {
  if(ncol(df_vif) < 1) return(NULL)
  lm(as.formula(paste(response, "~ .")), data = df_vif)
}

# Fit linear models
lm1 <- fit_vif(df1_vif)
lm2 <- fit_vif(df2_vif)
lm3 <- fit_vif(df3_vif)

# -------------------------------
# Compute VIFs safely
# -------------------------------
vif_safe <- function(lm_obj) {
  if(is.null(lm_obj)) return(NULL)
  tryCatch(vif(lm_obj), error = function(e) NULL)
}

vif1 <- vif_safe(lm1)
vif2 <- vif_safe(lm2)
vif3 <- vif_safe(lm3)

# -------------------------------
# Check for aliased coefficients in lm3
# -------------------------------
if(!is.null(lm3)) {
  aliased <- alias(lm3)$Complete
  if(any(aliased == 1)) {
    # Remove aliased columns
    aliased_cols <- colnames(aliased)[which(aliased[1,] != 0)]
    df3_num_clean <- df3_num %>% select(-any_of(aliased_cols))
    lm3_clean <- lm(lagtrs ~ ., data = df3_num_clean)
    vif3_final <- round(vif(lm3_clean), 2)
  } else {
    vif3_final <- round(vif3, 2)
  }
} else {
  vif3_final <- NULL
}

# -------------------------------
# Final VIFs for all groups
# -------------------------------
vif1_final <- round(vif1, 2)
vif2_final <- round(vif2, 2)

print("VIF Group 1:"); print(vif1_final)
print("VIF Group 2:"); print(vif2_final)
print("VIF Group 3:"); print(vif3_final)

# -------------------------------
# List VIFs < 5 for each group
# -------------------------------
vif_list_below5 <- list(
  Group1 = vif1_final[vif1_final < 5],
  Group2 = vif2_final[vif2_final < 5],
  Group3 = vif3_final[vif3_final < 5]
)

print("VIFs < 5 for each group:")
vif_list_below5

#----------------------------------------------------------
#SOIL INDICES
#----------------------------------------------------------
library(dplyr)
library(corrplot)

#----------------------------------------------------------
# --- Planting Date (PD) analysis ---
soil_mean_cols <- grep("^mean_", names(df), value = TRUE)
soil_doy_cols  <- grep("^DOY_maxROC_", names(df), value = TRUE)
hddoy_cols <- c("PDDOY", "HDDOY")  # include these too

selected_cols <- c(soil_mean_cols, soil_doy_cols, hddoy_cols)

df_corr <- df %>% select(all_of(selected_cols))

corr_matrix <- cor(df_corr, use = "pairwise.complete.obs")

# Correlations of PDDOY and HDDOY
corr_PDH <- corr_matrix[c("PDDOY", "HDDOY"), ]

features_PDDOY <- names(which(abs(corr_PDH["PDDOY", ]) > 0.55))
features_HDDOY <- names(which(abs(corr_PDH["HDDOY", ]) > 0.47))
features_HDDOY

# PDDOY plot
corr_PDDOY <- corr_matrix[c("PDDOY", features_PDDOY), c("PDDOY", features_PDDOY)]
corrplot(corr_PDDOY, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Correlations of PDDOY with Selected Features (Planting)")
features_PDDOY
# HDDOY plot
corr_HDDOY <- corr_matrix[c("HDDOY", features_HDDOY), c("HDDOY", features_HDDOY)]
corrplot(corr_HDDOY, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Correlations of HDDOY with Selected Features (Planting)")

#----------------------------------------------------------
# --Residuals


library(dplyr)
library(ggplot2)
library(corrplot)

# 1️⃣ Linear regression using features_PDDOY (excluding HDDOY)
predictors <- setdiff(features_PDDOY, "HDDOY")  # remove HDDOY if it was included
formula <- as.formula(paste("PDDOY ~", paste(predictors, collapse = " + ")))

lm_model <- lm(formula, data = df_corr)

# 2️⃣ Calculate residuals
df_complete <- df_corr %>% select(PDDOY, all_of(predictors)) %>% na.omit()
lm_model <- lm(formula, data = df_complete)
df_complete$residuals_PDDOY <- resid(lm_model)


# 3️⃣ Correlation plot of features excluding those used in linear model and response
remaining_features <- setdiff(names(df_corr), c(predictors, "PDDOY", "HDDOY"))

# Use only complete cases for remaining features
df_remaining <- df_corr %>% select(all_of(remaining_features)) %>% na.omit()

# Correlation matrix
corr_matrix_remaining <- cor(df_remaining, use = "pairwise.complete.obs")

# Convert matrix to named vector
cor_with_residuals_vec <- setNames(cor_with_residuals[,1], rownames(cor_with_residuals))

# Remove the residuals column itself
cor_with_residuals_vec <- cor_with_residuals_vec[names(cor_with_residuals_vec) != "residuals_PDDOY"]

# Filter features with |correlation| > 0.3
strong_corr_features <- names(cor_with_residuals_vec[abs(cor_with_residuals_vec) > 0.3])
strong_corr_features

strong_corr_features



#----------------------------------------------
#HDDOY to kNDVImax
#----------------------------------------------
# Calculate raw difference (Positive = Peak before Harvest, Negative = Peak after Harvest)
df_diagnostic <- df %>%
  mutate(
    Peak_to_HD_Raw = HDDOY - DOY_max_obs
  )

# Filter for the "Negative" cases
negative_cases <- df_diagnostic %>%
  filter(Peak_to_HD_Raw < 0) %>%
  select(Field_Year, PDDOY, HDDOY, DOY_max_obs, Peak_to_HD_Raw)

# Print the problematic cases
print(paste("Number of cases where Peak > Harvest:", nrow(negative_cases)))
print(negative_cases)


#-----------------------------------------------
#Cases where UD is lower than PDDOY
#-----------------------------------------------
# ==================================================
# Initializing libraries and sample data
# ==================================================

library(ggplot2)
library(dplyr)

# NOTE:
# Creating mock dataset 'df' and 'vi_list_gt20' for demonstration.
# In your actual workflow, ensure these objects are preloaded.
# ==================================================
# PART 1: Data Summary and Observation Counting
# ==================================================

total_obs <- nrow(df)
valid_hd <- sum(!is.na(df$HDDOY))
missing_hd <- sum(is.na(df$HDDOY))

cat("--- DATASET SUMMARY ---\n")
cat("Total Rows:    ", total_obs, "\n")
cat("Valid HDDOY:   ", valid_hd, "\n")
cat("Missing HDDOY: ", missing_hd, "\n\n")

# ==================================================
# PART 2: Filtering HDMaxdays < 25
# ==================================================

low_hd_indices <- which(df$HDMaxdays < 25)
low_hd_count <- length(low_hd_indices)

low_hd_df <- df[low_hd_indices, c("Field_Year", "HDMaxdays")]

total_percentage <- (low_hd_count / total_obs) * 100
valid_percentage <- (low_hd_count / valid_hd) * 100

cat("--- PERCENTAGE ANALYSIS ---\n")
cat("Cases with HDMaxdays < 25: ", low_hd_count, "\n")
cat("Percentage of Total Data:  ", round(total_percentage, 2), "%\n")
cat("Percentage of Valid Data:  ", round(valid_percentage, 2), "%\n\n")

cat("Sites meeting criteria:\n")
print(head(low_hd_df))

# ==================================================
# PART 3: Vegetation Phenology Visualization
# ==================================================

site_data <- vi_list_gt20[[1]]

site_name <- if ("Field_Year" %in% names(site_data)) {
  site_data$Field_Year[1]
} else {
  "Unknown Site"
}

ndvi_plot <- ggplot(site_data, aes(x = DOY, y = kNDVI)) +
  
  geom_point(color = "#2E7D32", size = 2, alpha = 0.6) +
  
  geom_line(color = "#2E7D32", linewidth = 0.5, alpha = 0.3) +
  
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    color = "#1B5E20",
    fill = "#C8E6C9",
    linewidth = 1.2,
    se = TRUE
  ) +
  
  labs(
    title = paste("Vegetation Phenology:", site_name),
    subtitle = "Time-series of Day of Year (DOY) vs. Kernel NDVI",
    x = "Day of Year (DOY)",
    y = "kNDVI",
    caption = paste("Analyzed on:", Sys.Date())
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1B5E20"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray90", fill = NA)
  )

# Render plot
print(ndvi_plot)


#--------------------------------------------------
#Cases where HD is less than 25
#---------------------------------------------------
# Find Field_Year values where HDMaxdays < 25
# STREAMING_CHUNK: Filtering the dataframe for HDMaxdays < 25...
# Use which() to find indices while ignoring NAs
# 1. Total observations in the dataset
total_count <- nrow(df)

# 2. Number of cases where HDMaxdays is less than 25
# We use na.rm = TRUE to ensure NAs don't break the sum
low_hd_count <- sum(df$HDMaxdays < 25, na.rm = TRUE)

# 3. Calculate total percentage (out of all rows in the dataframe)
total_percentage <- (low_hd_count / total_count) * 100

# 4. Calculate valid percentage (out of rows that actually have data)
non_na_count <- sum(!is.na(df$HDMaxdays))
valid_percentage <- (low_hd_count / non_na_count) * 100


cat("--- Distribution Analysis ---\n")
cat("Total Rows in Dataset:   ", total_count, "\n")
cat("Rows with Data (Non-NA): ", non_na_count, "\n")
cat("Rows with HDMaxdays < 25:", low_hd_count, "\n")
cat("-----------------------------\n")
cat("Total Percentage:        ", round(total_percentage, 2), "%\n")
cat("Valid Data Percentage:   ", round(valid_percentage, 2), "%\n")

summary_table <- data.frame(
  Metric = c("Total Rows", "Valid Rows", "Target Cases", "Percentage of Total", "Percentage of Valid"),
  Value = c(total_count, non_na_count, low_hd_count, 
            paste0(round(total_percentage, 2), "%"), 
            paste0(round(valid_percentage, 2), "%"))
)


library(ggplot2)
library(ggplot2)

# --- Setup Configuration ---
save_path <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/HDDOY25less/"

# Create the directory if it doesn't exist to prevent errors
if (!dir.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

# 1. Identify the Field_Year values where HDMaxdays < 25
target_sites <- df$Field_Year[which(df$HDMaxdays < 25)]

# 2. Iterate through the list of dataframes
lapply(vi_list_gt20, function(site_data) {
  
  # Identify the site name
  current_site <- if ("Field_Year" %in% names(site_data)) {
    as.character(site_data$Field_Year[1])
  } else {
    "Unknown_Site"
  }
  
  # ONLY proceed if this site name is in our target list
  if (current_site %in% target_sites) {
    
    # Extract vertical line coordinates (taking the first value available)
    pd_val <- site_data$PDDOY[1]
    hd_val <- site_data$HDDOY[1]
    
    ndvi_plot <- ggplot(site_data, aes(x = DOY, y = kNDVI)) +
      # Vertical lines for PD and HD
      geom_vline(aes(xintercept = pd_val), color = "blue", linetype = "dashed", linewidth = 0.8) +
      geom_vline(aes(xintercept = hd_val), color = "red", linetype = "dashed", linewidth = 0.8) +
      
      geom_point(color = "#2E7D32", size = 2, alpha = 0.6) +
      geom_line(color = "#2E7D32", linewidth = 0.5, alpha = 0.3) +
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        color = "#1B5E20",
        fill = "#C8E6C9",
        linewidth = 1.2,
        se = TRUE
      ) +
      # Annotate the lines so you know which is which
      annotate("text", x = pd_val, y = Inf, label = "PD", color = "blue", vjust = 1.5, angle = 90, size = 3) +
      annotate("text", x = hd_val, y = Inf, label = "HD", color = "red", vjust = 1.5, angle = 90, size = 3) +
      
      labs(
        title = paste("Vegetation Phenology:", current_site),
        subtitle = "Blue Line: PD | Red Line: HD",
        x = "Day of Year (DOY)",
        y = "kNDVI",
        caption = paste("Analyzed on:", Sys.Date())
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#1B5E20"),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "gray90", fill = NA)
      )
    
    # 3. Save the Plot
    # Replacing spaces/special characters in filename just in case
    file_name <- paste0(gsub("[^A-Za-z0-9]", "_", current_site), ".png")
    
    ggsave(
      filename = file_name,
      plot = ndvi_plot,
      path = save_path,
      width = 8,
      height = 5,
      units = "in",
      dpi = 300
    )
    
    # Optional: Print to console so you can see progress
    print(paste("Saved plot for:", current_site))
  }
})




# =========================================================
# PART 10: METEOROLOGICAL DATA QC + CONSISTENCY CHECKS
# =========================================================
# ==============================================
# METEOROLOGICAL QC AND ANOMALY ANALYSIS
# ==============================================
library(tidyverse)
library(patchwork)
library(GGally)
library(lubridate)

# ==============================================
# STEP 1: BUILD met_daily FROM meteo_list
# ==============================================
# Combine all meteo_list entries into one flat dataframe
colnames(meteo_list[[1]])
# ==============================================
# STEP 1: BUILD met_daily FROM meteo_list
# ==============================================

met_daily <- bind_rows(lapply(names(meteo_list), function(nm) {
  
  df <- meteo_list[[nm]]
  
  # Standardize problematic columns
  if ("Variety" %in% colnames(df)) {
    df$Variety <- as.character(df$Variety)
  }
  
  df$field_key <- nm
  df
  
})) %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    DOY  = yday(Date)
  ) %>%
  rename_with(~ case_when(
    . == "VPD"                  ~ "VPD",
    . == "tmin"                 ~ "Tmin",
    . == "RH"                   ~ "RH",
    . == "Srad"                 ~ "Rad",
    . == "gdd"                  ~ "GDD",
    . == "tmean"                ~ "Tmean",
    . == "SoilTMP0_10cm_inst"   ~ "SoilT",
    TRUE ~ .
  ))

# Quick check
cat("Rows in met_daily:", nrow(met_daily), "\n")
cat("Years:", paste(sort(unique(met_daily$Year)), collapse=", "), "\n")
cat("Columns:", paste(names(met_daily), collapse=", "), "\n")


# ==============================================
# STEP 2: DATA COMPLETENESS PER YEAR
# ==============================================
met_counts <- met_daily %>%
  group_by(Year) %>%
  summarise(
    n_field_days  = n(),
    n_fields      = n_distinct(field_key),
    
    missing_VPD   = round(mean(is.na(vpd))   * 100, 1),
    missing_Tmin  = round(mean(is.na(Tmin))  * 100, 1),
    missing_RH    = round(mean(is.na(avgRH)) * 100, 1),
    missing_Rad   = round(mean(is.na(srad))  * 100, 1),
    missing_GDD   = round(mean(is.na(GDD))   * 100, 1),
    
    .groups = "drop"
  )

print(met_counts)

# Prepare plotting dataframe first
plot_df <- met_counts %>%
  pivot_longer(
    cols = starts_with("missing_"),
    names_to = "Variable",
    values_to = "pct_missing"
  ) %>%
  mutate(
    Variable = str_remove(Variable, "missing_")
  )

# Now build ggplot separately
p_completeness <- ggplot(
  plot_df,
  aes(
    x = factor(Year),
    y = pct_missing,
    fill = Variable
  )
) +
  geom_col(position = "dodge") +
  theme_classic(base_size = 12) +
  labs(
    title = "% Missing Values per Meteorological Variable by Year",
    x = "Year",
    y = "% Missing",
    fill = "Variable"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_completeness

# ==============================================
# STEP 3: YEARLY DISTRIBUTIONS (BOXPLOTS)
# Checks whether anomalous years have plausible ranges
# ==============================================
vars_to_check <- c("vpd", "Tmin", "avgRH", "srad", "GDD", "Tmean")
vars_present  <- intersect(vars_to_check, names(met_daily))

met_long <- met_daily %>%
  select(Year, all_of(vars_present)) %>%
  pivot_longer(cols = -Year,
               names_to = "Variable",
               values_to = "Value") %>%
  dplyr::filter(!is.na(Value))

p_boxplots <- ggplot(met_long,
                     aes(x = factor(Year), y = Value)) +
  geom_boxplot(outlier.alpha = 0.2, fill = "steelblue", alpha = 0.6) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  theme_classic(base_size = 11) +
  labs(title = "Yearly Meteorological Variable Distributions",
       subtitle = "Check for physically unreasonable ranges or outlier years",
       x = "Year", y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("meteo_boxplots.png", p_boxplots,
       width = 10, height = 12, dpi = 300)


# ==============================================
# STEP 4: ANNUAL SUMMARY + Z-SCORES
# Core of what Ben is asking — are anomalous years real?
# ==============================================
# ==============================================
# YEARLY METEOROLOGICAL SUMMARY
# ==============================================

year_summary <- met_daily %>%
  group_by(Year) %>%
  summarise(
    mean_VPD   = mean(vpd,    na.rm = TRUE),
    mean_Tmin  = mean(Tmin,   na.rm = TRUE),
    mean_RH    = mean(avgRH,  na.rm = TRUE),
    mean_Rad   = mean(srad,   na.rm = TRUE),
    total_GDD  = sum(GDD,     na.rm = TRUE),
    mean_Tmean = mean(Tmean,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    z_VPD   = scale(mean_VPD)[,1],
    z_Tmin  = scale(mean_Tmin)[,1],
    z_RH    = scale(mean_RH)[,1],
    z_Rad   = scale(mean_Rad)[,1],
    z_GDD   = scale(total_GDD)[,1],
    z_Tmean = scale(mean_Tmean)[,1]
  )

print(as.data.frame(year_summary))


# ==============================================
# STEP 5: ANOMALY HEATMAP
# Red = warmer/drier than average, Blue = cooler/wetter
# ==============================================
year_long <- year_summary %>%
  select(Year, starts_with("z_")) %>%
  pivot_longer(cols = -Year,
               names_to = "Variable",
               values_to = "Z") %>%
  mutate(Variable = str_remove(Variable, "z_"))

p_heatmap <- ggplot(year_long,
                    aes(x = factor(Year), y = Variable, fill = Z)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = round(Z, 2)),
            color = "white", size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "steelblue", mid = "grey30",
                       high = "firebrick", midpoint = 0) +
  theme_classic(base_size = 12) +
  labs(title = "Meteorological Anomaly Heatmap (Rice Season: Apr–Oct)",
       subtitle = "Red = above average | Blue = below average | Values = z-scores",
       x = "Year", y = NULL, fill = "Z-score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("meteo_anomaly_heatmap.png", p_heatmap,
       width = 10, height = 5, dpi = 300)


# ==============================================
# STEP 6: PHYSICAL COHERENCE CHECK
# VPD vs Tmin, RH vs VPD — if anomalies are real,
# these should be internally consistent
# ==============================================
p_coherence <- ggplot(year_summary,
                      aes(x = mean_Tmin, y = mean_VPD, label = Year)) +
  geom_point(size = 3, color = "firebrick") +
  geom_text(vjust = -0.8, size = 3.5) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue", linetype = "dashed") +
  theme_classic(base_size = 12) +
  labs(title = "Physical Coherence: Mean Tmin vs Mean VPD (Apr–Oct)",
       subtitle = "Warmer years should have higher VPD — if not, data may be suspect",
       x = "Mean Daily Tmin (°C)", y = "Mean Daily VPD (kPa)")

p_rh_vpd <- ggplot(year_summary,
                   aes(x = mean_RH, y = mean_VPD, label = Year)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_text(vjust = -0.8, size = 3.5) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue", linetype = "dashed") +
  theme_classic(base_size = 12) +
  labs(title = "Physical Coherence: Mean RH vs Mean VPD (Apr–Oct)",
       subtitle = "Higher RH should correspond to lower VPD",
       x = "Mean RH (%)", y = "Mean Daily VPD (kPa)")

p_combined_coherence <- p_coherence / p_rh_vpd
ggsave("meteo_physical_coherence.png", p_combined_coherence,
       width = 8, height = 10, dpi = 300)


# ==============================================
# STEP 7: STRESS INDEX — which years are most anomalous?
# This directly addresses Ben's question
# ==============================================
extreme_years <- year_summary %>%
  mutate(
    stress_index = z_VPD - z_RH + z_GDD   # hot + dry + high GDD = stressed
  ) %>%
  arrange(desc(stress_index)) %>%
  select(Year, mean_VPD, mean_Tmin, mean_RH, total_GDD,
         z_VPD, z_Tmin, z_RH, z_GDD, stress_index)

cat("\n--- Years ranked by thermal/moisture stress index ---\n")
print(as.data.frame(extreme_years))

# Flag physically suspect years:
# If VPD is high but RH is also high and Tmin is low → possible data artifact
coherence_check <- year_summary %>%
  mutate(
    vpd_rh_conflict = (z_VPD > 1 & z_RH > 1),   # both high = physically odd
    vpd_t_conflict  = (z_VPD > 1 & z_Tmin < -1), # hot-dry but cold min temp = odd
    flag = case_when(
      vpd_rh_conflict ~ "⚠️ VPD high but RH also high — check data",
      vpd_t_conflict  ~ "⚠️ VPD high but Tmin low — check data",
      TRUE            ~ "✅ Physically coherent"
    )
  ) %>%
  select(Year, z_VPD, z_RH, z_Tmin, z_GDD, flag)

cat("\n--- Physical coherence flags ---\n")
print(as.data.frame(coherence_check))


# ==============================================
# STEP 8: TIME SERIES OF ANNUAL MEANS
# Visual trend check — does the data look reasonable over 2015–2024?
# ==============================================
p_trends <- year_summary %>%
  select(Year, mean_VPD, mean_Tmin, mean_RH, total_GDD) %>%
  pivot_longer(cols = -Year, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Year, y = Value)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 2.5, color = "firebrick") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  theme_classic(base_size = 11) +
  labs(title = "Annual Meteorological Trends (Rice Season: Apr–Oct)",
       subtitle = "Look for sudden jumps that might indicate data artifacts",
       x = "Year", y = NULL)

ggsave("meteo_annual_trends.png", p_trends,
       width = 10, height = 8, dpi = 300)

cat("\n✅ All plots saved. Review:\n")
cat("  - meteo_completeness.png\n")
cat("  - meteo_boxplots.png\n")
cat("  - meteo_anomaly_heatmap.png\n")
cat("  - meteo_physical_coherence.png\n")
cat("  - meteo_annual_trends.png\n")
cat("\nShare coherence_check table with Ben to answer his question.\n")
