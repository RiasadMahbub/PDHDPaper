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


