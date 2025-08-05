
#========================================================
#LOAD the LIBRARIES
#========================================================
library("wesanderson")



### plot some DOP and DOH data
## read the csv file
dfgroudtruthdata<- read.csv('C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/CG_RiceDOPDOH_201520162018201920202021.csv')
plot(dfgroudtruthdata)
dfgroudtruthdata
library(ggplot2)

# Load the data
data(diamonds)
dfgroudtruthdata$DOP_doy <-as.numeric(dfgroudtruthdata$DOP_doy)
# Create the histogram
ggplot(dfgroudtruthdata, aes(x=DOP_doy)) +
  geom_histogram()+
  xlab("Day of planting") +
  ylab("Frequency")+
  scale_x_continuous(breaks = seq(0, 170, by = 10)) +
  geom_vline(aes(xintercept = mean(DOP_doy, na.rm = TRUE)),col='red',size=2)+
  annotate("text", x= 20, y = 30, label = paste0("N= ", nrow(dfgroudtruthdata)))+
  theme_classic() +
  theme(text = element_text(size = 12))

# Create the histogram
ggplot(dfgroudtruthdata, aes(x=DOH_doy)) +
  geom_histogram()+
  xlab("Day of harvest")+
  ylab("Frequency")+
  scale_x_continuous(breaks = seq(220, 300, by = 10)) +
  geom_vline(aes(xintercept = mean(DOH_doy)),col='red',size=2)+
  annotate("text", x= 230, y = 15, label = paste0("N= ", nrow(dfgroudtruthdata)))+
  theme_classic() +
  theme(text = element_text(size = 12))

dfgroudtruthdata$LOGS<-as.numeric(dfgroudtruthdata$LOGS)
# Create the histogram
ggplot(dfgroudtruthdata, aes(x=LOGS)) +
  geom_histogram()+
  xlab("Length of growing season (days)")+
  ylab("Frequency")+
  scale_x_continuous(breaks = seq(100, 270, by = 15)) +
  geom_vline(aes(xintercept = mean(LOGS, na.rm = TRUE)),col='red',size=2)+
  annotate("text", x= 220, y = 40, label = paste0("N= ", nrow(dfgroudtruthdata)))+
  theme_classic() +
  theme(text = element_text(size = 12))

#==================================================================================
#==================================================================================
# Generated from 'OrganizeGroundData.R' - Planting day-of-year (PDDOY) distribution

# Get first color from Zissou1 palette
zissou_color <- wes_palette("Zissou1")[1]  # "#3B9AB2"
# Plot
# Create the plot object
p <- ggplot(combined_data, aes(x = PDDOY)) +
  geom_histogram(binwidth = 5, color = "black", fill = zissou_color, alpha = 0.7) +
  labs(
    x = "Day of Year",
    y = "Frequency of Planting Day"
  ) +
  theme_minimal(base_size = 14) +  # Set base text size to 14
  scale_x_continuous(breaks = seq(0, 366, by = 30)) +
  annotate("text", x = 210, y = 75, 
           label = paste0("N = ", nrow(combined_data)), 
           size = 5, color = zissou_color)

# Save the plot
ggsave(
  filename = "planting_date.png",
  plot = p,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  width = 8,
  height = 5,
  dpi = 300
)

#==================================================================================
#==================================================================================
#Script: OrganizingHarvestDate and OrganizeGroundData.R
# Generated from 'OrganizeGroundData.R' - Harvest day-of-year (HDDOY) distribution

# Get 4th color from Zissou1 palette
zissou_color4 <- wes_palette("Zissou1")[4]  # "#F21A00"

# Create the plot object
p_hd <- ggplot(combined_data, aes(x = HDDOY)) +
  geom_histogram(binwidth = 5, color = "black", fill = zissou_color4, alpha = 0.7) +
  labs(
    x = "Day of Year",
    y = "Number of Harvests"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(0, 366, by = 30)) +
  annotate("text", x = 210, y = 75, 
           label = paste0("N = ", sum(!is.na(combined_data$HDDOY))), 
           size = 5, color = zissou_color4)

# Save the plot
ggsave(
  filename = "harvest_date.png",
  plot = p_hd,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  width = 8,
  height = 5,
  dpi = 300
)



# Load required libraries
library(ggplot2)
library(wesanderson)
library(dplyr)
library(ggdist)  # For half-violin; install if missing: install.packages("ggdist")
library(tidyverse)
# Load required libraries
library(ggplot2)
library(wesanderson)
library(dplyr)
library(tidyr)
library(ggdist)
library(ggplot2)
library(wesanderson)
library(dplyr)
library(tidyr)
library(ggdist)

# Prepare long-format data
pd_hd_long <- combined_data %>%
  select(PDDOY, HDDOY) %>%
  pivot_longer(cols = everything(), names_to = "Type", values_to = "DOY") %>%
  dplyr::filter(!is.na(DOY))

# Assign colors from Wes Anderson Zissou1 palette
zissou_colors <- wes_palette("Zissou1")
color_map <- c(PDDOY = zissou_colors[1], HDDOY = zissou_colors[4])

# Compute summary statistics including count (N)
summary_stats <- pd_hd_long %>%
  group_by(Type) %>%
  summarise(
    Mean = mean(DOY, na.rm = TRUE),
    Median = median(DOY, na.rm = TRUE),
    Mode = DOY[which.max(tabulate(match(DOY, DOY)))],
    N = n(),
    .groups = "drop"
  )

# Plot
p <- ggplot(pd_hd_long, aes(x = Type, y = DOY, fill = Type)) +
  ggdist::stat_halfeye(adjust = 0.5, width = 0.6, .width = 0, justification = -0.2, 
                       point_colour = NA, slab_colour = NA) +
  geom_boxplot(width = 0.2, outlier.shape = NA, position = position_nudge(x = 0.15)) +
  scale_fill_manual(values = color_map) +
  theme_minimal(base_size = 12) +
  labs(
    x = NULL,
    y = "Day of Year",
    title = "Distribution of Planting and Harvest Days (DOY)"
  ) +
  geom_text(
    data = summary_stats, aes(
      x = Type, y = 365,
      label = paste0(
        "Mean: ", round(Mean), "\n",
        "Median: ", round(Median), "\n",
        "Mode: ", round(Mode), "\n",
        "N = ", N
      ),
      hjust = ifelse(Type == "HDDOY", 1, 0.5)
    ),
    color = "black",
    size = 4,
    vjust = 1
  ) +
  theme(legend.position = "none")

# Save the plot
ggsave(
  filename = "pd_hd_violin_box.png",
  plot = p,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  width = 7,
  height = 5,
  dpi = 300
)


#---------------------------------------------------------------------------------------
# SAVE THE dtmdldeines_summary_df table PD
#--------------------------------------------------------------------------------------
write.csv(
  dtmdldeines_summary_df,
  "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/DTM_Performance_Summary.csv",
  row.names = FALSE
)

#---------------------------------------------------------------------------------------
# SAVE THE dtmdldeines_summary_df table HD
#--------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------
# SAVE THE Field data of PD and HD
#--------------------------------------------------------------------------------------
write.csv(
  combined_data,
  "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/combined_data.csv",
  row.names = FALSE
)


# ---------------------------
# PLANTING
# ---------------------------
# Define the output directory
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"

# Ensure the directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ---------------------------
# Add model names (from your original code)
# ---------------------------
best_sos_model$Model <- "Threshold VI"
summary_metrics_planting$Model <- "Random Forest"
summary_metrics_df_dl_first$Model <- "Double Logistic"

# ---------------------------
# Helper function to split "43.90 (1.80)" into mean and sd (from your original code)
# ---------------------------
split_mean_sd <- function(x) {
  parts <- sub("\\)", "", unlist(strsplit(x, " \\(")))
  return(as.numeric(parts))
}

# ---------------------------
# Parse Threshold VI (best_sos_model) metrics (from your original code)
# ---------------------------
# Train
mae_train_parts <- split_mean_sd(best_sos_model$MAE_train)
rmse_train_parts <- split_mean_sd(best_sos_model$RMSE_train)
r2_train_parts   <- split_mean_sd(best_sos_model$R2_train)

# Validation
mae_val_parts <- split_mean_sd(best_sos_model$MAE_val)
rmse_val_parts <- split_mean_sd(best_sos_model$RMSE_val)
r2_val_parts   <- split_mean_sd(best_sos_model$R2_val)

# Test (no SD)
mae_test_val <- as.numeric(best_sos_model$MAE_test)
rmse_test_val <- as.numeric(best_sos_model$RMSE_test)
r2_test_val   <- as.numeric(best_sos_model$R2_test)

# ---------------------------
# Create Threshold VI row (from your original code)
# ---------------------------
threshold_vi_metrics <- data.frame(
  Model = "Threshold VI",
  
  Train_MAE_mean = mae_train_parts[1],
  Train_MAE_sd = mae_train_parts[2],
  Val_MAE_mean = mae_val_parts[1],
  Val_MAE_sd = mae_val_parts[2],
  Test_MAE = mae_test_val,
  
  Train_RMSE_mean = rmse_train_parts[1],
  Train_RMSE_sd = rmse_train_parts[2],
  Val_RMSE_mean = rmse_val_parts[1],
  Val_RMSE_sd = rmse_val_parts[2],
  Test_RMSE = rmse_test_val,
  
  Train_R2_mean = r2_train_parts[1],
  Train_R2_sd = r2_train_parts[2],
  Val_R2_mean = r2_val_parts[1],
  Val_R2_sd = r2_val_parts[2],
  Test_R2 = r2_test_val
)

# ---------------------------
# Random Forest row (Test metrics not available in summary) (from your original code)
# ---------------------------
rf_model_metrics <- data.frame(
  Model = "Random Forest",
  
  Train_MAE_mean = summary_metrics_planting$Train_MAE_mean,
  Train_MAE_sd = summary_metrics_planting$Train_MAE_sd,
  Val_MAE_mean = summary_metrics_planting$Val_MAE_mean,
  Val_MAE_sd = summary_metrics_planting$Val_MAE_sd,
  Test_MAE = test_results_planting$MAE,
  
  Train_RMSE_mean = summary_metrics_planting$Train_RMSE_mean,
  Train_RMSE_sd = summary_metrics_planting$Train_RMSE_sd,
  Val_RMSE_mean = summary_metrics_planting$Val_RMSE_mean,
  Val_RMSE_sd = summary_metrics_planting$Val_RMSE_sd,
  Test_RMSE = test_results_planting$RMSE,
  
  Train_R2_mean = summary_metrics_planting$Train_R2_mean,
  Train_R2_sd = summary_metrics_planting$Train_R2_sd,
  Val_R2_mean = summary_metrics_planting$Val_R2_mean,
  Val_R2_sd = summary_metrics_planting$Val_R2_sd,
  Test_R2 = test_results_planting$R2
)

# ---------------------------
# Double Logistic row (Test metrics available in summary) (from your original code)
# ---------------------------
dl_model_metrics <- data.frame(
  Model = "Double Logistic",
  
  Train_MAE_mean = summary_metrics_df_dl_first$Train_MAE_Mean,
  Train_MAE_sd = summary_metrics_df_dl_first$Train_MAE_SD,
  Val_MAE_mean = summary_metrics_df_dl_first$Val_MAE_Mean,
  Val_MAE_sd = summary_metrics_df_dl_first$Val_MAE_SD,
  Test_MAE = summary_metrics_df_dl_first$Test_MAE,
  
  Train_RMSE_mean = summary_metrics_df_dl_first$Train_RMSE_Mean,
  Train_RMSE_sd = summary_metrics_df_dl_first$Train_RMSE_SD,
  Val_RMSE_mean = summary_metrics_df_dl_first$Val_RMSE_Mean,
  Val_RMSE_sd = summary_metrics_df_dl_first$Val_RMSE_SD,
  Test_RMSE = summary_metrics_df_dl_first$Test_RMSE,
  
  Train_R2_mean = summary_metrics_df_dl_first$Train_R2_Mean,
  Train_R2_sd = summary_metrics_df_dl_first$Train_R2_SD,
  Val_R2_mean = summary_metrics_df_dl_first$Val_R2_Mean,
  Val_R2_sd = summary_metrics_df_dl_first$Val_R2_SD,
  Test_R2 = summary_metrics_df_dl_first$Test_R2
)

# ---------------------------
# Combine all models (from your original code)
# ---------------------------
combined_full_metrics <- rbind(threshold_vi_metrics, rf_model_metrics, dl_model_metrics)


# ---------------------------
# Function to format mean and sd for CSV
# Changed from $\\pm$ to +-
# ---------------------------
format_mean_sd <- function(mean_val, sd_val) {
  sprintf("%.2f +- %.2f", mean_val, sd_val)
}

# ---------------------------
# Save the combined_full_metrics data frame to the specified path
# ---------------------------
# Save the original numeric data frame as an .rds file
saveRDS(combined_full_metrics, file.path(output_dir, "combined_full_metrics.rds"))

# Create a version of combined_full_metrics formatted for CSV with mean+-sd
combined_full_metrics_csv_formatted <- data.frame(Model = combined_full_metrics$Model)

combined_full_metrics_csv_formatted$Train_MAE <- format_mean_sd(combined_full_metrics$Train_MAE_mean, combined_full_metrics$Train_MAE_sd)
combined_full_metrics_csv_formatted$Val_MAE <- format_mean_sd(combined_full_metrics$Val_MAE_mean, combined_full_metrics$Val_MAE_sd)
combined_full_metrics_csv_formatted$Test_MAE <- sprintf("%.2f", combined_full_metrics$Test_MAE)

combined_full_metrics_csv_formatted$Train_RMSE <- format_mean_sd(combined_full_metrics$Train_RMSE_mean, combined_full_metrics$Train_RMSE_sd)
combined_full_metrics_csv_formatted$Val_RMSE <- format_mean_sd(combined_full_metrics$Val_RMSE_mean, combined_full_metrics$Val_RMSE_sd)
combined_full_metrics_csv_formatted$Test_RMSE <- sprintf("%.2f", combined_full_metrics$Test_RMSE)

combined_full_metrics_csv_formatted$Train_R2 <- format_mean_sd(combined_full_metrics$Train_R2_mean, combined_full_metrics$Train_R2_sd)
combined_full_metrics_csv_formatted$Val_R2 <- format_mean_sd(combined_full_metrics$Val_R2_mean, combined_full_metrics$Val_R2_sd)
combined_full_metrics_csv_formatted$Test_R2 <- sprintf("%.2f", combined_full_metrics$Test_R2)

# Save the formatted data frame as a CSV file
write.csv(combined_full_metrics_csv_formatted, file.path(output_dir, "combined_full_metrics_formatted.csv"), row.names = FALSE)

message(paste("Original combined_full_metrics data frame saved to:", file.path(output_dir, "combined_full_metrics.rds")))
message(paste("Formatted combined_full_metrics data frame saved as CSV to:", file.path(output_dir, "combined_full_metrics_formatted.csv")))



# ---------------------------
# Harvesting 
# ---------------------------


# Define the output directory
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"

# Ensure the directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ---------------------------
# Helper function to split "mean (sd)" into mean and sd
# ---------------------------
split_mean_sd <- function(x) {
  # Handle cases where input might be NA or not in "mean (sd)" format
  if (is.na(x) || !grepl("\\(", x)) {
    return(c(as.numeric(x), NA)) # Return NA for SD if not in expected format
  }
  parts <- sub("\\)", "", unlist(strsplit(x, " \\(")))
  return(as.numeric(parts))
}

# ---------------------------
# Function to format mean and sd for CSV (using '+-')
# ---------------------------
format_mean_sd_csv <- function(mean_val, sd_val) {
  if (is.na(sd_val)) {
    return(sprintf("%.2f", mean_val)) # If SD is NA, just print mean
  }
  sprintf("%.2f +- %.2f", mean_val, sd_val)
}


# ---------------------------
# DTM Model (eos_summary_table) metrics
# Process each row (each VI) from eos_summary_table
# ---------------------------
dtm_metrics_list <- list()

# Assuming eos_summary_table is already loaded in your R environment
# Example structure for eos_summary_table (replace with your actual data)
# eos_summary_table <- data.frame(
#   VI = c("NDVI", "kNDVI", "EVI", "ATSAVI", "GDVI"),
#   Best_Threshold = c(0.45, 0.55, 0.55, 0.40, 0.15),
#   MAE_train = c("18.33 (0.96)", "12.85 (0.46)", "19.02 (0.72)", "11.52 (0.39)", "13.55 (0.46)"),
#   RMSE_train = c("39.69 (2.80)", "19.39 (1.00)", "30.70 (1.15)", "16.75 (0.55)", "20.88 (0.82)"),
#   R2_train = c("-3.92 (0.68)", "-0.17 (0.13)", "-1.94 (0.25)", "0.13 (0.06)", "-0.36 (0.12)"),
#   MAE_val = c("17.82 (2.87)", "12.69 (1.39)", "18.96 (2.15)", "11.30 (1.15)", "13.27 (1.37)"),
#   RMSE_val = c("37.42 (8.52)", "18.78 (2.97)", "30.47 (3.40)", "16.20 (1.72)", "20.17 (2.44)"),
#   R2_val = c("-3.65 (2.04)", "-0.15 (0.41)", "-1.98 (0.74)", "0.16 (0.19)", "-0.31 (0.38)"),
#   MAE_test = c(18.54, 15.59, 19.92, 9.36, 14.38),
#   RMSE_test = c(42.35, 24.54, 33.00, 13.42, 23.18),
#   R2_test = c(-6.73, -1.59, -3.69, 0.22, -1.32)
# )

for (i in 1:nrow(eos_summary_table)) {
  row_data <- eos_summary_table[i, ]
  
  mae_train_parts <- split_mean_sd(row_data$MAE_train)
  rmse_train_parts <- split_mean_sd(row_data$RMSE_train)
  r2_train_parts   <- split_mean_sd(row_data$R2_train)
  
  mae_val_parts <- split_mean_sd(row_data$MAE_val)
  rmse_val_parts <- split_mean_sd(row_data$RMSE_val)
  r2_val_parts   <- split_mean_sd(row_data$R2_val)
  
  dtm_metrics_list[[i]] <- data.frame(
    Model = paste0("DTM - ", row_data$VI),
    
    Train_MAE_mean = mae_train_parts[1],
    Train_MAE_sd = mae_train_parts[2],
    Val_MAE_mean = mae_val_parts[1],
    Val_MAE_sd = mae_val_parts[2],
    Test_MAE = as.numeric(row_data$MAE_test), # Test has no SD
    
    Train_RMSE_mean = rmse_train_parts[1],
    Train_RMSE_sd = rmse_train_parts[2],
    Val_RMSE_mean = rmse_val_parts[1],
    Val_RMSE_sd = rmse_val_parts[2],
    Test_RMSE = as.numeric(row_data$RMSE_test), # Test has no SD
    
    Train_R2_mean = r2_train_parts[1],
    Train_R2_sd = r2_train_parts[2],
    Val_R2_mean = r2_val_parts[1],
    Val_R2_sd = r2_val_parts[2],
    Test_R2 = as.numeric(row_data$R2_test) # Test has no SD
  )
}
dtm_combined_metrics <- do.call(rbind, dtm_metrics_list)


# ---------------------------
# Random Forest Harvest (summary_metrics_harvest) metrics
# ---------------------------
# Assuming summary_metrics_harvest is already loaded in your R environment
# Example structure for summary_metrics_harvest (replace with your actual data)
# summary_metrics_harvest <- data.frame(
#   Train_R2_mean = 0.9692695, Train_R2_sd = 0.002615117,
#   Train_MAE_mean = 2.351678, Train_MAE_sd = 0.09573647,
#   Train_RMSE_mean = 3.375139, Train_RMSE_sd = 0.1295758,
#   Train_NSE_mean = 0.9636561, Train_NSE_sd = 0.002891547,
#   Train_Bias_mean = 0.04455085, Train_Bias_sd = 0.06501647,
#   Val_R2_mean = 0.7919113, Val_R2_sd = 0.04786901,
#   Val_MAE_mean = 5.824343, Val_MAE_sd = 0.628476,
#   Val_RMSE_mean = 8.20058, Val_RMSE_sd = 1.022704,
#   Val_NSE_mean = 0.7810462, Val_NSE_sd = 0.04977328,
#   Val_Bias_mean = -0.03566397, Val_Bias_sd = 1.050056
# )

rf_harvest_metrics <- data.frame(
  Model = "Random Forest Harvest",
  
  Train_MAE_mean = summary_metrics_harvest$Train_MAE_mean,
  Train_MAE_sd = summary_metrics_harvest$Train_MAE_sd,
  Val_MAE_mean = summary_metrics_harvest$Val_MAE_mean,
  Val_MAE_sd = summary_metrics_harvest$Val_MAE_sd,
  Test_MAE = NA, # No Test MAE provided in summary_metrics_harvest
  
  Train_RMSE_mean = summary_metrics_harvest$Train_RMSE_mean,
  Train_RMSE_sd = summary_metrics_harvest$Train_RMSE_sd,
  Val_RMSE_mean = summary_metrics_harvest$Val_RMSE_mean,
  Val_RMSE_sd = summary_metrics_harvest$Val_RMSE_sd,
  Test_RMSE = NA, # No Test RMSE provided
  
  Train_R2_mean = summary_metrics_harvest$Train_R2_mean,
  Train_R2_sd = summary_metrics_harvest$Train_R2_sd,
  Val_R2_mean = summary_metrics_harvest$Val_R2_mean,
  Val_R2_sd = summary_metrics_harvest$Val_R2_sd,
  Test_R2 = NA # No Test R2 provided
)

# ---------------------------
# Combine all harvest models
# ---------------------------
combined_harvest_full_metrics <- rbind(dtm_combined_metrics, rf_harvest_metrics)

# ---------------------------
# Create a version of combined_harvest_full_metrics formatted for CSV with mean+-sd
# ---------------------------
combined_harvest_csv_formatted <- data.frame(Model = combined_harvest_full_metrics$Model)

combined_harvest_csv_formatted$Train_MAE <- format_mean_sd_csv(combined_harvest_full_metrics$Train_MAE_mean, combined_harvest_full_metrics$Train_MAE_sd)
combined_harvest_csv_formatted$Val_MAE <- format_mean_sd_csv(combined_harvest_full_metrics$Val_MAE_mean, combined_harvest_full_metrics$Val_MAE_sd)
combined_harvest_csv_formatted$Test_MAE <- sprintf("%.2f", combined_harvest_full_metrics$Test_MAE)

combined_harvest_csv_formatted$Train_RMSE <- format_mean_sd_csv(combined_harvest_full_metrics$Train_RMSE_mean, combined_harvest_full_metrics$Train_RMSE_sd)
combined_harvest_csv_formatted$Val_RMSE <- format_mean_sd_csv(combined_harvest_full_metrics$Val_RMSE_mean, combined_harvest_full_metrics$Val_RMSE_sd)
combined_harvest_csv_formatted$Test_RMSE <- sprintf("%.2f", combined_harvest_full_metrics$Test_RMSE)

combined_harvest_csv_formatted$Train_R2 <- format_mean_sd_csv(combined_harvest_full_metrics$Train_R2_mean, combined_harvest_full_metrics$Train_R2_sd)
combined_harvest_csv_formatted$Val_R2 <- format_mean_sd_csv(combined_harvest_full_metrics$Val_R2_mean, combined_harvest_full_metrics$Val_R2_sd)
combined_harvest_csv_formatted$Test_R2 <- sprintf("%.2f", combined_harvest_full_metrics$Test_R2)

# Save the formatted data frame as a CSV file
write.csv(combined_harvest_csv_formatted, file.path(output_dir, "combined_harvest_metrics_formatted.csv"), row.names = FALSE)

message(paste("Original combined_harvest_full_metrics data frame saved to:", file.path(output_dir, "combined_harvest_full_metrics.rds")))
message(paste("Formatted combined_harvest_metrics data frame saved as CSV to:", file.path(output_dir, "combined_harvest_metrics_formatted.csv")))


#------------------------------------------------------------------------
#Number of points from different data sources
#-----------------------------------------------------------------------

summary_counts <- combined_data %>%
  mutate(source_clean = case_when(
    source == "arva_rice_data"           ~ "Arva Intelligence (ARD)",
    source == "DWMRU_rice_data"          ~ "DWMRU",
    source == "matt_morris_data"         ~ "Matt Morris (MMD)",
    source == "ryan_moore_sullivan_data" ~ "Ryan Moore/Sullivan Farms (RMS)",
    source == "seeding_rice_data"        ~ "Scott Matthews/Seeding Data (SCM)",
    source == "unilever_data"            ~ "Unilever (UD)",
    source == "isbellcl"                 ~ "Isbell Farms",
    TRUE                                 ~ source
  )) %>%
  group_by(source_clean) %>%
  summarise(
    PD_count = sum(!is.na(PDDOY)),
    HD_count = sum(!is.na(HDDOY)),
    .groups = "drop"
  ) %>%
  # Add total row
  bind_rows(
    summarise(., source_clean = "Total", PD_count = sum(PD_count), HD_count = sum(HD_count))
  ) %>%
  # Rename columns
  rename(
    Source = source_clean,
    `PD Observation` = PD_count,
    `HD Observation` = HD_count
  )

# View the updated table
print(summary_counts)

# Make sure to use double backslashes or forward slashes in file paths on Windows
write.csv(summary_counts, 
          file = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/PDHDObservation.csv",
          row.names = FALSE)

