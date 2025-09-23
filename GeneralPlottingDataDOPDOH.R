#========================================================
# LOAD the LIBRARIES
#========================================================
library(wesanderson)
library(dplyr)
library(ggdist)        # For half-violin; install if missing: install.packages("ggdist")
library(tidyverse)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(Metrics)       # For rmse() and mae()
library(ggcorrplot)
library(ggpubr)
library(grid)          # For textGrob and gpar
library(cowplot)
library(ggpubfigs)
library(corrplot)
library(RColorBrewer)
library(patchwork)

#==================================================================================
#==================================================================================
# Generated from 'OrganizeGroundData.R' - Planting day-of-year (PDDOY) distribution
# Get first color from Zissou1 palette
zissou_color <- wes_palette("Zissou1")[1]  # "#3B9AB2"

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

# Prepare long-format data
pd_hd_long <- combined_data %>%
  select(PDDOY, HDDOY) %>%
  pivot_longer(cols = everything(), names_to = "Type", values_to = "DOY") %>%
  dplyr::filter(!is.na(DOY))

pd_hd_long$Type <- factor(pd_hd_long$Type,
                          levels = c("PDDOY", "HDDOY"),
                          labels = c("PD", "HD"))
# Ensure correct order for x-axis
pd_hd_long <- pd_hd_long %>%
  mutate(Type = factor(Type, levels = c("PD", "HD")))
# Assign colors from Wes Anderson Zissou1 palette
zissou_colors <- wes_palette("Zissou1")
color_map <- c(PD = zissou_colors[1], HD = zissou_colors[4])

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
pdhdviolin <- ggplot(pd_hd_long, aes(x = Type, y = DOY, fill = Type)) +
  ggdist::stat_halfeye(adjust = 0.5, width = 0.6, .width = 0, justification = -0.2, 
                       point_colour = NA, slab_colour = NA) +
  geom_boxplot(width = 0.2, outlier.shape = NA, position = position_nudge(x = 0.15)) +
  scale_fill_manual(values = color_map) +
  theme_classic(base_size = 12) +
  labs(
    x = NULL,
    y = "Day of Year (day)"
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
  theme(
    axis.title = element_text(color = "black"), 
    axis.text  = element_text(color = "black", size = 14),
    # Add major and minor grid lines
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey95"),
    legend.position = c(0.85, 0.25),   # move legend inside
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA), # semi-transparent box
    legend.key.size = unit(0.6, "cm")
  )



# Save the plot
ggsave(
  filename = "pd_hd_violin_box.png",
  plot = pdhdviolin,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  width = 7,
  height = 5,
  dpi = 300
)

##-----------------------------------------------------------------
# Histogram of pd-doymax doymax-hd 
##-----------------------------------------------------------------

# Assign colors from Wes Anderson Zissou1 palette
zissou_colors <- wes_palette("Zissou1")
color_map <- c(PDMaxdays = zissou_colors[1], HDMaxdays = zissou_colors[4])

# Add PDMaxdays and HDMaxdays to df
df <- df %>%
  mutate(
    PDMaxdays = abs(DOY_max_obs - PDDOY),
    HDMaxdays = abs(HDDOY - DOY_max_obs)
  )

# Calculate mean and SD
stats <- df %>%
  summarise(
    mean_PD = mean(PDMaxdays, na.rm = TRUE),
    sd_PD   = sd(PDMaxdays, na.rm = TRUE),
    mean_HD = mean(HDMaxdays, na.rm = TRUE),
    sd_HD   = sd(HDMaxdays, na.rm = TRUE)
  )

# Plot PDMaxdays
p1 <- ggplot(df, aes(x = PDMaxdays)) +
  geom_histogram(fill = color_map["PDMaxdays"], color = "white", bins = 30) +
  labs( x = "Time to peak (kNDVImax) to PD", y = "Count") +
  theme_classic(base_size = 12) +
  annotate(
    "text",
    x = 120,       # explicit x-coordinate
    y = 60,        # explicit y-coordinate
    label = paste0("Mean = ", round(stats$mean_PD,1), "\nSD = ", round(stats$sd_PD,1)),
    hjust = 0,
    vjust = 0,
    size = 4
  ) +
  theme(
    axis.title = element_text(color = "black"), 
    axis.text  = element_text(color = "black", size = 14),
    # Add major and minor grid lines
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey95")
  )

# Plot HDMaxdays
p2 <- ggplot(df, aes(x = HDMaxdays)) +
  geom_histogram(fill = color_map["HDMaxdays"], color = "white", bins = 30) +
  labs( x = "Time to peak (kNDVImax) to HD", y = "Count") +
  theme_classic(base_size = 12) +
  annotate(
    "text",
    x = 60,        # explicit x-coordinate
    y = 60,        # explicit y-coordinate
    label = paste0("Mean = ", round(stats$mean_HD,1), "\nSD = ", round(stats$sd_HD,1)),
    hjust = 0,
    vjust = 0,
    size = 4
  ) +
  theme(
    axis.title = element_text(color = "black"), 
    axis.text  = element_text(color = "black", size = 14),
    # Add major and minor grid lines
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey95")
  )

# Combine with A, B notation
pdhddoymax <- p1 + p2 + plot_annotation(tag_levels = "A")

# Save the figure
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/PD_HD_MaxDays_Hist.png",
  plot = pdhddoymax,
  width = 10,
  height = 6,
  dpi = 300
)

# ---------------------------
# Growing Season length and Lag
# ---------------------------
# reshape into long format 
df_long <- df %>% 
  select(gsl, lagsosderpddoy, lagsostrspddoy, lagudpddoy) %>% 
  pivot_longer( 
    cols = c(lagsosderpddoy, lagsostrspddoy, lagudpddoy), 
    names_to = "LagType", 
    values_to = "LagValue" 
  ) 

# clean legend labels 
df_long$LagType <- factor(df_long$LagType, 
                          levels = c("lagsosderpddoy", "lagsostrspddoy", "lagudpddoy"), 
                          labels = c("Planting_SOSDeriv_Period", "Planting_SOSTRS_Period", 
                                     "Planting_UD_Period")) 


# use first 3 colors from GrandBudapest1 
pal <- wes_palette("GrandBudapest1")[2:4] 

# plot 
lagall <- ggplot(df_long, aes(x = LagValue, y = gsl, color = LagType)) + 
  geom_point(alpha = 0.7, size = 3) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) + 
  stat_cor(aes(label = ..r.label..), method = "pearson", 
           label.x.npc = "left", label.y.npc = "top", size = 5, show.legend = FALSE) + 
  theme_classic(base_size = 12) + 
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = seq(-40, max(df_long$LagValue, na.rm = TRUE), by = 20)) +
  scale_y_continuous(breaks = seq(0, max(df_long$gsl, na.rm = TRUE), by = 25)) +
  labs(
    x = "Lag (days)", 
    y = "Growing season length (HD–PD)", 
    color = NULL
  ) +
  theme(
    axis.title = element_text(color = "black"), 
    axis.text  = element_text(color = "black", size = 14),
    legend.position = c(0.45, 0.87),  # x = 80% from left, y = 85% from bottom
    legend.direction = "vertical",
    legend.background = element_rect(fill = alpha('white', 0.5)), # semi-transparent bg
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 12) 
  )


lagall

# Save the plot to the specified file path
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/laggsl.jpeg",
  plot = lagall,
  device = "jpeg", # Specifies the output format as JPEG
  width = 8,       # Set the width in inches
  height = 6,      # Set the height in inches
  dpi = 300        # Set the resolution for high-quality output
)


#--------------------------------------------------------------------
#COMBINE VIOLING DOYMAX AND LAG GRAPH INTO ONE
#--------------------------------------------------------------------


# Combine plots in 2 rows x 2 columns
combined_all <- (
  (pdhdviolin | lagall) /        # first row: two plots side by side
    (p1 + p2)                      # second row: two plots combined
) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16, face = "bold"))

# Save the combined figure
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/combined_violin_lag_hist.png",
  plot = combined_all,
  width = 12,
  height = 10,
  dpi = 300
)


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


#-----------------------------------------------------------------------
# Plotting four methods with panel labels A, B, C, D
#-----------------------------------------------------------------------

# Output file
outfile <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/PhenologyPanel.jpg"

jpeg(filename = outfile, width = 15, height = 8, units = "in", res = 300)

# Extract data
dates <- as.Date(vi_list_gt20$Way_03_2022$Date)
ndvi  <- vi_list_gt20$Way_03_2022$kNDVI
t <- as.numeric(format(dates, "%j"))
tout <- 1:365

# Fit
methods <- c("AG", "Beck", "Elmore", "Gu", "Zhang")
fit <- curvefit(ndvi, t, tout, methods)
x <- fit$model$AG

# Extract metrics
trs <- PhenoTrs(x)
der <- PhenoDeriv(x)
gu  <- PhenoGu(x)
kl  <- PhenoKl(x)

op <- par(mfrow = c(2, 2), mar = c(3,3,2,1), oma = c(0,0,2,0))
on.exit(par(op), add = TRUE)

# Panel 1 (A) - keep Y axis only
par(yaxt="s", xaxt="n")
PhenoTrs(x)
title(main = "    
      
      ")   # 👈 multiple spaces
box()
mtext("kNDVI (unitless)", side = 2, line = 2)
mtext("A", side = 3, adj = 0, line = 0.5, font = 2, cex = 1.5)

# Panel 2 (B) - hide both axes
par(yaxt="n", xaxt="n")
PhenoDeriv(x)
box()
mtext("B", side = 3, adj = 0, line = 0.5, font = 2, cex = 1.5)

# Panel 3 (C) - show both axes
par(yaxt="s", xaxt="s")
PhenoGu(x)
box()
mtext("kNDVI (unitless)", side = 2, line = 2)
mtext("Day of the year (Day)", side = 1, line = 2)
mtext("C", side = 3, adj = 0, line = 0.5, font = 2, cex = 1.5)

# Panel 4 (D) - keep X axis only
par(yaxt="n", xaxt="s")
PhenoKl(x)
box()
mtext("Day of the year (Day)", side = 1, line = 2)
mtext("D", side = 3, adj = 0, line = 0.5, font = 2, cex = 1.5)

dev.off()
#==========================================================================
# Planting Phenological Methods
#==========================================================================
remove_list <- c(
  "F_20581_68_MF_2015",
  "Judys_2022",
  "Seed_Rice_2022",
  "Walls_06_07_2022",
  "Walls_09_2021",
  "Cattlet_02_2020",
  "East_Joe_T_2022",
  "F_8252_7_HF_2015",
  "F_8319_65_5_2017",
  "F_8320_66_6_2017",
  "F_8320_66_6_2020"
)
phenology_df <- phenology_df[!phenology_df$Field_Year %in% remove_list, ]


# Plot A
metrics_label1 <- get_metrics_label(phenology_df$PDDOY, phenology_df$SOS_trs.sos)
n1 <- sum(!is.na(phenology_df$PDDOY) & !is.na(phenology_df$SOS_trs.sos))
label1 <- paste0(metrics_label1, "\nn = ", n1)
p1 <- ggplot(phenology_df, aes(x = PDDOY, y = SOS_trs.sos)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs( x = NULL, y = "Start of Season (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 20)) +
  scale_y_continuous(breaks = seq(0, 400, 20)) +
  annotate("text", x = 82, y = max(phenology_df$SOS_trs.sos, na.rm = TRUE),
           label = "A", hjust = 0, vjust = 1.2, size = 5, fontface = "bold") +
  annotate("text", x = 140, y = 60,   # bottom right placement
           label = label1, hjust = 0, vjust = 0) +
  custom_theme

# Plot B
metrics_label2 <- get_metrics_label(phenology_df$PDDOY, phenology_df$SOS_deriv.sos)
n2 <- sum(!is.na(phenology_df$PDDOY) & !is.na(phenology_df$SOS_deriv.sos))
label2 <- paste0(metrics_label2, "\nn = ", n2)
p2 <- ggplot(phenology_df, aes(x = PDDOY, y = SOS_deriv.sos)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs( x = NULL, y = "Start of Season (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 20)) +
  scale_y_continuous(breaks = seq(0, 400, 20)) +
  annotate("text", x = 82, y = max(phenology_df$SOS_deriv.sos, na.rm = TRUE),
           label = "B", hjust = 0, vjust = 1.2, size = 5, fontface = "bold") +
  annotate("text", x = 140, y = 60,   # bottom right placement
           label = label2, hjust = 0, vjust = 0) +
  custom_theme

# Plot C
metrics_label3 <- get_metrics_label(phenology_df$PDDOY, phenology_df$UD.UD)
n3 <- sum(!is.na(phenology_df$PDDOY) & !is.na(phenology_df$UD.UD))
label3 <- paste0(metrics_label3, "\nn = ", n3)
p3 <- ggplot(phenology_df, aes(x = PDDOY, y = UD.UD)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs( x = "Planting Date (DOY)", y = "Update Date (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 20)) +
  scale_y_continuous(breaks = seq(0, 400, 20)) +
  annotate("text", x = 82, y = max(phenology_df$UD.UD, na.rm = TRUE),
           label = "C", hjust = 0, vjust = 1.2, size = 5, fontface = "bold") +
  annotate("text", x = 140, y = 60,   # moved a bit lower than before
           label = label3, hjust = 0, vjust = 0) +
  custom_theme

# Plot D (unchanged, already bottom right style)
metrics_label4 <- get_metrics_label(phenology_df$PDDOY, phenology_df$Greenup.Greenup)
n4 <- sum(!is.na(phenology_df$PDDOY) & !is.na(phenology_df$Greenup.Greenup))
label4 <- paste0(metrics_label4, "\nn = ", n4)
p4 <- ggplot(phenology_df, aes(x = PDDOY, y = Greenup.Greenup)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs( x = "Planting Date (DOY)", y = "Greenup (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 20)) +
  scale_y_continuous(breaks = seq(0, 400, 20)) +
  annotate("text", x = 82, y = max(phenology_df$Greenup.Greenup, na.rm = TRUE),
           label = "D", hjust = 0, vjust = 1.2, size = 5, fontface = "bold") +
  annotate("text", x = 140, y = 60,
           label = label4, hjust = 0, vjust = 0) +
  custom_theme

# Arrange all plots
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2)

# Save to file
output_path <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/PlantDayPhenoMethod.png"
png(filename = output_path, width = 14, height = 8, units = "in", res = 300)
grid.arrange(p1, p2, p3, p4, ncol = 2)
dev.off()
PDmetrics_label1_clean <- gsub("\n", ", ", metrics_label1)
PDmetrics_label2_clean <- gsub("\n", ", ", metrics_label2)
PDmetrics_label3_clean <- gsub("\n", ", ", metrics_label3)
PDmetrics_label4_clean <- gsub("\n", ", ", metrics_label4)


#==========================================================================
#Harvest Phenological Methods
#==========================================================================
# Define a common theme
custom_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Helper function to calculate metrics and return a label
# Note: Assuming the rmse and mae functions are defined elsewhere in your script.
get_metrics_label <- function(y, y_hat) {
  r2 <- round(cor(y, y_hat, use = "complete.obs")^2, 2)
  rmse_val <- round(rmse(y, y_hat), 2)
  mae_val <- round(mae(y, y_hat), 2)
  bias_val <- round(mean(y_hat - y, na.rm = TRUE), 2)
  paste0("R² = ", r2, 
         "\nRMSE = ", rmse_val, " days",
         "\nMAE = ", mae_val, " days",
         "\nBias = ", bias_val, " days")
}

# Plot 1 - A
metrics_label1 <- get_metrics_label(phenology_df$HDDOY, phenology_df$EOS_trs.eos)
n1 <- sum(!is.na(phenology_df$HDDOY) & !is.na(phenology_df$EOS_trs.eos))
label1 <- paste0(metrics_label1, "\nn = ", n1)
p1 <- ggplot(phenology_df, aes(x = HDDOY, y = EOS_trs.eos)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs( x = NULL, y = "End of Season (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 10)) +
  scale_y_continuous(limits = c(200, NA), breaks = seq(200, 300, 10)) + # Y-axis now starts at 200
  annotate("text", x = 235, y = Inf, # Use y=Inf to pin the label to the top
           label = "A", hjust = 0, vjust = 1.2, size = 5, fontface = "bold") +
  annotate("text", x = 300, y = -Inf, # Moved metrics to bottom right
           label = label1, hjust = 0, vjust = -0.5) +
  custom_theme

# Plot 2 - B
metrics_label2 <- get_metrics_label(phenology_df$HDDOY, phenology_df$EOS_deriv.eos)
n2 <- sum(!is.na(phenology_df$HDDOY) & !is.na(phenology_df$EOS_deriv.eos))
label2 <- paste0(metrics_label2, "\nn = ", n2)
p2 <- ggplot(phenology_df, aes(x = HDDOY, y = EOS_deriv.eos)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs( x = NULL, y = "End of Season (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 10)) +
  scale_y_continuous(limits = c(200, NA), breaks = seq(200, 300, 10)) + # Y-axis now starts at 200
  annotate("text", x = 235, y = Inf, # Use y=Inf to pin the label to the top
           label = "B", hjust = 0, vjust = 1.2, size = 5, fontface = "bold") +
  annotate("text", x = 300, y = -Inf, # Moved metrics to bottom right
           label = label2, hjust = 0, vjust = -0.5) +
  custom_theme

# Plot 3 - C
metrics_label3 <- get_metrics_label(phenology_df$HDDOY, phenology_df$DD.DD)
n3 <- sum(!is.na(phenology_df$HDDOY) & !is.na(phenology_df$DD.DD))
label3 <- paste0(metrics_label3, "\nn = ", n3)
p3 <- ggplot(phenology_df, aes(x = HDDOY, y = DD.DD)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs( x = "Harvest Date (DOY)", y = "Final Adjusted Downturn Date (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 10)) +
  scale_y_continuous(limits = c(180, 300), breaks = seq(180, 300, 10)) + # Y-axis now starts at 180
  annotate("text", x = 300, y = 205, # Moved metrics down to y=205
           label = label3, hjust = 0, vjust = 0.5) +
  annotate("text", x = 235, y = Inf, # Use y=Inf to pin the label to the top
           label = "C", hjust = 0, vjust = 1.2, size = 5, fontface = "bold") +
  custom_theme

# Plot 4 - D (metrics at bottom right)
metrics_label4 <- get_metrics_label(phenology_df$HDDOY, phenology_df$Senescence.Senescence)
n4 <- sum(!is.na(phenology_df$HDDOY) & !is.na(phenology_df$Senescence.Senescence))
label4 <- paste0(metrics_label4, "\nn = ", n4)
p4 <- ggplot(phenology_df, aes(x = HDDOY, y = Senescence.Senescence)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Harvest Date (DOY)", y = "Dormancy (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 10)) +
  scale_y_continuous(limits = c(180, 300), breaks = seq(180, 300, 10)) + # Y-axis now starts at 180
  annotate("text", x = 235, y = Inf, # Use y=Inf to pin the label to the top
           label = "D", hjust = 0, vjust = 1.2, size = 5, fontface = "bold") +
  annotate("text", x = 305, y = 205, # Moved metrics down to y=205
           label = label4, hjust = 0, vjust = 0.5) +
  custom_theme

# Arrange all plots
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Set the output path
output_path <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/HarvestDayPhenoMethod.png"

# Save the arranged plots as a PNG (300 dpi, 10x8 inches)
png(filename = output_path, width = 14, height = 8, units = "in", res = 300)
grid.arrange(p1, p2, p3, p4, ncol = 2)
dev.off()

HDmetrics_label1_clean <- gsub("\n", ", ", metrics_label1)
HDmetrics_label2_clean <- gsub("\n", ", ", metrics_label2)
HDmetrics_label3_clean <- gsub("\n", ", ", metrics_label3)
HDmetrics_label4_clean <- gsub("\n", ", ", metrics_label4)

# ---------------------------
# Lag correlation plot
# ---------------------------
# Define the columns of interest
# Output folder

# Create your custom palette
pal <- friendly_pal("contrast_three", 50, type = "continuous")

# Output folder
out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"

# Define lag column sets
lag_cols <- list(
  lagsosderpddoy  = c("cum_RH", "SOS_deriv.sos", "UD.UD", "cum_meansrad", "avgsoilorg", 
                      "cum_tmin", "EOS_trs.eos", "EOS_deriv.eos", "cum_soiltemp", 
                      "cum_gdd", "DD.DD", "cum_vpd", "Value_max_obs"),
  
  lagsostrspddoy  = c("cum_RH", "SOS_trs.sos", "UD.UD", "cum_meansrad", "avgsoilorg", 
                      "cum_tmin", "EOS_trs.eos", "EOS_deriv.eos", "cum_soiltemp", 
                      "cum_gdd", "DD.DD", "cum_vpd", "Value_max_obs"),
  
  lagudpddoy      = c("cum_RH", "SOS_trs.sos", "SOS_deriv.sos", "UD.UD", "cum_meansrad", 
                      "avgsoilorg", "cum_tmin", "EOS_trs.eos", "EOS_deriv.eos", 
                      "cum_soiltemp", "cum_gdd", "DD.DD", "cum_vpd", "Value_max_obs")
)

# Create correlation plots with custom palette
plots <- lapply(names(lag_cols), function(lag) {
  df_sub <- df %>%
    select(any_of(lag_cols[[lag]]))
  
  cor_mat <- cor(df_sub, use = "pairwise.complete.obs")
  
  ggcorrplot(cor_mat,
             hc.order = TRUE,
             type = "lower",
             lab = TRUE,
             colors = c(pal[1], "white", pal[length(pal)]),  # min, mid, max
             title = lag)
})

# Combine plots in 2x2 grid with labels A, B, C
combined <- grid.arrange(
  grobs = plots,
  ncol = 2,
  top = textGrob("Lag Correlation Plots", gp = gpar(fontsize = 16, fontface = "bold"))
)

# Save as one file
outfile <- file.path(out_dir, "CorrelationPlots_Combined.png")
png(outfile, width = 20, height = 14, units = "in", res = 300)
grid.arrange(
  arrangeGrob(
    grobs = plots,
    ncol = 2,
    top = textGrob("Lag Correlation Plots", gp = gpar(fontsize = 18, fontface = "bold"))
  )
)
dev.off()
# ---------------------------------------------------------------------------------
# FOUR BY RANDOM FOREST PREDICTION RESULT
# ---------------------------------------------------------------------------------
# === Plot 1: RMSE and MAE for Planting ===
p1_planting_pheno <- ggplot(plot1_df_planting, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "LIMP Random Forest", y = "Error") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Plot 2: R² and Bias for Planting ===
p2_planting_pheno <- ggplot(plot2_df_planting, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "LIMP Random Forest", y = "Score") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )


# === Plot 1: RMSE and MAE for Planting ===
p1_planting_deines <- ggplot(plot1_df_planting_deines, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "Deines et al. 2023 features", y = "Error") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Plot 2: R² and Bias for Planting ===
p2_planting_deines <- ggplot(plot2_df_planting_deines, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "Deines et al. 2023 features", y = "Score") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )


# Make 2x2 grid with labels
combined_plot <- plot_grid(

  p1_planting_deines, 
  p2_planting_deines,
  p1_planting_pheno, 
  p2_planting_pheno, 
  labels = c("A", "B", "C", "D"),  # panel labels
  label_size = 18,                 # label font size
  ncol = 2, nrow = 2
)

# Save to file
out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"
outfile <- file.path(out_dir, "RF_Prediction_Results_Grid.png")

ggsave(outfile, plot = combined_plot, width = 14, height = 10, dpi = 300)


#---------------------------------------------------------------------------
#HARVEST
#---------------------------------------------------------------------------
# === Plot 1: RMSE and MAE for Harvest ===
p1_harvest_deines <- ggplot(plot1_df_harvest_deines, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "Harvesting: RMSE and MAE", y = "Error", x = "Metric") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Plot 2: R² and Bias for Harvest ===
p2_harvest_deines <- ggplot(plot2_df_harvest_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "Harvesting: R² and Bias", y = "Value", x = "Metric") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )
# === Plot 1: RMSE and MAE for Harvest ===
p1_harvest_pheno <- ggplot(plot1_df_harvest_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "LIMP Random Forest", y = "Error") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# === Plot 2: R² and Bias for Harvest ===
p2_harvest_pheno <- ggplot(plot2_df_harvest_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.7),
                width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = ifelse(is.na(SD),
                               sprintf("%.2f", Value),
                               sprintf("%.2f ± %.2f", Value, SD))),
            position = position_dodge(width = 0.7),
            vjust = -0.8,
            size = 4.5) +
  labs(title = "LIMP Random Forest", y = "Score") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# Combine harvest plots into 2x2 grid
combined_harvest <- plot_grid(
  p1_harvest_pheno, 
  p2_harvest_pheno, 
  labels = c("A", "B"),  # panel labels
  label_size = 18,                 # label font size
  ncol = 2, nrow = 1
)

# Save to file
out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"
outfile <- file.path(out_dir, "RF_Harvest_Results_Grid.png")

ggsave(outfile, plot = combined_harvest, width = 14, height = 6, dpi = 300)


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



#---------------------------------------------------------------
#Features used to predict PD 
#---------------------------------------------------------------
colnames(df_planting)
#---------------------------------------------------------------
#Features used to predict HD 
#---------------------------------------------------------------
colnames(df_harvest)

colnames(df_planting_pheno)



#---------------------------------------------------------------------------------------
#PHENOLOGICAL VS RESIDUALS
#---------------------------------------------------------------------------------------
# Create residuals for each prediction method
# UD and Greenup was the corr between cum tmax from UD-50/PDDOY days
# SOStrs and SOSdev was the corr between cum tmax from SOS trs-50/PDDOY days
# Calculate the meteo again and merge them with the df 
# do two df corr and join the rbind the corr matt
df_sos <- deinesharmonicminmaxdf %>%
  left_join(phenology_df, by = "Field_Year")
df_sos <- df_sos %>%
  left_join(meteo_summary_df_sos, by = "Field_Year")
df_sos <- df_sos %>%
  rename(DOY_max_fit = DOY_max_fit.x) %>%  # remove .x suffix
  select(-DOY_max_fit.y)                   # drop the .y column
df_sos <- df_sos %>%
  rename(
    PDDOY = PDDOY.x,
    HDDOY = HDDOY.x
  ) %>%
  select(
    -PDDOY.y,
    -HDDOY.y
  )     # drop the .y column

# Step 1: Add residuals
df <- df %>%
  mutate(
    residual_ud        = UD.UD - PDDOY,
    residual_greenup   = Greenup.Greenup - PDDOY,
  )
df_sos <- df_sos %>%
  mutate(
    residual_sos_deriv = SOS_deriv.sos - PDDOY,
    residual_sos_trs   = SOS_trs.sos - PDDOY
  )
# Step 2: Identify residual and cumulative columns
resid_cols <- grep("^residual_", names(df), value = TRUE)
cum_cols   <- grep("^cum", names(df), value = TRUE)

resid_cols_sos <- grep("^residual_", names(df_sos), value = TRUE)
cum_cols_sos   <- grep("^cum", names(df_sos), value = TRUE)

# 🔹 Remove cumRNDVI explicitly if present
cum_cols <- setdiff(cum_cols, "cumRNDVI")
cum_cols_sos <- setdiff(cum_cols_sos, "cumRNDVI")

# Step 3: Build correlation dataset (only residual vs cum)
df_corr <- df %>% select(all_of(resid_cols), all_of(cum_cols))
df_corr_sos <- df_sos %>% select(all_of(resid_cols_sos), all_of(cum_cols_sos))

# Step 4: Compute correlation matrix
cor_mat <- cor(df_corr, use = "pairwise.complete.obs")
cor_mat_sos <- cor(df_corr_sos, use = "pairwise.complete.obs")

# Step 1: Find common columns (ensures cumRNDVI already excluded)
common_cols <- intersect(colnames(cor_mat), colnames(cor_mat_sos))

# Step 2: Subset each correlation matrix for residuals and common cols
cor1_sub <- cor_mat[c("residual_ud", "residual_greenup"), common_cols, drop = FALSE]
cor2_sub <- cor_mat_sos[c("residual_sos_deriv", "residual_sos_trs"), common_cols, drop = FALSE]

# Step 3: Row-bind
residual_common <- rbind(cor1_sub, cor2_sub)

# Step 4: Round to 2 decimal places
residual_common_rounded <- round(residual_common, 2)

# Step 5: Save to CSV
out_file <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/Residuals_Pheno.csv"
write.csv(residual_common_rounded, out_file, row.names = TRUE)


#-----------------------------------------------------------------
val100test_define_set$Field_Year<-val100test_define_set$Field_ID
df_planting_pheno_residualdeines <- df %>%
  dplyr::select(
    cum_RH, SOS_trs.sos, SOS_deriv.sos, UD.UD,
    cum_meansrad, avgsoilorg, cum_tmin, EOS_trs.eos, 
    EOS_deriv.eos, cum_soiltemp, cum_gdd, DD.DD, cum_vpd,
    Value_max_obs , PDDOY, Field_Year
  ) %>% # drop Field_ID for modeling
  dplyr::filter(!is.na(PDDOY))%>%
  drop_na() # This removes rows with NA in any column

# Keep only rows present in val100test_define_set
df_planting_pheno_residualdeines <- df_planting_pheno_residualdeines %>%
  dplyr::inner_join(
    val100test_define_set %>%
      dplyr::select(Field_Year, obs, pred, residual, set),
    by = "Field_Year"
  )
# Step 1: Ensure residuals exist (replace 'residual_*' with your actual residual column names if needed)
# For example, if residuals are already in df_planting_pheno_residualdeines
# Step 2: Select only numeric columns (residuals + cumulative/other numeric variables)
numeric_cols <- df_planting_pheno_residualdeines %>% select(where(is.numeric))

# Step 3: Compute correlation matrix
cor_mat <- cor(numeric_cols, use = "pairwise.complete.obs")
# Assume cor_mat is your full correlation matrix
# Define the columns to keep
keep_cols <- c("cum_RH", "SOS_trs.sos", "SOS_deriv.sos", "UD.UD",
               "cum_meansrad", "avgsoilorg", "cum_tmin", "EOS_trs.eos",
               "EOS_deriv.eos", "cum_soiltemp", "cum_gdd", "DD.DD",
               "cum_vpd", "Value_max_obs")

# Step 1: Extract only the "residual" row and selected columns
residual_row <- cor_mat["residual", keep_cols, drop = FALSE]

# Step 2: Round to 2 decimal places
residual_row_rounded <- round(residual_row, 2)

# Step 3: Save to CSV
out_file <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/Residual_Deines.csv"
write.csv(residual_row_rounded, out_file, row.names = TRUE)



#---------------------------------------------------------------
# CORRELATION PLOT
#---------------------------------------------------------------
# Map readable labels with subscripts
library(dplyr)
library(ggcorrplot)

# Build df_corr by selecting and cleaning the data
df_corr <- df %>%
  dplyr::select(
    cum_RH, SOS_trs.sos, SOS_deriv.sos, UD.UD,
    cum_meansrad, avgsoilorg, cum_tmin, EOS_trs.eos,
    EOS_deriv.eos, cum_soiltemp, cum_gdd, DD.DD, cum_vpd,
    Value_max_obs, PDDOY
  ) %>%
  # Filter and drop NA as in your original script
  filter(!is.na(PDDOY)) %>%
  drop_na()

# Create the correlation matrix
corr_mat <- cor(df_corr, use = "pairwise.complete.obs")

# Reorder columns to put PDDOY first
cols_order <- c("PDDOY", setdiff(colnames(corr_mat), "PDDOY"))
corr_mat2 <- corr_mat[cols_order, cols_order]

# Define the readable labels with subscripts
variable_labels <- c(
  "cum_RH" = "RH[cum]",
  "SOS_trs.sos" = "SOS[TRS]",
  "SOS_deriv.sos" = "SOS[Deriv]",
  "UD.UD" = "UD",
  "cum_meansrad" = "Srad[cum]",
  "avgsoilorg" = "SOC[mean]",
  "cum_tmin" = "AirTmin[cum]",
  "EOS_trs.eos" = "EOS[TRS]",
  "EOS_deriv.eos" = "EOS[Deriv]",
  "cum_soiltemp" = "SoilTmean[cum]",
  "cum_gdd" = "GDD[cum]",
  "DD.DD" = "DD",
  "cum_vpd" = "VPD[cum]",
  "Value_max_obs" = "kNDVI[max]",
  "PDDOY" = "PD"
)

# Crucial step: Rename the row and column names of the matrix
# This ensures ggplot will use the correct labels for the plot axes
colnames(corr_mat2) <- variable_labels[colnames(corr_mat2)]
rownames(corr_mat2) <- variable_labels[rownames(corr_mat2)]

# Melt the correlation matrix for ggplot2
# Only keep the lower triangle of the matrix to avoid redundancy
corr_mat_long <- as.data.frame(as.table(corr_mat2)) %>%
  rename(Var1 = Var1, Var2 = Var2, value = Freq) %>%
  mutate(Var1 = factor(Var1, levels = rev(unique(Var1))),
         Var2 = factor(Var2, levels = unique(Var1))) %>%
  filter(as.integer(Var2) <= as.integer(Var1))

# Create your custom palette
pal <- yarrr::friendly_pal("contrast_three", 50, type = "continuous")

# Generate the plot directly with ggplot2 and assign to a variable
p <- ggplot(data = corr_mat_long, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 5) +
  scale_fill_gradientn(
    colors = pal,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  theme_minimal() +
  coord_fixed() +
  labs(title = "Correlation Plot") +
  # Use scale_x_discrete and scale_y_discrete to control the labels
  scale_x_discrete(labels = parse(text = levels(corr_mat_long$Var2))) +
  scale_y_discrete(labels = parse(text = levels(corr_mat_long$Var1))) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.position = "right"
  )

# Save the plot to the specified directory
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/corr plot.png",
  plot = p,
  width =12,
  height = 8,
  dpi = 500
)


# Print top 7 correlations of PD with other features (descending)
pd_corrs <- corr_mat["PDDOY", colnames(corr_mat) != "PDDOY"]
top7 <- sort(pd_corrs, decreasing = TRUE)[1:7]
top7_rounded <- round(top7, 3)
print(top7_rounded)
# optional single-line print
cat("Top 7 correlations with PD:", paste(names(top7_rounded), top7_rounded, collapse = "; "), "\n")





corr_mat
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#OLD PLOTS
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------

### plot some DOP and DOH data
## read the csv file
dfgroudtruthdata<- read.csv('C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/CG_RiceDOPDOH_201520162018201920202021.csv')
plot(dfgroudtruthdata)
dfgroudtruthdata

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
p

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
