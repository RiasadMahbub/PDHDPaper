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
library(vangogh)
library(phenofit)
#utils::browseURL(getwd())
#==================================================================================
#==================================================================================
# Generated from 'OrganizeGroundData.R' - Planting day-of-year (PDDOY) distribution
zissou_color <- wes_palette("Zissou1")[1]  # "#3B9AB2"# Get first color from Zissou1 palette
#==================================================================================
#==================================================================================
#Script: OrganizingHarvestDate and OrganizeGroundData.R
# Generated from 'OrganizeGroundData.R' - Harvest day-of-year (HDDOY) distribution
zissou_color4 <- wes_palette("Zissou1")[4]  # "#F21A00"# Get 4th color from Zissou1 palette
# Create the plot object
p_hd <- ggplot(combined_data, aes(x = HDDOY)) +
  geom_histogram(binwidth = 5, color = "black", fill = zissou_color4, alpha = 0.7) +
  labs(
    x = "Day of Year",
    y = "Number of Harvests"
  ) +
  theme_minimal(base_size = 16) +
  scale_x_continuous(breaks = seq(0, 366, by = 30)) +
  annotate("text", x = 210, y = 75, 
           label = paste0("N = ", sum(!is.na(combined_data$HDDOY))), 
           size = 6, color = zissou_color4)
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
  theme_classic(base_size = 18) +
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
      hjust = ifelse(Type == "HDDOY", 1, 0.8)
    ),
    color = "black",
    size = 6,
    vjust = 1
  ) +
  theme(
    axis.title = element_text(color = "black"), 
    axis.text  = element_text(color = "black", size = 18),
    # Add major and minor grid lines
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey95"),
    legend.position = c(0.85, 0.25),   # move legend inside
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA), # semi-transparent box
    legend.key.size = unit(0.6, "cm")
  )

pdhdviolin
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
zissou_colors <- wes_palette("Zissou1")# Assign colors from Wes Anderson Zissou1 palette
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
  labs(  x = expression("DOY of   (" * italic(k) * "NDVI"[max] * ") to PD (day)"),
        y = "Count") +
  theme_classic(base_size = 18) +
  annotate(
    "text",
    x = 120,       # explicit x-coordinate
    y = 60,        # explicit y-coordinate
    label = paste0("Mean = ", round(stats$mean_PD,1), "\nSD = ", round(stats$sd_PD,1)),
    hjust = 0,
    vjust = 0,
    size = 6
  ) +
  theme(
    axis.title = element_text(color = "black"), 
    axis.text  = element_text(color = "black", size = 16),
    # Add major and minor grid lines
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey95")
  )

# Plot HDMaxdays
p2 <- ggplot(df, aes(x = HDMaxdays)) +
  geom_histogram(fill = color_map["HDMaxdays"], color = "white", bins = 30) +
  labs(  x = expression("DOY of  (" * italic(k) * "NDVI"[max] * ") to HD (day)"),
        y = "Count") +
  theme_classic(base_size = 18) +
  annotate(
    "text",
    x = 70,        # explicit x-coordinate
    y = 45,        # explicit y-coordinate
    label = paste0("Mean = ", round(stats$mean_HD,1), "\nSD = ", round(stats$sd_HD,1)),
    hjust = 0,
    vjust = 0,
    size = 6
  ) +
  theme(
    axis.title = element_text(color = "black"), 
    axis.text  = element_text(color = "black", size = 16),
    # Add major and minor grid lines
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey95")
  )

# Combine with A, B notation
pdhddoymax <- p1 + p2 + plot_annotation(tag_levels = "A")
pdhddoymax
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

# clean legend labels - Use simple strings for the factor labels
# The expressions will be applied later in scale_color_manual
df_long$LagType <- factor(df_long$LagType,
                          levels = c("lagsosderpddoy", "lagsostrspddoy", "lagudpddoy"),
                          labels = c("lagsosderpddoy", "lagsostrspddoy", "lagudpddoy"))


# use first 3 colors from GrandBudapest1
pal <- wes_palette("GrandBudapest1")[2:4]

# Define the expressions for the legend labels once
legend_labels <- c(
  expression(italic("Duration_PD_SOS"["DER"])),
  expression(italic("Duration_PD_SOS"["TRS"])),
  expression(italic("Duration_PD_UD"))
)
# plot
# plot
lagall <- ggplot(df_long, aes(x = LagValue, y = gsl, color = LagType)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  
  stat_cor(
    aes(label = after_stat(paste0("italic(r) == ", sprintf("%.2f", r)))),
    method = "pearson",
    parse = TRUE,
    label.x.npc = "left",
    label.y.npc = "top",
    size = 6,
    show.legend = FALSE
  ) +
  theme_classic(base_size = 16) +
  scale_color_manual(
    values = pal,
    labels = legend_labels
  ) +
  
  scale_x_continuous(
    breaks = seq(-40, max(df_long$LagValue, na.rm = TRUE), by = 20)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, max(df_long$gsl, na.rm = TRUE), by = 25)
  ) +
  
  labs(
    x = "Lag (day)",
    y = "Growing season length (HD–PD)",
    color = NULL
  ) +
  
  theme(
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 16),
    legend.position = c(0.45, 0.87),
    legend.direction = "vertical",
    legend.background = element_rect(fill = alpha("white", 0.5)),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 16)
  )

lagall

# 1. Map the EXACT column values to your desired Plotmath expressions
custom_titles <- c(
  "lagsosderpddoy" = "italic(Duration_PD_SOS)[DER]",
  "lagsostrspddoy" = "italic(Duration_PD_SOS)[TRS]",
  "lagudpddoy"     = "italic(Duration_PD_UD)"
)

# 2. Re-run the plot logic
lagall <- ggplot(df_long, aes(x = LagValue, y = gsl, color = LagType)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  
  # stat_cor fix (parse outside aes)
  stat_cor(
    aes(label = after_stat(paste0("italic(r) == ", sprintf("%.2f", r)))),
    method = "pearson",
    parse = TRUE, 
    label.x.npc = "left",
    label.y.npc = "top",
    size = 6,
    show.legend = FALSE
  ) +
  
  # Use the as_labeller with the corrected keys
  facet_wrap(
    ~LagType,
    nrow = 1,
    ncol = 3,
    labeller = as_labeller(custom_titles, default = label_parsed)
  ) +
  
  theme_classic(base_size = 16) +
  scale_color_manual(values = pal) + 
  
  labs(
    x = "Lag (day)",
    y = "Growing season length (HD–PD)",
    color = NULL
  ) +
  
  theme(
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 16),
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold"),
    legend.position = "none"
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

# 1. Define the layout
# We use the '/' and '+' operators, but we ensure lagall is 
# treated as three separate areas if possible, or we adjust the tagging.

combined_all <- (
  pdhdviolin /             # Becomes A
    lagall /                 # Becomes B, C, D (if facets are tagged)
    (p1 + p2)                # Becomes E and F
) + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 20, face = "bold"))
combined_all
# NOTE: If patchwork still gives lagall only one tag (B), 
# use the 'collect' or manual tagging approach below:
# Save the combined figure
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/combined_violin_lag_hist.png",
  plot = combined_all,
  width = 14,
  height = 14,
  dpi = 300
)
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Field data of PD and HD
#--------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
write.csv(
  combined_data,
  "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/combined_data.csv",
  row.names = FALSE
)

#---------------------------------------------------------------------------------------
# SUMMARY COUNT OF THE DATA
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
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
#-----------------------------------------------------------------------
# PHENOLOGICAL METHODS
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

# Helper function to calculate metrics and return a label
# Note: Assuming the rmse and mae functions are defined elsewhere in your script.
get_metrics_label <- function(y, y_hat) {
  r2 <- round(cor(y, y_hat, use = "complete.obs")^2, 2)
  rmse_val <- round(rmse(y, y_hat), 2)
  mae_val <- round(mae(y, y_hat), 2)
  bias_val <- round(mean(y_hat - y, na.rm = TRUE), 2)
  paste0("R² = ", r2, 
         "\nRMSD = ", rmse_val, " days",
         "\nMAD = ", mae_val, " days",
         "\nMBD = ", bias_val, " days")
}
get_metrics_label <- function(y, y_hat) {
  valid <- complete.cases(y, y_hat)  # keep only rows where both are non-NA
  y <- y[valid]
  y_hat <- y_hat[valid]
  
  r2 <- round(cor(y, y_hat)^2, 2)
  rmse_val <- round(rmse(y, y_hat), 2)
  mae_val <- round(mae(y, y_hat), 2)
  bias_val <- round(mean(y_hat - y), 2)
  
  paste0("R² = ", r2, 
         "\nRMSD = ", rmse_val, " days",
         "\nMAD = ", mae_val, " days",
         "\nMBD = ", bias_val, " days")
}
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
  annotate("text", x = 140, y = 40,
           label = label4, hjust = 0, vjust = 0) +
  custom_theme

# Arrange all plots
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2)
combined_plot
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
# Plot 1 - A
metrics_label1 <- get_metrics_label(phenology_df$HDDOY, phenology_df$EOS_trs.eos)
n1 <- sum(!is.na(phenology_df$HDDOY) & !is.na(phenology_df$EOS_trs.eos))
label1 <- paste0(metrics_label1, "\nn = ", n1)
# Top and bottom positions for annotations
top_y <- Inf
metrics_y <- 190   # adjust to be slightly below the data points
label_size <- 4.5

# Plot 1 - A
p1 <- ggplot(phenology_df, aes(x = HDDOY, y = EOS_trs.eos)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = NULL, y = "End of Season (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 10)) +
  coord_cartesian(ylim = c(190, 310)) +
  annotate("text", x = 235, y = top_y, label = "A", hjust = 0, vjust = 1.2, size = label_size, fontface = "bold") +
  annotate("text", x = 300, y = metrics_y, label = label1, hjust = 0, vjust = 0, size = label_size) +
  custom_theme

# Plot 2 - B
p2 <- ggplot(phenology_df, aes(x = HDDOY, y = EOS_deriv.eos)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = NULL, y = "End of Season (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 10)) +
  coord_cartesian(ylim = c(190, 310)) +
  annotate("text", x = 235, y = top_y, label = "B", hjust = 0, vjust = 1.2, size = label_size, fontface = "bold") +
  annotate("text", x = 300, y = metrics_y, label = label2, hjust = 0, vjust = 0, size = label_size) +
  custom_theme

# Plot 3 - C
p3 <- ggplot(phenology_df, aes(x = HDDOY, y = DD.DD)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Harvest Date (DOY)", y = "Final Adjusted Downturn Date (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 10)) +
  coord_cartesian(ylim = c(190, 310)) +
  annotate("text", x = 235, y = top_y, label = "C", hjust = 0, vjust = 1.2, size = label_size, fontface = "bold") +
  annotate("text", x = 300, y = metrics_y, label = label3, hjust = 0, vjust = 0, size = label_size) +
  custom_theme

# Plot 4 - D
p4 <- ggplot(phenology_df, aes(x = HDDOY, y = Dormancy.Dormancy)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "orange", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Harvest Date (DOY)", y = "Dormancy (DOY)") +
  scale_x_continuous(breaks = seq(0, 400, 10)) +
  coord_cartesian(ylim = c(190, 310)) +
  annotate("text", x = 225, y = top_y, label = "D", hjust = 0, vjust = 1.2, size = label_size, fontface = "bold") +
  annotate("text", x = 290, y = metrics_y, label = label4, hjust = 0, vjust = 0, size = label_size) +
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


#========================================================
# METRIC FUNCTION (paired observations)
#========================================================

get_metrics <- function(management, phenology) {
  valid <- complete.cases(management, phenology)
  m <- management[valid]
  p <- phenology[valid]
  d <- p - m
  c(
    RMSD = round(sqrt(mean(d^2)), 2),
    MAD  = round(mean(abs(d)), 2),
    MBD  = round(mean(d), 2),
    r    = round(cor(m, p), 2)
  )
}

#========================================================
# VARIABLES
#========================================================

methods <- list(
  Threshold  = df$SOS_trs.sos,
  Derivative = df$SOS_deriv.sos,
  Infection   = df$Greenup.Greenup,
  GU  = df$UD.UD
)

#========================================================
# PAIRWISE METRICS
#========================================================

results <- t(sapply(methods, function(x) {
  get_metrics(df$PDDOY, x)
}))

metrics_tablepd <- data.frame(
  Method = rownames(results),
  RMSD = results[, "RMSD"],
  MAD  = results[, "MAD"],
  MBD  = results[, "MBD"],
  R    = results[, "r"],
  n = 662
)

metrics_tablepd

#========================================================
# METRIC FUNCTION
#========================================================

get_metrics <- function(management, phenology) {
  
  valid <- complete.cases(management, phenology)
  
  m <- management[valid]
  p <- phenology[valid]
  
  d <- p - m
  
  c(
    RMSD = round(sqrt(mean(d^2)), 2),
    MAD  = round(mean(abs(d)), 2),
    MBD  = round(mean(d), 2),
    R    = round(cor(m, p), 2),
    n    = length(d)
  )
}

#========================================================
# HARVEST METHODS
#========================================================

hd_methods <- list(
  Threshold  = df$EOS_trs.eos,
  Derivative = df$EOS_deriv.eos,
  Inflection= df$Dormancy.Dormancy,
  Gu         = df$DD.DD
  
)

#========================================================
# CALCULATE METRICS
#========================================================

hd_results <- t(sapply(hd_methods, function(x) {
  get_metrics(df$HDDOY, x)
}))

#========================================================
# CREATE FINAL TABLE
#========================================================

hd_metrics_table <- data.frame(
  Method = rownames(hd_results),
  RMSD = hd_results[, "RMSD"],
  MAD  = hd_results[, "MAD"],
  MBD  = hd_results[, "MBD"],
  R    = hd_results[, "R"],
  n    = hd_results[, "n"]
)

hd_metrics_table

#========================================================
# SAVE TABLES AS CSV
#========================================================

write.csv(
  hd_metrics_table,
  "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/hd_metrics_table.csv",
  row.names = FALSE
)

write.csv(
  metrics_table,
  "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/metrics_tablepd.csv",
  row.names = FALSE
)

# ---------------------------
# ---------------------------
# Lag correlation plot
# ---------------------------
# ---------------------------
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
# RANDOM FOREST PREDICTION - Updated for MBD
# ---------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(cowplot)
library(vangogh)

# --- 1. DATA PREPARATION & CONSISTENCY ---
# This ensures that the code finds the correct rows even if the original data says "Bias"
rename_bias_to_mbd <- function(df) {
  if("Metric" %in% names(df)) {
    df$Metric <- gsub("Bias", "MBD", df$Metric)
  }
  return(df)
}

# Apply renaming and factor leveling to all dataframes
plot1_df <- rename_bias_to_mbd(plot1_df)
plot2_df <- rename_bias_to_mbd(plot2_df)
plot1_df_planting_pheno  <- rename_bias_to_mbd(plot1_df_planting_pheno)
plot2_df_planting_pheno  <- rename_bias_to_mbd(plot2_df_planting_pheno)
plot1_df_planting_deines <- rename_bias_to_mbd(plot1_df_planting_deines)
plot2_df_planting_deines <- rename_bias_to_mbd(plot2_df_planting_deines)

dataset_levels <- c("Train", "Validation", "Test")

# Apply factors
plot1_df$Dataset <- factor(plot1_df$Dataset, levels = dataset_levels)
plot2_df$Dataset <- factor(plot2_df$Dataset, levels = dataset_levels)
plot1_df_planting_pheno$Dataset <- factor(plot1_df_planting_pheno$Dataset, levels = dataset_levels)
plot2_df_planting_pheno$Dataset <- factor(plot2_df_planting_pheno$Dataset, levels = dataset_levels)
plot1_df_planting_deines$Dataset <- factor(plot1_df_planting_deines$Dataset, levels = dataset_levels)
plot2_df_planting_deines$Dataset <- factor(plot2_df_planting_deines$Dataset, levels = dataset_levels)

# --- 2. THEME & COLOR SETUP ---
vvg_colors <- vangogh_palette("SunflowersMunich", n = 5, type = "discrete")
vg_colors <- vvg_colors[c(1, 3, 5)]

y_rmse_range <- c(0, 14)
y_r2_range   <- c(0, 1)
y_mbd_range  <- c(-2, 2) # Updated variable name for clarity
r2_label     <- expression(italic(R)^2 ~ "(unitless)")
mbd_label    <- "MBD (days)"

# --- 3. PLOTTING FUNCTIONS ---
# --- Function for RMSE+MAE
make_rmse_mae_plot <- function(df, title, y_range) {
  ggplot(df, aes(x = Metric, y = Value, fill = Dataset)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                  position = position_dodge(width = 0.8), width = 0.2, na.rm = TRUE) +
    geom_text(aes(label = ifelse(is.na(SD), sprintf("%.2f", Value),
                                 sprintf("%.2f ± %.1f", Value, SD))),
              position = position_dodge(width = 1), vjust = -2.5, size =7 ) +
    scale_fill_manual(values = vg_colors) +
    scale_y_continuous(limits = y_range, breaks = seq(0, 12, 4)) +
    labs(title = title, y = "Error", x = "") +
    theme_classic(base_size = 17) +
    theme(axis.title.y = element_text(size = 24),
          axis.text.y = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          plot.title = element_text(size = 17, face = "bold"))   # <-- bold
}

# --- Function for R² or MBD
make_score_plot <- function(df, y_label, title, y_range, vjust_text = -3) {
  ggplot(df, aes(x = Dataset, y = Value, fill = Dataset)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.7) +
    geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), width = 0.2, na.rm = TRUE) +
    geom_text(aes(label = ifelse(is.na(SD), sprintf("%.2f", Value),
                                 sprintf("%.2f ± %.1f", Value, SD))),
              vjust = vjust_text, size = 6.5) +
    scale_fill_manual(values = vg_colors) +
    scale_y_continuous(limits = y_range) +
    labs(title = title, y = y_label, x = "") +
    theme_classic(base_size = 17) +
    theme(axis.title.y = element_text(size = 22),
          axis.text.y = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          plot.title = element_text(size = 17, face = "bold"))   # <-- bold
}

# --- 4. CREATE INDIVIDUAL PLOTS ---

# PD[SOSDER] Plots
p1 <- make_rmse_mae_plot(plot1_df, title = expression(PD[SOSDER] ~ ": MAE & RMSE"), y_range = y_rmse_range) +
  guides(fill = guide_legend(title = "Dataset", nrow = 1)) +
  theme(legend.position = c(0.05, 1), legend.justification = c(0, 1),
        legend.background = element_rect(fill = alpha('white', 0.6)),
        axis.text.x = element_blank())

p2 <- make_score_plot(filter(plot2_df, Metric == "R²"), r2_label, 
                      expression(PD[SOSDER] ~ ":" ~ italic(R)^2), y_r2_range, vjust_text = 2.5) +
  theme(legend.position = "none", axis.text.x = element_blank())

p3 <- make_score_plot(filter(plot2_df, Metric == "MBD"), mbd_label, 
                      expression(PD[SOSDER] ~ ": MBD"), y_mbd_range, vjust_text = -3.5) +
  theme(legend.position = "none", axis.text.x = element_blank())

# Deines Plots
p1_deines <- make_rmse_mae_plot(plot1_df_planting_deines, "Deines et al. (2023): MAE & RMSE", y_rmse_range) +
  theme(legend.position = "none", axis.text.x = element_blank())

p2_deines <- make_score_plot(filter(plot2_df_planting_deines, Metric == "R²"), r2_label,
                             expression("Deines RF:" ~ italic(R)^2), y_r2_range, vjust_text = 2.5) +
  theme(legend.position = "none", axis.text.x = element_blank())

# Deines plot MBD (F)
p3_deines <- make_score_plot(
  plot2_bias_deines, 
  "MBD (days)", 
  "Deines et al., 2023 Random Forest: MBD",  # <-- changed from Bias to MBD
  y_bias_range, 
  vjust_text = -1
) +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank())


# LIMP Plots
p1_pheno <- make_rmse_mae_plot(plot1_df_planting_pheno, "LIMP RF: MAE & RMSE", y_rmse_range) +
  theme(legend.position = "none")

p2_pheno <- make_score_plot(filter(plot2_df_planting_pheno, Metric == "R²"), r2_label,
                            expression("LIMP RF:" ~ italic(R)^2), y_r2_range, vjust_text = 2.5) +
  theme(legend.position = "none")

# LIMP plot MBD (I)
p3_pheno <- make_score_plot(
  plot2_bias_pheno, 
  "MBD (days)", 
  "LIMP Random Forest: MBD",  # <-- changed from Bias to MBD
  y_bias_range, 
  vjust_text = -1
) +
  theme(legend.position = "none")

# --- 5. COMBINE & SAVE ---
combined_3x3 <- plot_grid(
  p1, p2, p3,
  p1_deines, p2_deines, p3_deines,
  p1_pheno, p2_pheno, p3_pheno,
  labels = "AUTO",
  ncol = 3, nrow = 3,
  align = "hv",
  rel_widths = c(1.5, 1, 1),
  label_size = 20,
  label_fontface = "bold"
)

# Show and Save
print(combined_3x3)
outfile <- file.path(out_dir, "Random_Forest_Planting_MBD_Updated.png")
ggsave(outfile, plot = combined_3x3, width = 22, height = 16, dpi = 300)


#---------------------------------------------------------------------------
# HARVEST PLOTS — LIMP (Pheno) Random Forest only
#---------------------------------------------------------------------------
plot2_bias_df <- plot2_df_planting_pheno %>%
  dplyr::filter(Metric == "Bias")

plot3_r2_df <- plot2_df_planting_pheno %>%
  dplyr::filter(Metric == "R²")
# --- Ensure Dataset factor levels
plot1_df_harvest_pheno$Dataset <- factor(plot1_df_harvest_pheno$Dataset, 
                                         levels = c("Train","Validation","Test"))
plot2_df_harvest_pheno$Dataset <- factor(plot2_df_harvest_pheno$Dataset, 
                                         levels = c("Train","Validation","Test"))

# --- Separate R² and Bias
plot2_r2_harvest_pheno <- dplyr::filter(plot2_df_harvest_pheno, Metric == "R²")
plot2_bias_harvest_pheno <- dplyr::filter(plot2_df_harvest_pheno, Metric == "Bias (day)")

# --- Colors
vvg_colors <- vangogh_palette("SunflowersMunich", n = 5, type = "discrete")
vg_colors <- vvg_colors[c(1,3,5)]

# --- Compute y-axis ranges for consistency
y_rmse_range <- range(c(0, 9), na.rm = TRUE)
y_r2_range   <- c(0,1)
y_bias_range <- range(c(-1,1.2), na.rm = TRUE)

# --- Function for RMSE+MAE plot
make_rmse_mae_plot <- function(df, title, y_range) {
  ggplot(df, aes(x = Metric, y = Value, fill = Dataset)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                  position = position_dodge(width = 0.7), width = 0.2, na.rm = TRUE) +
    geom_text(aes(label = ifelse(is.na(SD), sprintf("%.2f", Value),
                                 sprintf("%.2f ± %.2f", Value, SD))),
              position = position_dodge(width = 0.7), vjust = -1, size = 4) +
    labs(title = title, y = "Error", x = "") +
    scale_fill_manual(values = vg_colors) +
    coord_cartesian(ylim = y_range) +
    theme_classic(base_size = 10) +
    theme(axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 16))
}

# --- Function for R² or Bias plot
make_score_plot <- function(df, y_label, title, y_range, vjust_text = -0.8) {
  ggplot(df, aes(x = Dataset, y = Value, fill = Dataset)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), width = 0.2, na.rm = TRUE) +
    geom_text(aes(label = ifelse(is.na(SD), sprintf("%.2f", Value),
                                 sprintf("%.2f ± %.2f", Value, SD))),
              vjust = vjust_text, size = 4) +
    labs(title = title, y = y_label, x = "") +
    scale_fill_manual(values = vg_colors) +
    coord_cartesian(ylim = y_range) +
    theme_classic(base_size = 10) +
    theme(axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 16))
}

# --- R² y-axis label
r2_label <- expression(italic(R)^2~"(unitless)")

# --- Create PHENO (LIMP) MAE/RMSE plot (legend inside horizontal)
# --- Create PHENO (LIMP) MAE/RMSE plot with legend in top-left (horizontal)
p1_pheno <- make_rmse_mae_plot(plot1_df_harvest_pheno, "LIMP: MAE & RMSE", y_rmse_range) +
  guides(fill = guide_legend(title = "Fold", nrow = 1)) +
  theme(
    legend.position = c(0.05, 0.95),   # top-left, slightly above the plot
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = alpha('white', 0.6), color = NA),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 14),
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  )

# --- PHENO R² plot (no legend)
p2_pheno <- make_score_plot(
  plot2_r2_harvest_pheno, r2_label, expression("LIMP: " * italic(R)^2),
  y_r2_range, vjust_text = 2.3
) +
  theme(
    legend.position = "none",
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  )

# --- PHENO Bias plot (no legend)
p3_pheno <- make_score_plot(
  plot2_bias_harvest_pheno, "Bias (day)", "LIMP: Bias",
  y_bias_range, vjust_text = -1
) +
  theme(
    legend.position = "none",
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  )

# --- Combine PHENO plots (keep legend only from p1)
combined_pheno_plot <- (
  p1_pheno + p2_pheno + p3_pheno +
    plot_layout(
      nrow = 1,
      ncol = 3,
      widths = c(1.85, 1, 1)
    )
) &
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
combined_pheno_plot

# --- Add A, B, C labels
combined_pheno_plot <- combined_pheno_plot +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 18))
  )
combined_pheno_plot
# --- Save
out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"
outfile <- file.path(out_dir, "RF_Harvest_Pheno_3plots_Grid_LegendBottom_Labeled.png")
outfile
ggsave(outfile, plot = combined_pheno_plot, width = 16, height = 5, dpi = 200)

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
#--------------------------------------------------------------------
# Residual Correlation with Significance Levels (Organized Version)
#--------------------------------------------------------------------
library(dplyr)
#--------------------------------------------------------------------
# Step 1: Prepare Data
#--------------------------------------------------------------------
df_planting_pheno_sos_resi <- df %>%
  dplyr::select(
    cum_tmin, cum_soiltemp, SOS_trs.sos, cum_gdd, SOS_deriv.sos,
    cum_RH, cum_vpd, avgsoilorg, PDDOY, UD.UD, Greenup.Greenup
  ) %>%
  dplyr::filter(!is.na(PDDOY)) %>%
  drop_na() %>%
  dplyr::mutate(
    residual_SOS_deriv = SOS_deriv.sos - PDDOY,
    residual_SOS_trs   = SOS_trs.sos - PDDOY,
    residual_ud        = UD.UD - PDDOY,
    residual_greenup   = Greenup.Greenup - PDDOY
  )

#--------------------------------------------------------------------
# Step 2: Define Columns
#--------------------------------------------------------------------
feature_cols <- c("cum_tmin", "cum_soiltemp", "cum_gdd",
                  "cum_RH", "cum_vpd", "avgsoilorg")
resid_cols   <- c("residual_SOS_deriv", "residual_SOS_trs",
                  "residual_ud", "residual_greenup")

#--------------------------------------------------------------------
# Step 3: Function to Calculate Correlation + Significance
#--------------------------------------------------------------------
cor_with_p <- function(x, y) {
  test <- suppressWarnings(cor.test(x, y, use = "pairwise.complete.obs"))
  r <- round(test$estimate, 2)
  p <- test$p.value
  stars <- ifelse(p < 0.01, "***",
                  ifelse(p < 0.05, "**",
                         ifelse(p < 0.1, "*", "")))
  return(paste0(r, stars))
}

#--------------------------------------------------------------------
# Step 4: Compute Correlation Table
#--------------------------------------------------------------------
res_list <- list()
for (res in resid_cols) {
  row_values <- sapply(feature_cols, function(f)
    cor_with_p(df_planting_pheno_sos_resi[[res]], df_planting_pheno_sos_resi[[f]]))
  res_list[[res]] <- row_values
}
residual_corr_sig <- do.call(rbind, res_list)
rownames(residual_corr_sig) <- resid_cols

#--------------------------------------------------------------------
# Step 5: Rename Rows and Columns
#--------------------------------------------------------------------
rownames(residual_corr_sig) <- c(
  "Duration_PD_SOSDER",
  "Duration_PD_SOSTRS",
  "Duration_PD_UD",
  "Duration_PD_Greenup"
)

colnames(residual_corr_sig) <- c(
  "AirTmin_cum",
  "SoilTmean_cum",
  "GDDcum",
  "RHcum",
  "VPDcum",
  "SOCmean"
)

#--------------------------------------------------------------------
# Step 6: Save and Print
#--------------------------------------------------------------------
out_file <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/Residuals_Pheno_withSignificance_Renamed.csv"
write.csv(residual_corr_sig, out_file, row.names = TRUE)
print(residual_corr_sig)
#--------------------------------------------------------------------
# Replace stars with p-value text
#--------------------------------------------------------------------
# Make a copy as character matrix
residual_corr_sig_p <- residual_corr_sig

# Replace stars with p-values
residual_corr_sig_p[,] <- apply(residual_corr_sig, c(1,2), function(x) {
  x <- gsub("\\*\\*\\*", " (p < 0.01)", x)
  x <- gsub("\\*\\*", " (p < 0.05)", x)
  x <- gsub("\\*", " (p < 0.1)", x)
  return(x)
})

# Convert to data frame if needed
residual_corr_sig_p <- as.data.frame(residual_corr_sig_p)
residual_corr_sig_p

# Mapping for nicer residual names
resid_name_map <- c(
  "Duration_PD_SOSDER"  = "ResidualSOSDER",
  "Duration_PD_SOSTRS"  = "ResidualSOSTRS",
  "Duration_PD_UD"      = "ResidualUD",
  "Duration_PD_Greenup" = "ResidualGreenup"
)

# --- CORRECTED SUMMARY GENERATION LOGIC ---
# Create sentence per residual (top 3 only)
sentences <- sapply(rownames(residual_corr_sig), function(resid_name) {
  # 1. Get the original values (e.g., "-0.3***") for robust numerical sorting
  original_values <- residual_corr_sig[resid_name, ]
  
  # 2. Extract numeric values for sorting by removing ONLY the '*' characters 
  # This correctly turns "-0.3***" into -0.3
  numeric_vals <- abs(as.numeric(gsub("\\*", "", original_values)))
  
  # 3. Get the top 3 indices based on absolute numeric value
  top3_idx <- order(numeric_vals, decreasing = TRUE)[1:3]
  
  # 4. Use the index to pull both the predictor names and the nicely formatted values
  top_predictors <- colnames(residual_corr_sig)[top3_idx]
  # Pull the formatted values (e.g., "-0.3 (p < 0.01)") from the output matrix
  cor_values <- residual_corr_sig_p[resid_name, top3_idx]
  
  # 5. Create the sentence
  paste0(resid_name_map[resid_name], " were most correlated with ",
         paste0(top_predictors, " (r = ", cor_values, ")", collapse = ", "), ".")
})

# Combine sentences
summary_sentence <- paste(sentences, collapse = " ")

# Print the final, corrected summary
cat("\n--- Corrected Summary Sentence ---\n")
cat(summary_sentence, "\n")


#-----------------------------------------------------------------
#Deines Residual 
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# Deines Residual Correlation Analysis (with significance)
#-----------------------------------------------------------------
val100test_define_set$Field_Year <- val100test_define_set$Field_ID
df_planting_pheno_residualdeines <- df %>%
  dplyr::select(
    cum_RH, SOS_trs.sos, SOS_deriv.sos,
     avgsoilorg, cum_tmin,
    cum_soiltemp, cum_gdd,  cum_vpd,
     PDDOY, Field_Year
  ) %>%
  dplyr::filter(!is.na(PDDOY)) %>%
  drop_na() %>%
  dplyr::inner_join(
    val100test_define_set %>%
      dplyr::select(Field_Year, obs, pred, residual, set),
    by = "Field_Year"
  )
df_planting_deines_residual$Field_Year <- df_planting_deines_residual$Field_ID
df_planting_pheno_residualdeines <- df %>%
  dplyr::select(
    cum_RH, SOS_trs.sos, SOS_deriv.sos,
    avgsoilorg, cum_tmin,
    cum_soiltemp, cum_gdd,  cum_vpd,
    PDDOY, Field_Year
  ) %>%
  dplyr::filter(!is.na(PDDOY)) %>%
  drop_na() %>%
  dplyr::inner_join(
    df_planting_deines_residual %>%
      dplyr::select(Field_Year, residual_PDDeines),
    by = "Field_Year"
  )

# Select numeric columns
numeric_cols <- df_planting_pheno_residualdeines %>% select(where(is.numeric))

# Compute correlation and p-values
cor_test <- function(x, y) {
  test <- cor.test(x, y, use = "pairwise.complete.obs")
  c(cor = test$estimate, p = test$p.value)
}

vars_to_keep <- c("cum_RH", "SOS_trs.sos", "SOS_deriv.sos", 
                  "avgsoilorg", "cum_tmin",
               "cum_soiltemp", "cum_gdd", 
                  "cum_vpd")

results <- sapply(vars_to_keep, function(v) {
  cor_test(numeric_cols$residual_PDDeines, numeric_cols[[v]])
})

# Convert results to data frame
results_df <- as.data.frame(t(results))
colnames(results_df) <- c("Correlation", "p_value")

# Add significance stars
results_df$Significance <- cut(
  results_df$p_value,
  breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", "")
)

# Rename variables for clarity
results_df$Variable <- c("RHcum", "SOSTRS", "SOSDER", 
                         "SOCmean", "AirTmin_cum",
                         "Soil_T_meancum",
                         "GDDcum", "VPDcum")

# Round correlation values
results_df_clean <- results_df[, c("Correlation", "Significance")]
# Combine correlation and significance into one column
results_df_clean$Correlation_and_Significance <- paste0(
  round(results_df_clean$Correlation, 2),
  results_df_clean$Significance
)
# Keep only the new combined column and rename
results_df_final <- data.frame(
  Variable = results_df$Variable,
  `Correlation~(with~significance)` = results_df_clean$Correlation_and_Significance
)
# Rename column with proper spaces
colnames(results_df_final)[2] <- "Correlation (with significance)"
# Save to CSV
out_file <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/Residual_Deines_CorrSign.csv"
write.csv(results_df_final, out_file, row.names = FALSE)
# Preview
print(results_df_final)
# Select top variables based on absolute correlation
# Map stars to italic p-values
convert_stars_to_pval <- function(x) {
  if (grepl("\\*\\*\\*", x)) {
    return("<0.01")
  } else if (grepl("\\*\\*", x)) {
    return("<0.05")
  } else if (grepl("\\*", x)) {
    return("<0.1")
  } else {
    return("")
  }
}

# Prepare top variables
top_vars <- results_df_final %>%
  dplyr::mutate(
    abs_corr = abs(as.numeric(sub("\\*+", "", `Correlation (with significance)`))),
    pval_text = sapply(`Correlation (with significance)`, convert_stars_to_pval),
    corr_text = ifelse(pval_text != "",
                       paste0(sub("\\*+", "", `Correlation (with significance)`), " (p-value ", pval_text, ")"),
                       sub("\\*+", "", `Correlation (with significance)`))
  ) %>%
  dplyr::arrange(desc(abs_corr)) %>%
  dplyr::slice(1:9)

# Create sentence
summary_sentence <- paste0(
  "The PD residuals from Deines et al. 2023 were most correlated with ",
  paste0(top_vars$Variable, " (r = ", top_vars$corr_text, ")", collapse = ", "),
  ", highlighting stronger associations with phenological and temperature-related predictors."
)

cat(summary_sentence, "\n")




#========================================================
# 6. PLOT VARIABLE IMPORTANCE (HARVESTING)
#========================================================
# Prepare data for plotting Harvest variable importance
harvest_importance_combined <- harvest_var_imp_summary %>%
  rename(
    `%IncMSE` = MeanIncMSE,
    Gini = MeanIncNodePurity
  ) %>%
  mutate(
    # Simple rescaling: divide Gini by a constant to bring it to same order as %IncMSE
    Gini_scaled = Gini / 5000,  # adjust 5000 as needed
    Total_scaled = `%IncMSE` + Gini_scaled
  )

# Convert to long format for Harvest
harvest_importance_long <- harvest_importance_combined %>%
  select(variable, `%IncMSE`, `Gini_scaled`, Total_scaled) %>%
  pivot_longer(cols = c(`%IncMSE`, `Gini_scaled`), names_to = "Metric", values_to = "Value") %>%
  mutate(variable = reorder(variable, Total_scaled))
library(ggplot2)
library(dplyr)
library(tidyr)

# Original variables in your dataframe
original_vars <- c(
  "RD.RD", "Dormancy.Dormancy", "EOS_trs.eos", "EOS_deriv.eos", "cum_RH", 
  "a2", "avgsoilorg", "UD.UD", "DOY_maxROC_IAVI", "SOS_trs.sos", "cum_meansrad",
  "cum_soiltemp", "DOY_maxROC_MRBVI", "DOY_maxROC_NMDI", "DD.DD", "avgsoilclay",
  "DOY_maxROC_TGI", "SOS_deriv.sos", "cum_gdd", "cum_tmax", "cum_vpd",
  "DOY_maxROC_ExGR", "cumGDVI", "cum_tmin", "b1", "DOY_max_before_min_fit",
  "DOY_maxROC_EVI", "a1", "a3.a3", "DOY_min_fit"
)

# New names (simplified and consistent)
new_vars <- c(
  "RD", "Dormancy", "EOSTRS", "EOSDeriv", "RHcum", 
  "kNDVIa2", "SOCmean", "UD", "DOY_mxROCPoD_IAVI", "SOSTRS", "SRADcum",
  "SoilTmeancum", "DOY_mxROCPoD_MRBVI", "DOY_mxROCPoD_NMDI", "DD", "clayCmean",
  "DOY_mxROCPoD_TGI", "SOSDeriv", "GDDcum", "AirTmax_cum", "VPDcum",
  "DOY_mxROCPoD_ExGR", "GDVIcum", "AirTmincum", "kNDVIb1", "DOY_earlymin_kNDVI",
  "DOY_mxROCPoD_EVI", "kNDVIa1", "a3", "DOY_earlymax_kNDVI"
)

# Assign the new variable names
harvest_importance_combined$variable <- factor(
  harvest_importance_combined$variable,
  levels = original_vars,
  labels = new_vars
)

# Map readable labels with subscripts for plotting
variable_labels <- c(
  "SOCmean" = expression(SOC[mean]),
  "RHcum" = expression(RH[cum]),
  "kNDVImax" = expression(kNDVI[max]),
  "EOSTRS" = expression(EOS[TRS]),
  "AirTmincum" = expression(AirTmin[cum]),
  "AirTmax_cum"= expression(AirTax[cum]),
  "SOSDeriv" = expression(SOS[Deriv]),
  "SOSTRS" = expression(SOS[TRS]),
  "SoilTmeancum" = expression(SoilTmean[cum]),
  "EOSDeriv" = expression(EOS[Deriv]),
  "DD" = expression(DD),
  "VPDcum" = expression(VPD[cum]),
  "GDDcum" = expression(GDD[cum]),
  "SRADcum" = expression(Srad[cum]),
  "UD" = expression(UD)
)

# Convert to long format for plotting
harvest_importance_long <- harvest_importance_combined %>%
  select(variable, `%IncMSE`, `Gini_scaled`, Total_scaled) %>%
  pivot_longer(cols = c(`%IncMSE`, `Gini_scaled`), names_to = "Metric", values_to = "Value") %>%
  mutate(variable = reorder(variable, Total_scaled))

# Plot with subscripts
p_harvest_total_importance <- ggplot(harvest_importance_long, aes(x = Value, y = variable, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = c("%IncMSE" = "#3C5488FF", "Gini_scaled" = "#9C51B6"),
    name = "Importance Metric"
  ) +
  scale_y_discrete(labels = variable_labels) +  # Apply subscript labels
  labs(
    title = "Harvesting: Variable Importance (Combined %IncMSE and Gini)",
    x = "Total Rescaled Importance",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

print(p_harvest_total_importance)
p_harvest_total_importance
# Save the plot for Harvest
ggsave(
  filename = "harvesting_variable_importance_total_stacked_scaled.jpeg", # Changed filename
  plot = p_harvest_total_importance,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 10,
  height = 8,
  units = "in"
)

#========================================================
# 6. PLOT VARIABLE IMPORTANCE (PLANTING)
#========================================================
planting_importance_combined <- planting_var_imp_summary %>%
  rename(
    `%IncMSE` = MeanIncMSE,
    Gini = MeanIncNodePurity
  ) %>%
  mutate(
    # Simple rescaling: divide Gini by a constant to bring it to same order as %IncMSE
    Gini_scaled = Gini / 5000,  # adjust 5000 as needed
    Total_scaled = `%IncMSE` + Gini_scaled
  )


# Convert to long format for Planting
planting_importance_long <- planting_importance_combined %>%
  select(Variable, `%IncMSE`, `Gini_scaled`, Total_scaled) %>%
  pivot_longer(cols = c(`%IncMSE`, `Gini_scaled`), names_to = "Metric", values_to = "Value") %>%
  mutate(Variable = reorder(Variable, Total_scaled))



# Map original variable names to readable names
planting_importance_long <- planting_importance_long %>%
  mutate(
    Variable = recode(
      Variable,
      "avgsoilorg" = "SOCmean",
      "cum_RH" = "RHcum",
      "Value_max_obs" = "kNDVImax",
      "EOS_trs.eos" = "EOSTRS",
      "cum_tmin" = "AirTmincum",
      "SOS_deriv.sos" = "SOSDeriv",
      "SOS_trs.sos" = "SOSTRS",
      "cum_soiltemp" = "SoilTmeancum",
      "EOS_deriv.eos" = "EOSDeriv",
      "DD.DD" = "DD",
      "cum_vpd" = "VPDcum",
      "cum_gdd" = "GDDcum",
      "cum_meansrad" = "Sradcum",
      "UD.UD" = "UD"
    ),
    Variable = reorder(Variable, Total_scaled)
  )
# Map readable labels with subscripts
variable_labels <- c(
  "SOCmean" = expression(SOC[mean]),
  "RHcum" = expression(RH[cum]),
  "kNDVImax" = expression(kNDVI[max]),
  "EOSTRS" = expression(EOS[TRS]),
  "AirTmincum" = expression(AirTmin[cum]),
  "SOSDeriv" = expression(SOS[Deriv]),
  "SOSTRS" = expression(SOS[TRS]),
  "SoilTmeancum" = expression(SoilTmean[cum]),
  "EOSDeriv" = expression(EOS[Deriv]),
  "DD" = expression(DD),
  "VPDcum" = expression(VPD[cum]),
  "GDDcum" = expression(GDD[cum]),
  "Sradcum" = expression(Srad[cum]),
  "UD" = expression(UD)
)


# Plot with formatted y-axis
p_planting_total_importance <- ggplot(planting_importance_long, aes(x = Value, y = Variable, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = c("%IncMSE" = "#3C5488FF", "Gini_scaled" = "#9C51B6"),
    name = "Importance Metric"
  ) +
  scale_y_discrete(labels = variable_labels) +
  labs(
    title = "Planting: Variable Importance (Combined Standardized %IncMSE and Gini)",
    x = "Total Standardized Importance",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

print(p_planting_total_importance)
# Save the plot for Planting
ggsave(
  filename = "planting_variable_importance_total_stacked_scaled.jpeg", # Changed filename
  plot = p_planting_total_importance,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 10,
  height = 8,
  units = "in"
)

#---------------------------------------------------------------
# CORRELATION PLOT
#---------------------------------------------------------------
# Map readable labels with subscripts
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

# Use viridis palette instead
library(viridis)

# Generate the plot directly with ggplot2 and assign to a variable
p <- ggplot(data = corr_mat_long, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 5) +
  scale_fill_viridis(
    limits = c(-1, 1),
    name = "Correlation",
    option = "plasma"  # or "viridis", "magma", "inferno", "cividis"
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
p
# Save the plot to the specified directory
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/corr plot.png",
  plot = p,
  width =12,
  height = 8,
  dpi = 500
)
#--------------------------------------------------
#Climate anomalies 
#----------------------------------------------------
#-------------------------------
# 1. Load climate data
#-------------------------------
clim <- read.csv("C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Data/Arkansas_Climate_2015_2024.csv")
clim
year_signal <- clim %>%
  rename(
    year_vpd  = vpd,
    year_tmin = air_temp,
    year_RH   = rh,
    year_rad  = srad,
    year_soil = soil_temp
  ) %>%
  mutate(
    z_vpd  = scale(year_vpd)[,1],
    z_tmin = scale(year_tmin)[,1],
    z_RH   = scale(year_RH)[,1],
    z_rad  = scale(year_rad)[,1],
    z_soil = scale(year_soil)[,1]
  )
#-------------------------------
# 2. Join with phenology data
#-------------------------------
pd_yr_clim <- pd_yr %>%
  left_join(year_signal, by = "year")
hd_yr_clim <- hd_yr %>%
  left_join(year_signal, by = "year")
#-------------------------------
# 3. Prepare data function (NOW USING Z-SCORES)
#-------------------------------
prepare_data <- function(df){
  
  df_long <- df %>%
    select(
      mean_RMSE,
      z_vpd,
      z_tmin,
      z_soil
    ) %>%
    pivot_longer(
      cols = c(z_vpd, z_tmin, z_soil),
      names_to = "variable",
      values_to = "value"
    )
  
  stats <- df_long %>%
    group_by(variable) %>%
    summarise(
      cor_test = list(cor.test(value, mean_RMSE)),
      .groups = "drop"
    ) %>%
    mutate(
      r = map_dbl(cor_test, ~ .x$estimate),
      p = map_dbl(cor_test, ~ .x$p.value),
      
      # ✔ 2 significant figures for r
      label = paste0(
        "atop(italic(r)==", formatC(r, format = "fg", digits = 2),
        ", italic(p)==", signif(p, 3), ")"
      )
    )
  
  list(data = df_long, stats = stats)
}
#-------------------------------
# 4. Run analysis
#-------------------------------
pd_res <- prepare_data(pd_yr_clim)
hd_res <- prepare_data(hd_yr_clim)
pd_data  <- pd_res$data
pd_stats <- pd_res$stats
hd_data  <- hd_res$data
hd_stats <- hd_res$stats

#-------------------------------
# 5. Rename panels
#-------------------------------
rename_map <- c(
  "z_vpd"  = "VPD~mean",
  "z_tmin" = "AirT[min]~mean",
  "z_soil" = "SoilT[min]~mean"
)

pd_data$panel  <- recode(pd_data$variable, !!!rename_map)
pd_stats$panel <- recode(pd_stats$variable, !!!rename_map)

hd_data$panel  <- recode(hd_data$variable, !!!rename_map)
hd_stats$panel <- recode(hd_stats$variable, !!!rename_map)
pd_data
hd_data
#-------------------------------
# 6. Plot function
#-------------------------------
make_plot <- function(data, stats, ylab){
  
  ggplot(data, aes(x = value, y = mean_RMSE)) +
    geom_point(alpha = 0.75, size = 2.5) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    
    facet_wrap(~panel, ncol = 3, labeller = label_parsed) +
    
    labs(
      x = "Annual Climate Anomaly (z-score)",
      y = ylab
    ) +
    
    theme_classic(base_size = 16) +
    
    geom_text(
      data = stats,
      aes(x = Inf, y = Inf, label = label),
      inherit.aes = FALSE,
      hjust = 1.55,
      vjust = 1.3,
      size = 6,
      parse = TRUE
    )
}

#-------------------------------
# 7. Build plots
#-------------------------------
p1 <- make_plot(pd_data, pd_stats,
                "RMSE (PD model, L2YO) (days)")
p2 <- make_plot(hd_data, hd_stats,
                "RMSE (HD model, L2YO)(days)")
#-------------------------------
# 8. Combine
#-------------------------------
combined_plot <- (p1 / p2) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.tag   = element_text(size = 16, face = "bold")
    )
  )

print(combined_plot)
# -----------------------------
# 7. Save Combined Plot for Manuscript
# -----------------------------
out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ggsave(
  filename = file.path(out_dir, "Combined_PD_HD_LO2YO_RMSE_drivers.png"),
  plot     = combined_plot,
  width    = 10,       # Slightly wider to comfortably fit multi-panel text
  height   = 8,      # Taller canvas since we are stacking two multi-panel figures
  dpi      = 300,     # Publication quality
  bg       = "white"
)

cat("\n✓ Combined graph with A/B tags saved successfully to:", out_dir, "\n")



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


#------------------------
#OLD PLOT
#-------------------------


# p1_planting_pheno <- ggplot(plot1_df_planting_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
#   geom_col(position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
#                 position = position_dodge(width = 0.7),
#                 width = 0.2, na.rm = TRUE) +
#   geom_text(aes(label = ifelse(is.na(SD),
#                                sprintf("%.2f", Value),
#                                sprintf("%.2f ± %.2f", Value, SD))),
#             position = position_dodge(width = 0.7),
#             vjust = -0.8,
#             size = 4.5) +
#   labs(title = "LIMP Random Forest", y = "Error") +
#   scale_fill_manual(values = vg_colors) +   # ← Use Van Gogh colors
#   theme_classic(base_size = 14) +
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18)
#   )
# 
# # === Plot 2: R² and Bias for Planting ===
# p2_planting_pheno <- ggplot(plot2_df_planting_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
#   geom_col(position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
#                 position = position_dodge(width = 0.7),
#                 width = 0.2, na.rm = TRUE) +
#   geom_text(aes(label = ifelse(is.na(SD),
#                                sprintf("%.2f", Value),
#                                sprintf("%.2f ± %.2f", Value, SD))),
#             position = position_dodge(width = 0.7),
#             vjust = -0.8,
#             size = 4.5) +
#   labs(title = "LIMP Random Forest", y = "Score") +
#   scale_fill_manual(values = vg_colors) +   # ← Use Van Gogh colors
#   theme_classic(base_size = 14) +
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18)
#   )
# 
# 
# # === Plot 1: RMSE and MAE for Planting ===
# p1_planting_deines <- ggplot(plot1_df_planting_deines, aes(x = Metric, y = Value, fill = Dataset)) +
#   geom_col(position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
#                 position = position_dodge(width = 0.7),
#                 width = 0.2, na.rm = TRUE) +
#   geom_text(aes(label = ifelse(is.na(SD),
#                                sprintf("%.2f", Value),
#                                sprintf("%.2f ± %.2f", Value, SD))),
#             position = position_dodge(width = 0.7),
#             vjust = -0.8,
#             size = 4.5) +
#   labs(title = "Deines et al. 2023 features", y = "Error") +
#   scale_fill_manual(values = vg_colors) +   # ← Use Van Gogh colors
#   theme_classic(base_size = 14) +
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18)
#   )
# 
# # === Plot 2: R² and Bias for Planting ===
# p2_planting_deines <- ggplot(plot2_df_planting_deines, aes(x = Metric, y = Value, fill = Dataset)) +
#   geom_col(position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
#                 position = position_dodge(width = 0.7),
#                 width = 0.2, na.rm = TRUE) +
#   geom_text(aes(label = ifelse(is.na(SD),
#                                sprintf("%.2f", Value),
#                                sprintf("%.2f ± %.2f", Value, SD))),
#             position = position_dodge(width = 0.7),
#             vjust = -0.8,
#             size = 4.5) +
#   labs(title = "Deines et al. 2023 features", y = "Score") +
#   scale_fill_manual(values = vg_colors) +   # ← Use Van Gogh colors
#   theme_classic(base_size = 14) +
# 
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18)
#   )
# 
# 
# # Make 2x2 grid with labels
# combined_plot <- plot_grid(
# 
#   p1_planting_deines, 
#   p2_planting_deines,
#   p1_planting_pheno, 
#   p2_planting_pheno, 
#   labels = c("A", "B", "C", "D"),  # panel labels
#   label_size = 18,                 # label font size
#   ncol = 2, nrow = 2
# )
# combined_plot
# # Save to file
# out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"
# outfile <- file.path(out_dir, "RF_Prediction_Results_Grid.png")
# 
# ggsave(outfile, plot = combined_plot, width = 14, height = 10, dpi = 300)
#---------------------------------------------------------------------------
#HARVEST
#---------------------------------------------------------------------------
# # === Plot 1: RMSE and MAE for Harvest ===
# p1_harvest_deines <- ggplot(plot1_df_harvest_deines, aes(x = Metric, y = Value, fill = Dataset)) +
#   geom_col(position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
#                 position = position_dodge(width = 0.7),
#                 width = 0.2, na.rm = TRUE) +
#   geom_text(aes(label = ifelse(is.na(SD),
#                                sprintf("%.2f", Value),
#                                sprintf("%.2f ± %.2f", Value, SD))),
#             position = position_dodge(width = 0.7),
#             vjust = -0.8,
#             size = 4.5) +
#   labs(title = "Harvesting: RMSE and MAE", y = "Error", x = "Metric") +
#   scale_fill_brewer(palette = "Set2") +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18, face = "bold")
#   )
# 
# # === Plot 2: R² and Bias for Harvest ===
# p2_harvest_deines <- ggplot(plot2_df_harvest_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
#   geom_col(position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
#                 position = position_dodge(width = 0.7),
#                 width = 0.2, na.rm = TRUE) +
#   geom_text(aes(label = ifelse(is.na(SD),
#                                sprintf("%.2f", Value),
#                                sprintf("%.2f ± %.2f", Value, SD))),
#             position = position_dodge(width = 0.7),
#             vjust = -0.8,
#             size = 4.5) +
#   labs(title = "Harvesting: R² and Bias", y = "Value", x = "Metric") +
#   scale_fill_brewer(palette = "Set2") +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18, face = "bold")
#   )
# # === Plot 1: RMSE and MAE for Harvest ===
# p1_harvest_pheno <- ggplot(plot1_df_harvest_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
#   geom_col(position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
#                 position = position_dodge(width = 0.7),
#                 width = 0.2, na.rm = TRUE) +
#   geom_text(aes(label = ifelse(is.na(SD),
#                                sprintf("%.2f", Value),
#                                sprintf("%.2f ± %.2f", Value, SD))),
#             position = position_dodge(width = 0.7),
#             vjust = -0.8,
#             size = 4.5) +
#   labs(title = "LIMP Random Forest", y = "Error") +
#   scale_fill_brewer(palette = "Set2") +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18, face = "bold")
#   )
# 
# # === Plot 2: R² and Bias for Harvest ===
# p2_harvest_pheno <- ggplot(plot2_df_harvest_pheno, aes(x = Metric, y = Value, fill = Dataset)) +
#   geom_col(position = position_dodge(width = 0.7), width = 0.6) +
#   geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
#                 position = position_dodge(width = 0.7),
#                 width = 0.2, na.rm = TRUE) +
#   geom_text(aes(label = ifelse(is.na(SD),
#                                sprintf("%.2f", Value),
#                                sprintf("%.2f ± %.2f", Value, SD))),
#             position = position_dodge(width = 0.7),
#             vjust = -0.8,
#             size = 4.5) +
#   labs(title = "LIMP Random Forest", y = "Score") +
#   scale_fill_brewer(palette = "Set2") +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.title.x = element_text(size = 16),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     plot.title = element_text(size = 18, face = "bold")
#   )
# 
# # Combine harvest plots into 2x2 grid
# combined_harvest <- plot_grid(
#   p1_harvest_pheno, 
#   p2_harvest_pheno, 
#   labels = c("A", "B"),  # panel labels
#   label_size = 18,                 # label font size
#   ncol = 2, nrow = 1
# )
# combined_harvest
# # Save to file
# out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"
# outfile <- file.path(out_dir, "RF_Harvest_Results_Grid.png")
# 
# ggsave(outfile, plot = combined_harvest, width = 14, height = 6, dpi = 300)


#cor plot
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
library(ggplot2)

# Quantile breaks
pd_quantiles <- quantile(df$PDDOY, probs = c(0, 0.33, 0.66, 0.99), na.rm = TRUE)

# Create a single box for the whole range
ggplot(df, aes(x = 1, y = PDDOY)) +
  geom_boxplot(width = 0.3, fill = "lightblue", outlier.shape = NA) +  # full box
  labs(title = "Planting Date Distribution", x = "", y = "Day of Year (DOY)") +
  theme_minimal(base_size = 16) +  # increase font size
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  # Add vertical lines for 33%, 66%, 99% quantiles
  geom_hline(yintercept = pd_quantiles[2], linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = pd_quantiles[3], linetype = "dashed", color = "orange", size = 1) +
  geom_hline(yintercept = pd_quantiles[4], linetype = "dashed", color = "blue", size = 1) +
  # Add labels for the marks, pushed to the left
  annotate("text", x = 1, y = pd_quantiles[2], label = "33% (Early)", color = "red", hjust = 1, size = 6) +
  annotate("text", x = 1, y = pd_quantiles[3], label = "66% (Mid)", color = "orange", hjust = 1, size = 6) +
  annotate("text", x = 1, y = pd_quantiles[4], label = "99% (Late)", color = "blue", hjust = 1, size = 6)

