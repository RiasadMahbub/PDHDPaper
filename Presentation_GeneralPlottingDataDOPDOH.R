library(ggplot2)
library(dplyr)
library(cowplot)
library(vangogh)

# ---------------------------------------------------------------------------------
# DATA PREPARATION
# ---------------------------------------------------------------------------------

# Ensure consistent factor order for all dataframes
plot1_df$Dataset <- factor(plot1_df$Dataset, levels = c("Train", "Validation", "Test"))
plot2_df$Dataset <- factor(plot2_df$Dataset, levels = c("Train", "Validation", "Test"))

plot1_df_planting_deines$Dataset <- factor(plot1_df_planting_deines$Dataset, levels = c("Train", "Validation", "Test"))
plot2_df_planting_deines$Dataset <- factor(plot2_df_planting_deines$Dataset, levels = c("Train", "Validation", "Test"))

plot1_df_planting_pheno$Dataset <- factor(plot1_df_planting_pheno$Dataset, levels = c("Train", "Validation", "Test"))
plot2_df_planting_pheno$Dataset <- factor(plot2_df_planting_pheno$Dataset, levels = c("Train", "Validation", "Test"))

# --- Filter Data for MAE Only ---
# UPDATED: Using grepl to match "MAE" even if it says "MAE (day)"
plot1_mae_df <- filter(plot1_df, grepl("MAE", Metric))
plot1_mae_deines <- filter(plot1_df_planting_deines, grepl("MAE", Metric))
plot1_mae_pheno <- filter(plot1_df_planting_pheno, grepl("MAE", Metric))

# --- Filter Data for R2 Only ---
plot2_r2_df <- filter(plot2_df, Metric == "R²")
plot2_r2_deines <- filter(plot2_df_planting_deines, Metric == "R²")
plot2_r2_pheno <- filter(plot2_df_planting_pheno, Metric == "R²")

# ---------------------------------------------------------------------------------
# STYLING & FUNCTIONS
# ---------------------------------------------------------------------------------

# Colors
vvg_colors <- vangogh_palette("SunflowersMunich", n = 5, type = "discrete")
vg_colors <- vvg_colors[c(1, 3, 5)]

# Y-axis ranges
y_mae_range <- c(0, 10) 
y_r2_range  <- c(0, 1)

r2_label <- expression(italic(R)^2~"(unitless)")

# --- Unified Plotting Function ---
make_single_metric_plot <- function(df, y_label, title, y_range, vjust_text = -1.5) {
  ggplot(df, aes(x = Dataset, y = Value, fill = Dataset)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), 
                  width = 0.2, na.rm = TRUE) +
    geom_text(aes(label = ifelse(is.na(SD), sprintf("%.2f", Value),
                                 sprintf("%.2f ± %.1f", Value, SD))),
              vjust = vjust_text, size = 6) +
    scale_fill_manual(values = vg_colors) +
    scale_y_continuous(limits = y_range) +
    labs(title = title, y = y_label, x = "") +
    theme_classic(base_size = 16) +
    theme(axis.title.y = element_text(size = 22),
          axis.text.y = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          plot.title = element_text(size = 16))
}

# ---------------------------------------------------------------------------------
# GENERATE PLOTS
# ---------------------------------------------------------------------------------

# --- Row 1: PD_SOSDER ---
# MAE
p1_mae <- make_single_metric_plot(plot1_mae_df, y_label = "MAE (days)", 
                                  title = expression(PD[SOSDER] ~ ": MAE"), 
                                  y_range = y_mae_range) +
  guides(fill = guide_legend(title = "Fold", nrow = 1)) +
  theme(legend.position = c(0.05, 1),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = alpha('white', 0.6), color = NA),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_blank(),
        axis.title.x = element_blank())

# R2
p1_r2 <- make_single_metric_plot(plot2_r2_df, y_label = r2_label,
                                 title = expression(PD[SOSDER] ~ ": " * italic(R)^2),
                                 y_range = y_r2_range,
                                 vjust_text = 2.3) + # Text inside bar for R2
  theme(legend.position = "none",
        axis.text.x  = element_blank(),
        axis.title.x = element_blank())

# --- Row 2: Deines ---
# MAE
p2_mae_deines <- make_single_metric_plot(plot1_mae_deines, y_label = "MAE (days)", 
                                         title = "Deines et al., 2023 RF: MAE", 
                                         y_range = y_mae_range) +
  theme(legend.position = "none",
        axis.text.x  = element_blank(),
        axis.title.x = element_blank())

# R2
p2_r2_deines <- make_single_metric_plot(plot2_r2_deines, y_label = r2_label,
                                        title = expression("Deines et al., 2023 RF: " * italic(R)^2),
                                        y_range = y_r2_range,
                                        vjust_text = 2.3) +
  theme(legend.position = "none",
        axis.text.x  = element_blank(),
        axis.title.x = element_blank())

# --- Row 3: LIMP ---
# MAE
p3_mae_pheno <- make_single_metric_plot(plot1_mae_pheno, y_label = "MAE (days)", 
                                        title = "LIMP RF: MAE", 
                                        y_range = y_mae_range) +
  theme(legend.position = "none") # Keeping X axis text here (bottom row)

# R2
p3_r2_pheno <- make_single_metric_plot(plot2_r2_pheno, y_label = r2_label,
                                       title = expression("LIMP RF: " * italic(R)^2),
                                       y_range = y_r2_range,
                                       vjust_text = 2.3) +
  theme(legend.position = "none") # Keeping X axis text here (bottom row)

# ---------------------------------------------------------------------------------
# COMBINE & SAVE
# ---------------------------------------------------------------------------------

combined_grid <- plot_grid(
  # p1_mae, p1_r2,
  p2_mae_deines, p2_r2_deines,
  p3_mae_pheno, p3_r2_pheno,
  # labels = c("A", "B", "C", "D", "E", "F"),
  ncol = 2, nrow = 2,
  align = "v",
  # Equal widths now since both cols are single-bar metrics
  rel_widths = c(1, 1), 
  label_size = 19,
  label_fontface = "bold",
  label_x = 0.95,
  label_y = 0.95
)

# --- Save
outfile <- file.path(out_dir, "Random_Forest_MAE_R2_6plots.png")
ggsave(outfile, plot = combined_grid, width = 12, height = 8, dpi = 200)

# --- Show
combined_grid





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
  labs( x = "Time from Planting Date to peak (kNDVImax)", y = "Count") +
  theme_classic(base_size = 15) +
  annotate(
    "text",
    x = 120,       # explicit x-coordinate
    y = 50,        # explicit y-coordinate
    label = paste0("Mean = ", round(stats$mean_PD,1), "\nSD = ", round(stats$sd_PD,1)),
    hjust = 0,
    vjust = 0,
    size = 7
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
  labs( x = "Time to peak (kNDVImax) to Harvest Date", y = "Count") +
  theme_classic(base_size = 15) +
  annotate(
    "text",
    x = 50,        # explicit x-coordinate
    y = 50,        # explicit y-coordinate
    label = paste0("Mean = ", round(stats$mean_HD,1), "\nSD = ", round(stats$sd_HD,1)),
    hjust = 0,
    vjust = 0,
    size = 7
  ) +
  theme(
    axis.title = element_text(color = "black"), 
    axis.text  = element_text(color = "black", size = 14),
    # Add major and minor grid lines
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey95")
  )

# Combine with A, B notation
pdhddoymax <- p1 + p2 
pdhddoymax
# Save the figure
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/PD_HD_MaxDays_Hist.png",
  plot = pdhddoymax,
  width = 10,
  height = 6,
  dpi = 300
)