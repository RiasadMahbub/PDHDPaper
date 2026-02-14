
#----------------------------------------------------
#PHENOLOGY TRS 
#----------------------------------------------------
#----------------------------------------------------
#PHENOLOGY TRS 
#----------------------------------------------------
# Extract your pieces
pheno <- phenology_list[[1]]
vi <- vi_list_gt20[[1]]
# Replicate phenology row to match nrow(vi)
pheno_expanded <- pheno[rep(1, nrow(vi)), ]
# Combine them side by side
baker20_2019 <- cbind(pheno_expanded, vi)
# Inspect
head(baker20_2019)
baker20_2019$Planting_SOSTRS_Period <- baker20_2019$SOS_trs.sos - baker20_2019$PDDOY
baker20_2019$Planting_SOSDER_Period <- baker20_2019$SOS_deriv.sos - baker20_2019$PDDOY
baker20_2019$Planting_UD_Period <- baker20_2019$UD.UD - baker20_2019$PDDOY

library(ggplot2)
# Ensure unique names
colnames(baker20_2019) <- make.unique(colnames(baker20_2019))
# detect DOY and kNDVI column names (robust)
xcol <- if ("doy" %in% names(baker20_2019)) "doy" else if ("DOY" %in% names(baker20_2019)) "DOY" else stop("No DOY/doy column found")
kcol <- names(baker20_2019)[grepl("^kNDVI", names(baker20_2019))][1]
if (is.na(kcol)) stop("No kNDVI column found")

library(ggplot2)

# Ensure unique names
colnames(baker20_2019) <- make.unique(colnames(baker20_2019))
# Detect DOY and kNDVI column names (robust)
xcol <- if ("doy" %in% names(baker20_2019)) "doy" else if ("DOY" %in% names(baker20_2019)) "DOY" else stop("No DOY/doy column found")
kcol <- names(baker20_2019)[grepl("^kNDVI", names(baker20_2019))][1]
if (is.na(kcol)) kcol <- names(baker20_2019)[grepl("^kNDVI_smoothed", names(baker20_2019))][1] # Use mock name if kNDVI not found
if (is.na(kcol)) stop("No kNDVI column found")

# Extract scalar phenology values (first row)
pddoy   <- as.numeric(baker20_2019$PDDOY[1])
sos_trs <- as.numeric(baker20_2019$SOS_trs.sos[1])
sos_der <- as.numeric(baker20_2019$SOS_deriv.sos[1])
ud_val  <- as.numeric(baker20_2019$UD.UD[1])
gu_val  <- as.numeric(baker20_2019$Greenup.Greenup[1])

# --- Legend Data Frame for Vertical Lines ---
pheno_lines <- data.frame(
  DOY = c(pddoy, sos_trs, sos_der, ud_val, gu_val),
  # --- UPDATED EVENT LABELS using expression() for subscripting ---
  Event = c(
    "PD", 
    "SOS[TRS]", 
    "SOS[DER]", 
    "Upturn Date (UD)", 
    "Greenup Date"
  ),
  Color = c("orange", "deepskyblue", "red", "purple", "navy")
)
# Ensure the factor levels keep the order defined above
pheno_lines$Event <- factor(pheno_lines$Event, levels = pheno_lines$Event) 
# --- End Legend Data Frame ---

# Compute y positions for horizontal segments
kmin <- min(baker20_2019[[kcol]], na.rm = TRUE)
kmax <- max(baker20_2019[[kcol]], na.rm = TRUE)
yr <- kmax - kmin
y1 <- kmax - 0.05 * yr    # Planting -> SOSTRS
y2 <- kmax - 0.20 * yr    # Planting -> SOSDER
y3 <- kmax - 0.35 * yr    # Planting -> UD
y4 <- kmax - 0.50 * yr    # UD -> SOSTRS (not used in text labels, but kept for consistency)
y5 <- kmax - 0.65 * yr    # Planting -> Greenup

# Base plot
p <- ggplot(baker20_2019, aes_string(x = xcol, y = kcol)) +
  geom_point(color = "forestgreen", alpha = 0.6) +
  labs(
    x = "Day of Year (day)",
    y = expression(italic(k) * "NDVI (unitless)")
  ) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(0, max(baker20_2019[[xcol]], na.rm = TRUE), by = 20)) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.1), limits = c(0, 0.8)) +
  
  # --- Legend Adjustments ---
  scale_color_manual(
    name = "Phenology Events", # Legend Title (will be hidden by element_blank below)
    values = setNames(pheno_lines$Color, pheno_lines$Event),
    # Use expression() to correctly render the subscripted legend labels
    labels = expression(
      "PD", 
      italic(SOS)[italic(TRS)], 
      italic(SOS)[italic(DER)], 
      italic(UD), 
      Greenup
    )
  ) +
  theme(
    legend.position = c(0.98, 0.98), # Top right corner (0,0 is bottom-left, 1,1 is top-right)
    legend.justification = c("right", "top"),
    legend.title = element_blank(), # Remove legend title for cleaner look
    legend.background = element_rect(fill = "white", color = "black"), # Add a border/background
    legend.key.size = unit(1.5, "lines"), # Adjust size of legend keys
    legend.key.width = unit(3, "lines") # NEW: Increase width (length) of the line in the legend
  ) +
  # --- Increased line thickness in legend via size = 2 ---
  guides(color = guide_legend(override.aes = list(linetype = "dashed", size = 5))) 


# Add vertical lines for key phenology points (using new data frame and aes for legend)
if (any(!is.na(pheno_lines$DOY))) {
  p <- p + 
    geom_vline(data = na.omit(pheno_lines), 
               aes(xintercept = DOY, color = Event), 
               linetype = "dashed", 
               size = 0.7)
}


# Add horizontal segments and labels between phenology points

# --- REVISED SECTION FOR IMPROVED ITALICS AND SUBSCRIPT PARSING ---

# Planting -> SOSTRS (Duration_PD and Period are italic, SOS is now italic)
if (!is.na(pddoy) && !is.na(sos_trs)) {
  p <- p +
    annotate("segment", x = pddoy, xend = sos_trs, y = y1, yend = y1, colour = "deepskyblue", size = 1.2) +
    annotate("text", x = (pddoy + sos_trs) / 2.85, y = y1 + 0.02 * yr,
             # Label: Duration_PD_SOS[TRS] = X days
             label = paste0("italic(Duration_PD_) * italic(SOS)[italic(TRS)] * \" = \" * ", round(sos_trs - pddoy, 1), " * days"),
             colour = "deepskyblue", size = 5, hjust = 0.5, parse = TRUE)
}
# Planting -> SOSDER (Duration_PD and Period are italic, SOS is now italic)
if (!is.na(pddoy) && !is.na(sos_der)) {
  p <- p +
    annotate("segment", x = pddoy, xend = sos_der, y = y2, yend = y2, colour = "red", size = 1.2) +
    annotate("text", x = (pddoy + sos_der) / 2.85, y = y2 + 0.02 * yr,
             # Label: Duration_PD_SOS[DER] = X days
             label = paste0("italic(Duration_PD_) * italic(SOS)[italic(DER)] * \" = \" * ", round(sos_der - pddoy, 1), " * days"),
             colour = "red", size = 5, hjust = 0.5, parse = TRUE)
}

# Planting -> UD (Duration_PD_UD is italic)
if (!is.na(pddoy) && !is.na(ud_val)) {
  p <- p +
    annotate("segment", x = pddoy, xend = ud_val, y = y3, yend = y3, colour = "purple", size = 1.2) +
    annotate("text", x = (pddoy + ud_val) / 2.70, y = y3 + 0.02 * yr,
             # Label: Duration_PD_UD = X days
             label = paste0("italic(Duration_PD_UD) * \" = \" * ", round(ud_val - pddoy, 1), " * days"),
             colour = "purple", size = 5, hjust = 0.5, parse = TRUE)
}

# NEW: Planting -> Greenup (Duration_PD_Greenup is italic)
if (!is.na(pddoy) && !is.na(gu_val)) {
  p <- p +
    annotate("segment", x = pddoy, xend = gu_val, y = y5, yend = y5, colour = "navy", size = 1.2) +
    annotate("text", x = (pddoy + gu_val) / 2.75, y = y5 + 0.02 * yr,
             # Label: Duration_PD_Greenup = X days
             label = paste0("italic(Duration_PD_Greenup) * \" = \" * ", round(gu_val - pddoy, 1), " * days"),
             colour = "navy", size = 5, hjust = 0.5, parse = TRUE)
}
# Print plot
print(p)
# define file path
save_path <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/phenology_trs_lag.jpeg"
# save the plot with 300 dpi
ggsave(filename = save_path, plot = p, dpi = 300, width = 14, height = 6, units = "in")

#-----------------------------------------------------------
#GDD-VI Divergence-----------------------------------------
#-----------------------------------------------------------
# Define the custom color palette for consistency across both plots
color_palette <- c("Early" = "#0072B2", "Mid" = "#D55E00", "Late" = "#CC79A7")

# ==============================================================================
# 2. DATA MANIPULATION (Including the new cum_gdd column)
# ==============================================================================

# ==============================================================================
# 2. DATA MANIPULATION (Including the new cum_gdd and cumkNDVI columns)
# ==============================================================================

# Calculate Percentiles and assign PD_group (Now df_percentile includes cum_gdd and cumkNDVI)
df_percentile <- df %>%
  mutate(
    PD_group = cut(PDDOY,
                   breaks = quantile(PDDOY, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                   include.lowest = TRUE,
                   labels = c("Early", "Mid", "Late")),
    # Ensures factor levels are in the desired order for the box plot legend
    PD_group = factor(PD_group, levels = c("Early", "Mid", "Late"))
  )

# Sample the data (used in the original script but not strictly necessary for the main plot)
set.seed(245)
example_fields_10 <- df_percentile %>%
  group_by(PD_group) %>%
  # We need to ensure we sample enough fields for demonstration purposes
  sample_n(size = min(4, n()), replace = FALSE) %>%
  select(PD_group, Field_Year, PDDOY, cum_gdd, cumkNDVI) %>% # UPDATED: Include cumkNDVI
  arrange(PD_group, PDDOY) %>%
  ungroup()
# Combine the three sample field-years for the line plot
df_plot <- bind_rows(
  merged_list$Way_03_2024 %>% mutate(Field_Year = "F_20578_65_Wg_N_2023"),
  merged_list$Shanes_2015 %>% mutate(Field_Year = "Clark13_2021"),
  merged_list$Kelly_02_2019  %>% mutate(Field_Year = "Haley_2017")
)
# Create a mapping from Field_Year to PD_group names
df_plot <- df_plot %>%
  mutate(PD_group_label = case_when(
    Field_Year == "F_20578_65_Wg_N_2023" ~ "Early",
    Field_Year == "Clark13_2021" ~ "Mid",
    Field_Year == "Haley_2017" ~ "Late",
    TRUE ~ Field_Year  # keep original if not in mapping
  )) %>%
  # Explicitly convert to factor with desired order for guaranteed legend order
  mutate(PD_group_label = factor(PD_group_label, levels = c("Early", "Mid", "Late")))

kndvi_annotation_df <- tibble(
  PD_group_label = factor(c("Early", "Mid", "Late"), levels = c("Early", "Mid", "Late")),
  DOY = 163,
  kNDVI = c(0.82, 0.79, 0.76), # Requested Y coordinates
  # Mocked cumkNDVI values to display (these would be the actual values for the 3 fields)
  cumkNDVI = c(45.2, 30.1, 15.5)
) %>%
  # NEW: Create a plotmath compatible label string to italicize 'k'
  mutate(
    annotation_label = paste0("'Cumulative'~italic(k)*'NDVI: '~", round(cumkNDVI, 1))
  )

g_main <- ggplot(df_plot, aes(x = DOY, y = kNDVI, color = PD_group_label)) +
  # Shaded region for DOY 163–190
  annotate("rect",
           xmin = 163, xmax = 190,
           ymin = -Inf, ymax = Inf,
           alpha = 0.8, fill = "gray80") +
  geom_line(size = 1.8) +
  # UPDATED: Use annotation_label and parse=TRUE to render italic 'k'
  geom_text(
    data = kndvi_annotation_df,
    aes(
      label = annotation_label, # Use the pre-calculated plotmath label
      color = PD_group_label # Apply group color for clarity
    ),
    hjust = -0.1, # Push text slightly to the right of DOY 163
    vjust = 0.5,
    size = 4,
    fontface = "bold",
    show.legend = FALSE, # Hide legend for these labels
    parse = TRUE # Instructs ggplot to interpret the label string as a math/plotmath expression
  ) +
  scale_x_continuous(breaks = seq(0, max(df_plot$DOY, na.rm = TRUE), by = 20)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  # APPLY NEW COLORS TO LINES
  scale_color_manual(values = color_palette) +
  labs(
    title = expression(italic(k) * "NDVI Seasonal Curves for Early, Mid, and Late Planting Years"),
    subtitle = "Shaded region = DOY 163–190",
    x = "Day of Year (Day)",
    # This axis title already has the correct italic 'k'
    y = expression(italic(k)*"NDVI (unitless)"),
    color = "Planting Date Class"
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(size = 18),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 18),
    # Legend position remains top right as requested previously
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top") # Anchor the legend to the top-right corner
  )

# Prepare annotation data for the box plot (median cum_gdd per group)
gdd_annotation_df <- df_percentile %>%
  group_by(PD_group) %>%
  summarise(
    # Calculate the median GDD (the value the user wants to annotate)
    median_cum_gdd = median(cum_gdd, na.rm = TRUE),
    # Calculate the upper hinge (75th percentile) and add a small offset for label placement
    y_position = quantile(cum_gdd, 0.75, na.rm = TRUE) + (sd(cum_gdd, na.rm = TRUE) / 10)
  )

g_inset <- ggplot(df_percentile, aes(x = PD_group, y = cum_gdd, fill = PD_group)) +
  geom_boxplot(width = 0.6, alpha = 0.8) +
  # NEW: Add annotation for median cum_gdd value on top of the box
  geom_text(
    data = gdd_annotation_df,
    aes(
      x = PD_group,
      y = y_position,
      label = format(round(median_cum_gdd, 0), big.mark = ",", trim = TRUE) # Format the median number
    ),
    vjust = 0.7, # Position above the box
    size = 0.38,
    fontface = "bold",
    color = "black"
  ) +
  labs(
    #title = expression(bold("Cumulative GDD Distribution")),
    y = expression("Cumulative GDD ("*degree*C*" day)"),
    x = NULL # Remove x-axis label for cleaner inset
  ) +
  # APPLY NEW COLORS TO BOX PLOT FILLS
  scale_fill_manual(values = color_palette) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text( hjust = 1, size = 10),
    axis.title.y = element_text(size = 10),
    legend.position = "none", # No legend in the inset plot
    panel.background = element_rect(fill = "gray98"), # Light background for contrast
    plot.background = element_rect(colour = "black", linewidth = 0.5) # Border for the inset
  )

# Normalized y position (0.5 to 0.8, height = 0.3)
final_plot <- ggdraw(g_main) +
  draw_plot(
    g_inset,
    x = 0.10,  # start x (left)
    y = 0.45,  # start y (bottom)
    width = 0.35,  # width of the inset
    height = 0.35 # height of the inset
  )

# Display the final plot
print(final_plot)

# --- Save Plot 2 ---
ggsave(
  filename = "GDDDIvergenceGVI.jpeg",
  plot = final_plot,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 12,
  height = 8,
  units = "in"
)

# --- Save Plot 2 ---
ggsave(
  filename = "GDDDIvergenceGVIpresentation.jpeg",
  plot = final_plot,
  path = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure",
  dpi = 300,
  width = 20,
  height = 8,
  units = "in"
)


# Load necessary libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(viridis)

way32015PD <- as.Date("2015-04-08") 
way42015PD <- as.Date("2015-04-07")
way32015HD <- as.Date("2015-08-19") 
way42015HD <- as.Date("2015-08-19")
# 2016
way32016PD <- as.Date("2016-04-23") 
way42016PD <- as.Date("2016-04-23")
way32016HD <- as.Date("2016-09-13") 
way42016HD <- as.Date("2016-09-13")
# 2017
way32017PD <- as.Date("2017-04-10")
way42017PD <- as.Date("2017-04-09")
way32017HD <- as.Date("2017-08-27")
way42017HD <- as.Date("2017-08-27")
# 2018
way32018PD <- as.Date("2018-04-30")
way42018PD <- as.Date("2018-04-30")
way32018HD <- as.Date("2018-09-15")
way42018HD <- as.Date("2018-08-31")
# 2019
way32019PD <- as.Date("2019-05-13")
way42019PD <- as.Date("2019-05-13")
way32019HD <- as.Date("2019-09-12")
way42019HD <- as.Date("2019-09-12")
# 2020
way32020PD <- as.Date("2020-04-02")
way42020PD <- as.Date("2020-04-02")
way32020HD <- as.Date("2020-08-19")
way42020HD <- as.Date("2020-08-18")
# 2021
way32021PD <- as.Date("2021-05-01")
way42021PD <- as.Date("2021-05-01") 
way32021HD <- as.Date("2021-09-28")
way42021HD <- as.Date("2021-09-27")

# 2022
way32022PD <- as.Date("2022-04-29")
way42022PD <- as.Date("2022-04-28")
way32022HD <- as.Date("2022-09-05")
way42022HD <- as.Date("2022-09-05")

# 2023
way32023PD <- as.Date("2023-04-14")
way42023PD <- as.Date("2023-04-12")
way32023HD <- as.Date("2023-08-26")
way42023HD <- as.Date("2023-08-23")

# 2024
way32024PD <- as.Date("2024-04-03")
way42024PD <- as.Date("2024-04-03")
way32024HD <- as.Date("2024-08-16") ####edited values ## mail from Bea
way42024HD <- as.Date("2024-08-16") ####edited values ## mail from Bea

# Define the file paths
path <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/VIexample/"
# Read the CSV files
USHRA_WI2019 <- read.csv(paste0(path, "USHRA_WI2019.csv"))
USHRA_WT12019 <- read.csv(paste0(path, "USHRA_WT12019.csv"))
USHRA_LSWI2019 <- read.csv(paste0(path, "USHRA_LSWI2019.csv"))
USHRA_KNDVI2022 <- read.csv(paste0(path, "USHRA_KNDVI2022.csv"))
# Convert system.time_start column to Date format
USHRA_LSWI2019$system.time_start <- mdy(USHRA_LSWI2019$system.time_start)
USHRA_WT12019$system.time_start <- mdy(USHRA_WT12019$system.time_start)
USHRA_WI2019$system.time_start <- mdy(USHRA_WI2019$system.time_start)
USHRA_KNDVI2022$system.time_start <- mdy(USHRA_KNDVI2022$system.time_start)

# Combine datasets with correct variable names
all_data <- bind_rows(
  USHRA_LSWI2019 %>% mutate(Source = "USHRA_LSWI2019"),
  USHRA_WI2019 %>% mutate(Source = "USHRA_WI2019"),
  USHRA_KNDVI2022 %>% mutate(Source = "USHRA_KNDVI2022")
)

# Reshape data from wide to long format
long_data <- all_data %>%
  pivot_longer(cols = c(SENTINEL_2, LANDSAT_7, LANDSAT_8), 
               names_to = "Sensor", 
               values_to = "Value") %>%
  drop_na(Value)  # Remove rows with NA values

# Extract band name from the 'Source' column
long_data <- long_data %>%
  mutate(Band = str_extract(Source, "(?<=USHRA_)[A-Z]+"))

# View the cleaned dataset
View(long_data)
# Convert date to month names
long_data <- long_data %>%
  mutate(Month = format(system.time_start, "%b"))  # Extract month name

# Ensure system.time_start is in Date format
long_data <- long_data %>%
  mutate(system.time_start = as.Date(system.time_start))
# Ensure system.time_start is in Date format
long_data <- long_data %>%
  mutate(system.time_start = as.Date(system.time_start))

# Filter for LSWI and WT
filtered_data <- long_data %>% dplyr::filter(Band %in% c("LSWI", "WI"))
# Define planting and harvesting dates
way42019HD <- as.Date("2019-09-12")
way42019PD <- as.Date("2019-05-13")
# Define custom colors for the sensors
sensor_colors <- c("SENTINEL_2" = "purple", 
                   "LANDSAT_7" = "orange", 
                   "LANDSAT_8" = "#21918c")  # Updated color

# Plot
ggplot(filtered_data, aes(x = system.time_start, y = Value, color = Sensor, shape = Band)) +
  geom_point(size = 4, alpha = 0.7) +  # Different shapes for LSWI & WT
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +  # Show month names
  labs(title = "LSWI and WI Values Over Time",
       x = "Month",
       y = "Band Values",
       color = "Sensor",
       shape = "Band") +
  geom_vline(xintercept = as.numeric(way42019HD), linetype = "dashed", color = "red", linewidth = 1) +  # Harvest date (purple)
  geom_vline(xintercept = as.numeric(way42019PD), linetype = "dashed", color = "blue", linewidth = 1) +  # Planting date (orange)
  annotate("text", x = way42019HD, y = max(filtered_data$Value, na.rm = TRUE), 
           label = "Harvest Date", color = "red", angle = 90, hjust = 1, vjust = 2) +
  annotate("text", x = way42019PD, y = max(filtered_data$Value, na.rm = TRUE), 
           label = "Planting Date", color = "blue", angle = 90, hjust = 1, vjust = 2) +
  scale_color_manual(values = sensor_colors) +  # Apply custom colors to the sensors
  theme_minimal() +theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 14),  # Increase x-axis text size
                          axis.text.y = element_text(size = 14),  # Increase y-axis text size
                          axis.title.x = element_text(size = 16),  # Increase x-axis title size
                          axis.title.y = element_text(size = 16),  # Increase y-axis title size
                          legend.text = element_text(size = 14),  # Increase legend text size
                          plot.title = element_text(size = 18),  # Increase plot title size
                          plot.subtitle = element_text(size = 14))  # Increase subtitle text size
# Save the plot to the specified directory
ggsave("C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/VIexample/LSWI_WT_Plot.png", width = 10, height = 4)

# Filter the data for kndvi and 2022
kndvi_data <- long_data %>%
  dplyr::filter(Band == "KNDVI" & year(system.time_start) == 2022)

# Define planting and harvesting dates for 2022
way32022PD <- as.Date("2022-04-29")
way42022PD <- as.Date("2022-04-28")
way32022HD <- as.Date("2022-09-05")
way42022HD <- as.Date("2022-09-05")

# Define custom colors for the sensors
sensor_colors <- c("SENTINEL_2" = "purple", 
                   "LANDSAT_7" = "orange", 
                   "LANDSAT_8" = "#21918c")  # Updated color

# Plot
ggplot(kndvi_data, aes(x = system.time_start, y = Value, color = Sensor, shape = Band)) +
  geom_point(size = 3, alpha = 0.7) +  # Different shapes for kndvi
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +  # Show month names
  labs(title = "kNDVI Values Over Time (2022)",
       x = "Month",
       y = "kNDVI Value",
       color = "Sensor",
       shape = "Band") +
  geom_vline(xintercept = as.numeric(way42022HD), linetype = "dashed", color = "red", linewidth = 1) +  # Way4 Harvest date (orange)
  geom_vline(xintercept = as.numeric(way42022PD), linetype = "dashed", color = "blue", linewidth = 1) +  # Way4 Planting date (orange)
  annotate("text", x = way42022HD, y = max(kndvi_data$Value, na.rm = TRUE), 
           label = "Way4 Harvest Date", color = "red", angle = 90, vjust = -0.5, hjust = 1) +
  annotate("text", x = way42022PD, y = max(kndvi_data$Value, na.rm = TRUE), 
           label = "Way4 Planting Date", color = "blue", angle = 90, vjust = -0.5, hjust = 1) +
  scale_color_manual(values = sensor_colors) +  # Apply custom colors to the sensors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability



# Filter for Landsat 7 data
landsat7_data <- kndvi_data %>%
  dplyr::filter(Sensor == "LANDSAT_7")

# Plot for Landsat 7
ggplot(landsat7_data, aes(x = system.time_start, y = Value)) +
  geom_point(size = 3, alpha = 0.7, color = "#21918c") +  # Landsat 7 color
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +  # Show month names
  labs(title = "kNDVI Values for Landsat 7 (2022)",
       x = "Month",
       y = "kNDVI Value") +
  geom_vline(xintercept = as.numeric(way32022HD), linetype = "dashed", color = "red", linewidth = 1) +  # Way3 Harvest date
  geom_vline(xintercept = as.numeric(way32022PD), linetype = "dashed", color = "blue", linewidth = 1) +  # Way3 Planting date
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 14),  # Increase x-axis text size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.x = element_text(size = 16),  # Increase x-axis title size
    axis.title.y = element_text(size = 16),  # Increase y-axis title size
    legend.text = element_text(size = 14),  # Increase legend text size
    plot.title = element_text(size = 18),  # Increase plot title size
    plot.subtitle = element_text(size = 14)  # Increase subtitle text size
  ) +
  theme(legend.position = "none")  # Remove legend if not needed
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability
  # Save the plot to the specified directory
  ggsave("C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/VIexample/kndvil7.png", width = 10, height = 4)
  

# Filter for Landsat 7 and Landsat 8 data
landsat7_8_data <- kndvi_data %>%
  dplyr::filter(Sensor %in% c("LANDSAT_7", "LANDSAT_8"))

# Plot for Landsat 7 and Landsat 8
ggplot(landsat7_8_data, aes(x = system.time_start, y = Value, color = Sensor)) +
  geom_point(size = 3, alpha = 0.7) +  # Different colors for Landsat 7 and Landsat 8
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +  # Show month names
  labs(title = "kNDVI Values for Landsat 7 and Landsat 8 (2022)",
       x = "Month",
       y = "kNDVI Value",
       color = "Sensor") +
  geom_vline(xintercept = as.numeric(way32022HD), linetype = "dashed", color = "red", linewidth = 1) +  # Way3 Harvest date
  geom_vline(xintercept = as.numeric(way32022PD), linetype = "dashed", color = "blue", linewidth = 1) +  # Way3 Planting date
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 14),  # Increase x-axis text size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.x = element_text(size = 16),  # Increase x-axis title size
    axis.title.y = element_text(size = 16),  # Increase y-axis title size
    legend.text = element_text(size = 14),  # Increase legend text size
    plot.title = element_text(size = 18),  # Increase plot title size
    plot.subtitle = element_text(size = 14)  # Increase subtitle text size
  ) +
  theme(legend.position = "none")  # Remove legend if not needed
theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability
# Save the plot to the specified directory
ggsave("C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/VIexample/kndvil78.png", width = 10, height = 4)


# Filter for Landsat 7, Landsat 8, and Sentinel 2 data
all_sensors_data <- kndvi_data %>%
  dplyr::filter(Sensor %in% c("LANDSAT_7", "LANDSAT_8", "SENTINEL_2"))
# Add a, b, and c points and annotations to the plot
ggplot(all_sensors_data, aes(x = system.time_start, y = Value, color = Sensor)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    title = "kNDVI Values for Landsat 7, Landsat 8, and Sentinel 2 (2022)",
    x = "Month",
    y = "kNDVI Value",
    color = "Sensor"
  ) +
  geom_vline(xintercept = as.numeric(way32022HD), linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = as.numeric(way32022PD), linetype = "dashed", color = "blue", linewidth = 1) +
  
  # Highlight and annotate max peak (c), left min (a), right min (b)
  geom_point(aes(x = c_date, y = c), color = "black", size = 4, shape = 17) +
  geom_text(aes(x = c_date, y = c + 0.02), label = "c (peak)", color = "black", size = 5) +
  
  geom_point(aes(x = a_date, y = a), color = "darkgreen", size = 4, shape = 15) +
  geom_text(aes(x = a_date, y = a - 0.02), label = "a (left min)", color = "darkgreen", size = 5) +
  
  geom_point(aes(x = b_date, y = b), color = "darkorange", size = 4, shape = 15) +
  geom_text(aes(x = b_date, y = b - 0.02), label = "b (right min)", color = "darkorange", size = 5) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14)
  )


# Save the plot to the specified directory
ggsave("C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/VIexample/kndvil78s2.png", width = 10, height = 4)


# Define the file path
file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmeriFluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/Way3/way3_2019.csv"
# Read the data from the CSV file
way3_2019 <- read.csv(file_path)
# Filter the required columns: date, WTD_Avg, and Lvl_m_corr_Avg
filtered_data <- way3_2019 %>% select(date, WTD_Avg, Lvl_m_Avg)
# View the filtered data
head(filtered_data)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Convert the 'date' column to Date format

# Filter to keep only values greater than -10 for 'Lvl_m_corr_Avg'
filtered_data <- filtered_data %>%
  filter(Lvl_m_Avg > -10)
# Convert the 'date' column to Date format
filtered_data$date <- mdy(filtered_data$date)
# Plot date vs. Lvl_m_Avg
ggplot(filtered_data, aes(x = date, y = Lvl_m_Avg)) +
  geom_point(color = "#3b528b") +  # Scatter plot for data points
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +  # Show month labels
  labs(title = "Lvl_m_Avg Over Time (WTD Sensor)",
       x = "Month",
       y = "Lvl_m_Avg") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 14),  # Increase x-axis text size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.x = element_text(size = 16),  # Increase x-axis title size
    axis.title.y = element_text(size = 16),  # Increase y-axis title size
    legend.text = element_text(size = 14),  # Increase legend text size
    plot.title = element_text(size = 18),  # Increase plot title size
    plot.subtitle = element_text(size = 14)  # Increase subtitle text size
  ) +
  theme(legend.position = "none")  # Remove legend if not needed

# Save the plot to the specified directory
ggsave("C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/VIexample/WTD.png", width = 10, height = 4)
# Smooth VI series
vi_smooth <- stats::filter(all_sensors_data$Value, rep(1/3, 3), sides = 2)
dates <- all_sensors_data$system.time_start

# Identify peak and corresponding date
c_idx <- which.max(vi_smooth)
c <- vi_smooth[c_idx]
c_date <- dates[c_idx]

# Left and right segments
vi_left <- vi_smooth[1:c_idx]
vi_right <- vi_smooth[c_idx:length(vi_smooth)]
date_left <- dates[1:c_idx]
date_right <- dates[c_idx:length(vi_smooth)]

# Identify left min (a) and right min (b) with their dates
a <- min(vi_left, na.rm = TRUE)
b <- min(vi_right, na.rm = TRUE)
a_date <- date_left[which.min(vi_left)]
b_date <- date_right[which.min(vi_right)]

# Plot
ggplot(all_sensors_data, aes(x = system.time_start, y = Value, color = Sensor)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    title = "kNDVI Values for Landsat 7, Landsat 8, and Sentinel 2 (2022)",
    x = "Month",
    y = "kNDVI Value",
    color = "Sensor"
  ) +
  geom_vline(xintercept = as.numeric(way32022HD), linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = as.numeric(way32022PD), linetype = "dashed", color = "blue", linewidth = 1) +
  
  # Annotated points
  geom_point(aes(x = c_date, y = c), color = "black", size = 4, shape = 17) +
  geom_text(aes(x = c_date, y = c + 0.02), label = "c (peak)", color = "black", size = 5, vjust = 0) +
  
  geom_point(aes(x = a_date, y = a), color = "darkgreen", size = 4, shape = 15) +
  geom_text(aes(x = a_date, y = a - 0.02), label = "a (left min)", color = "darkgreen", size = 5, vjust = 1) +
  
  geom_point(aes(x = b_date, y = b), color = "darkorange", size = 4, shape = 15) +
  geom_text(aes(x = b_date, y = b - 0.02), label = "b (right min)", color = "darkorange", size = 5, vjust = 1) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14)
  )



#-----------------------------------------------------------------------
# Single Phenology Plot: PhenoTrs only (with 1:1 line)
#-----------------------------------------------------------------------

# Output file
outfile <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/TRS2.jpeg"

jpeg(
  filename = outfile,
  width = 6,
  height = 5,
  units = "in",
  res = 300
)

# Plot PhenoTrs only
par(mar = c(4, 4, 2, 1))
PhenoTrs(x)

# Add 1:1 line
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "black")

# Axis labels
mtext("Day of the year (Day)", side = 1, line = 2)
mtext("kNDVI (unitless)", side = 2, line = 2)



box()
dev.off()

