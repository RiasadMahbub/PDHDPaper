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

# extract scalar phenology values (first row)
pddoy    <- as.numeric(baker20_2019$PDDOY[1])
sos_trs  <- as.numeric(baker20_2019$SOS_trs.sos[1])
sos_der  <- as.numeric(baker20_2019$SOS_deriv.sos[1])
ud_val   <- as.numeric(baker20_2019$UD.UD[1])

# compute y positions for horizontal segments
kmin <- min(baker20_2019[[kcol]], na.rm = TRUE)
kmax <- max(baker20_2019[[kcol]], na.rm = TRUE)
yr <- kmax - kmin
y1 <- kmax - 0.05 * yr   # Planting → SOSTRS
y2 <- kmax - 0.20 * yr   # Planting → SOSDER
y3 <- kmax - 0.35 * yr   # Planting → UD
y4 <- kmax - 0.50 * yr   # UD → SOSTRS

# Create a legend data frame
legend_df <- data.frame(
  x = rep(NA, 4),
  y = rep(NA, 4),
  label = c("Planting Date (PD)", "SOSTRS", "SOSDERIV", "UD"),
  color = c("orange", "blue", "red", "purple")
)

# start base plot
p <- ggplot(baker20_2019, aes_string(x = xcol, y = kcol)) +
  geom_point(color = "forestgreen", alpha = 0.6) +
  labs(
    x = "Day of Year (day)",
    y = "kNDVI (unitless)"
  ) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(0, max(baker20_2019[[xcol]], na.rm = TRUE), by = 20)) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.1), limits = c(0, 0.8))

# add vertical lines
if (!is.na(pddoy))   p <- p + geom_vline(xintercept = pddoy,  colour = "orange", linetype = "dashed", size = 0.7)
if (!is.na(sos_trs)) p <- p + geom_vline(xintercept = sos_trs, colour = "blue",   linetype = "dashed", size = 0.7)
if (!is.na(sos_der)) p <- p + geom_vline(xintercept = sos_der, colour = "red",    linetype = "dashed", size = 0.7)
if (!is.na(ud_val))  p <- p + geom_vline(xintercept = ud_val,   colour = "purple", linetype = "dashed", size = 0.7)

# add horizontal segments and labels between PDDOY and each SOS
if (!is.na(pddoy) && !is.na(sos_trs)) {
  p <- p +
    annotate("segment", x = pddoy, xend = sos_trs, y = y1, yend = y1, colour = "blue", size = 1.2) +
    annotate("text", x = (pddoy + sos_trs) / 2, y = y1 + 0.02 * yr,
             label = paste0("Planting→SOSTRS = ", round(sos_trs - pddoy, 1), " d"),
             colour = "blue", size = 4, hjust = 0.5)
}
if (!is.na(pddoy) && !is.na(sos_der)) {
  p <- p +
    annotate("segment", x = pddoy, xend = sos_der, y = y2, yend = y2, colour = "red", size = 1.2) +
    annotate("text", x = (pddoy + sos_der) / 2, y = y2 + 0.02 * yr,
             label = paste0("Planting→SOSDER = ", round(sos_der - pddoy, 1), " d"),
             colour = "red", size = 4, hjust = 0.5)
}
if (!is.na(pddoy) && !is.na(ud_val)) {
  p <- p +
    annotate("segment", x = pddoy, xend = ud_val, y = y3, yend = y3, colour = "purple", size = 1.2) +
    annotate("text", x = (pddoy + ud_val) / 2, y = y3 + 0.02 * yr,
             label = paste0("Planting→UD = ", round(ud_val - pddoy, 1), " d"),
             colour = "purple", size = 4, hjust = 0.5)
}

# NEW: horizontal segment between UD and SOSTRS
if (!is.na(ud_val) && !is.na(sos_trs)) {
  p <- p +
    annotate("segment", x = sos_trs, xend = ud_val, y = y4, yend = y4, colour = "darkgreen", size = 1.2) +
    annotate("text", x = (sos_trs + ud_val) / 2, y = y4 + 0.02 * yr,
             label = paste0("UD→SOSTRS = ", round(sos_trs-ud_val  , 1), " d"),
             colour = "darkgreen", size = 4, hjust = 0.5)
}

# print plot
print(p)
# define file path
save_path <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/phenology_trs_lag.jpeg"

# save the plot with 300 dpi
ggsave(filename = save_path, plot = p, dpi = 300, width = 14, height = 6, units = "in")



# compute y positions for horizontal segments (adjusted for spacing)
y1 <- kmax - 0.05 * yr   # Planting→SOSTRS
y2 <- kmax - 0.20 * yr   # Planting→SOSDER
y3 <- kmax - 0.35 * yr   # Planting→UD
y4 <- kmax - 0.50 * yr   # UD→SOSTRS
y5 <- kmax - 0.65 * yr   # UD - 50 days

# start base plot
p <- ggplot(baker20_2019, aes_string(x = xcol, y = kcol)) +
  geom_point(color = "forestgreen", alpha = 0.6) +
  labs(
    x = "Day of Year (day)",
    y = "kNDVI (unitless)"
  ) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(0, max(baker20_2019[[xcol]], na.rm = TRUE), by = 20)) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.1), limits = c(0, 0.8))

# vertical lines
if (!is.na(pddoy))   p <- p + geom_vline(xintercept = pddoy,  colour = "orange", linetype = "dashed", size = 0.7)
if (!is.na(sos_trs)) p <- p + geom_vline(xintercept = sos_trs, colour = "blue",   linetype = "dashed", size = 0.7)
if (!is.na(sos_der)) p <- p + geom_vline(xintercept = sos_der, colour = "red",    linetype = "dashed", size = 0.7)
if (!is.na(ud_val))  p <- p + geom_vline(xintercept = ud_val,   colour = "purple", linetype = "dashed", size = 0.7)

# horizontal segments & labels (text on left)
if (!is.na(pddoy) && !is.na(sos_trs)) {
  p <- p +
    annotate("segment", x = pddoy, xend = sos_trs, y = y1, yend = y1, colour = "blue", size = 1.2) +
    annotate("text", x = pddoy-70, y = y1 + 0.02 * yr,
             label = paste0("Planting_SOSTRS_Period = ", round(sos_trs - pddoy, 1), " days"),
             colour = "blue", size = 4, hjust = 0)
}
if (!is.na(pddoy) && !is.na(sos_der)) {
  p <- p +
    annotate("segment", x = pddoy, xend = sos_der, y = y2, yend = y2, colour = "red", size = 1.2) +
    annotate("text", x = pddoy-70, y = y2 + 0.02 * yr,
             label = paste0("Planting_SOSDER_Period = ", round(sos_der - pddoy, 1), " days"),
             colour = "red", size = 4, hjust = 0)
}
if (!is.na(pddoy) && !is.na(ud_val)) {
  p <- p +
    annotate("segment", x = pddoy, xend = ud_val, y = y3, yend = y3, colour = "purple", size = 1.2) +
    annotate("text", x = pddoy-70, y = y3 + 0.02 * yr,
             label = paste0("Planting_UD_Period = ", round(ud_val - pddoy, 1), " days"),
             colour = "purple", size = 4, hjust = 0)
}
if (!is.na(ud_val) && !is.na(sos_trs)) {
  p <- p +
    annotate("segment", x = sos_trs, xend = ud_val, y = y4, yend = y4, colour = "darkgreen", size = 1.2) +
    annotate("text", x = sos_trs-70, y = y4 + 0.02 * yr,
             label = paste0("UD_SOSTRS_days = ", round(sos_trs - ud_val, 1), " days"),
             colour = "darkgreen", size = 4, hjust = 0)
}
# UD - 50 days line
p <- p +
  annotate("segment", x = ud_val - 50, xend = ud_val, y = y5, yend = y5, colour = "brown", size = 1.2) +
  annotate("text", x = ud_val - 50 -20, y = y5 + 0.02 * yr,
           label = "UD - 50 days", colour = "brown", size = 4, hjust = 0)

# print plot
print(p)

# save figure
save_path <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/phenology_trs_lag.jpeg"
ggsave(filename = save_path, plot = p, dpi = 300, width = 14, height = 6, units = "in")


