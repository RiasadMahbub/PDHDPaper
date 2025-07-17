library(dplyr)

# Helper function to compute mode safely (returns NA if multiple modes)
safe_mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  modes <- ux[tab == max(tab)]
  if(length(modes) == 1) return(modes)
  else return(NA)  # multiple modes or no mode
}

summary_stats <- combined_data %>%
  summarise(
    Mean_PD = mean(PDDOY, na.rm = TRUE),
    SD_PD = sd(PDDOY, na.rm = TRUE),
    Median_PD = median(PDDOY, na.rm = TRUE),
    Mode_PD = safe_mode(PDDOY),
    Min_PD = min(PDDOY, na.rm = TRUE),
    Max_PD = max(PDDOY, na.rm = TRUE),
    Mean_HD = mean(HDDOY, na.rm = TRUE),
    SD_HD = sd(HDDOY, na.rm = TRUE),
    Median_HD = median(HDDOY, na.rm = TRUE),
    Mode_HD = safe_mode(HDDOY),
    Min_HD = min(HDDOY, na.rm = TRUE),
    Max_HD = max(HDDOY, na.rm = TRUE)
  )

# Create mode text or skip if NA
mode_text_pd <- ifelse(is.na(summary_stats$Mode_PD), "", paste0(", mode = ", summary_stats$Mode_PD))
mode_text_hd <- ifelse(is.na(summary_stats$Mode_HD), "", paste0(", mode = ", summary_stats$Mode_HD))

# Print means, medians, modes
cat(sprintf(
  "Planting Day (PD) has a mean of %.1f ± %.1f, median of %d%s; Harvest Day (HD) has a mean of %.1f ± %.1f, median of %d%s.\n",
  summary_stats$Mean_PD, summary_stats$SD_PD, summary_stats$Median_PD, mode_text_pd,
  summary_stats$Mean_HD, summary_stats$SD_HD, summary_stats$Median_HD, mode_text_hd
))

# Print min and max values
cat(sprintf(
  "The planting days range from %d to %d, while the harvest days range from %d to %d.\n",
  summary_stats$Min_PD, summary_stats$Max_PD, summary_stats$Min_HD, summary_stats$Max_HD
))
