
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
  filter(!is.na(DOY))

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


