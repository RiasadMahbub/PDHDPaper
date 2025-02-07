#### Reading excel ####
# Load the required libraries
library(readxl)
library(lubridate)
library(readxl)
library(readxl)
library(dplyr)
library(purrr)
library(writexl)
library(ggplot2)
####
#PD = Planting Date
#DOY = Day of the year
#FIELD_NAME 
#YEAR
#HD = Harvesting Date
#Source = Provider of the data
#Harvest data missing = matt_morris_data, scott matthews (seeding_rice_data)
ryan_moore_sullivan_data
unilever_data
isbell_merged_data
arva_rice_data
seeding_rice_data
matt_morris_data
unilever_data
colby_rice_data




##############
####ARVA######
##############
## FIELD_NAME = FIELD_INFO_field_id_/ FIELD_INFO_field_name_
## PD = PLANTING_date_

arva_data <- read.csv("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/Arva_Intelligence/ARF - Sustainability Quantification Report.csv")
# Clean the column names
colnames(arva_data) <- gsub("X..", "", colnames(arva_data))  # Remove "X.."
colnames(arva_data) <- gsub("\\.\\.+", "_", colnames(arva_data))  # Replace dots with underscores
colnames(arva_data) <- gsub("\\.$", "", colnames(arva_data))  # Remove trailing dot

# Print the updated column names
unique(arva_data$PLANTING_crop_)

# Filter rows where PLANTING_crop_ is "rice"
arva_rice_data <- arva_data[arva_data$PLANTING_crop_ == "rice", ]
write.csv(subset(arva_data, PLANTING_crop_ == "rice"), "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/Arva_Intelligence/arva_rice_data.csv", row.names = FALSE)

arva_rice_data <- arva_rice_data %>%
  rename(PD = PLANTING_date_,
         HD = HARVEST_harvest_date_,
         FIELD_NAME = FIELD_INFO_field_name_) %>%
  select(FIELD_NAME, PD, HD)  # Select only the relevant columns

arva_rice_data <- arva_rice_data %>%
  mutate(PD = na_if(PD, "")) %>%  # Replace empty strings with NA
  filter(!is.na(PD))  # Remove rows where PD is NA


##########################
######DWMRU################
###################
colby_data <- read_excel("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/ColbyReavis/DWMRU_planting_info.xlsx")
colby_rice_data <- subset(colby_data, Crop == "rice")
colby_rice_data <- rename(colby_rice_data, FIELD_NAME = Field)

colby_rice_data <- colby_rice_data %>%
  rename(PD = Plant_date, HD = Harvest_date) %>%
  select(FIELD_NAME, PD, HD)

##############
####MATTMORRIS#
##############
matt_morris_data <- read_excel("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/MattMorris/MattMorrisPD_9_30_2024.xlsx")
matt_morris_data <- rename(matt_morris_data, FIELD_NAME = Field)

matt_morris_data <- matt_morris_data %>%
  rename(PD = PlantingDate) %>%
  select(FIELD_NAME, PD)

##############
####Sullivan#
##############
ryan_moore_sullivan_data <- read_excel("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/RyanMoore_Sullivan/Sullivan Family Ag-2023 Rice Dates.xlsx")
ryan_moore_sullivan_data <- rename(ryan_moore_sullivan_data, FIELD_NAME = Field)

ryan_moore_sullivan_data <- ryan_moore_sullivan_data %>%
  rename(PD = `Planting Date`, HD = `Harvested Date`) %>%
  select(FIELD_NAME, PD, HD)
ryan_moore_sullivan_data
##############
####Scottmatthews#
##############
seeding_data <- read_excel("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/ScottMatthews/Seeding_2024.xlsx")
seeding_rice_data <- subset(seeding_data, `Crop Type` == "Rice")
seeding_rice_data <- rename(seeding_rice_data, FIELD_NAME = Fields)


seeding_rice_data <- seeding_rice_data %>%
  rename(PD = `First Seeded`) %>%
  select(FIELD_NAME, PD)

##############
####Unilever#
##############
###check the sheet numnber for this##
unilever_data <- read_excel("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/Unilever/AllFieldsAllYearsInfoUpdatedFeb2023.xlsx")
unilever_data <- unilever_data[unilever_data$Farm != "Isbell farm", ]

unilever_data <- unilever_data %>%
  rename(FIELD_NAME = Field, PD = `Planting dates`,
         HD = `Harvest dates`) %>%
  select(FIELD_NAME, PD, HD)

############
####Isbell###
#############
# Define the directory path
directory_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/IsbellFarm/FlyingRecords"
# List all .xlsx files in the directory
xlsx_files <- list.files(path = directory_path, pattern = "\\.xlsx$", full.names = TRUE)
# Read each .xlsx file into a named list of data frames
xlsx_data <- lapply(xlsx_files, read_excel)
# Name the list elements based on the file names (without the directory and extension)
names(xlsx_data) <- gsub(" ", "", gsub("\\.xlsx$", "", basename(xlsx_files)))
print(names(xlsx_data))

# Convert 'Plant Date' column from numeric to Date format for each sheet
# 2015 Flying Records
xlsx_data$`2015FlyingRecords`$`Plant Date` <- as.Date(
  xlsx_data$`2015FlyingRecords`$`Plant Date`, origin = "1899-12-30"
)
xlsx_data$`2015FlyingRecords`$`Plant Date`[xlsx_data$`2015FlyingRecords`$`Plant Date` < as.Date("1900-01-01")] <- NA
print(xlsx_data$`2015FlyingRecords`$`Plant Date`)


xlsx_data$`2016FlyingRecords`$`Plant Date`
# Step 1: Convert the column to character for inspection
plant_date_column <- as.character(xlsx_data$`2016FlyingRecords`$`Plant Date`)
# Step 2: Identify numeric and textual entries
valid_numeric_entries <- suppressWarnings(as.numeric(plant_date_column))
invalid_entries <- plant_date_column[is.na(valid_numeric_entries) & !is.na(plant_date_column)]
# Step 3: Print invalid entries for review (optional)
print(invalid_entries)
# Step 4: Handle invalid entries (replace them with NA for now)
plant_date_column[is.na(valid_numeric_entries)] <- NA
# Step 5: Convert valid numeric entries to dates
# Excel serial date format with origin "1900-01-01"
cleaned_plant_date <- as.Date(as.numeric(plant_date_column), origin = "1900-01-01")
# Step 6: Replace the original column with cleaned dates
xlsx_data$`2016FlyingRecords`$`Plant Date` <- cleaned_plant_date
print(xlsx_data$`2016FlyingRecords`$`Plant Date`)



# 2017 Flying Records
xlsx_data$`2017FlyingRecords`$`Plant Date` <- as.Date(
  xlsx_data$`2017FlyingRecords`$`Plant Date`, origin = "1899-12-30"
)
xlsx_data$`2017FlyingRecords`$`Plant Date`[xlsx_data$`2017FlyingRecords`$`Plant Date` < as.Date("1900-01-01")] <- NA
print(xlsx_data$`2017FlyingRecords`$`Plant Date`)
# 2018 Flying Records_2018Nov
xlsx_data$`2018FlyingRecords_2018Nov`$`Plant Date` <- as.Date(
  xlsx_data$`2018FlyingRecords_2018Nov`$`Plant Date`, origin = "1899-12-30"
)
xlsx_data$`2018FlyingRecords_2018Nov`$`Plant Date`[xlsx_data$`2018FlyingRecords_2018Nov`$`Plant Date` < as.Date("1900-01-01")] <- NA
print(xlsx_data$`2018FlyingRecords_2018Nov`$`Plant Date`)



xlsx_data$`2019FlyingRecords_12_09`$`Plant Date`
# Step 1: Convert the column to character for inspection
plant_date_column <- as.character(xlsx_data$`2019FlyingRecords_12_09`$`Plant Date`)
# Step 2: Identify numeric and textual entries
valid_numeric_entries <- suppressWarnings(as.numeric(plant_date_column))
invalid_entries <- plant_date_column[is.na(valid_numeric_entries) & !is.na(plant_date_column)]
# Step 3: Print invalid entries for review (optional)
print(invalid_entries)
# Step 4: Handle invalid entries (replace them with NA for now)
plant_date_column[is.na(valid_numeric_entries)] <- NA
# Step 5: Convert valid numeric entries to dates
# Excel serial date format with origin "1900-01-01"
cleaned_plant_date <- as.Date(as.numeric(plant_date_column), origin = "1900-01-01")
# Step 6: Replace the original column with cleaned dates
xlsx_data$`2019FlyingRecords_12_09`$`Plant Date` <- cleaned_plant_date
print(xlsx_data$`2019FlyingRecords_12_09`$`Plant Date`)


# 2020 Flying Records
xlsx_data$`2020FlyingRecords`$`Plant Date` <- as.Date(
  xlsx_data$`2020FlyingRecords`$`Plant Date`, origin = "1899-12-30"
)
xlsx_data$`2020FlyingRecords`$`Plant Date`[xlsx_data$`2020FlyingRecords`$`Plant Date` < as.Date("1900-01-01")] <- NA
print(xlsx_data$`2020FlyingRecords`$`Plant Date`)


xlsx_data$`2021FlyingRecords`$`Plant Date`
sum(is.na(xlsx_data$`2021FlyingRecords`$`Plant Date`))
# Step 1: Convert the column to character for inspection
plant_date_column <- as.character(xlsx_data$`2021FlyingRecords`$`Plant Date`)
# Step 2: Identify numeric and textual entries
valid_numeric_entries <- suppressWarnings(as.numeric(plant_date_column))
invalid_entries <- plant_date_column[is.na(valid_numeric_entries) & !is.na(plant_date_column)]
# Step 3: Handle invalid textual entries
# Replace "4//8/2021" with "4/8/2021" for proper parsing
plant_date_column <- gsub("4//8/2021", "4/8/2021", plant_date_column)
# Step 4: Convert numeric entries to dates
numeric_dates <- as.Date(as.numeric(plant_date_column), origin = "1900-01-01")
# Step 5: Parse corrected textual dates
textual_dates <- suppressWarnings(as.Date(plant_date_column, format = "%m/%d/%Y"))
# Step 6: Combine numeric and textual dates (Preserve Date Class)
cleaned_plant_date <- numeric_dates
cleaned_plant_date[is.na(numeric_dates)] <- textual_dates[is.na(numeric_dates)]
# Step 7: Replace the original column with cleaned dates
xlsx_data$`2021FlyingRecords`$`Plant Date` <- cleaned_plant_date

# Step 8: View the cleaned column
print("Cleaned Plant Date Column:")
print(xlsx_data$`2021FlyingRecords`$`Plant Date`)
sum(is.na(xlsx_data$`2021FlyingRecords`$`Plant Date`))


# 2022 Flying Records
xlsx_data$`2022FlyingRecords`$`Plant Date` <- as.Date(
  xlsx_data$`2022FlyingRecords`$`Plant Date`, origin = "1899-12-30"
)
xlsx_data$`2022FlyingRecords`$`Plant Date`[xlsx_data$`2022FlyingRecords`$`Plant Date` < as.Date("1900-01-01")] <- NA
print(xlsx_data$`2022FlyingRecords`$`Plant Date`)

# 2023 Flying Records
xlsx_data$`2023FlyingRecords`$`Plant Date` <- as.Date(
  xlsx_data$`2023FlyingRecords`$`Plant Date`, origin = "1899-12-30"
)
xlsx_data$`2023FlyingRecords`$`Plant Date`[xlsx_data$`2023FlyingRecords`$`Plant Date` < as.Date("1900-01-01")] <- NA
print(xlsx_data$`2023FlyingRecords`$`Plant Date`)


xlsx_data$`2024FlyingRecords`$`Plant Date`
# Convert the column to character for inspection
plant_date_column <- as.character(xlsx_data$`2024FlyingRecords`$`Plant Date`)
# Replace incorrect entries ("4/4/0244" -> "4/4/2024")
plant_date_column <- gsub("4/4/0244", "4/4/2024", plant_date_column)
# Remove completely invalid entries like "Way Corner"
plant_date_column[plant_date_column == "Way Corner"] <- NA
# Convert numeric entries to dates
numeric_dates <- as.Date(as.numeric(plant_date_column), origin = "1900-01-01")
# Parse textual dates
textual_dates <- suppressWarnings(as.Date(plant_date_column, format = "%m/%d/%Y"))
# Combine numeric and textual dates
cleaned_plant_date <- numeric_dates
cleaned_plant_date[is.na(numeric_dates)] <- textual_dates[is.na(numeric_dates)]
# Replace the original column with cleaned dates
xlsx_data$`2024FlyingRecords`$`Plant Date` <- cleaned_plant_date
# View the cleaned column
print("Cleaned Plant Date Column:")
print(xlsx_data$`2024FlyingRecords`$`Plant Date`)
xlsx_data$`2015FlyingRecords`$`Plant Date`
names(xlsx_data)

# Rename "Field" to "FIELD_NAME" in all sheets using lapply
xlsx_data <- lapply(xlsx_data, function(df) {
  names(df)[names(df) == "Field"] <- "FIELD_NAME"
  df
})
# Calculate the number of NA values in "Plant Date" for each list
na_counts <- sapply(xlsx_data, function(df) {
  sum(is.na(df$`Plant Date`))
})
# Print the results
names(na_counts) <- names(xlsx_data) 
print(na_counts)

# Filter and merge data frames
isbell_merged_data <- do.call(rbind, lapply(xlsx_data, function(df) {
  # Filter the data frame
  filtered_df <- df[c("FIELD_NAME", "Plant Date")] 
  # Rename the "Plant Date" column to "PD"
  colnames(filtered_df)[colnames(filtered_df) == "Plant Date"] <- "PD"
  # Return the filtered and renamed data frame
  filtered_df
}))

library(dplyr)
isbell_merged_data <- isbell_merged_data %>% filter(!is.na(PD))

View(isbell_merged_data)

# Assuming your data is loaded into a list named 'xlsx_data'

# Loop through each list element (each Excel sheet)
for (sheet_name in names(xlsx_data)) {
  # Extract the current sheet's data frame
  current_sheet <- xlsx_data[[sheet_name]] 
  
  # Print the sheet name and its 'Plant Date' column
  cat("\n", sheet_name, ":\n")
  print(current_sheet$`Plant Date`) 
}


#### Check the number of rows#######
nrow(ryan_moore_sullivan_data)+ nrow(matt_morris_data)+ 
  nrow(colby_rice_data)+ nrow(arva_rice_data)+nrow(seeding_rice_data)+
  nrow(unilever_data) + nrow(isbell_merged_data)



# Add the source column with the respective dataframe name
ryan_moore_sullivan_data$source <- "ryan_moore_sullivan_data"
unilever_data$source <- "unilever_data"
isbell_merged_data$source <- "isbell_merged_data"
arva_rice_data$source <- "arva_rice_data"
seeding_rice_data$source <- "seeding_rice_data"
matt_morris_data$source <- "matt_morris_data"
colby_rice_data$source<-"colby_rice_data"


# List of dataframes to check the format and first element of the PD column
dataframes <- list(
  ryan_moore_sullivan_data = ryan_moore_sullivan_data,
  unilever_data = unilever_data,
  isbell_merged_data = isbell_merged_data,
  arva_rice_data = arva_rice_data,
  seeding_rice_data = seeding_rice_data,
  matt_morris_data = matt_morris_data,
  colby_rice_data = colby_rice_data
)

# Check the class and the first element of the PD column in each dataframe
for (df_name in names(dataframes)) {
  df <- dataframes[[df_name]]
  cat("Dataframe:", df_name, "\n")
  cat("Class of PD:", class(df$PD), "\n")
  cat("First element of PD:", df$PD[1], "\n\n")
}

# Convert the PD column in each dataframe to Date format (yyyy-mm-dd)
ryan_moore_sullivan_data <- ryan_moore_sullivan_data %>%
  mutate(PD = as.Date(PD, origin = "1970-01-01"))

unilever_data <- unilever_data %>%
  mutate(PD = as.Date(PD, origin = "1970-01-01"))

isbell_merged_data <- isbell_merged_data %>%
  mutate(PD = as.Date(PD))

arva_rice_data <- arva_rice_data %>%
  mutate(PD = as.Date(PD))

seeding_rice_data <- seeding_rice_data %>%
  mutate(PD = as.Date(PD, origin = "1970-01-01"))

matt_morris_data <- matt_morris_data %>%
  mutate(PD = as.Date(PD, origin = "1970-01-01"))

colby_rice_data <- colby_rice_data %>%
  mutate(PD = as.Date(PD, origin = "1970-01-01"))


# List of dataframes
dataframes <- list(
  ryan_moore_sullivan_data,
  unilever_data,
  isbell_merged_data,
  arva_rice_data,
  seeding_rice_data,
  matt_morris_data,
  colby_rice_data
)

# Ensure all dataframes have the 'HD' column and standardize its type
dataframes <- lapply(dataframes, function(df) {
  if (!"HD" %in% names(df)) {
    df$HD <- NA  # Add HD column if absent
  }
  df$HD <- as.character(df$HD)  # Convert HD to character for consistency
  return(df)
})



# Combine all dataframes into one
combined_data <- bind_rows(dataframes)

# Assuming your dataframe is named 'combined_data'
combined_data <- combined_data %>%
  mutate(PDDOY = yday(PD))

View(combined_data)
# Assuming your dataframe is named 'combined_data'
ggplot(combined_data, aes(x = PD)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue", alpha = 0.5) +
  labs(title = "Histogram of Planting Dates", x = "Planting Date", y = "Count") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month")

# Specify the path where you want to save the CSV
output_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/COMBINEDEXCELSHEET/COMBINEDEXCELSHEET.csv"
# Write the combined data to CSV
write.csv(combined_data, file = output_path, row.names = FALSE)
# Print confirmation message
cat("CSV file has been written to:", output_path)


# Print the 5 lowest values of PDDOY
min_values <- head(combined_data[order(combined_data$PDDOY), ], 20)
cat("5 Lowest PDDOY values:\n")
print(min_values)

# Print the 5 highest values of PDDOY
max_values <- tail(combined_data[order(combined_data$PDDOY), ], 20)
cat("\n5 Highest PDDOY values:\n")
print(max_values)


# Find the minimum "Plant Date" for each dataframe in xlsx_data
min_plant_dates <- sapply(xlsx_data, function(df) min(df$`Plant Date`, na.rm = TRUE))
# Print the minimum "Plant Date" for each dataset
print(min_plant_dates)

# Convert numeric values to Date (assuming the reference date is 1970-01-01)
min_plant_dates_converted <- as.Date(min_plant_dates, origin = "1970-01-01")

# Print the converted minimum "Plant Date" for each dataset in yyyy-mm-dd format
print(min_plant_dates_converted)

xlsx_data$`2015FlyingRecords`$`Plant Date`

###After checking the data we removed walls11 which is 49 
combined_data_cleaned <- combined_data[-which.min(combined_data$PDDOY), ]

# Create the histogram
ggplot(combined_data_cleaned, aes(x=PDDOY)) +
  geom_histogram() +
  xlab("Day of planting") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0, 170, by = 10)) +
  # Mean line (red)
  geom_vline(aes(xintercept = mean(PDDOY, na.rm = TRUE)), col='red', size=2, linetype="solid") +
  # Mode line (blue)
  geom_vline(aes(xintercept = as.numeric(names(sort(table(PDDOY), decreasing = TRUE))[1])), col='blue', size=2, linetype="dashed") +
  # Median line (green)
  geom_vline(aes(xintercept = median(PDDOY, na.rm = TRUE)), col='green', size=2, linetype="dotted") +
  annotate("text", x = 170, y = 100, label = paste0("Number of observations= ", nrow(combined_data_cleaned)), size =6) +
  scale_color_manual(name = "Statistic", values = c("mean" = "red", "mode" = "blue", "median" = "green"), 
                     breaks = c("mean", "mode", "median"), labels = c("Mean", "Mode", "Median")) +
  theme_classic() +
  theme(text = element_text(size = 24))

# Calculate the mean of PDDOY
mean_PDDOY <- mean(combined_data_cleaned$PDDOY, na.rm = TRUE)

# Calculate the median of PDDOY
median_PDDOY <- median(combined_data_cleaned$PDDOY, na.rm = TRUE)

# Calculate the mode of PDDOY
mode_PDDOY <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_PDDOY <- mode_PDDOY(combined_data_cleaned$PDDOY)

# Print the results
cat("The mean is", round(mean_PDDOY, 2), 
    ", the median is", median_PDDOY, 
    ", the mode is", mode_PDDOY, "\n")


print(table(combined_data_cleaned$source))





##### Organize the harvest data
### Data that already have harvest data


arva_data$X..HARVEST....harvest_date..

