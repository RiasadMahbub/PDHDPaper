# --------------------------
# LOAD REQUIRED PACKAGES
# --------------------------
library(readxl)    # For reading Excel files
library(dplyr)     # For data manipulation
library(lubridate) # For date handling
library(stringr)   # For string operations
library(purrr)     # For functional programming
library(writexl)   # For writing Excel files
library(ggplot2)   # For plotting
library(sf)        # For spatial data handling

# --------------------------
# DATA DICTIONARY
# --------------------------
# PD = Planting Date
# DOY = Day of the year
# FIELD_NAME = Field identifier
# YEAR = Year of observation
# HD = Harvesting Date
# Source = Provider of the data

# --------------------------
# DATA SOURCES
# --------------------------
# 1. arva_rice_data
# 2. DWMRU_rice_data
# 3. matt_morris_data
# 4. ryan_moore_sullivan_data
# 5. unilever_data
# 6. isbell_merged_data
# 7. seeding_rice_data

# Note: Harvest data missing for matt_morris_data and scott_matthews (seeding_rice_data)

# ==========================
# ARVA DATA PROCESSING
# ==========================
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
arva_non_rice_data <- arva_data[arva_data$PLANTING_crop_ != "rice", ]
write.csv(subset(arva_data, PLANTING_crop_ == "rice"), "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/Arva_Intelligence/arva_rice_data.csv", row.names = FALSE)

#########Connccatenate FIELD_INFO_field_id_, FIELD_NAME
arva_rice_data <- arva_rice_data %>%
  # Concatenate the 'FIELD_INFO_field_id_' and 'FIELD_INFO_field_name_' columns
  mutate(FIELD_NAME = paste('F',FIELD_INFO_field_id_, FIELD_INFO_field_name_, sep = "_")) %>%
  rename(PD = PLANTING_date_, # Rename other columns
         HD = HARVEST_harvest_date_,
         Variety=PLANTING_variety_) %>%
  select(FIELD_NAME, PD, HD, Variety)  # Select only the relevant columns

arva_rice_data <- arva_rice_data %>%
  mutate(PD = na_if(PD, "")) %>%  # Replace empty strings with NA
  dplyr::filter(!is.na(PD))  # Remove rows where PD is NA

# Standardize field names
arva_rice_data <- arva_rice_data %>%
  mutate(FIELD_NAME = FIELD_NAME %>%
           gsub(" - ", "_", .) %>%
           gsub("#", "_", .) %>%
           gsub("Reservoir", "Res", .) %>%
           gsub("South", "S", .) %>%
           gsub("North", "N", .) %>%
           gsub("East", "E", .) %>%
           gsub("Airstrip", "AP", .) %>%
           gsub("Main Field", "MF", .) %>%
           gsub("Willingham", "Wg", .) %>%
           gsub("Person", "Pn", .) %>%
           gsub("Martin Place", "MP", .) %>%
           gsub("Black Shop", "BS", .) %>%
           gsub("Smith Half Moon", "SHM", .) %>%
           gsub("Smith", "Sm", .) %>%
           gsub("Hemp Field", "HF", .) %>%
           gsub("West", "W", .) %>%
           gsub("Birdoe", "BD", .) %>%
           gsub("Carter", "Cr", .) %>%  # Replace "Carter" with "Cr"
           gsub(" ", "_", .) %>%  # Replace spaces with "_"
           gsub("__", "_", .))  # Replace double underscores with a single "_"

# Rename columns and select relevant ones
arva_non_rice_data <- arva_non_rice_data %>%
  rename(PD = PLANTING_date_,
         HD = HARVEST_harvest_date_,
         FIELD_NAME = FIELD_INFO_field_name_,
         Variety=PLANTING_variety_) %>%
  dplyr::select(FIELD_NAME, PD, HD, Variety)  # Select only relevant columns
# Replace empty strings with NA and filter out rows where PD is NA
arva_non_rice_data <- arva_non_rice_data %>%
  dplyr::mutate(PD = na_if(PD, "")) %>%  # Replace empty strings with NA
  dplyr::filter(!is.na(PD))  # Remove rows where PD is NA

# Ensure PD and HD are in Date format
arva_rice_data <- arva_rice_data %>%
  mutate(
    PD = as.Date(PD, format="%Y-%m-%d"),  # Convert PD to Date format
    HD = na_if(HD, ""),                   # Convert empty strings to NA
    HD = as.Date(HD, format="%Y-%m-%d"),  # Convert HD to Date format
    YEAR = year(PD),
    PDDOY = yday(PD),
    HDDOY = ifelse(!is.na(HD), yday(HD), NA) # Handle missing HD values
  )

arva_rice_data$source<-"ARD"
arva_rice_data_2016 <- arva_rice_data %>% dplyr::filter(YEAR == 2016) %>% select(-PD, -HD)
arva_rice_data_2017 <- arva_rice_data %>% dplyr::filter(YEAR == 2017) %>% select(-PD, -HD)
arva_rice_data_2018 <- arva_rice_data %>% dplyr::filter(YEAR == 2018) %>% select(-PD, -HD)
arva_rice_data_2019 <- arva_rice_data %>% dplyr::filter(YEAR == 2019) %>% select(-PD, -HD)
arva_rice_data_2020 <- arva_rice_data %>% dplyr::filter(YEAR == 2020) %>% select(-PD, -HD)
arva_rice_data_2021 <- arva_rice_data %>% dplyr::filter(YEAR == 2021) %>% select(-PD, -HD)
arva_rice_data_2022 <- arva_rice_data %>% dplyr::filter(YEAR == 2022) %>% select(-PD, -HD)
arva_rice_data_2023 <- arva_rice_data %>% dplyr::filter(YEAR == 2023) %>% select(-PD, -HD)
arva_rice_data_2024 <- arva_rice_data %>% dplyr::filter(YEAR == 2024) %>% select(-PD, -HD)

# ==========================
# DWMRU DATA PROCESSING
# ==========================
DWMRU_data <- read_excel("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/ColbyReavis/DWMRU_planting_info.xlsx")

DWMRU_rice_data <- subset(DWMRU_data, Crop == "rice")
DWMRU_rice_data <- rename(DWMRU_rice_data, FIELD_NAME = Field)
DWMRU_rice_data <- DWMRU_rice_data %>%
  rename(PD = Plant_date, HD = Harvest_date) %>%
  select(FIELD_NAME, PD, HD)

# Standardize field names
# Replace the values in the 'Field' column based on the given rules
DWMRU_rice_data <- DWMRU_rice_data %>%
  mutate(FIELD_NAME = case_when(
    FIELD_NAME == "Greenfield-NW19" ~ "Greenfield_NW19",
    FIELD_NAME == "NW18" ~ "NW_18",
    FIELD_NAME == "403" ~ "F_403",
    FIELD_NAME == "404" ~ "F_404",
    FIELD_NAME == "4N" ~ "F_4N",
    FIELD_NAME == "201" ~ "F_201",
    FIELD_NAME == "202" ~ "F_202",
    FIELD_NAME == "16SW" ~ "F_16SW",
    FIELD_NAME == "16NW" ~ "F_16NW",
    
    TRUE ~ as.character(FIELD_NAME)  # Keep other values unchanged
  ))

# Add YEAR column from PD, create PDDOY and HDDOY, and then drop PD and HD columns
DWMRU_rice_data <- DWMRU_rice_data %>%
  mutate(
    YEAR = year(PD),  # Extract Year from PD
    PDDOY = yday(PD),  # Day of Year for Planting Date
    HDDOY = yday(HD)   # Day of Year for Harvesting Date
  ) %>%
  select(-PD, -HD)  # Drop the original PD and HD columns

DWMRU_rice_data$source<-"DWMRU"
DWMRU_rice_data$Variety<-NA

# Filter the data for each year and create a dataframe for each year
DWMRU_2017 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2017)
DWMRU_2018 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2018)
DWMRU_2020 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2020)
DWMRU_2021 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2021)
DWMRU_2022 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2022)
DWMRU_2023 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2023)
DWMRU_2024 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2024)


# ==========================
# MATT MORRIS DATA PROCESSING
# ==========================
matt_morris_data <- read_excel("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/MattMorris/MattMorrisPD_9_30_2024.xlsx")
matt_morris_data <- rename(matt_morris_data, FIELD_NAME = Field)
matt_morris_data <- matt_morris_data %>%
  mutate(FIELD_NAME = str_trim(FIELD_NAME))

matt_morris_data <- matt_morris_data %>%
  rename(PD = PlantingDate) %>%
  select(FIELD_NAME, PD)

# Ensure PD and HD are in Date format, create HD if missing
matt_morris_data <- matt_morris_data %>%
  mutate(
    PD = as.Date(PD, format = "%Y-%m-%d"),  # Convert PD to Date format
    HD = if("HD" %in% names(.)) HD else NA, # Create HD if missing
    HD = na_if(HD, ""),                      # Convert empty strings to NA
    HD = as.Date(HD, format = "%Y-%m-%d"),   # Convert HD to Date format
    YEAR = year(PD),
    PDDOY = yday(PD),
    HDDOY = ifelse(!is.na(HD), yday(HD), NA) # Handle missing HD values
  ) %>%
  select(-PD, -HD)  # Drop PD and HD columns

matt_morris_data$source<-"MMD"
matt_morris_data$Variety<-NA
matt_morris_data_2024 <- matt_morris_data %>% dplyr::filter(YEAR == 2024) # Only 2024 data available

# ==========================
# SULLIVAN DATA PROCESSING
# ==========================
ryan_moore_sullivan_data <- read_excel("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/RyanMoore_Sullivan/Sullivan Family Ag-2023 Rice Dates.xlsx")
ryan_moore_sullivan_data <- rename(ryan_moore_sullivan_data, FIELD_NAME = Field)
ryan_moore_sullivan_data <- ryan_moore_sullivan_data %>%
  rename(PD = `Planting Date`, 
         HD = `Harvested Date`, 
         Variety = Variety) %>%
  dplyr::select(FIELD_NAME, PD, HD, Variety)


# Replace "#" with "_" and spaces with "_"
# Clean FIELD_NAME by replacing '#' and spaces, ensuring no double underscores
ryan_moore_sullivan_data <- ryan_moore_sullivan_data %>%
  mutate(FIELD_NAME = gsub("#| ", "_", FIELD_NAME),   # Replace '#' and spaces with '_'
         FIELD_NAME = gsub("_+", "_", FIELD_NAME))    # Replace multiple underscores with a single '_'


ryan_moore_sullivan_data <- ryan_moore_sullivan_data %>%
  mutate(
    PD = as.Date(PD),  # Convert PD to Date format
    HD = as.Date(HD, tz = "UTC"),  # Remove timezone, keep only date
    YEAR = year(PD),
    PDDOY = yday(PD),
    HDDOY = ifelse(!is.na(HD), yday(HD), NA)  # Compute HDDOY only if HD is not missing
  ) %>%
  select(-PD, -HD)  # Drop PD and HD columns

ryan_moore_sullivan_data$source<-"RMS"
ryan_moore_sullivan_data_2023 <- ryan_moore_sullivan_data %>%
  dplyr::filter(YEAR == 2023) ## ONLY ONE YEAR

# ==========================
# SCOTT MATTHEWS DATA PROCESSING
# ==========================
seeding_data <- read_excel("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/ScottMatthews/Seeding_2024.xlsx")
seeding_rice_data <- subset(seeding_data, `Crop Type` == "Rice") # Filter for rice crops only
seeding_rice_data <- seeding_rice_data %>%
  rename(FIELD_NAME = Fields) %>%
  mutate(FIELD_NAME = paste(Farms, FIELD_NAME, sep = "_")) # Create combined field identifier
seeding_rice_data <- seeding_rice_data %>%
  mutate(FIELD_NAME = str_trim(FIELD_NAME))
seeding_rice_data <- seeding_rice_data %>%
  rename(PD = `First Seeded`, Variety = Varieties) %>%
  select(FIELD_NAME, PD, Variety)
seeding_rice_data <- seeding_rice_data %>%
  mutate(
    PD = as.POSIXct(PD),  # Ensure PD is in POSIXct format for proper handling of date-time
    YEAR = year(PD),      # Extract the year from PD
    PDDOY = yday(PD),     # Extract the day of year from PD
    HDDOY = NA            # Set HDDOY to NA since HD is NA
  ) %>%
  select(FIELD_NAME, YEAR, PDDOY, HDDOY, Variety)  # Select the relevant columns

seeding_rice_data$source<-"SCM"
seeding_rice_data_2024 <- seeding_rice_data %>%
  dplyr::filter(YEAR == 2024)
seeding_non_rice_data <- subset(seeding_data, `Crop Type` != "Rice") # Filter for non-rice crops
seeding_non_rice_data<-seeding_non_rice_data %>%
  rename(FIELD_NAME = Fields) %>% # Create combined field identifier
  mutate(FIELD_NAME = paste(Farms, FIELD_NAME, sep = "_"))   # Clean field names

# ================================
# -------- UNILEVER---------------
# ================================
###check the sheet numnber for this##
# READ DATA FROM EXCEL (SHEET 2)
# --------------------------
unilever_data <- read_excel(
  path = "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/Unilever/AllFieldsAllYearsInfoUpdatedSep2024_lastversion.xlsx",
  sheet = 2  # Specify to read the second sheet
)

# Remove Isbell farm records from Unilever data since we have them 
# in a separate dataset (processed in the Isbell section below)
unilever_data <- unilever_data[!(unilever_data$Farm %in% c("Isbell", "Chris Isbell", "Mark Isbell")), ]
unilever_data <- unilever_data %>%
  rename(FIELD_NAME = Field, PD = `Planting dates`, HD = `Harvest dates`, Variety= `Rice cultivar`) %>%
  select(FIELD_NAME, PD, HD, Variety) %>%
  mutate(YEAR = year(PD))  # Add YEAR column after converting PD to year

unilever_data <- unilever_data %>%
  mutate(
    PDDOY = yday(as.Date(PD)),  
    HDDOY = ifelse(!is.na(HD), yday(as.Date(HD)), NA)
  ) %>%
  select(FIELD_NAME, PDDOY, HDDOY, YEAR, Variety)

# Replace the values in the 'Field' column based on the given rules
unilever_data <- unilever_data %>%
  mutate(FIELD_NAME = case_when(
    FIELD_NAME == "1" ~ "F_1_Farm_8",
    FIELD_NAME == "4 East" ~ "F_4East_Farm_8",
    FIELD_NAME == "4East" ~ "F_4East_Farm_8",
    FIELD_NAME == "6E" ~ "F_6E_Farm_8",
    FIELD_NAME == "B5" ~ "B5_Farm_5",
    FIELD_NAME == "Field 2" ~ "Field_2_Farm_8",
    FIELD_NAME == "G2" ~ "G2_Farm_5",
    FIELD_NAME == "H110" ~ "H110_Farm_3",
    FIELD_NAME == "HT1" ~ "HT1_Farm_1",
    FIELD_NAME == "K-24" ~ "K24_Farm_3",
    FIELD_NAME == "K140E" ~ "K140E_Farm_03",
    FIELD_NAME == "K140W" ~ "K140W_Farm_03",
    FIELD_NAME == "K164" ~ "K164_Farm_03",
    FIELD_NAME == "LIB2" ~ "LIB2_Farm_01",
    FIELD_NAME == "LOB" ~ "LOB_Farm_01",
    FIELD_NAME == "N4" ~ "N4_Farm_05",
    FIELD_NAME == "North 0" ~ "North_0_Farm_09",
    FIELD_NAME == "P2" ~ "P2_Farm_01",
    FIELD_NAME == "R10E" ~ "R10E_Farm_02",
    FIELD_NAME == "R10W" ~ "R10W_Farm_02",
    FIELD_NAME == "R2" ~ "R2_Farm_02",
    FIELD_NAME == "Regions 40" ~ "Regions_40_Farm_06",
    FIELD_NAME == "RS 5W" ~ "RS_5W_Farm_02",
    FIELD_NAME == "S01" ~ "S01_Farm_01",
    FIELD_NAME == "S03" ~ "S03_Farm_01",
    FIELD_NAME == "South 0" ~ "South_0_Farm_09",
    FIELD_NAME == "T11" ~ "T11",
    FIELD_NAME == "T18N" ~ "T18M_Farm_05",
    FIELD_NAME == "T19M" ~ "T19M_Farm_05",
    FIELD_NAME == "Town Square" ~ "Town_Square_Farm_06",
    TRUE ~ as.character(FIELD_NAME)  # Keep other values unchanged
  ))


unilever_data$source<-"UD" ###add the source
# Now filter year-wise
unilever_data_2018 <- unilever_data %>% dplyr::filter(YEAR == 2018)
unilever_data_2019 <- unilever_data %>% dplyr::filter(YEAR == 2019)
unilever_data_2020 <- unilever_data %>% dplyr::filter(YEAR == 2020)
unilever_data_2021 <- unilever_data %>% dplyr::filter(YEAR == 2021)
unilever_data_2022 <- unilever_data %>% dplyr::filter(YEAR == 2022)
unilever_data_2023 <- unilever_data %>% dplyr::filter(YEAR == 2023)
unilever_data_2024 <- unilever_data %>% dplyr::filter(YEAR == 2024)

############
####Isbell###
#############
# Define the directory path
directory_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/IsbellFarm/FlyingRecords"
# List all .xlsx files in the directory
xlsx_files <- list.files(path = directory_path, pattern = "\\.xlsx$", full.names = TRUE)
# Read each .xlsx file into a named list of data frames, including both "Zero Spray" and "Generation Dry" sheets
xlsx_data <- lapply(xlsx_files, function(file) {
  list(
    ZeroSpray = read_excel(file, sheet = "Zero Spray"),
    GenerationDry = read_excel(file, sheet = "Generation Dry")
  )
})
# Name the list elements based on the file names (without the directory and extension)
names(xlsx_data) <- gsub(" ", "", gsub("\\.xlsx$", "", basename(xlsx_files)))

# Convert 'Plant Date' column from numeric to Date format for each sheet
xlsx_data$`2015FlyingRecords`$ZeroSpray$`Plant Date` <- as.Date(
  xlsx_data$`2015FlyingRecords`$ZeroSpray$`Plant Date`, origin = "1899-12-31")
xlsx_data$`2015FlyingRecords`$ZeroSpray$`Plant Date`[
  xlsx_data$`2015FlyingRecords`$ZeroSpray$`Plant Date` < as.Date("1899-12-31")
] <- NA
xlsx_data$`2015FlyingRecords`$GenerationDry$`Plant Date` <- as.Date(
  xlsx_data$`2015FlyingRecords`$GenerationDry$`Plant Date`, origin = "1899-12-31")
xlsx_data$`2015FlyingRecords`$GenerationDry$`Plant Date`[
  xlsx_data$`2015FlyingRecords`$GenerationDry$`Plant Date` < as.Date("1899-12-31")
] <- NA
colnames(xlsx_data$`2015FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2015FlyingRecords`$GenerationDry) == "East Farm"] <- "Field"
# Print to check
xlsx_data$`2015FlyingRecords`$ZeroSpray$`Plant Date` <- as.POSIXct(xlsx_data$`2015FlyingRecords`$ZeroSpray$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")
xlsx_data$`2015FlyingRecords`$GenerationDry$`Plant Date` <- as.POSIXct(xlsx_data$`2015FlyingRecords`$GenerationDry$`Plant Date`, format = "%Y-%m-%d %H:%M:%S") + 1 * 24 * 60 * 60
xlsx_data$`2015FlyingRecords`$ZeroSpray$`Plant Date`
xlsx_data$`2015FlyingRecords`$GenerationDry$`Plant Date`

# Print to check
print(xlsx_data$`2015FlyingRecords`$ZeroSpray)
print(xlsx_data$`2015FlyingRecords`$GenerationDry)


#############
###2016
############
# Zero Spray sheet
plant_date_column <- as.character(xlsx_data$`2016FlyingRecords`$ZeroSpray$`Plant Date`)
valid_numeric_entries <- suppressWarnings(as.numeric(plant_date_column))
invalid_entries <- plant_date_column[is.na(valid_numeric_entries) & !is.na(plant_date_column)]
print(invalid_entries)  # Print invalid entries for review
plant_date_column[is.na(valid_numeric_entries)] <- NA
cleaned_plant_date <- as.Date(as.numeric(plant_date_column), origin = "1899-12-31")
xlsx_data$`2016FlyingRecords`$ZeroSpray$`Plant Date` <- cleaned_plant_date
print(xlsx_data$`2016FlyingRecords`$ZeroSpray$`Plant Date`)


# Rename the column in GenerationDry sheet from "Plnt Date" to "Plant Date"
colnames(xlsx_data$`2016FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2016FlyingRecords`$GenerationDry) == "Plnt Date"] <- "Plant Date"
colnames(xlsx_data$`2016FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2016FlyingRecords`$GenerationDry) == "East Farm"] <- "Field"
# Print to check
xlsx_data$`2016FlyingRecords`$ZeroSpray$`Plant Date` <- as.POSIXct(xlsx_data$`2016FlyingRecords`$ZeroSpray$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")
xlsx_data$`2016FlyingRecords`$GenerationDry$`Plant Date` <- as.POSIXct(xlsx_data$`2016FlyingRecords`$GenerationDry$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")

print(xlsx_data$`2016FlyingRecords`$ZeroSpray)
print(xlsx_data$`2016FlyingRecords`$GenerationDry)



# 2017 Flying Records
# For ZeroSpray sheet
xlsx_data$`2017FlyingRecords`$ZeroSpray$`Plant Date` <- as.Date(
  xlsx_data$`2017FlyingRecords`$ZeroSpray$`Plant Date`, origin = "1899-12-31"
)
xlsx_data$`2017FlyingRecords`$ZeroSpray$`Plant Date`[xlsx_data$`2017FlyingRecords`$ZeroSpray$`Plant Date` < as.Date("1900-01-01")] <- NA
print(xlsx_data$`2017FlyingRecords`$ZeroSpray$`Plant Date`)
# Rename the column in GenerationDry sheet from "Plnt Date" to "Plant Date"
colnames(xlsx_data$`2017FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2017FlyingRecords`$GenerationDry) == "Plnt Date"] <- "Plant Date"
colnames(xlsx_data$`2017FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2017FlyingRecords`$GenerationDry) == "East Farm"] <- "Field"
# Print to check
xlsx_data$`2017FlyingRecords`$ZeroSpray$`Plant Date` <- as.POSIXct(xlsx_data$`2017FlyingRecords`$ZeroSpray$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")+ 1 * 24 * 60 * 60
xlsx_data$`2017FlyingRecords`$GenerationDry$`Plant Date` <- as.POSIXct(xlsx_data$`2017FlyingRecords`$GenerationDry$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")
xlsx_data$`2017FlyingRecords`$ZeroSpray
xlsx_data$`2017FlyingRecords`$GenerationDry


# 2018 Flying Records_2018Nov
xlsx_data$`2018FlyingRecords_2018Nov`$ZeroSpray$`Plant Date` <- as.Date(
  xlsx_data$`2018FlyingRecords_2018Nov`$ZeroSpray$`Plant Date`, origin = "1899-12-30"
)
xlsx_data$`2018FlyingRecords_2018Nov`$ZeroSpray$`Plant Date`[xlsx_data$`2017FlyingRecords`$ZeroSpray$`Plant Date` < as.Date("1900-01-01")] <- NA
colnames(xlsx_data$`2024FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2024FlyingRecords`$GenerationDry) == "45399"] <- "Field"
print(xlsx_data$`2018FlyingRecords_2018Nov`$ZeroSpray$`Plant Date`)

# Rename the column in GenerationDry sheet from "Plnt Date" to "Plant Date"
colnames(xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry)[colnames(xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry) == "Plnt Date"] <- "Plant Date"
colnames(xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry)[colnames(xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry) == "45399"] <- "Field"
plant_date_column <- as.character(xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry$`Plant Date`)
valid_numeric_entries <- suppressWarnings(as.numeric(plant_date_column))
plant_date_column[is.na(valid_numeric_entries)] <- NA
cleaned_plant_date <- as.Date(as.numeric(plant_date_column), origin = "1899-12-30")
xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry$`Plant Date` <- cleaned_plant_date
print(xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry$`Plant Date`)
colnames(xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry)[colnames(xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry) == "East Farm"] <- "Field"

# Print to check
xlsx_data$`2018FlyingRecords_2018Nov`$ZeroSpray$`Plant Date` <- as.POSIXct(xlsx_data$`2018FlyingRecords_2018Nov`$ZeroSpray$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")+ 1 * 24 * 60 * 60
xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry$`Plant Date` <- as.POSIXct(xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")+ 1 * 24 * 60 * 60
xlsx_data$`2018FlyingRecords_2018Nov`$ZeroSpray
xlsx_data$`2018FlyingRecords_2018Nov`$GenerationDry


####################
##########2019#######
########################
plant_date_column <- as.character(xlsx_data$`2019FlyingRecords_12_09`$ZeroSpray$`Plant Date`)
valid_numeric_entries <- suppressWarnings(as.numeric(plant_date_column))
invalid_entries <- plant_date_column[is.na(valid_numeric_entries) & !is.na(plant_date_column)]
print(invalid_entries)
plant_date_column[is.na(valid_numeric_entries)] <- NA
cleaned_plant_date <- as.Date(as.numeric(plant_date_column), origin = "1900-01-01")
xlsx_data$`2019FlyingRecords_12_09`$ZeroSpray$`Plant Date` <- cleaned_plant_date
print(xlsx_data$`2019FlyingRecords_12_09`$ZeroSpray$`Plant Date`)


colnames(xlsx_data$`2019FlyingRecords_12_09`$GenerationDry)[colnames(xlsx_data$`2019FlyingRecords_12_09`$GenerationDry) == "Plnt Date"] <- "Plant Date"
colnames(xlsx_data$`2019FlyingRecords_12_09`$GenerationDry)[colnames(xlsx_data$`2019FlyingRecords_12_09`$GenerationDry) == "East Farm"] <- "Field"
plant_date_column <- as.character(xlsx_data$`2019FlyingRecords_12_09`$GenerationDry$`Plant Date`)
valid_numeric_entries <- suppressWarnings(as.numeric(plant_date_column))
plant_date_column[is.na(valid_numeric_entries)] <- NA
cleaned_plant_date <- as.Date(as.numeric(plant_date_column), origin = "1899-12-30")
xlsx_data$`2019FlyingRecords_12_09`$GenerationDry$`Plant Date` <- cleaned_plant_date
print(xlsx_data$`2019FlyingRecords_12_09`$GenerationDry)

# Convert the time format
xlsx_data$`2019FlyingRecords_12_09`$ZeroSpray$`Plant Date` <- as.POSIXct(xlsx_data$`2019FlyingRecords_12_09`$ZeroSpray$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")- 1 * 24 * 60 * 60
xlsx_data$`2019FlyingRecords_12_09`$GenerationDry$`Plant Date` <- as.POSIXct(xlsx_data$`2019FlyingRecords_12_09`$GenerationDry$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")+ 1 * 24 * 60 * 60
xlsx_data$`2019FlyingRecords_12_09`$ZeroSpray
xlsx_data$`2019FlyingRecords_12_09`$GenerationDry

####################
##### 2020 #########
####################
xlsx_data$`2020FlyingRecords`$ZeroSpray$`Plant Date` <- as.Date(
  xlsx_data$`2020FlyingRecords`$ZeroSpray$`Plant Date`, origin = "1899-12-30"
)
xlsx_data$`2020FlyingRecords`$ZeroSpray$`Plant Date`[xlsx_data$`2020FlyingRecords`$ZeroSpray$`Plant Date` < as.Date("1900-01-01")] <- NA
print(xlsx_data$`2020FlyingRecords`$ZeroSpray$`Plant Date`)
# Rename the column in GenerationDry sheet from "Plnt Date" to "Plant Date"
colnames(xlsx_data$`2020FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2020FlyingRecords`$GenerationDry) == "Plnt Date"] <- "Plant Date"
colnames(xlsx_data$`2020FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2020FlyingRecords`$GenerationDry) == "East Farm"] <- "Field"
xlsx_data$`2020FlyingRecords`$GenerationDry$`Plant Date` <- as.Date(as.numeric(xlsx_data$`2020FlyingRecords`$GenerationDry$`Plant Date`), origin = "1899-12-30")
xlsx_data$`2020FlyingRecords`$GenerationDry

# Convert the time format
xlsx_data$`2020FlyingRecords`$ZeroSpray$`Plant Date` <- as.POSIXct(xlsx_data$`2020FlyingRecords`$ZeroSpray$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")+ 1 * 24 * 60 * 60
xlsx_data$`2020FlyingRecords`$GenerationDry$`Plant Date` <- as.POSIXct(xlsx_data$`2020FlyingRecords`$GenerationDry$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")+ 1 * 24 * 60 * 60
xlsx_data$`2020FlyingRecords`$ZeroSpray
xlsx_data$`2020FlyingRecords`$GenerationDry


###################
######2021#########
###################
plant_date_column <- as.character(xlsx_data$`2021FlyingRecords`$ZeroSpray$`Plant Date`)
valid_numeric_entries <- suppressWarnings(as.numeric(plant_date_column))
invalid_entries <- plant_date_column[is.na(valid_numeric_entries) & !is.na(plant_date_column)]
print(invalid_entries)
plant_date_column <- gsub("4//8/2021", "4/8/2021", plant_date_column)
plant_date_column[is.na(valid_numeric_entries)] <- NA
numeric_dates <- as.Date(as.numeric(plant_date_column), origin = "1900-01-01")
textual_dates <- suppressWarnings(as.Date(plant_date_column, format = "%m/%d/%Y"))
cleaned_plant_date <- numeric_dates
cleaned_plant_date[is.na(numeric_dates)] <- textual_dates[is.na(numeric_dates)]
xlsx_data$`2021FlyingRecords`$ZeroSpray$`Plant Date` <- cleaned_plant_date
print(xlsx_data$`2021FlyingRecords`$ZeroSpray$`Plant Date`)

colnames(xlsx_data$`2021FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2021FlyingRecords`$GenerationDry) == "Plnt Date"] <- "Plant Date"
colnames(xlsx_data$`2021FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2021FlyingRecords`$GenerationDry) == "1"] <- "Field"

# Convert the time format
xlsx_data$`2021FlyingRecords`$ZeroSpray$`Plant Date` <- as.POSIXct(xlsx_data$`2021FlyingRecords`$ZeroSpray$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")- 1 * 24 * 60 * 60
xlsx_data$`2021FlyingRecords`$GenerationDry$`Plant Date` <- as.POSIXct(xlsx_data$`2021FlyingRecords`$GenerationDry$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")
xlsx_data$`2021FlyingRecords`$ZeroSpray
xlsx_data$`2021FlyingRecords`$GenerationDry

###################
# 2022 Flying Records
###################
xlsx_data$`2022FlyingRecords`$ZeroSpray$`Plant Date` <- as.Date(
  xlsx_data$`2022FlyingRecords`$ZeroSpray$`Plant Date`, origin = "1899-12-30"
)
xlsx_data$`2022FlyingRecords`$ZeroSpray$`Plant Date`[xlsx_data$`2022FlyingRecords`$ZeroSpray$`Plant Date` < as.Date("1900-01-01")] <- NA
print(xlsx_data$`2022FlyingRecords`$ZeroSpray$`Plant Date`)
# Rename the column in GenerationDry sheet from "Plnt Date" to "Plant Date"
colnames(xlsx_data$`2022FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2022FlyingRecords`$GenerationDry) == "Plnt Date"] <- "Plant Date"
colnames(xlsx_data$`2022FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2022FlyingRecords`$GenerationDry) == "45399"] <- "Field"
plant_date_column <- as.character(xlsx_data$`2022FlyingRecords`$GenerationDry$`Plant Date`)
valid_numeric_entries <- suppressWarnings(as.numeric(plant_date_column))
invalid_entries <- plant_date_column[is.na(valid_numeric_entries) & !is.na(plant_date_column)]
print(invalid_entries)
plant_date_column[is.na(valid_numeric_entries)] <- NA

cleaned_plant_date <- as.Date(as.numeric(plant_date_column), origin = "1899-12-30")
xlsx_data$`2022FlyingRecords`$GenerationDry$`Plant Date` <- cleaned_plant_date
colnames(xlsx_data$`2022FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2022FlyingRecords`$GenerationDry) == "Plnt Date"] <- "Plant Date"
colnames(xlsx_data$`2022FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2022FlyingRecords`$GenerationDry) == "1"] <- "Field"
print(xlsx_data$`2022FlyingRecords`$GenerationDry)

# Convert the time format
xlsx_data$`2022FlyingRecords`$ZeroSpray$`Plant Date` <- as.POSIXct(xlsx_data$`2022FlyingRecords`$ZeroSpray$`Plant Date`, format = "%Y-%m-%d %H:%M:%S") + 1 * 24 * 60 * 60
xlsx_data$`2022FlyingRecords`$GenerationDry$`Plant Date` <- as.POSIXct(xlsx_data$`2022FlyingRecords`$GenerationDry$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")+ 1 * 24 * 60 * 60
xlsx_data$`2022FlyingRecords`$ZeroSpray
xlsx_data$`2022FlyingRecords`$GenerationDry


# 2023 Flying Records
xlsx_data$`2023FlyingRecords`$ZeroSpray$`Plant Date` <- as.Date(
  xlsx_data$`2023FlyingRecords`$ZeroSpray$`Plant Date`, origin = "1899-12-30"
)
xlsx_data$`2023FlyingRecords`$ZeroSpray$`Plant Date`[xlsx_data$`2023FlyingRecords`$ZeroSpray$`Plant Date` < as.Date("1900-01-01")] <- NA
print(xlsx_data$`2023FlyingRecords`$ZeroSpray$`Plant Date`)

colnames(xlsx_data$`2023FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2023FlyingRecords`$GenerationDry) == "Plnt Date"] <- "Plant Date"
colnames(xlsx_data$`2023FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2023FlyingRecords`$GenerationDry) == "45399"] <- "Field"
xlsx_data$`2023FlyingRecords`$GenerationDry

# Convert the time format
xlsx_data$`2023FlyingRecords`$ZeroSpray$`Plant Date` <- as.POSIXct(xlsx_data$`2023FlyingRecords`$ZeroSpray$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")+ 1 * 24 * 60 * 60
xlsx_data$`2023FlyingRecords`$GenerationDry$`Plant Date` <- as.POSIXct(xlsx_data$`2023FlyingRecords`$GenerationDry$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")
xlsx_data$`2023FlyingRecords`$ZeroSpray
xlsx_data$`2023FlyingRecords`$GenerationDry


# 2024 Flying Records
plant_date_column <- as.character(xlsx_data$`2024FlyingRecords`$ZeroSpray$`Plant Date`)
plant_date_column <- gsub("4/4/0244", "4/4/2024", plant_date_column)
plant_date_column[plant_date_column == "Way Corner"] <- NA
numeric_dates <- as.Date(as.numeric(plant_date_column), origin = "1900-01-01")
textual_dates <- suppressWarnings(as.Date(plant_date_column, format = "%m/%d/%Y"))
cleaned_plant_date <- numeric_dates
cleaned_plant_date[is.na(numeric_dates)] <- textual_dates[is.na(numeric_dates)]
xlsx_data$`2024FlyingRecords`$ZeroSpray$`Plant Date` <- cleaned_plant_date
colnames(xlsx_data$`2024FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2024FlyingRecords`$GenerationDry) == "Plnt Date"] <- "Plant Date"
colnames(xlsx_data$`2024FlyingRecords`$GenerationDry)[colnames(xlsx_data$`2024FlyingRecords`$GenerationDry) == "45399"] <- "Field"
xlsx_data$`2024FlyingRecords`$GenerationDry
# Convert the time format
xlsx_data$`2024FlyingRecords`$ZeroSpray$`Plant Date` <- as.POSIXct(xlsx_data$`2024FlyingRecords`$ZeroSpray$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")- 1 * 24 * 60 * 60
xlsx_data$`2024FlyingRecords`$GenerationDry$`Plant Date` <- as.POSIXct(xlsx_data$`2024FlyingRecords`$GenerationDry$`Plant Date`, format = "%Y-%m-%d %H:%M:%S")
xlsx_data$`2024FlyingRecords`$ZeroSpray
xlsx_data$`2024FlyingRecords`$GenerationDry

# Initialize an empty list to store combined data for each year
isbellcl <- list()
# Iterate over each element in xlsx_data and process the data for each year
for (year in names(xlsx_data)) {
  # Access the data for each year
  year_data <- xlsx_data[[year]]
  # Filter only the 'Field' and 'Plant Date' columns for each dataset
  zero_spray_filtered <- year_data$ZeroSpray[, c("Field", "Plant Date")]
  generation_dry_filtered <- year_data$GenerationDry[, c("Field", "Plant Date")]
  # Bind the filtered data frames for this year
  isbellcl[[year]] <- rbind(zero_spray_filtered, generation_dry_filtered)
}

# Combine all year data frames into one by rbind
isbellcl <- do.call(rbind, isbellcl)
isbellcl$`Plant Date` <- as.Date(isbellcl$`Plant Date`) # Convert 'Plant Date' to Date if it's not already
# Filter rows where year is between 2015 and 2024 and 'Plant Date' is not NA
isbellcl <- isbellcl[!is.na(isbellcl$`Plant Date`) & #### Remove the rows having year <2015 and >2024
                                      format(isbellcl$`Plant Date`, "%Y") >= 2015 & 
                                      format(isbellcl$`Plant Date`, "%Y") <= 2024, ]

### Remove the rows where Plant Date is NA
isbellcl <- isbellcl[!is.na(isbellcl$`Plant Date`), ]
######################################
# Rename columns####################
#######################################
isbellcl <- isbellcl %>%
  rename(FIELD_NAME = Field, PD = `Plant Date`)
# Replace the values in the 'Field' column based on the given rules
isbellcl <- isbellcl %>%
  filter(!is.na(PD)) %>%
  mutate(FIELD_NAME = case_when(
    FIELD_NAME %in% c(1, 1.0, "1", "1.0", "East#1", "East #1") ~ "East_01",
    FIELD_NAME %in% c(2, 2.0, "2", "2.0", "East#2", "East #2") ~ "East_02",
    FIELD_NAME %in% c(3, 3.0, "3", "3.0", "East#3", "East #3") ~ "East_03",
    FIELD_NAME %in% c(4, 4.0, "4", "4.0", "East#4", "East #4") ~ "East_04",
    FIELD_NAME %in% c(5, 5.0, "5", "5.0", "East#5", "East #5") ~ "East_05",
    FIELD_NAME %in% c(6, 6.0, "6", "6.0", "East#6", "East #6", "East 6") ~ "East_06",
    FIELD_NAME %in% c(7, 7.0, "7", "7.0", "East#7", "East #7") ~ "East_07",
    FIELD_NAME %in% c(8, 8.0, "8", "8.0", "East#8", "East #8") ~ "East_08",
    FIELD_NAME %in% c(9, 9.0, "9", "9.0", "East#9", "East #9") ~ "East_09",
    FIELD_NAME %in% c(10, 10.0, "10", "10.0", "East#10", "East #10") ~ "East_10",
    FIELD_NAME %in% c(11, 11.0, "11", "11.0", "East#11", "East #11") ~ "East_11",
    FIELD_NAME %in% c(12, 12.0, "12", "12.0", "East#12", "East #12") ~ "East_12",
    FIELD_NAME %in% c(13, 13.0, "13", "13.0", "East#13", "East #13") ~ "East_13",
    FIELD_NAME %in% c(14, 14.0, "14", "14.0", "East#14", "East #14") ~ "East_14",
    FIELD_NAME == "Baker 20" ~ "Baker_20",
    FIELD_NAME == "Baker 30" ~ "Baker_30",
    FIELD_NAME == "Baker 40" ~ "Baker_40",
    FIELD_NAME == "Baker 50" ~ "Baker_50",
    FIELD_NAME %in% c("Bott of Res", "Bott of Res.") ~ "Bott_of_Res",
    FIELD_NAME == "Way 1" ~ "Way_01",
    FIELD_NAME == "Way 2"~ "Way_02",
    FIELD_NAME == "Way 3" ~ "Way_03",
    FIELD_NAME == "Way 4"~ "Way_04",
    FIELD_NAME == "Way 5"~ "Way_05",
    FIELD_NAME == "Kelly 1"  ~ "Kelly_01",
    FIELD_NAME == "Kelly 2"  ~ "Kelly_02",
    FIELD_NAME == "Kelly 3"  ~ "Kelly_03",
    FIELD_NAME == "Kelly 4"  ~ "Kelly_04",
    FIELD_NAME == "Kelly 5"  ~ "Kelly_05",
    FIELD_NAME == "Kelly 6"  ~ "Kelly_06",
    FIELD_NAME == "Kelly 7"  ~ "Kelly_07",
    FIELD_NAME == "Kelly 8"  ~ "Kelly_08",
    FIELD_NAME == "Kelly 9"  ~ "Kelly_09",
    FIELD_NAME == "Kelly 10" ~ "Kelly_10",
    FIELD_NAME == "Kelly 11" ~ "Kelly_11",
    # FIELD_NAME == "East #1"  ~ "East_01",
    # FIELD_NAME == "East #2"  ~ "East_02",
    # FIELD_NAME == "East #3"  ~ "East_03",
    # FIELD_NAME == "East #4"  ~ "East_04",
    # FIELD_NAME == "East #5"  ~ "East_05",
    # FIELD_NAME == "East 6"  ~ "East_06",
    # FIELD_NAME == "East #7"  ~ "East_07",
    # FIELD_NAME == "East #8"  ~ "East_08",
    # FIELD_NAME == "East #9"  ~ "East_09",
    # FIELD_NAME == "East #10" ~ "East_10",
    # FIELD_NAME == "East #11" ~ "East_11",
    # FIELD_NAME == "East #12" ~ "East_12",
    # FIELD_NAME == "East #13" ~ "East_13",
    # FIELD_NAME == "East #14" ~ "East_14",
    FIELD_NAME == "Walls 1"  ~ "Walls_01",
    FIELD_NAME == "Walls 2"  ~ "Walls_02",
    FIELD_NAME == "Walls 3"  ~ "Walls_03",
    FIELD_NAME == "Walls 4"  ~ "Walls_04",
    FIELD_NAME == "Walls 5"  ~ "Walls_05",
    FIELD_NAME == "Walls 6"  ~ "Walls_06",
    FIELD_NAME == "Walls 7"  ~ "Walls_07",
    FIELD_NAME == "Walls_6_+7"  ~ "Walls_06_07",
    FIELD_NAME == "Walls 8"  ~ "Walls_08",
    FIELD_NAME == "Walls 9"  ~ "Walls_09",
    FIELD_NAME == "Walls 10" ~ "Walls_10",
    FIELD_NAME == "Walls 11" ~ "Walls_11",
    FIELD_NAME == "Walls 12" ~ "Walls_12",
    FIELD_NAME == "Walls 13" ~ "Walls_13",
    FIELD_NAME == "Top of Res." ~ "Top_of_Res",
    FIELD_NAME == "Hudson's" ~ "Hudsons",
    # New Field Name Mappings
    FIELD_NAME == "Bransford Est" ~ "Bransford_Est",
    FIELD_NAME == "Carr North" ~ "Carr_North",
    FIELD_NAME == "Carr South" ~ "Carr_South",
    FIELD_NAME == "Cattlet 1" ~ "Cattlet_01",
    FIELD_NAME == "Cattlet 2" ~ "Cattlet_02",
    FIELD_NAME == "Cotton Patch" ~ "Cotton_Patch",
    FIELD_NAME == "East Harvey" ~ "East_Harvey",
    FIELD_NAME == "East Joe T" ~ "East_Joe_T",
    FIELD_NAME == "Experiment" ~ "Experiment",
    FIELD_NAME == "Flat" ~ "Flat",
    FIELD_NAME == "Frog 40" ~ "Frog_40",
    FIELD_NAME %in% c("Frog 40 NW", "Frog 40  NW") ~ "Frog_40_NW",
    FIELD_NAME == "Haley" ~ "Haley",
    FIELD_NAME == "Hole 40" ~ "Hole_40",
    FIELD_NAME %in% c("Hole 40 NE", "Hole 40  NE") ~ "Hole_40_NE",
    FIELD_NAME == "HQ Shop" ~ "HQ_Shop",
    FIELD_NAME == "Hudsons" ~ "Hudsons",
    FIELD_NAME == "Hwy Shop 40" ~ "Hwy_Shop_40",
    FIELD_NAME == "Hwy Shop 40 SE" ~ "Hwy_Shop_40_SE",
    FIELD_NAME == "Judys" ~ "Judys",
    FIELD_NAME == "Lentz" ~ "Lentz",
    FIELD_NAME == "Lost 40" ~ "Lost_40",
    FIELD_NAME == "Lyntz" ~ "Lyntz",
    FIELD_NAME == "Md 40 West" ~ "Md_40_West",
    FIELD_NAME == "Md 40 West MidW" ~ "Md_40_West_MidW",
    FIELD_NAME == "Mid 40 Hwy" ~ "Mid_40_Hwy",
    FIELD_NAME == "Mid 40 Hwy MidE" ~ "Mid_40_Hwy_MidE",
    FIELD_NAME == "Mid Bransford" ~ "Mid_Bransford",
    FIELD_NAME == "Morris" ~ "Morris",
    FIELD_NAME == "New Ground" ~ "New_Ground",
    FIELD_NAME == "North Cotton" ~ "North_Cotton",
    FIELD_NAME == "Pops" ~ "Pops",
    FIELD_NAME == "Seed Rice" ~ "Seed_Rice",
    FIELD_NAME == "Shanes" ~ "Shanes",
    FIELD_NAME == "SI Shop" ~ "SI_Shop",
    FIELD_NAME == "The 90" ~ "The_90",
    FIELD_NAME == "Walls 6 +7" ~ "Walls_06_07",
    FIELD_NAME == "West Harvey" ~ "West_Harvey",
    FIELD_NAME == "West Joe T" ~ "West_Joe_T",
    FIELD_NAME == "Wst Bransford" ~ "Wst_Bransford",
    FIELD_NAME == "Wst Shop 40" ~ "Wst_Shop_40",
    FIELD_NAME == "Wst Shop 40 SW" ~ "Wst_Shop_40_SW",
    #FIELD_NAME %in% c("Lentz") ~ "Lyntz",  # Added Lentz to Lyntz
    TRUE ~ as.character(FIELD_NAME)  # Keep other values unchanged
  ))

isbellcl$YEAR <- format(as.Date(isbellcl$PD), "%Y")
isbellcl$FIELDNAME_YEAR <- paste(isbellcl$FIELD_NAME, isbellcl$YEAR, sep = "_") ## to merge with HD# Create the new column by concatenating FIELD_NAME and YEAR
HDmerged_data <- HDmerged_data %>% select(HD, FIELDNAME_YEAR) # Keep only the 'HD' and 'FIELDNAME_YEAR' columns in HDmerged_data
isbellcl <- full_join(isbellcl, HDmerged_data, by = "FIELDNAME_YEAR")
isbellcl_inner <- inner_join(isbellcl, HDmerged_data, by = "FIELDNAME_YEAR")
isbellcl_not_in_HD <- anti_join(isbellcl, HDmerged_data, by = "FIELDNAME_YEAR")
HD_not_in_isbellcl <- anti_join(HDmerged_data, isbellcl, by = "FIELDNAME_YEAR")
sort(unique(isbellcl_not_in_HD$FIELD_NAME))
sort(unique(HD_not_in_isbellcl$FIELD_NAME))

isbellcl <- isbellcl %>% mutate(PDDOY = yday(PD), HDDOY = yday(HD)) %>% select(FIELD_NAME, PDDOY, HDDOY, YEAR)

isbellcl$source<-"Isbell" ###add the source
# Filter data for each year
isbell_2015 <- filter(isbellcl, YEAR == "2015")
isbell_2016 <- filter(isbellcl, YEAR == "2016")
isbell_2017 <- filter(isbellcl, YEAR == "2017")
isbell_2018 <- filter(isbellcl, YEAR == "2018")
isbell_2019 <- filter(isbellcl, YEAR == "2019")
isbell_2020 <- filter(isbellcl, YEAR == "2020")
isbell_2021 <- filter(isbellcl, YEAR == "2021")
isbell_2022 <- filter(isbellcl, YEAR == "2022")
isbell_2023 <- filter(isbellcl, YEAR == "2023")
isbell_2024 <- filter(isbellcl, YEAR == "2024")

sort(unique(isbellcl$FIELD_NAME))

#### Check the number of rows#######
nrow(ryan_moore_sullivan_data)+ nrow(matt_morris_data)+ 
  nrow(DWMRU_rice_data)+ nrow(arva_rice_data)+nrow(seeding_rice_data)+
  nrow(unilever_data) + nrow(isbellcl)



# Add the source column with the respective dataframe name
ryan_moore_sullivan_data$source <- "ryan_moore_sullivan_data"
unilever_data$source <- "unilever_data"
isbellcl$source <- "isbellcl"
arva_rice_data$source <- "arva_rice_data"
seeding_rice_data$source <- "seeding_rice_data"
matt_morris_data$source <- "matt_morris_data"
DWMRU_rice_data$source<-"DWMRU_rice_data"


# List of dataframes to check the format and first element of the PD column
dataframes <- list(
  ryan_moore_sullivan_data = ryan_moore_sullivan_data,
  unilever_data = unilever_data,
  isbellcl = isbellcl,
  arva_rice_data = arva_rice_data,
  seeding_rice_data = seeding_rice_data,
  matt_morris_data = matt_morris_data,
  DWMRU_rice_data = DWMRU_rice_data
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

isbellcl <- isbellcl %>%
  mutate(PD = as.Date(PD))

arva_rice_data <- arva_rice_data %>%
  mutate(PD = as.Date(PD))

seeding_rice_data <- seeding_rice_data %>%
  mutate(PD = as.Date(PD, origin = "1970-01-01"))

matt_morris_data <- matt_morris_data %>%
  mutate(PD = as.Date(PD, origin = "1970-01-01"))

DWMRU_rice_data <- DWMRU_rice_data %>%
  mutate(PD = as.Date(PD, origin = "1970-01-01"))


# List of dataframes
dataframes <- list(
  ryan_moore_sullivan_data,
  unilever_data,
  isbellcl,
  arva_rice_data,
  seeding_rice_data,
  matt_morris_data,
  DWMRU_rice_data
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










##################SHP FIles#############

# Define the file path
shapefile_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/Arvafieldboundaries.shp"
# Read the shapefile
arvashp <- st_read(shapefile_path)
arvashp$field_id
arvashp$field_name
# Concatenate the 'field_id' and 'field_name' columns to create the 'FIELD_NAME' column
arvashp$FIELD_NAME <- paste(arvashp$field_id, arvashp$field_name, sep = "_")
# Use dplyr's mutate and gsub to edit the FIELD_NAME column
arvashp <- arvashp %>%
  mutate(FIELD_NAME = FIELD_NAME %>%
           gsub(" - ", "_", .) %>%
           gsub("#", "_", .) %>%
           gsub("Reservoir", "Res", .) %>%
           gsub("South", "S", .) %>%
           gsub("North", "N", .) %>%
           gsub("East", "E", .) %>%
           gsub("Airstrip", "AP", .) %>%
           gsub("Main Field", "MF", .) %>%
           gsub("Willingham", "Wg", .) %>%
           gsub("Person", "Pn", .) %>%
           gsub("Martin Place", "MP", .) %>%
           gsub("Black Shop", "BS", .) %>%
           gsub("Smith Half Moon", "SHM", .) %>%
           gsub("Smith", "Sm", .) %>%
           gsub("Hemp Field", "HF", .) %>%
           gsub("West", "W", .) %>%
           gsub("Birdoe", "BD", .) %>%
           gsub("Carter", "Cr", .) %>%  # Replace "Carter" with "Cr"
           gsub(" ", "_", .) %>%  # Replace spaces with "_"
           gsub("__", "_", .))  # Replace double underscores with a single "_"
# View the modified data
print(head(arvashp))
# Drop the 'field_hash' column
arvashp <- arvashp %>%
  select(-field_hash)
# Define the output file path
output_shapefile_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/RenamedFieldNames/Arvafieldnames.shp"
# Save the modified shapefile
st_write(arvashp, output_shapefile_path)
# Confirm the save
cat("Shapefile saved to:", output_shapefile_path)











#################################
#################################
######old reading Isbell###3
#################################
#################################
#################################
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