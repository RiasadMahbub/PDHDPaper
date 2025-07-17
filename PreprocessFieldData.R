library(readxl)
library(tidyverse)

# File paths
file_2023 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/Unilever/UnileverFieldNameIssue_5_19_2025/RivianaKnorr2023.xlsx"
file_2024 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/Unilever/UnileverFieldNameIssue_5_19_2025/RivianaKnorr2024.xlsx"

# Read the 2023 data from row 4 of the specified sheet
riviana_2023 <- read_excel(file_2023, sheet = "Farmer data 2023", skip = 3)

# Read the 2024 data from row 4 of the specified sheet
riviana_2024 <- read_excel(file_2024, sheet = "Farmer data 2024", skip = 3)


# Process both datasets using pipes
riviana_2023 <- riviana_2023 %>%
  slice(-c(22:35)) %>%
  rename(FIELD_NAME = `Field name`)

riviana_2024 <- riviana_2024 %>%
  slice(-c(21:33)) %>%
  rename(FIELD_NAME = `Field name`)

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

unilever_data2023 <- unilever_data %>%
  mutate(
    PDDOY = yday(as.Date(PD)),  
    HDDOY = ifelse(!is.na(HD), yday(as.Date(HD)), NA)
  ) %>%
  filter(YEAR %in% c(2023, 2024)) %>%
  select(FIELD_NAME, PDDOY, HDDOY, YEAR, Variety)

# Inner joins
unirivajoin_2023 <- riviana_2023 %>%
  inner_join(unilever_data2023, by = "FIELD_NAME")

unirivajoin_2024 <- riviana_2024 %>%
  inner_join(unilever_data2023, by = "FIELD_NAME")

# Anti joins
rivuniaantijoin_2023 <- riviana_2023 %>%
  anti_join(unilever_data2023, by = "FIELD_NAME")

rivuniaantijoin_2024 <- riviana_2024 %>%
  anti_join(unilever_data2023, by = "FIELD_NAME")

unirivaantijoin_2024 <- unilever_data2023 %>%
  anti_join(riviana_2024, by = "FIELD_NAME")


