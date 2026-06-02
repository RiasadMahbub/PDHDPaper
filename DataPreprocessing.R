# Load the required libraries
library(readxl)
library(lubridate)
library(dplyr)

# Set the directory path
dir_path <- "C:/Users/rbmahbub/Box/Research/Data/RiceDOPDOHDL/FlyingRecordBeaMailDec2022/FlyingRecords"

# Get a list of all Excel files in the directory
file_list <- list.files(path = dir_path, pattern = "\\.xlsx$", full.names = TRUE)

# Read each file into a list
data_list <- lapply(file_list, read_excel)

# Optionally, name the list with the file names for easy reference
names(data_list) <- basename(file_list)

# Initialize an empty list to store the extracted data
combined_data <- list()

# Loop through each dataset in the data_list
for (file_name in names(data_list)) {
  if (all(c("Field", "Plant Date") %in% colnames(data_list[[file_name]]))) {
    temp_data <- data_list[[file_name]][, c("Field", "Plant Date")]
    
    # Drop rows where 'Plant Date' is NA
    temp_data <- temp_data[!is.na(temp_data$`Plant Date`), ]
    
    # Convert 'Plant Date' to date format, handling different types
    temp_data$`Plant Date` <- sapply(temp_data$`Plant Date`, function(x) {
      if (is.numeric(x)) {
        as.Date(x, origin = "1899-12-30")  # Numeric (Excel format)
      } else if (is.character(x)) {
        as.Date(mdy(x))  # Assuming 'mdy' for character strings
      } else if (inherits(x, "POSIXct")) {
        as.Date(x)  # POSIXct
      } else {
        NA  # Return NA for unsupported types
      }
    })
    
    # Drop rows where 'Plant Date' is still NA after conversion
    temp_data <- temp_data[!is.na(temp_data$`Plant Date`), ]
    
    # Add a column to track the file source
    temp_data$Source <- file_name
    
    # Append the extracted data to the combined list
    combined_data[[file_name]] <- temp_data
  }
}

# Combine all data frames into one using rbind
PD_FR_ISbell_2015_2024 <- do.call(rbind, combined_data)

# Display the combined dataframe
head(PD_FR_ISbell_2015_2024)

# Ensure 'Plant Date' is in Date format
PD_FR_ISbell_2015_2024$`Plant Date` <- as.Date(PD_FR_ISbell_2015_2024$`Plant Date`, origin = "1970-01-01")

View(PD_FR_ISbell_2015_2024)
# Check the class of 'Plant Date'
class(PD_FR_ISbell_2015_2024$`Plant Date`)


# Check the class of 'Plant Date'
class(PD_FR_ISbell_2015_2024$`Plant Date`)






# Initialize a named list to store the classes of 'Plant Date'
plant_date_classes <- list()
names(plant_date_classes) <- basename(data_list)  # Name the list with the filenames

# Loop through each dataframe in the data_list
for (file_name in names(data_list)) {
  df <- data_list[[file_name]]
  
  if ("Plant Date" %in% colnames(df)) {
    # Get the class of 'Plant Date' and store it
    plant_date_classes[[file_name]] <- class(df$`Plant Date`)
  } else {
    # If 'Plant Date' column is missing, store NA
    plant_date_classes[[file_name]] <- NA
  }
}

# Print the classes of 'Plant Date' for each dataframe
print(plant_date_classes)
