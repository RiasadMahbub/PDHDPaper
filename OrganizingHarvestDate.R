# Load the necessary library
library(readxl)

###IsbellFieldName underscore (_) than space
#### Files 2019, 2020, 2021, and 2022 the sheet numbers were modified

########################################
###### Read the Files########
########################################
# Define the directory path
directory_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/IsbellFarm/YieldMapsIsbells"
# List all .xlsx files in the directory
xlsx_files <- list.files(path = directory_path, pattern = "\\.xlsx$", full.names = TRUE)
# Read each .xlsx file into a named list of data frames
xlsx_data <- lapply(xlsx_files, read_excel)
# Name the list elements based on the file names (remove directory and extension, also remove spaces)
names(xlsx_data) <- gsub(" ", "", gsub("\\.xlsx$", "", basename(xlsx_files)))


########################################
###### Print the names of the columns
##Check the date format of Start Harvest or Start harvest columns########
########################################
# Print the column names of each data frame in the list
for (df_name in names(xlsx_data)) {
  cat("\nData frame:", df_name, "\n")
  cat("Columns:\n")
  print(colnames(xlsx_data[[df_name]]))
}

# Check the 'Start harvest' or 'Start Harvest' column for each data frame
for (df_name in names(xlsx_data)) {
  cat("\nChecking 'Start harvest' column in data frame:", df_name, "\n")
  
  # Access the data frame
  df <- xlsx_data[[df_name]]
  
  # Check if either version of the column exists
  column_name <- if ("Start harvest" %in% colnames(df)) {
    "Start harvest"
  } else if ("Start Harvest" %in% colnames(df)) {
    "Start Harvest"
  } else {
    NULL
  }
  
  # If the column exists, process it
  if (!is.null(column_name)) {
    # Print the first few values of the column
    cat("Sample values from", column_name, "column:\n")
    print(head(df[[column_name]]))
    
    # Check the class of the column
    cat("Class of", column_name, "column:", class(df[[column_name]]), "\n")
  } else {
    cat("'Start harvest' column not found in this data frame.\n")
  }
}


#### Create FieldNAME Year column
for (df_name in names(xlsx_data)) {
  cat("\nData frame:", df_name, "\n")
  cat("Columns:\n")
  
  # Rename columns 'Start Harvest' and 'Start harvest' to 'HD'
  colnames(xlsx_data[[df_name]]) <- gsub("^Start Harvest$|^Start harvest$", "HD", colnames(xlsx_data[[df_name]]))
  
  # Rename column 'Field' to 'FIELD_NAME'
  colnames(xlsx_data[[df_name]]) <- gsub("^Field$", "FIELD_NAME", colnames(xlsx_data[[df_name]]))
  
  # Ensure 'HD' is in Date format (assuming it's not already)
  xlsx_data[[df_name]]$HD <- as.Date(xlsx_data[[df_name]]$HD)
  
  # Create 'year' column by extracting the year from 'HD'
  xlsx_data[[df_name]]$year <- format(xlsx_data[[df_name]]$HD, "%Y")
}



########################################
###### Print the FIELD_NAME column######
########################################
# Print the 'FIELD_NAME' column for each data frame in the list
for (df_name in names(xlsx_data)) {
  cat("\nData frame:", df_name, "\n")
  cat("FIELD_NAME column:\n")
  
  # Check if 'FIELD_NAME' exists in the data frame
  if("FIELD_NAME" %in% colnames(xlsx_data[[df_name]])) {
    print(xlsx_data[[df_name]]$FIELD_NAME)
  } else {
    cat("FIELD_NAME column not found in", df_name, "\n")
  }
}

# Remove the prefix "Generation F_Generation F_" 
# Remove the suffix "_Harvest_YYYY-MM-DD_00.shp"
# Remove the prefix "Zero Grade F_Zero Grade F_"

# Loop through each data frame and clean the 'FIELD_NAME' column
for (df_name in names(xlsx_data)) {
  cat("\nData frame:", df_name, "\n")
  cat("Cleaned FIELD_NAME column:\n")
  
  # Check if 'FIELD_NAME' exists in the data frame
  if ("FIELD_NAME" %in% colnames(xlsx_data[[df_name]])) {
    # Remove the unwanted parts using regular expressions
    cleaned_field_name <- xlsx_data[[df_name]]$FIELD_NAME
    
    # Remove the "Generation F_Generation F_" and "_Harvest_YYYY-MM-DD_00.shp" part
    cleaned_field_name <- gsub("^Generation F_Generation F_|_Harvest_\\d{4}-\\d{2}-\\d{2}_00\\.shp$", "", cleaned_field_name)
    
    # Remove the "Zero Grade F_Zero Grade F_" part
    cleaned_field_name <- gsub("^Zero Grade F_Zero Grade F_", "", cleaned_field_name)
    
    # Print the cleaned 'FIELD_NAME'
    print(cleaned_field_name)
  } else {
    cat("FIELD_NAME column not found in", df_name, "\n")
  }
}

# Loop through each data frame and clean the 'FIELD_NAME' column
for (df_name in names(xlsx_data)) {
  cat("\nData frame:", df_name, "\n")
  cat("Cleaned FIELD_NAME column:\n")
  
  # Check if 'FIELD_NAME' exists in the data frame
  if ("FIELD_NAME" %in% colnames(xlsx_data[[df_name]])) {
    # Remove the unwanted parts using regular expressions
    cleaned_field_name <- xlsx_data[[df_name]]$FIELD_NAME
    
    # Remove the "Generation F_Generation F_" and "_Harvest_YYYY-MM-DD_00.shp" part
    cleaned_field_name <- gsub("^Generation F_Generation F_|_Harvest_\\d{4}-\\d{2}-\\d{2}_00\\.shp$", "", cleaned_field_name)
    
    # Remove the "Zero Grade F_Zero Grade F_" part
    cleaned_field_name <- gsub("^Zero Grade F_Zero Grade F_", "", cleaned_field_name)
    
    # Remove the new "Zero Grade F_Zero Grade F_" and "_Harvest_YYYY-MM-DD_00.shp" part
    cleaned_field_name <- gsub("^Zero Grade F_Zero Grade F_", "", cleaned_field_name)
    cleaned_field_name <- gsub("_Harvest_\\d{4}-\\d{2}-\\d{2}_00\\.shp$", "", cleaned_field_name)
    
    # Print the cleaned 'FIELD_NAME'
    print(cleaned_field_name)
  } else {
    cat("FIELD_NAME column not found in", df_name, "\n")
  }
}





#### Create FieldNAME Year column
for (df_name in names(xlsx_data)) {
  # Create 'FIELD_NAME_Year' column by concatenating 'year' and 'FIELD_NAME'
  xlsx_data[[df_name]]$FIELD_NAME_Year <- paste(xlsx_data[[df_name]]$year, xlsx_data[[df_name]]$FIELD_NAME, sep = "_")
  
  # Print the updated column names
  print(colnames(xlsx_data[[df_name]]))
}
xlsx_data$AllFields2015Summary_v2022_03_24$FIELD_NAME_Year

#### Create FieldNAME Year column
for (df_name in names(xlsx_data)) {
  # Create 'FIELD_NAME_Year' column by concatenating 'year' and 'FIELD_NAME'
  xlsx_data[[df_name]]$FIELD_NAME_Year <- paste(xlsx_data[[df_name]]$year, xlsx_data[[df_name]]$FIELD_NAME, sep = "_")
  
  # Print the updated column names
  print(colnames(xlsx_data[[df_name]]))
  
  # Print the 'FIELD_NAME_Year' column for each data frame
  cat("\nFIELD_NAME_Year column for", df_name, ":\n")
  print(xlsx_data[[df_name]]$FIELD_NAME_Year)
}

