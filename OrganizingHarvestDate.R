# Load required libraries
library(readxl)
library(dplyr)

#==========================================
###### File Preparation Notes ##########
#==========================================
# Isbell FieldName uses underscore (_) instead of space
# For files 2019, 2020, 2021, and 2022 the sheet numbers were modified
# Field names for 2021, 2022 and 2023 need to be shortened from prefix/suffixes

########################################
###### Read and Process Excel Files ####
########################################
# Define directory path
directory_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/IsbellFarm/YieldMapsIsbells"

# List all .xlsx files in directory
xlsx_files <- list.files(path = directory_path, 
                         pattern = "\\.xlsx$", 
                         full.names = TRUE)

# Read each .xlsx file into named list of data frames
xlsx_data <- lapply(xlsx_files, read_excel)

# Clean file names for list elements
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

xlsx_data$AllFields2023Summary_v2024_09_24$FIELD_NAME
xlsx_data$AllFields2022Summary_v2024_09_24$FIELD_NAME
xlsx_data$AllFields2021Summary_v2024_09_24$FIELD_NAME
View(xlsx_data$AllFields2022Summary_v2024_09_24)

# Remove the prefix "Generation F_Generation F_" 
# Remove the suffix "_Harvest_YYYY-MM-DD_00.shp"
# Remove the prefix "Zero Grade F_Zero Grade F_"
# Apply cleaning to all data frames in xlsx_data
for (df in names(xlsx_data)) {
  if ("FIELD_NAME" %in% names(xlsx_data[[df]])) {
    xlsx_data[[df]]$FIELD_NAME <- gsub("^(Generation F_Generation F_|Zero Grade F_Zero Grade F_)", "", xlsx_data[[df]]$FIELD_NAME)
    xlsx_data[[df]]$FIELD_NAME <- gsub("_Harvest_\\d{4}-\\d{2}-\\d{2}_00.shp$", "", xlsx_data[[df]]$FIELD_NAME)
    xlsx_data[[df]]$FIELD_NAME <- gsub(" ([0-9]+)", "_\\1", xlsx_data[[df]]$FIELD_NAME)  # Convert "East 02" to "East_02"
  }
}


####Rbind the data
# Combine all dataframes into one
HDmerged_data <- bind_rows(xlsx_data, .id = "Year")
sort(unique(HDmerged_data$FIELD_NAME))

# Replace the values in the 'Field' column based on the given rules
HDmerged_data <- HDmerged_data %>%
  mutate(FIELD_NAME = case_when(
    FIELD_NAME %in% c(1, 1.0, "1", "1.0", "East#1", "East 01", "east_1") ~ "East_01",
    FIELD_NAME %in% c(2, 2.0, "2", "2.0", "East#2", "East 02", "east_2") ~ "East_02",
    FIELD_NAME %in% c(3, 3.0, "3", "3.0", "East#3", "East 03", "east_3") ~ "East_03",
    FIELD_NAME %in% c(4, 4.0, "4", "4.0", "East#4", "east_4") ~ "East_04",
    FIELD_NAME %in% c(5, 5.0, "5", "5.0", "East#5", "east_5") ~ "East_05",
    FIELD_NAME %in% c(6, 6.0, "6", "6.0", "East#6", "east_6") ~ "East_06",
    FIELD_NAME %in% c(7, 7.0, "7", "7.0", "East#7", "east_7") ~ "East_07",
    FIELD_NAME %in% c(8, 8.0, "8", "8.0", "East#8", "east_8") ~ "East_08",
    FIELD_NAME %in% c(9, 9.0, "9", "9.0", "East#9", "east_9") ~ "East_09",
    FIELD_NAME %in% c(10, 10.0, "10", "10.0", "East#10", "east_10") ~ "East_10",
    FIELD_NAME %in% c(11, 11.0, "11", "11.0", "East#11", "east_11") ~ "East_11",
    FIELD_NAME %in% c(12, 12.0, "12", "12.0", "East#12", "east_12") ~ "East_12",
    FIELD_NAME %in% c(13, 13.0, "13", "13.0", "East#13", "east_13") ~ "East_13",
    FIELD_NAME %in% c(14, 14.0, "14", "14.0", "East#14", "east_14") ~ "East_14",
    FIELD_NAME %in% c("Baker 20", "Baker_20_Harvest_2022-09-15_1_00.shp") ~ "Baker_20",
    FIELD_NAME == "Baker 30" ~ "Baker_30",
    FIELD_NAME == "Baker 40" ~ "Baker_40",
    FIELD_NAME == "Baker 50" ~ "Baker_50",
    FIELD_NAME %in% c("Bott of Res", "Bott of Res.", "Bottom Res.", "Bottom Res") ~ "Bott_of_Res",
    FIELD_NAME %in% c("Way_1", "Way 1") ~ "Way_01",
    FIELD_NAME %in% c("Way_2", "Way 2") ~ "Way_02",
    FIELD_NAME %in% c("Way_3", "Way 3") ~ "Way_03",
    FIELD_NAME %in% c("Way_4", "Way 4") ~ "Way_04",
    FIELD_NAME %in% c("Way_5", "Way 5") ~ "Way_05",
    FIELD_NAME %in% c("Kelly_1", "Kelly 1") ~ "Kelly_01",
    FIELD_NAME %in% c("Kelly_2", "Kelly 2") ~ "Kelly_02",
    FIELD_NAME %in% c("Kelly_3", "Kelly 3") ~ "Kelly_03",
    FIELD_NAME %in% c("Kelly_4", "Kelly 4") ~ "Kelly_04",
    FIELD_NAME %in% c("Kelly_5", "Kelly 5") ~ "Kelly_05",
    FIELD_NAME %in% c("Kelly_6", "Kelly 6") ~ "Kelly_06",
    FIELD_NAME %in% c("Kelly_7", "Kelly 7") ~ "Kelly_07",
    FIELD_NAME %in% c("Kelly_8", "Kelly 8") ~ "Kelly_08",
    FIELD_NAME %in% c("Kelly_9", "Kelly 9") ~ "Kelly_09",
    FIELD_NAME %in% c("Kelly_10", "Kelly 10") ~ "Kelly_10",
    FIELD_NAME %in% c("Kelly_11", "Kelly 11") ~ "Kelly_11",
    FIELD_NAME == "East #1"  ~ "East_01",
    FIELD_NAME == "East #2"  ~ "East_02",
    FIELD_NAME == "East #3"  ~ "East_03",
    FIELD_NAME == "East #4"  ~ "East_04",
    FIELD_NAME == "East #5"  ~ "East_05",
    FIELD_NAME == "East 6"  ~ "East_06",
    FIELD_NAME == "East #7"  ~ "East_07",
    FIELD_NAME == "East #8"  ~ "East_08",
    FIELD_NAME == "East #9"  ~ "East_09",
    FIELD_NAME == "East #10" ~ "East_10",
    FIELD_NAME == "East #11" ~ "East_11",
    FIELD_NAME == "East #12" ~ "East_12",
    FIELD_NAME == "East #13" ~ "East_13",
    FIELD_NAME == "East #14" ~ "East_14",
    FIELD_NAME %in% c("Walls_1", "Walls 1")  ~ "Walls_01",
    FIELD_NAME %in% c("Walls_2", "Walls 2")  ~ "Walls_02",
    FIELD_NAME %in% c("Walls_3", "Walls 3")  ~ "Walls_03",
    FIELD_NAME %in% c("Walls_4", "Walls 4")  ~ "Walls_04",
    FIELD_NAME %in% c("Walls_5", "Walls 5")  ~ "Walls_05",
    FIELD_NAME %in% c("Walls_6", "Walls 6")  ~ "Walls_06",
    FIELD_NAME %in% c("Walls_7", "Walls 7")  ~ "Walls_07",
    FIELD_NAME %in% c("Walls_6_+7", "Walls_67") ~ "Walls_06_07",
    FIELD_NAME %in% c("Walls_8", "Walls 8")  ~ "Walls_08",
    FIELD_NAME %in% c("Walls_9", "Walls 9")  ~ "Walls_09",
    FIELD_NAME %in% c("Walls_10", "Walls 10") ~ "Walls_10",
    FIELD_NAME %in% c("Walls_11", "Walls 11") ~ "Walls_11",
    FIELD_NAME %in% c("Walls_12", "Walls 12") ~ "Walls_12",
    FIELD_NAME %in% c("Walls_13", "Walls 13") ~ "Walls_13",
    FIELD_NAME %in% c("Top_of_Res", "Top of Res.", "Top of Res") ~ "Top_of_Res",
    FIELD_NAME == "Hudson's" ~ "Hudsons",
    # New Field Name Mappings
    FIELD_NAME %in% c("Bransford Est", "Bransford East", "East Bransford", "East Bransfo") ~ "Bransford_Est",
    FIELD_NAME == "Carr North" ~ "Carr_North",
    FIELD_NAME == "Carr South" ~ "Carr_South",
    FIELD_NAME %in% c( "Cattlet 1", "Cattlet Top", "catlett top") ~ "Cattlet_01",
    FIELD_NAME %in% c( "Cattlet 2", "Cattlet Bott", "cattlet bottom") ~ "Cattlet_02",
    FIELD_NAME == "Cattlet 2" ~ "Cattlet_02",
    FIELD_NAME %in% c( "Cotton Patch", "Cotton Patch_Harvest_2022-08-26_1_00.shp") ~ "Cotton_Patch",
    FIELD_NAME %in% c("East Harvey", "Top Harvey") ~ "East_Harvey",
    FIELD_NAME == "East Harvey" ~ "East_Harvey",
    FIELD_NAME %in% c("East Joe T", "Joe T East") ~ "East_Joe_T",
    FIELD_NAME == "Experiment" ~ "Experiment",
    FIELD_NAME %in% c("Flat") ~ "Flat",
    FIELD_NAME %in% c( "Pop's Flat", "pops flat", "Pops Flat") ~ "Pops_Flat",
    FIELD_NAME == "Frog 40" ~ "Frog_40",
    FIELD_NAME %in% c("Frog 40 NW", "Frog 40  NW") ~ "Frog_40_NW",
    FIELD_NAME == "Haley" ~ "Haley",
    FIELD_NAME == "Hole 40" ~ "Hole_40",
    FIELD_NAME %in% c("Hole 40 NE", "Hole 40  NE") ~ "Hole_40_NE",
    FIELD_NAME == "HQ Shop" ~ "HQ_Shop",
    FIELD_NAME == "Hudsons" ~ "Hudsons",
    FIELD_NAME %in% c("Hwy Shop 40", "40 Shop Hwy ", "Hwy Shop_40") ~ "Hwy_Shop_40",
    FIELD_NAME == "Hwy Shop 40 SE" ~ "Hwy_Shop_40_SE",
    FIELD_NAME %in% c("Judy's", "Judy's Field", "Judys Field") ~ "Judys",
    FIELD_NAME == "Lentz" ~ "Lentz",
    FIELD_NAME == "Lost 40" ~ "Lost_40",
    FIELD_NAME == "Lyntz" ~ "Lyntz",
    FIELD_NAME %in% c("Md 40 West", "Md_40 West", 'middle_40 west', "West Mid_40") ~ "Md_40_West",
    FIELD_NAME == "Md 40 West MidW" ~ "Md_40_West_MidW",
    FIELD_NAME %in% c("Mid 40 Hwy", "Mid_40 Hwy", "Mid_40 Hwy_Harvest_2021-09-09_1_00.shp") ~ "Mid_40_Hwy",
    FIELD_NAME %in% c('Mid 40 Hwy MidE') ~ "Mid_40_Hwy_MidE",
    FIELD_NAME %in% c("Middle Bransford", "Mid Bransford", "Middle Brans") ~ "Mid_Bransford",
    FIELD_NAME == "Morris" ~ "Morris",
    FIELD_NAME == "New Ground" ~ "New_Ground",
    FIELD_NAME == "North Cotton" ~ "North_Cotton",
    FIELD_NAME %in% c( "Pop's Field", "Pops Field") ~ "Pops",
    FIELD_NAME == "Pops" ~ "Pops",
    FIELD_NAME %in% c( "Seed Rice", "Seed Rice_Harvest_2022-10-25_1_00.shp") ~ "Seed_Rice",
    FIELD_NAME %in% c( "Shanes", "Shane's") ~ "Shanes",
    FIELD_NAME == "SI Shop" ~ "SI_Shop",
    FIELD_NAME %in% c( "The 90", "90") ~ "The_90",
    FIELD_NAME == "Walls 6 +7" ~ "Walls_06_07",
    FIELD_NAME %in% c("West Harvey", "Bottom Harvey", "Bottom Harve") ~ "West_Harvey",
    FIELD_NAME == "West Harvey" ~ "West_Harvey",
    FIELD_NAME  %in% c( "West Joe T", "Joe T West") ~ "West_Joe_T",
    FIELD_NAME %in% c( "West Bransfo", "West Bransford", "Wst Bransford") ~ "Wst_Bransford",
    FIELD_NAME %in% c( "Wst Shop 40", "West Shop_40") ~ "Wst_Shop_40",
    FIELD_NAME == "Wst Shop 40 SW" ~ "Wst_Shop_40_SW",
    #FIELD_NAME %in% c("Lentz") ~ "Lyntz",  # Added Lentz to Lyntz
    TRUE ~ as.character(FIELD_NAME)  # Keep other values unchanged
  ))

#sort(unique(isbellcl$FIELD_NAME))
sort(unique(HDmerged_data$FIELD_NAME))

# Fields in isbellcl but not in HDmerged_data
#setdiff(unique(isbellcl$FIELD_NAME), unique(HDmerged_data$FIELD_NAME))
# Fields in HDmerged_data but not in isbellcl
#setdiff(unique(HDmerged_data$FIELD_NAME), unique(isbellcl$FIELD_NAME))

#sort(unique(HD_not_in_isbellcl$FIELD_NAME))
# Create a new column by concatenating FIELD_NAME and Year
HDmerged_data$FIELDNAME_YEAR <- paste(HDmerged_data$FIELD_NAME, HDmerged_data$year, sep = "_")

HDmerged_data$FIELDNAME_YEAR 

View(HDmerged_data)
#======================================================================================










xlsx_data$FIELD_NAME <- gsub("^(Generation F_Generation F_|Zero Grade F_Zero Grade F_)", "", xlsx_data$FIELD_NAME)

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




########################################
###### Fix FIELD_NAME_Year Creation ######
########################################

# First, identify and remove non-dataframe elements
to_remove <- sapply(xlsx_data, function(x) !is.data.frame(x) || nrow(x) == 0)
if (any(to_remove)) {
  cat("Removing the following invalid elements:\n")
  print(names(xlsx_data)[to_remove])
  xlsx_data <- xlsx_data[!to_remove]
}

# Now safely create FIELD_NAME_Year column
for (df_name in names(xlsx_data)) {
  # Standardize column names first (note the tildes in some files)
  colnames(xlsx_data[[df_name]]) <- gsub("^~", "", colnames(xlsx_data[[df_name]]))
  
  # Create the new column
  xlsx_data[[df_name]] <- xlsx_data[[df_name]] %>%
    mutate(FIELD_NAME_Year = paste(year, FIELD_NAME, sep = "_"))
  
  # Print verification
  cat("\nCreated FIELD_NAME_Year in", df_name, "- Sample values:\n")
  print(head(xlsx_data[[df_name]]$FIELD_NAME_Year))
}

# Optional: Check which files were processed
cat("\nSuccessfully processed files:\n")
print(names(xlsx_data))




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

# Combine all data frames in the list into one
HDIsbellCombined <- do.call(rbind, xlsx_data)

unique(HDIsbellCombined$FIELD_NAME)


# ==========================
# ARVA DATA PROCESSING
# ==========================


# ==========================
# DWMRU DATA PROCESSING
# ==========================


# ==========================
# MATT MORRIS DATA PROCESSING
# ==========================

# ==========================
# SULLIVAN DATA PROCESSING
# ==========================


# ==========================
# SCOTT MATTHEWS DATA PROCESSING
# ==========================


# ================================
# -------- UNILEVER---------------
# ================================
