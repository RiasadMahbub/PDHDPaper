# ==============================================
# CODE OVERVIEW: PROCESSING METEOROLOGICAL AND VEGETATION INDEX DATA
# ==============================================
#
# This script performs the following steps:
#
# 1. LOAD REQUIRED LIBRARIES
#    - Data manipulation (dplyr, tidyr, etc.)
#    - Time series processing (zoo, lubridate)
#    - Parallel processing (furrr, pbapply)
#    - Signal processing (signal)
#
# 2. DEFINE HELPER FUNCTIONS
#    - Time conversion (Unix to POSIXct)
#    - Data cleaning (dropping columns, filtering dates)
#    - Temporal processing (daily interpolation, growing season filter)
#    - Data smoothing (Savitzky-Golay filter)
#    - Merging utilities (by site-year-date)
#
# 3. LOAD INPUT DATA
#    - Meteorological data from PDHDMeteo directory
#    - Vegetation Index (VI) data from PDHDVI directory
#    - Both loaded in parallel for efficiency
#
# 4. PERFORM INITIAL DATA INSPECTION
#    - Print metadata about loaded files (row/column counts)
#    - Create summary tables of file characteristics
#
# 5. FILTER VEGETATION INDEX DATA
#    - Identify and separate files with sufficient observations (>20 rows)
#    - Create summary statistics of observation counts by year
#    - Save filtered file lists for downstream processing
#
# 6. PROCESS TIME DATA
#    - Convert Unix timestamps to POSIXct format
#    - Sort all data chronologically by Date
#    - Filter to growing season (March-October)
#
# 7. CREATE DAILY TIME SERIES
#    - Expand all data to daily resolution
#    - Fill missing dates with NA values
#
# 8. IMPUTE AND SMOOTH DATA
#    - Linear interpolation for missing values
#    - Apply Savitzky-Golay filter for noise reduction
#
# 9. PREPARE FOR MERGING
#    - Add composite site-year-date keys
#    - Prepare meteo data for joining by removing duplicate Date column
#
# 10. VISUALIZATION EXAMPLE
#     - Plot kNDVI values for the first processed site
#
# OUTPUTS:
# - observation_levels_less_than_20_by_year.csv
# - observation_levels_greater_than_20_by_year.csv
# - Processed data in memory ready for analysis
#
# ==============================================
# IMPLEMENTATION FOLLOWS BELOW
# ==============================================


# ==============================================
# LIBRARIES
# ==============================================
library(dplyr)        # Data manipulation
library(stringr)      # String operations
library(purrr)        # Functional programming
library(readr)        # Reading data
library(tibble)       # Modern data frames
library(zoo)          # Time series (na.approx for linear interpolation)
library(tidyr)        # Data tidying
library(ggplot2)      # Visualization
library(signal)       # Signal processing (sgolayfilt)
library(tidyverse)    # Core tidyverse packages
library(lubridate)    # Date handling
library(pbapply)      # Progress bar apply functions
library(furrr)        # Parallel processing
library(data.table)   # Fast data operations
plan(multisession)    # Use available cores for parallel processing
library(furrr)
library(furrr)
library(purrr)
library(parallel)
library(furrr)
library(purrr)
library(parallel)# Set up parallel processing
library(parallel)
library(pbapply)
#========================================
#HELPER FUNCTIONS
#========================================
# Function to convert Unix time (milliseconds) to R Date format
convert_unix_time <- function(df) {
  if ("Date" %in% colnames(df)) {
    df$Date <- as.POSIXct(df$Date / 1000, origin = "1970-01-01", tz = "UTC")
  }
  return(df)
}
convert_unix_time <- function(df) {
  if ("Date" %in% colnames(df)) {
    # Convert from milliseconds to seconds, then to POSIXct
    df$Date <- as.POSIXct(df$Date / 1000, origin = "1970-01-01", tz = "UTC")
  }
  return(df)
}

convert_unix_time_meteo <- function(df) {
  if ("Date" %in% colnames(df)) {
    df$Date <- as.POSIXct(df$Date, origin = "1970-01-01", tz = "UTC")
  }
  return(df)
}
# Function to sort dataframe by Date
sort_by_date <- function(df) {
  if ("Date" %in% colnames(df)) {
    df <- df[order(df$Date), ]
  }
  return(df)
}
get_num_columns <- function(file) {# Function to get the number of columns in each file
  data <- read.csv(file, nrows = 5)  # Read only the first few rows to check structure
  return(ncol(data))
}
# Function to read a CSV file
read_csv_file <- function(file) {
  data <- read.csv(file)  # Read the entire file
  return(data)
}
# Function to filter data between March and October
filter_march_to_october <- function(df) {
  if ("Date" %in% colnames(df)) {
    df <- df %>%
      dplyr::filter(format(Date, "%m") %in% c("03", "04", "05", "06", "07", "08", "09", "10"))
  }
  return(df)
}
# Function to classify columns in each dataframe
classify_columns <- function(df_name, df) {
  col_classes <- sapply(df, class)
  character_cols <- names(col_classes)[col_classes %in% c("character", "factor")]
  date_cols      <- names(col_classes)[col_classes %in% c("Date", "POSIXct", "POSIXt")]
  numeric_cols   <- names(col_classes)[col_classes %in% c("numeric", "integer", "double")]
  cat("DataFrame:", df_name, "\n")
  cat("Character columns:", toString(character_cols), "\n")
  cat("Date columns     :", toString(date_cols), "\n")
  cat("Numeric columns  :", toString(numeric_cols), "\n\n")
}
####Dropping character columns####
cols_to_drop <- c("system.index", "Name", "image_id", ".geo")# Columns to drop
drop_columns <- function(df) {
  df[ , !(names(df) %in% cols_to_drop)]
} # Function to remove specified columns
# Function to make the dataset daily by adding missing dates with NA
make_daily <- function(df) {
  if ("Date" %in% colnames(df)) {
    df$Date <- as.Date(df$Date)# Truncate time to just date
    date_seq <- seq(from = min(df$Date), to = max(df$Date), by = "day")# Create a sequence of dates from min to max date
    daily_df <- data.frame(Date = date_seq)    # Create a new dataframe with all dates
    df <- left_join(daily_df, df, by = "Date")    # Merge with original dataframe
  }
  return(df)
}
# Function to interpolate missing values in all numeric columns, modifying in place
interpolate_missing_values <- function(df) {
  if (is.data.frame(df)) {# Check if df is actually a dataframe
    numeric_cols <- sapply(df, is.numeric)  # Identify numeric columns
    # Apply na.approx only to numeric columns, making sure to handle single-column dataframes
    for (col_name in names(df)[numeric_cols]) {
      df[[col_name]] <- na.approx(df[[col_name]], na.rm = FALSE)  # Apply interpolation directly on column
    }
  }
  return(df)
}

#######Functoin for savitzkly golay filter##################
apply_sg_filter <- function(df, order = 3, window_size = 15) {
  if (!is.data.frame(df)) return(df)  # skip if not a dataframe
  numeric_cols <- sapply(df, is.numeric)
  if (!any(numeric_cols)) return(df)  # skip if no numeric columns
  df[numeric_cols] <- lapply(df[numeric_cols], function(col) {
    if (sum(!is.na(col)) > window_size) {
      tryCatch({
        sgolayfilt(col, p = order, n = window_size)
      }, error = function(e) {
        warning("SG filter error: ", conditionMessage(e))
        col
      })
    } else {
      col
    }
  })
  return(df)
}

add_siteyeardate <- function(df, name) {
  if (is.data.frame(df)) {
    site_code <- substr(name, 1, 5)  # Extract first 5 characters
    df$siteyeardate <- paste0(site_code, "_", format(df$Date, "%Y-%m-%d"))
  }
  return(df)
}

left_join_by_siteyeardate <- function(df1, df2) {
  if (is.data.frame(df1) && is.data.frame(df2)) {
    df2 <- df2[, setdiff(names(df2), "Date")]  # Drop 'Date' from meteo
    merged <- dplyr::left_join(df1, df2, by = "siteyeardate")
    return(merged)
  }
  return(df1)
}

# Function to replace negative values in NDVI and kNDVI with NA
replace_negative_vi <- function(df) {
  if (is.data.frame(df)) {
    if ("NDVI" %in% names(df)) {
      df$NDVI[df$NDVI < 0] <- NA
    }
    if ("kNDVI" %in% names(df)) {
      df$kNDVI[df$kNDVI < 0] <- NA
    }
  }
  return(df)
}

calculate_gdd_cumulative <- function(df) {
  if (!all(c("tmax", "tmin", "tmean", "Date", "PDDOY") %in% names(df))) return(df)
  
  # Calculate daily GDD (non-cumulative)
  df$gdd <- pmax(((df$tmax + df$tmin) / 2) - 10, 0)
  
  # Calculate DOY
  df$doy <- yday(df$Date)
  
  # Extract PDDOY
  pddoy <- unique(df$PDDOY)
  if (length(pddoy) != 1 || is.na(pddoy)) {
    df$cumulative_gdd_from_pddoy <- NA
    return(df)
  }
  
  # Calculate cumulative GDD starting from PDDOY
  df <- df %>%
    arrange(Date) %>%
    mutate(cumulative_gdd_from_pddoy = ifelse(doy >= pddoy, cumsum(ifelse(doy >= pddoy, gdd, 0)), NA))
  
  return(df)
}




# ==============================================
# DATA LOADING
# ==============================================
# Load meteorological data
meteo_data_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Data/PDHDMeteo"# Define the directory
meteo_csv_files <- list.files(meteo_data_dir, pattern = "\\.csv$", full.names = TRUE)# Get full paths of all CSV files
#meteo_list <- map(meteo_csv_files, read_csv)# Read all CSV files into a list of data frames
meteo_list <- pblapply(meteo_csv_files, fread, cl = parallel::detectCores() - 1)

# Load vegetation index data
vi_data_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Data/PDHDVI"# Define the VI data directory
vi_csv_files <- list.files(vi_data_dir, pattern = "\\.csv$", full.names = TRUE)# Get full paths of all CSV files
vi_list <- pblapply(vi_csv_files, fread, cl = parallel::detectCores() - 1)

# ==============================================
# DATA INSPECTION
# ==============================================
# For meteo_list
cat("---- Meteo Files ----\n")
walk2(meteo_list, meteo_csv_files, function(df, file) {
  cat("File:", basename(file), "\n")
  cat("Rows:", nrow(df), " | Columns:", ncol(df), "\n\n")
})

# For vi_list
cat("---- VI Files ----\n")
walk2(vi_list, vi_csv_files, function(df, file) {
  cat("File:", basename(file), "\n")
  cat("Rows:", nrow(df), " | Columns:", ncol(df), "\n\n")
})

# ==============================================
# PROCESSING VEGETATION INDEX (VI) DATA FILES
# ==============================================

vi_info <- tibble(
  file_path = vi_csv_files,
  file_name = basename(vi_csv_files),
  year = str_extract(basename(vi_csv_files), "(?<=_)(\\d{4})(?=\\.csv$)"),
  nrows = map_int(vi_list, nrow)
)

# Create a sorted summary of file information by year and filename
vi_info_summary <- vi_info %>%
  select(year, file_name, nrows) %>%
  arrange(year, file_name)

# ==============================================
# ANALYZE FILES WITH FEW OBSERVATIONS (<20 ROWS)
# ==============================================
data_filtered_l20 <- vi_info_summary %>%
  dplyr::filter(nrows < 10) %>%
  dplyr::mutate(year = substr(file_name, nchar(file_name) - 7, nchar(file_name) - 4)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(frequency = n())

# Save the results to a CSV file
write.csv(data_filtered_l20, 
          file = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Table/observation_levels_less_than_20_by_year.csv", 
          row.names = FALSE)

# ==============================================
# ANALYZE FILES WITH SUFFICIENT OBSERVATIONS (>20 ROWS)
# ==============================================
# Files with >20 observations
data_filtered_g20 <- vi_info_summary %>%
  dplyr::filter(nrows > 20) %>%
  dplyr::mutate(year = substr(file_name, nchar(file_name) - 7, nchar(file_name) - 4)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(frequency = n())

# Save the results to a CSV file
write.csv(data_filtered_g20, 
          file = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Table/observation_levels_greater_than_20_by_year.csv", 
          row.names = FALSE)

# ==============================================
# ADDITIONAL ANALYSIS
# ==============================================

# Calculate total number of files (NOTE: 'data_filtered' appears undefined in original code)
total_sum <- sum(data_filtered_g20$frequency)
print(total_sum)

# ==============================================
# CREATE FILTERED DATA SUBSETS
# ==============================================

# Get list of filenames that have more than 20 rows
valid_files_gt20 <- vi_info_summary %>%
  dplyr::filter(nrows > 20) %>%
  pull(file_name)

vi_list_gt20 <- vi_list[basename(vi_csv_files) %in% valid_files_gt20]
vi_csv_files_gt20 <- vi_csv_files[basename(vi_csv_files) %in% valid_files_gt20]


# ==============================================
# DATA TRANSFORMATION PIPELINE
# ==============================================


# Set up parallel cluster
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)

# Export ALL necessary functions AND operators to workers
clusterExport(cl, c("drop_columns", "convert_unix_time", "sort_by_date",
                    "filter_march_to_october", "make_daily",
                    "interpolate_missing_values", "apply_sg_filter",
                    "add_siteyeardate", "cols_to_drop", "%>%", "left_join", "na.approx"))

# Apply the negative value replacement
vi_list_gt20 <- pblapply(vi_list_gt20, replace_negative_vi, cl = cl)

# 1. Convert Unix to date format (parallel)
vi_list_gt20 <- pblapply(vi_list_gt20, convert_unix_time, cl = cl)
meteo_list <- pblapply(meteo_list, convert_unix_time_meteo, cl = cl)

# 2. Sort by date (parallel)
vi_list_gt20 <- pblapply(vi_list_gt20, sort_by_date, cl = cl)
meteo_list <- pblapply(meteo_list, sort_by_date, cl = cl)

# 3. Filter to growing season (parallel)
#vi_list_gt20 <- pblapply(vi_list_gt20, filter_march_to_october, cl = cl)

# 4. Convert to daily resolution (parallel)
vi_list_gt20 <- pblapply(vi_list_gt20, make_daily, cl = cl)
meteo_list <- pblapply(meteo_list, make_daily, cl = cl)


# 5. Interpolate missing values
vi_list_gt20 <- pblapply(vi_list_gt20, interpolate_missing_values, cl = cl)
meteo_list <- pblapply(meteo_list, interpolate_missing_values, cl = cl)

# With progress bar and parallel processing
vi_list_gt20 <- pblapply(vi_list_gt20, apply_sg_filter, cl = cl)

# Clean up
stopCluster(cl)
plan(sequential)  # Return to sequential processing

# ==============================================
# VISUALIZATION EXAMPLE
# ==============================================
plot(vi_list_gt20[[1]]$Date, vi_list_gt20[[1]]$kNDVI)
plot(meteo_list[[1]]$Date, meteo_list[[1]]$Ec)

# Replace HDDOY values of 360 with NA in all vi_list_gt20 dataframes
vi_list_gt20 <- lapply(vi_list_gt20, function(df) {
  if ("HDDOY" %in% names(df)) {
    df$HDDOY[df$HDDOY == 360] <- NA
  }
  return(df)
})


#-------------------------------------------------------
#Calculate GDD of each dataframe 
vi_list_gt20[[1]]$Date
meteo_list <- pblapply(meteo_list, calculate_gdd_cumulative, cl = detectCores() - 1)
plot(meteo_list[[1]]$Date, meteo_list[[1]]$cumulative_gdd)
plot(meteo_list[[1]]$Date, meteo_list[[1]]$gdd)


# 1. Function to gap-fill NA Field_Year values per dataframe
fill_field_year <- function(df) {
  # Get all unique non-NA Field_Year values in this dataframe
  valid_years <- na.omit(unique(df$Field_Year))
  
  # If we have exactly one unique value, fill all NAs with it
  if (length(valid_years) == 1) {
    df <- df %>% 
      mutate(Field_Year = ifelse(is.na(Field_Year), valid_years[1], Field_Year))
  } 
  # If multiple values exist, keep original (but warn)
  else if (length(valid_years) > 1) {
    warning("Multiple Field_Year values found in one dataframe - no filling done")
  }
  
  return(df)
}

# 2. Apply to all dataframes
vi_list_gt20 <- lapply(vi_list_gt20, fill_field_year)


#-------------------------------------------------------
# JOIN METEO AND vi_list_gt20
#-------------------------------------------------------
# Ensure both lists are the same length
vi_list_gt20 <- lapply(vi_list_gt20, function(df) {
  # Convert POSIXct with timezone CST to Date (no time)
  df$Date <- as.Date(with_tz(df$Date, tzone = "America/Chicago"))
  return(df)
})

meteo_list <- lapply(meteo_list, function(df) {
  # Already Date class? If character, convert:
  if (!inherits(df$Date, "Date")) {
    df$Date <- as.Date(df$Date)
  }
  return(df)
})
(length(vi_list_gt20) == length(meteo_list))
vi_names <- basename(vi_csv_files_gt20)       # From vi_list_gt20
meteo_names <- basename(meteo_csv_files)      # From meteo_list

# 1. Strip file extensions
vi_keys_raw <- tools::file_path_sans_ext(vi_names)
meteo_keys_raw <- tools::file_path_sans_ext(meteo_names)

# 2. Extract field and year into a unified key
vi_keys <- vi_keys_raw %>%
  stringr::str_replace("_VI_", "_")  # Convert to: Baker_20_2020

meteo_keys <- meteo_keys_raw %>%
  stringr::str_replace("_Meteo", "_")  # Convert to: Baker_20_2020

# 3. Assign names to the lists using these keys
names(vi_list_gt20) <- vi_keys
names(meteo_list) <- meteo_keys

# 4. Find common keys
common_keys <- intersect(vi_keys, meteo_keys)
length(common_keys)  # Should be <= 529

# 5. Filter matched lists
vi_matched <- vi_list_gt20[common_keys]
meteo_matched <- meteo_list[common_keys]

# 6. Confirm alignment
stopifnot(identical(names(vi_matched), names(meteo_matched)))
# Add siteyeardate
vi_matched <- Map(add_siteyeardate, vi_matched, names(vi_matched))
meteo_matched <- Map(add_siteyeardate, meteo_matched, names(meteo_matched))

left_join_by_siteyeardate <- function(df1, df2) {
  if (is.data.frame(df1) && is.data.frame(df2)) {
    # Identify overlapping columns (excluding 'siteyeardate')
    overlap_cols <- intersect(names(df1), names(df2))
    overlap_cols <- setdiff(overlap_cols, "siteyeardate")
    
    # Drop overlapping columns from meteo (df2) to avoid .x and .y
    df2 <- df2[, !(names(df2) %in% overlap_cols), drop = FALSE]
    
    # Perform left join safely
    merged <- dplyr::left_join(df1, df2, by = "siteyeardate")
    return(merged)
  }
  return(df1)
}

# Merge without suffixes
merged_list <- Map(left_join_by_siteyeardate, vi_matched, meteo_matched)
head(merged_list[[1]])
merged_list[[1]]$PDDOY
merged_list <- lapply(merged_list, function(df) {
  year <- format(as.Date(df$Date), "%Y")
  df$Field_Year <- ifelse(
    is.na(df$FIELD_NAME),
    NA,
    paste0(df$FIELD_NAME, "_", year)
  )
  return(df)
})

vi_list_gt20<-merged_list
# Check number of rows for each dataframe in the list
rows_per_df <- sapply(merged_list, function(x) {
  if (is.data.frame(x)) {
    return(nrow(x))
  } else {
    return(NA)  # In case some elements aren't dataframes
  }
})
# Print the results
print(rows_per_df)
row_counts <- tibble(
  Field_Year = names(merged_list),
  Rows = rows_per_df
)
# Get row counts for all dataframes
row_counts <- sapply(merged_list, function(df) if (is.data.frame(df)) nrow(df) else NA)
# Filter to keep only fields with >365 rows
fields_over_365 <- names(row_counts[row_counts > 365])
# Print the results
print(fields_over_365)

# Filter merged_list to keep only elements with >365 rows
merged_list <- merged_list[!(names(merged_list) %in% fields_over_365)]
vi_list_gt20<-merged_list

# Fill down Field_Year within each data frame
vi_list_gt20 <- lapply(vi_list_gt20, function(df) {
  df %>% tidyr::fill(Field_Year, .direction = "downup")
})
#-------------------------------------------------------
#-------------------------------------------------------
#End of the code
#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------

# 
# 
# ###############################################
# ###############################################
# ####VI yearly summary########
# # Extract year from the last 4 digits before ".csv"
# # Extract filename and year
# vi_info <- tibble(
#   file_path = vi_csv_files,
#   file_name = basename(vi_csv_files),
#   year = str_extract(basename(vi_csv_files), "(?<=_)(\\d{4})(?=\\.csv$)"),
#   nrows = map_int(vi_list, nrow)
# )
# vi_info_summary <- vi_info %>%
#   select(year, file_name, nrows) %>%
#   arrange(year, file_name)
# # Filter, extract year, and count frequency of 'nrows' < 20
# data_filtered_l20 <- vi_info_summary %>%
#   dplyr::filter(nrows <20) %>%
#   dplyr::mutate(year = substr(file_name, nchar(file_name) - 7, nchar(file_name) - 4)) %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(frequency = n())
# write.csv(data_filtered_l20, file = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Table/observation_levels_less_than_20_by_year.csv", row.names = FALSE)
# 
# # Filter, extract year, and count frequency of 'nrows' < 10
# data_filtered_g20 <- vi_info_summary %>%
#   dplyr::filter(nrows >20) %>%
#   dplyr:: mutate(year = substr(file_name, nchar(file_name) - 7, nchar(file_name) - 4)) %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(frequency = n())
# write.csv(data_filtered_g20, file = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Table/observation_levels_greater_than_20_by_year.csv", row.names = FALSE)
# # Calculate the total sum of frequencies
# total_sum <- sum(data_filtered$frequency)
# print(total_sum)
# # Recreate the list of valid file names with > 20 rows
# valid_files_gt20 <- vi_info_summary %>%
#   dplyr::filter(nrows > 20) %>%
#   pull(file_name)
# # Subset vi_list and vi_csv_files based on valid file names
# vi_list_gt20 <- vi_list[basename(vi_csv_files) %in% valid_files_gt20]
# vi_csv_files_gt20 <- vi_csv_files[basename(vi_csv_files) %in% valid_files_gt20]
# 
# vi_info <- tibble(
#   file_path = vi_csv_files,
#   file_name = basename(vi_csv_files),
#   year = str_extract(basename(vi_csv_files), "(?<=_)(\\d{4})(?=\\.csv$)"),
#   ncols = map_int(vi_list, ncol)
# )
# # Optional: View summary table
# vi_info_summary <- vi_info %>%
#   select(year, file_name, ncols) %>%
#   arrange(year, file_name)
# print(vi_info_summary, n = Inf)
# # Filter, extract year, and count frequency of 'nrows' < 10
# data_filtered <- vi_info_summary %>%
#   dplyr::filter(ncols > 152) %>%
#   dplyr::mutate(year = substr(file_name, nchar(file_name) - 7, nchar(file_name) - 4)) %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(frequency = n())
# data_filtered
# # Calculate the total sum of frequencies
# total_sum <- sum(data_filtered$frequency)
# print(total_sum)
# 
# # Get column names from the first file (e.g., vi_list[[61]])
# full_column_names <- colnames(vi_list[[61]])
# 
# # Extract column names from all other files
# column_names_list <- map(vi_list, colnames)
# 
# # Identify missing columns in each file by comparing with the full set of columns
# missing_columns_info <- map(column_names_list, ~ setdiff(full_column_names, .))
# 
# # Create a tibble to display the missing columns for each file
# missing_columns_summary <- tibble(
#   file_name = vi_info$file_name,
#   missing_columns = missing_columns_info
# )
# 
# # View the summary of missing columns
# View(missing_columns_summary)
# 
# 
# ######Meteo List of files
# #####################
# 
# 
# 
# # Apply the three processing steps to each dataframe in the list
# vi_list_gt20 <- lapply(vi_list_gt20, function(df) {
#   df %>%
#     make_daily() %>%
#     interpolate_missing_values() %>%
#     apply_sg_filter()
# })
# 
# plot(vi_list_gt20[[1]]$Date, vi_list_gt20[[1]]$kNDVI)
# 
