library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(tibble)
library(zoo)      # For na.approx (linear interpolation)
library(tidyr)
library(ggplot2)
library(signal)  # For sgolayfilt()
library(tidyverse)
library(lubridate)
library(pbapply)
library(furrr)
library(data.table)
plan(multisession)  # Use available cores

#######################
#####FUNCTIONS#########
#######################
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

##### Read the files
meteo_data_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Data/PDHDMeteo"# Define the directory
meteo_csv_files <- list.files(meteo_data_dir, pattern = "\\.csv$", full.names = TRUE)# Get full paths of all CSV files
#meteo_list <- map(meteo_csv_files, read_csv)# Read all CSV files into a list of data frames
meteo_list <- pblapply(meteo_csv_files, fread, cl = parallel::detectCores() - 1)

vi_data_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Data/PDHDVI"# Define the VI data directory
vi_csv_files <- list.files(vi_data_dir, pattern = "\\.csv$", full.names = TRUE)# Get full paths of all CSV files
#vi_list <- map(vi_csv_files, read_csv)# Read all CSV files into a list of data frames
#vi_list <- future_map(vi_csv_files, read_csv, .progress = TRUE)
#vi_list <- future_map(vi_csv_files, fread, .progress = TRUE)

vi_list <- pblapply(vi_csv_files, fread, cl = parallel::detectCores() - 1)

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

# Create a summary tibble containing file information:
# - file_path: Full path of each CSV file
# - file_name: Just the filename (without path)
# - year: Extracted from filename (pattern: _YYYY.csv)
# - nrows: Number of rows in each file (using map_int to apply nrow to each dataframe in vi_list)
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

# Filter for files with less than 20 rows, extract year from filename,
# and count how many files per year meet this criterion
data_filtered_l20 <- vi_info_summary %>%
  dplyr::filter(nrows < 20) %>%
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

# Filter for files with more than 20 rows, extract year from filename,
# and count how many files per year meet this criterion
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
total_sum <- sum(data_filtered$frequency)
print(total_sum)

# ==============================================
# CREATE FILTERED DATA SUBSETS
# ==============================================

# Get list of filenames that have more than 20 rows
valid_files_gt20 <- vi_info_summary %>%
  dplyr::filter(nrows > 20) %>%
  pull(file_name)

# Subset the original data to only include files with >20 rows:
# 1. Filter the list of dataframes (vi_list)
# 2. Filter the list of file paths (vi_csv_files)
vi_list_gt20 <- vi_list[basename(vi_csv_files) %in% valid_files_gt20]
vi_csv_files_gt20 <- vi_csv_files[basename(vi_csv_files) %in% valid_files_gt20]

#Convert the unix to date format for VI
vi_list_gt20 <- lapply(vi_list_gt20, convert_unix_time)# Apply the conversion to each dataframe in the list
#Convert the unix to date format for Meteo
meteo_list <- lapply(meteo_list, convert_unix_time_meteo)# Apply the conversion to each dataframe in the list


vi_list_gt20 <- lapply(vi_list_gt20, sort_by_date)
meteo_list <- lapply(meteo_list, sort_by_date)

# FILTER BETWEEN MARCH AND OCTOBER
vi_list_gt20 <- lapply(vi_list_gt20, filter_march_to_october)

# Apply the make_daily function to all dataframes in the list
vi_list <- lapply(vi_list, make_daily)
meteo_list <- lapply(meteo_list, make_daily)
### Linear interpolation


####


###############################################
###############################################
####VI yearly summary########
# Extract year from the last 4 digits before ".csv"
# Extract filename and year
vi_info <- tibble(
  file_path = vi_csv_files,
  file_name = basename(vi_csv_files),
  year = str_extract(basename(vi_csv_files), "(?<=_)(\\d{4})(?=\\.csv$)"),
  nrows = map_int(vi_list, nrow)
)
vi_info_summary <- vi_info %>%
  select(year, file_name, nrows) %>%
  arrange(year, file_name)
# Filter, extract year, and count frequency of 'nrows' < 20
data_filtered_l20 <- vi_info_summary %>%
  dplyr::filter(nrows <20) %>%
  dplyr::mutate(year = substr(file_name, nchar(file_name) - 7, nchar(file_name) - 4)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(frequency = n())
write.csv(data_filtered_l20, file = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Table/observation_levels_less_than_20_by_year.csv", row.names = FALSE)

# Filter, extract year, and count frequency of 'nrows' < 10
data_filtered_g20 <- vi_info_summary %>%
  dplyr::filter(nrows >20) %>%
  dplyr:: mutate(year = substr(file_name, nchar(file_name) - 7, nchar(file_name) - 4)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(frequency = n())
write.csv(data_filtered_g20, file = "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Table/observation_levels_greater_than_20_by_year.csv", row.names = FALSE)
# Calculate the total sum of frequencies
total_sum <- sum(data_filtered$frequency)
print(total_sum)
# Recreate the list of valid file names with > 20 rows
valid_files_gt20 <- vi_info_summary %>%
  dplyr::filter(nrows > 20) %>%
  pull(file_name)
# Subset vi_list and vi_csv_files based on valid file names
vi_list_gt20 <- vi_list[basename(vi_csv_files) %in% valid_files_gt20]
vi_csv_files_gt20 <- vi_csv_files[basename(vi_csv_files) %in% valid_files_gt20]

vi_info <- tibble(
  file_path = vi_csv_files,
  file_name = basename(vi_csv_files),
  year = str_extract(basename(vi_csv_files), "(?<=_)(\\d{4})(?=\\.csv$)"),
  ncols = map_int(vi_list, ncol)
)
# Optional: View summary table
vi_info_summary <- vi_info %>%
  select(year, file_name, ncols) %>%
  arrange(year, file_name)
print(vi_info_summary, n = Inf)
# Filter, extract year, and count frequency of 'nrows' < 10
data_filtered <- vi_info_summary %>%
  dplyr::filter(ncols > 152) %>%
  dplyr::mutate(year = substr(file_name, nchar(file_name) - 7, nchar(file_name) - 4)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(frequency = n())
data_filtered
# Calculate the total sum of frequencies
total_sum <- sum(data_filtered$frequency)
print(total_sum)

# Get column names from the first file (e.g., vi_list[[61]])
full_column_names <- colnames(vi_list[[61]])

# Extract column names from all other files
column_names_list <- map(vi_list, colnames)

# Identify missing columns in each file by comparing with the full set of columns
missing_columns_info <- map(column_names_list, ~ setdiff(full_column_names, .))

# Create a tibble to display the missing columns for each file
missing_columns_summary <- tibble(
  file_name = vi_info$file_name,
  missing_columns = missing_columns_info
)

# View the summary of missing columns
View(missing_columns_summary)


######Meteo List of files
#####################
