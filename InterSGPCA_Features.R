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
#plan(multisession)    # Use available cores for parallel processing
plan(multisession, workers =availableCores())
library(purrr)
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
# Function to extrapolate data for a given DOY range to a full 365-day year.
# This function is separate from interpolation and is designed to handle
# data with a continuous DOY range that does not start at 1 or end at 365.
extrapolate_to_365_days <- function(df) {
  # Initialize the final result data frame with 365 rows for consistency.
  full_doy <- 1:365
  df_final <- data.frame(DOY = full_doy)
  
  # Check if the input is a valid data frame and not empty.
  if (is.data.frame(df) && nrow(df) > 0) {
    # We assume 'DOY' is the column name for the day of year.
    doy_col_name <- "DOY"
    if (!doy_col_name %in% names(df)) {
      warning("No 'DOY' column found. Returning an empty 365-day data frame.")
      return(df_final)
    }
    
    # Identify all numeric columns in the input data frame.
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    # Process each numeric column to perform the extrapolation.
    for (col_name in numeric_cols) {
      # Skip the DOY column itself.
      if (col_name == doy_col_name) {
        next
      }
      
      # --- New logic for PDDOY and HDDOY as requested by the user ---
      # For these columns, we find the most common value and repeat it.
      if (col_name %in% c("PDDOY", "HDDOY")) {
        vec <- rep(NA, 365)
        
        # Find the most common value (mode) from the non-NA values.
        non_na_values <- df[[col_name]][!is.na(df[[col_name]])]
        
        # Check if there are any non-NA values to calculate the mode from.
        if (length(non_na_values) > 0) {
          # Get the most frequent value.
          mode_value <- as.numeric(names(sort(table(non_na_values), decreasing = TRUE)[1]))
          # Fill the entire vector with the common value.
          vec[] <- mode_value
        }
        
        # Add the resulting vector to the final data frame.
        df_final[[col_name]] <- vec
        next # Skip to the next column in the loop.
      }
      
      # --- Original extrapolation logic for all other numeric columns ---
      # Create a new vector for this column with NAs for all 365 days.
      vec <- rep(NA, 365)
      
      # Map the existing data from the data frame to the new 365-day vector.
      tryCatch({
        # Ensure DOY values are within the valid range (1-365) and are unique for mapping.
        unique_doy_df <- df[!duplicated(df[[doy_col_name]]), ]
        valid_doy_indices <- unique_doy_df[[doy_col_name]] >= 1 & unique_doy_df[[doy_col_name]] <= 365
        
        # Map data from the current column to the new vector.
        # We also check for and handle any non-finite values (like Inf or NaN)
        # by replacing them with NA, which na.approx can handle gracefully.
        mapped_values <- unique_doy_df[[col_name]][valid_doy_indices]
        mapped_values[!is.finite(mapped_values)] <- NA
        vec[unique_doy_df[[doy_col_name]][valid_doy_indices]] <- mapped_values
      }, error = function(e) {
        warning(paste("Error mapping data for column", col_name, ". Skipping extrapolation for this entry:", e$message))
      })
      
      # Find the first and last non-NA indices
      first_idx <- which(!is.na(vec))[1]
      last_idx <- tail(which(!is.na(vec)), 1)
      
      # Manually handle extrapolation at the beginning of the year (DOY 1 to first_idx-1).
      # This creates a linear ramp from 0 up to the first data point.
      if (!is.na(first_idx) && first_idx > 1) {
        start_value <- 0
        end_value <- vec[first_idx]
        
        # Guard against non-finite values in the data.
        if (!is.finite(end_value)) {
          warning(paste("Non-finite value found at first data point. Extrapolating to 0 instead."))
          end_value <- 0
        }
        
        ramp <- seq(from = start_value, to = end_value, length.out = first_idx)
        vec[1:first_idx] <- ramp
      }
      
      # Manually handle extrapolation at the end of the year (last_idx+1 to 365).
      # This creates a linear ramp from the last data point down to 0.
      if (!is.na(last_idx) && last_idx < 365) {
        start_value <- vec[last_idx]
        
        # Guard against non-finite values in the data.
        if (!is.finite(start_value)) {
          warning(paste("Non-finite value found at last data point. Extrapolating from 0 instead."))
          start_value <- 0
        }
        
        end_value <- 0
        ramp <- seq(from = start_value, to = end_value, length.out = 365 - last_idx + 1)
        vec[last_idx:365] <- ramp
      }
      
      # Use na.approx to fill any remaining gaps in the middle of the dataset
      # while keeping the extrapolated ends intact.
      vec_extrapolated <- na.approx(vec, na.rm = FALSE)
      
      # Add the extrapolated column to the final data frame.
      df_final[[col_name]] <- vec_extrapolated
    }
    
    # Add any other non-numeric columns (metadata) to the final data frame.
    # We will simply map these as they are not subject to extrapolation.
    non_numeric_cols <- names(df)[!sapply(df, is.numeric)]
    for (col_name in non_numeric_cols) {
      if (col_name != doy_col_name) {
        temp_vec <- rep(NA, 365)
        tryCatch({
          unique_doy_df <- df[!duplicated(df[[doy_col_name]]), ]
          valid_doy_indices <- unique_doy_df[[doy_col_name]] >= 1 & unique_doy_df[[doy_col_name]] <= 365
          temp_vec[unique_doy_df[[doy_col_name]][valid_doy_indices]] <- unique_doy_df[[col_name]][valid_doy_indices]
        }, error = function(e) {
          warning(paste("Error mapping data for non-numeric column", col_name, ". Skipping for this entry:", e$message))
        })
        df_final[[col_name]] <- temp_vec
      }
    }
  }
  
  return(df_final)
}

# Function to interpolate missing values in all numeric columns, modifying in place
interpolate_missing_values <- function(df) {
  if (is.data.frame(df)) { # Check if df is actually a dataframe
    numeric_cols <- sapply(df, is.numeric)   # Identify numeric columns
    # Apply na.approx only to numeric columns, making sure to handle single-column dataframes
    for (col_name in names(df)[numeric_cols]) {
      df[[col_name]] <- na.approx(df[[col_name]], na.rm = FALSE)  # Apply interpolation directly on column
    }
  }
  return(df)
}



#######Functoin for savitzkly golay filter##################
apply_sg_filter <- function(df, order = 5, window_size = 21) {
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

convert_kelvin_to_celsius <- function(df, col_name = "SoilTMP0_10cm_inst") {
  if (col_name %in% names(df)) {
    # Check if median value suggests the data is in Kelvin
    if (median(df[[col_name]], na.rm = TRUE) > 200) {
      df[[col_name]] <- df[[col_name]] - 273.15
    }
  } else {
    warning(paste("Column", col_name, "not found in data frame"))
  }
  return(df)
}
# ==============================================
# removevariables
# ==============================================
rm(meteo_list)
rm(vi_list_gt20)
rm(meteo_summary_list)
rm(meteo_summary_listharvest)
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
  dplyr::filter(nrows < 15) %>%
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
  dplyr::filter(nrows > 15) %>%
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
  dplyr::filter(nrows > 15) %>%
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
                    "extrapolate_to_365_days", "interpolate_missing_values", "apply_sg_filter",
                    "add_siteyeardate", "cols_to_drop", "%>%", "left_join", "na.approx"))

# Apply the negatiDOY# Apply the negative value replacement
vi_list_gt20 <- pblapply(vi_list_gt20, replace_negative_vi, cl = cl)

# 1. Convert Unix to date format (parallel)
vi_list_gt20 <- pblapply(vi_list_gt20, convert_unix_time, cl = cl)
meteo_list <- pblapply(meteo_list, convert_unix_time_meteo, cl = cl)

condition1 <- function(df) {
  df$DOY <- yday(df$Date)
  
  # Mask if (DOY < 100 & kNDVI > 0.2) OR (DOY > 300 & kNDVI > 0.2)
  mask <- ((df$DOY < 100) | (df$DOY > 300)) & df$kNDVI > 0.2
  
  df$kNDVI[mask] <- NA
  df$NDVI[mask]  <- NA
  df$GDVI[mask]  <- NA
  df$IAVI[mask]  <- NA
  df$VARI[mask]  <- NA
  
  return(df)
}
# Condition 2: Remove large jumps in kNDVI
condition2 <- function(df) {
  # Order by DOY first
  df <- df[order(df$DOY), ]
  diffs <- abs(diff(df$kNDVI))
  idx <- which(diffs > 0.25) + 1  # shift index to the second of the pair
  df$kNDVI[idx] <- NA
  return(df)
}

# Condition 3: Detect small valleys (derivative-based)
condition3 <- function(df) {
  df <- df[order(df$DOY), ]
  k <- df$kNDVI
  for (i in 2:(length(k) - 1)) {
    if (!is.na(k[i - 1]) && !is.na(k[i]) && !is.na(k[i + 1])) {
      if ((k[i] - k[i - 1]) <= -0.03 && (k[i + 1] - k[i]) >= 0.03) {
        df$kNDVI[i] <- NA
      }
    }
  }
  return(df)
}

# Condition 4: Skip double harvest, enforce non-increasing after first peak
condition4 <- function(df) {
  df <- df[order(df$DOY), ]
  k <- df$kNDVI
  if (all(is.na(k))) return(df)
  
  peak_idx <- which.max(k)
  if (peak_idx < length(k)) {
    for (i in (peak_idx + 1):length(k)) {
      if (!is.na(k[i]) && !is.na(k[i - 1]) && k[i] > k[i - 1]) {
        k[i] <- k[i - 1]
      }
    }
  }
  
  df$kNDVI <- k
  return(df)
}
vi_list_gt20 <- pblapply(vi_list_gt20, condition1)
vi_list_gt20 <- pblapply(vi_list_gt20, condition2)
vi_list_gt20 <- pblapply(vi_list_gt20, condition3)
vi_list_gt20 <- pblapply(vi_list_gt20, condition4)

# 2. Sort by date (parallel)
vi_list_gt20 <- pblapply(vi_list_gt20, sort_by_date, cl = cl)
meteo_list <- pblapply(meteo_list, sort_by_date, cl = cl)


# Get number of columns for each data frame in the list
column_lengths <- sapply(vi_list_gt20, ncol)
print(column_lengths)
which(column_lengths == 156)
which(column_lengths == 157)
# Find an example with 157 columns
df_157 <- vi_list_gt20[[which(column_lengths == 157)[1]]]
# Find an example with 156 columns
df_156 <- vi_list_gt20[[which(column_lengths == 156)[1]]]
# Compare column names
setdiff(names(df_157), names(df_156))


# 3. Filter to growing season (parallel)
#vi_list_gt20 <- pblapply(vi_list_gt20, filter_march_to_october, cl = cl)
# 4. Convert to daily resolution (parallel)
vi_list_gt20 <- pblapply(vi_list_gt20, make_daily, cl = cl)
meteo_list <- pblapply(meteo_list, make_daily, cl = cl)

# 5. Interpolate missing values
vi_list_gt20 <- pblapply(vi_list_gt20, interpolate_missing_values, cl = cl)
meteo_list <- pblapply(meteo_list, interpolate_missing_values, cl = cl)


vi_list_gt20 <- pblapply(vi_list_gt20, extrapolate_to_365_days, cl = cl)
plot(vi_list_gt20[[90]]$DOY, vi_list_gt20[[90]]$kNDVI)


# With progress bar and parallel processing
vi_list_gt20 <- pblapply(vi_list_gt20, apply_sg_filter, cl = cl)

#convert meteo_list of soil temp from kelvin to celsius
meteo_list <- lapply(meteo_list, convert_kelvin_to_celsius)

# Clean up
#closeAllConnections()
stopCluster(cl)
plan(sequential)  # Return to sequential processing
vi_list_gt20[[401]]$DOY
vi_list_gt20[[401]]$kNDVI
#plot(vi_list_gt20[[401]]$DOY, vi_list_gt20[[401]]$kNDVI)
# ==============================================
# VISUALIZATION EXAMPLE
# ==============================================
plot(vi_list_gt20[[1]]$Date, vi_list_gt20[[1]]$kNDVI)
plot(meteo_list[[1]]$Date, meteo_list[[1]]$Ec)

#-----------------------------------------------------
#Remove unrealistic HDDOY values HDDOY = 359/360
#-----------------------------------------------------
vi_list_gt20 <- lapply(vi_list_gt20, function(df) {
  if ("HDDOY" %in% names(df)) {
    df$HDDOY[df$HDDOY > 350] <- NA
  }
  return(df)
})

get_max_HDDOY <- function(vi_list) {
  hddoy_values <- sapply(vi_list, function(df) {
    if ("HDDOY" %in% names(df)) max(df$HDDOY, na.rm = TRUE) else NA
  })
  print(max(hddoy_values, na.rm = TRUE))
}
get_max_HDDOY(vi_list_gt20)


#-------------------------------------------------------
#Calculate GDD of each dataframe 
#-------------------------------------------------------
meteo_list <- pblapply(meteo_list, calculate_gdd_cumulative, cl = detectCores() - 1)
plot(meteo_list[[1]]$Date, meteo_list[[1]]$cumulative_gdd)
plot(meteo_list[[1]]$Date, meteo_list[[1]]$gdd)

#--------------------------------------------------------
#Fill missing FIELD_NAME consistently
#---------------------------------------------------------
fill_field_name <- function(df) {
  valid_names <- na.omit(unique(df$FIELD_NAME))
  if (length(valid_names) == 1) {
    df <- df %>%
      mutate(FIELD_NAME = ifelse(is.na(FIELD_NAME), valid_names[1], FIELD_NAME))
  } else if (length(valid_names) > 1) {
    warning("Multiple FIELD_NAME values found - no filling done")
  }
  return(df)
}

# Apply to all dataframes
vi_list_gt20 <- lapply(vi_list_gt20, fill_field_name)




#-------------------------------------------------------
# Merge VI (Vegetation Index) and Meteorological Data
#-------------------------------------------------------

# Convert Date in vi_list_gt20 to Date class (CST timezone)
vi_list_gt20 <- lapply(vi_list_gt20, function(df) {
  if ("Date" %in% names(df) && any(!is.na(df$Date))) {
    # convert numeric to Date first
    if (is.numeric(df$Date)) {
      df$Date <- as.Date(df$Date, origin = "1970-01-01")
    }
    
    # find first and last non-NA date
    first_date <- min(df$Date, na.rm = TRUE)
    last_date  <- max(df$Date, na.rm = TRUE)
    
    # build full sequence of dates with same length as df
    full_seq <- seq(first_date - (which(!is.na(df$Date))[1] - 1),
                    last_date + (nrow(df) - tail(which(!is.na(df$Date)), 1)),
                    by = "day")
    
    # replace Date column with full sequence
    df$Date <- full_seq[seq_len(nrow(df))]
  }
  return(df)
})




# Convert Date in meteo_list to Date class if not already
meteo_list <- lapply(meteo_list, function(df) {
  if (!inherits(df$Date, "Date")) {  # Check if Date class
    df$Date <- as.Date(df$Date)       # Convert character to Date
  }
  return(df)
})

# Check that both lists have the same length
(length(vi_list_gt20) == length(meteo_list))

# Extract filenames from the original CSV lists
vi_names <- basename(vi_csv_files_gt20)       # VI files
meteo_names <- basename(meteo_csv_files)      # Meteorological files

# Remove file extensions to create keys
vi_keys_raw <- tools::file_path_sans_ext(vi_names)        # e.g., Baker_20_2020_VI
meteo_keys_raw <- tools::file_path_sans_ext(meteo_names)  # e.g., Baker_20_2020_Meteo

# Standardize keys for matching
vi_keys <- vi_keys_raw %>% stringr::str_replace("_VI_", "_")          # Baker_20_2020
meteo_keys <- meteo_keys_raw %>% stringr::str_replace("_Meteo", "_")  # Baker_20_2020

# Assign names to the lists for easier matching
names(vi_list_gt20) <- vi_keys
names(meteo_list) <- meteo_keys

# Find common keys between VI and meteo lists
common_keys <- intersect(vi_keys, meteo_keys)  # Keep only matched fields
length(common_keys)  # Check number of matches

# Filter lists to matched keys
vi_matched <- vi_list_gt20[common_keys]
meteo_matched <- meteo_list[common_keys]

# Ensure order and names match
stopifnot(identical(names(vi_matched), names(meteo_matched)))

# Add site-year-date column for merging
vi_matched <- Map(add_siteyeardate, vi_matched, names(vi_matched))
meteo_matched <- Map(add_siteyeardate, meteo_matched, names(meteo_matched))

# Function to safely left join on 'siteyeardate' avoiding column suffixes
left_join_by_siteyeardate <- function(df1, df2) {
  if (is.data.frame(df1) && is.data.frame(df2)) {
    overlap_cols <- intersect(names(df1), names(df2))        # Find overlapping columns
    overlap_cols <- setdiff(overlap_cols, "siteyeardate")    # Exclude join key
    df2 <- df2[, !(names(df2) %in% overlap_cols), drop = FALSE]  # Remove overlaps from df2
    merged <- dplyr::left_join(df1, df2, by = "siteyeardate")   # Merge safely
    return(merged)
  }
  return(df1)
}

# Apply merge for all matched VI and meteo dataframes
merged_list <- Map(left_join_by_siteyeardate, vi_matched, meteo_matched)

# Add Field_Year column based on FIELD_NAME and year
merged_list <- lapply(merged_list, function(df) {
  year <- format(as.Date(df$Date), "%Y")
  df$Field_Year <- ifelse(
    is.na(df$FIELD_NAME),
    NA,
    paste0(df$FIELD_NAME, "_", year)
  )
  return(df)
})
vi_list_gt20 <- merged_list  # Update main list

# Check number of rows per dataframe
rows_per_df <- sapply(merged_list, function(x) if (is.data.frame(x)) nrow(x) else NA)
print(rows_per_df)

# Keep only dataframes with <=365 rows (remove longer than a year)
fields_over_365 <- names(rows_per_df[rows_per_df > 365])
merged_list <- merged_list[!(names(merged_list) %in% fields_over_365)]
vi_list_gt20 <- merged_list

# Fill down Field_Year within each dataframe
vi_list_gt20 <- lapply(vi_list_gt20, function(df) {
  df %>% tidyr::fill(Field_Year, .direction = "downup")  # Fill NA top/bottom
})



# #---------------------------------------------------------
# #Plot kNDVI figures for visual inspection
# #---------------------------------------------------------
# # Set output directory
# out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/kNDVIcheck"
# dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# 
# for (i in seq_along(vi_list_gt20)) {
#   df <- vi_list_gt20[[i]]# Loop through vi_list_gt20 and save plots as JPEG
#   # Check for required columns
#   if (!all(c("doy", "kNDVI", "FIELD_NAME", "YEAR") %in% names(df))) next
#   # Get field name and year
#   field_name <- df$FIELD_NAME[1]
#   year <- unique(df$YEAR)
#   # Handle missing or inconsistent metadata
#   if (is.na(field_name) || field_name == "") field_name <- paste0("UnknownField_", i)
#   if (length(year) != 1 || is.na(year)) year <- "UnknownYear"
#   
#   # Define JPEG filename with field name and year
#   jpeg_filename <- file.path(out_dir, paste0(field_name, "_", year, ".jpg"))
#   
#   # Create ggplot
#   p <- ggplot(df, aes(x = doy, y = kNDVI)) +
#     geom_line(color = "darkgreen", size = 1) +
#     labs(title = paste("kNDVI Plot -", field_name, "(", year, ")"),
#          x = "Day of Year (DOY)",
#          y = "kNDVI") +
#     theme_minimal()
#   
#   # Save plot as JPEG
#   ggsave(filename = jpeg_filename, plot = p, width = 8, height = 6, dpi = 300)
# }

colnames(meteo_list$Baker_20_2015)
colnames(meteo_list$Baker_20_2016)
colnames(meteo_list$Baker_20_2017)
colnames(meteo_list$B5_Farm_5_2018)

vi_list_gt20[[1]]$PDDOY

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
