# ==============================================
# REVISED DATA TRANSFORMATION PIPELINE
# ==============================================

# Your existing code to this point...
# ...
# Temporarily switching from pblapply to lapply with error handling
# This will help us find which file is causing the invalid connection error
vi_list_gt20_updated <- list()

for (i in seq_along(vi_list_gt20)) {
  df <- vi_list_gt20[[i]]
  file_name <- basename(vi_csv_files_gt20[i])
  
  tryCatch({
    # Apply the make_daily function and store the result
    daily_df <- make_daily(df)
    vi_list_gt20_updated[[i]] <- daily_df
    cat(paste("Successfully processed:", file_name, "\n"))
  }, error = function(e) {
    # Print the error message and the name of the file
    cat(paste("Error processing file:", file_name, "\n"))
    cat(paste("Error message:", e$message, "\n"))
    vi_list_gt20_updated[[i]] <- df # Keep the original dataframe in case of error
  })
}

# Replace the original list with the updated one
vi_list_gt20 <- vi_list_gt20_updated

# Now, we also wrap the next step in a tryCatch loop
vi_list_gt20_updated_2 <- list()

for (i in seq_along(vi_list_gt20)) {
  df <- vi_list_gt20[[i]]
  file_name <- basename(vi_csv_files_gt20[i])
  
  tryCatch({
    # Apply the interpolate_and_extend_to_zero function
    interpolated_df <- interpolate_and_extend_to_zero(df)
    vi_list_gt20_updated_2[[i]] <- interpolated_df
    cat(paste("Successfully interpolated:", file_name, "\n"))
  }, error = function(e) {
    cat(paste("Error during interpolation for file:", file_name, "\n"))
    cat(paste("Error message:", e$message, "\n"))
    vi_list_gt20_updated_2[[i]] <- df # Keep the original dataframe in case of error
  })
}

# Replace the list again with the newly processed one
vi_list_gt20 <- vi_list_gt20_updated_2


# FINAL STEP: Use lapply to debug the SG filter step
vi_list_gt20_updated_3 <- list()

for (i in seq_along(vi_list_gt20)) {
  df <- vi_list_gt20[[i]]
  file_name <- basename(vi_csv_files_gt20[i])
  
  tryCatch({
    # Apply the apply_sg_filter function
    filtered_df <- apply_sg_filter(df)
    vi_list_gt20_updated_3[[i]] <- filtered_df
    cat(paste("Successfully filtered:", file_name, "\n"))
  }, error = function(e) {
    cat(paste("Error during SG filter for file:", file_name, "\n"))
    cat(paste("Error message:", e$message, "\n"))
    vi_list_gt20_updated_3[[i]] <- df # Keep the original dataframe in case of error
  })
}

# Replace the list one more time with the final processed list
vi_list_gt20 <- vi_list_gt20_updated_3

# The rest of your code
# (Note: pblapply for this step is commented out to allow for debugging)
# vi_list_gt20 <- pblapply(vi_list_gt20, apply_sg_filter, cl = cl)



# Loop over the list
for(i in seq_along(vi_list_gt20)) {
  cat("Data frame", i, "\n")
  
  df <- vi_list_gt20[[i]]
  
  # Check which DOY column exists
  if("DOY" %in% names(df)) {
    doy_col <- df$DOY
  } else if("doy" %in% names(df)) {
    doy_col <- df$doy
  } else {
    cat("No DOY column in this data frame.\n\n")
    next
  }
  
  # Print DOY values
  print(doy_col)
  
  # Print number of unique DOYs
  cat("Number of unique DOYs:", length(unique(doy_col)), "\n\n")
}


interpolate_and_extend_to_zero <- function(df) {
  # Initialize a new, empty data frame with 365 rows
  full_doy <- 1:365
  df_final <- data.frame(doy = full_doy, interpolated_value = NA)
  
  # Check if the input is a data frame and not empty
  if (is.data.frame(df) && nrow(df) > 0) {
    # Assuming the first column is the DOY and the second is the value to be interpolated.
    # We will only process the first numeric column for this logic.
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    if (length(numeric_cols) > 0) {
      # The function will focus on the first numeric column for interpolation
      value_col_name <- numeric_cols[1]
      
      # Create a new vector with NAs for all 365 days
      vec <- rep(NA, 365)
      
      # Map the existing data from the data frame to the new 365-day vector.
      # We assume the DOY values are in the first column of the dataframe.
      doy_col_name <- names(df)[1]
      
      # Check if a valid DOY column exists
      if (doy_col_name == "doy" || doy_col_name == "DOY" || class(df[[doy_col_name]]) == "integer") {
        # This try-catch block helps handle cases where DOY values are out of bounds (1-365)
        tryCatch({
          # Make sure the DOY values are within the valid range before mapping
          valid_doy <- df[[doy_col_name]] >= 1 & df[[doy_col_name]] <= 365
          vec[df[[doy_col_name]][valid_doy]] <- df[[value_col_name]][valid_doy]
        }, error = function(e) {
          # If there is an error mapping, we'll just leave the vector as NA
          # and log a warning to the console.
          warning(paste("Error mapping data for a data frame. Skipping interpolation for this entry:", e$message))
        })
      } else {
        # If no DOY column is found, check if the data frame has a consistent length.
        if (nrow(df) == 365) {
          # If it has 365 rows, we can map directly by index.
          vec[1:nrow(df)] <- df[[value_col_name]]
        } else {
          # If the data frame is an inconsistent length, we cannot reliably map the values.
          # The function will return the empty 365-row data frame initialized at the start.
          warning("Input data frame has inconsistent row count and no 'doy' column. Returning an empty 365-row data frame.")
        }
      }
      
      # Explicitly set the first and last day of the year to 0 for extrapolation.
      # This handles cases where data does not start at DOY 1 or end at DOY 365.
      # Only set to 0 if the value is currently NA.
      if (is.na(vec[1])) { vec[1] <- 0 }
      if (is.na(vec[365])) { vec[365] <- 0 }
      
      # Perform the interpolation and extrapolation on the full 365-day vector.
      # na.approx with rule=2 will handle the remaining NAs by using the nearest non-NA values.
      vec_interpolated <- na.approx(vec, na.rm = FALSE, rule = 2)
      
      # Update the final data frame with the newly interpolated values
      df_final$interpolated_value <- vec_interpolated
    }
  }
  
  # Always return a data frame with 365 rows and consistent columns
  return(df_final)
}




#----------------------------------------------------
###Missing columns in the merged list 
#----------------------------------------------------

# All columns your summarise needs
required_cols <- c("tmean", "gdd", "srad", "GDVI", "Lai", "vpd",
                   "tmin", "tmax", "avgRH", "SoilTMP0_10cm_inst",
                   "SOC_avg_0_30cm", "Clay_avg_0_30cm")

# Find which dataframes are missing any
# The loop now directly iterates over the names of the merged_list,
# ensuring the check is always performed on the correct data.
missing_cols <- lapply(names(merged_list), function(field_id) {
  # Get the current list element using its name
  current_list <- merged_list[[field_id]]
  
  # Get all names from the current list element.
  # This correctly captures columns that are part of a dataframe
  # AND columns that are standalone vectors within the list.
  all_names <- names(current_list)
  
  # Columns not present in this dataframe
  missing <- setdiff(required_cols, all_names)
  
  if (length(missing) > 0) {
    return(data.frame(Field_ID = field_id,
                      Missing_Columns = paste(missing, collapse=", ")))
  } else {
    return(NULL)
  }
})

missing_cols <- do.call(rbind, missing_cols)
missing_cols

missing_cols <- do.call(rbind, missing_cols)
missing_cols


# list of soil indices
soil_indices <- c("BI","BITM","BIXS","BaI","DBSI","EMBI","MBI",
                  "NDSoI","NSDS","NSDSI1","NSDSI2","NSDSI3")
# Identify which dataframes are missing any soil index
# Identify which dataframes are missing any soil index
missing_soil_indices <- lapply(names(merged_list), function(field_id) {
  df <- merged_list[[field_id]]
  missing_cols <- setdiff(soil_indices, colnames(df))
  
  if(length(missing_cols) > 0) {
    return(data.frame(
      Field_ID = field_id,
      Missing_Columns = paste(missing_cols, collapse = ", ")
    ))
  } else {
    return(NULL)
  }
})

# Combine results into one dataframe and remove NULLs
missing_soil_indices_df <- do.call(rbind, missing_soil_indices)

# View the result
missing_soil_indices_df

# Define the directory
vi_data_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Data/PDHDVI"

# Get a list of all CSV files
vi_csv_files <- list.files(vi_data_dir, pattern = "\\.csv$", full.names = TRUE)

# This loop will print the name of the problematic file when it fails
for (file in vi_csv_files) {
  cat("Checking file:", file, "\n")
  tryCatch({
    # Try to read the file with fread
    data.table::fread(file, nrows = 10)
  }, error = function(e) {
    # If an error occurs, print the file name and the error message
    cat("Error found in file:", file, "\n")
    stop("Problematic file found. See above for the name.")
  })
}

cat("All files appear to be valid.\n")



# Check each dataframe in the list for presence of nir and green columns
diagnostic <- lapply(names(vi_list_gt20), function(name) {
  df <- vi_list_gt20[[name]]
  cols <- colnames(df)
  list_name <- name
  has_nir <- "nir" %in% cols
  has_green <- "green" %in% cols
  return(data.frame(
    Dataframe = list_name,
    Has_nir = has_nir,
    Has_green = has_green
  ))
})

# Combine into one summary table
diagnostic_summary <- do.call(rbind, diagnostic)
diagnostic_summary

# Filter out dataframes missing either nir or green
missing_bands <- diagnostic_summary %>%
  dplyr::filter(!(Has_nir & Has_green))

missing_bands



#-------------------------------------------------------------------
#Problematic file
#------------------------------------------------------------------
library(pbapply)
library(data.table)

safe_fread <- function(f) {
  tryCatch(
    {
      # attempt to read, but capture warnings
      out <- withCallingHandlers(
        fread(f),
        warning = function(w) {
          message("⚠️ Warning in file: ", f, " → ", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      return(out)
    },
    error = function(e) {
      message("❌ Error in file: ", f, " → ", conditionMessage(e))
      return(NULL)
    }
  )
}

vi_list <- pblapply(vi_csv_files, safe_fread, cl = parallel::detectCores() - 1)

library(data.table)

fix_bad_csv <- function(file, overwrite = FALSE) {
  # Read header only
  header <- strsplit(readLines(file, n = 1), ",")[[1]]
  
  # Read full data (no header)
  dat <- fread(file, header = FALSE)
  
  # If header count mismatches columns, fix
  if (length(header) != ncol(dat)) {
    message("⚠️ Mismatch in file: ", basename(file))
    message("   Header cols: ", length(header), " | Data cols: ", ncol(dat))
    # Assign generic names
    new_names <- paste0("V", seq_len(ncol(dat)))
    setnames(dat, new_names)
    message("   → Replaced with names: ", paste(new_names, collapse = ", "))
  } else {
    # Use header as is
    setnames(dat, header)
  }
  
  # Optionally overwrite file with fixed version
  if (overwrite) {
    fwrite(dat, file)
    message("✅ Overwritten fixed file: ", file)
  }
  
  return(dat)
}

# Apply across all files
vi_list <- pblapply(vi_csv_files, fix_bad_csv, cl = parallel::detectCores() - 1)

file <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Data/PDHDVI/Dewitt_2_VI_2023.csv"

# Show header
header <- readLines(file, n = 1)
cat("Header:\n", header, "\n\n")

# Show first few data rows
cat("First 3 rows of data:\n")
print(readLines(file, n = 4)[-1])

# Count number of columns in header vs. first row of data
header_cols <- length(strsplit(header, ",")[[1]])
data_cols <- length(strsplit(readLines(file, n = 2)[2], ",")[[1]])

cat("\nNumber of header columns:", header_cols, "\n")
cat("Number of data columns:", data_cols, "\n")
