# Load necessary libraries
library(sf)
library(dplyr)
library(stringr)
library(dplyr)   # For data manipulation
library(signal)  # For Savitzky-Golay filter
library(ggplot2) # For plotting
# Load necessary libraries
library(zoo)      # For na.approx (linear interpolation)
library(signal)   # For sgolayfilt (Savitzky-Golay filter)
################################
# Set the directory path########
################################
dir_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/SatelliteData/HarmonizedData/2019/HarmonizedLandsatSentinel"
# List all .shp files in the dir6ectory
shapefiles <- list.files(path = dir_path, pattern = "\\.shp$", full.names = TRUE)
# Extract and print the names of the shapefiles (without full path)
shp_file_names <- basename(shapefiles)
# Read each shapefile and convert it to a data frame (optional if you need the data)
shp_data_list <- lapply(shapefiles, function(shp) {
  shp_data <- st_read(shp)  # Read the shapefile
  as.data.frame(shp_data)   # Convert to data frame
})

# Optionally, you can assign data frames to variables named by the shapefile name (without the .shp extension)
names(shp_data_list) <- sub("\\.shp$", "", shp_file_names)

# Print a preview of the first shapefile's data frame
head(shp_data_list[[1]])

# List the number of shapefiles read
num_shp_files <- length(shp_data_list)
cat("Number of shapefiles read: ", num_shp_files, "\n")

########Number of Columns########################
# Function to read shapefile and return number of columns
get_num_columns <- function(filename) {
  # Read shapefile using sf package
  shp <- st_read(filename)
  # Return the number of columns in the shapefile
  return(ncol(shp))
}
# Apply the function to all shapefiles and print the results
num_columns <- sapply(shapefiles, function(file) get_num_columns(file))
# Print the number of columns for each file
for (i in 1:length(shp_file_names)) {
  cat(paste(shp_file_names[i], "has", num_columns[i], "columns.\n"))
}

# Function to read shapefile and return the number of rows
get_num_rows <- function(filename) {
  # Read shapefile using sf package
  shp <- st_read(filename)
  
  # Return the number of rows in the shapefile
  return(nrow(shp))
}
# Apply the function to all shapefiles and print the results
num_rows <- sapply(shapefiles, function(file) get_num_rows(file))
# Print the number of rows for each file
for (i in 1:length(shp_file_names)) {
  cat(paste(shp_file_names[i], "has", num_rows[i], "rows.\n"))
}

########NUMBER OF LENGTH########################
# Function to return the length of the filename
get_filename_length <- function(filename) {
  # Return the number of characters in the filename
  return(nchar(filename))
}

# Apply the function to all shapefiles and store the results
filename_lengths <- sapply(shapefiles, function(file) get_filename_length(file))
# Print the length of the filename for each file
for (i in 1:length(shp_file_names)) {
  cat(paste(shp_file_names[i], "filename has", filename_lengths[i], "characters.\n"))
}

# Read the first shapefile to get column names
example_shp <- st_read(shapefiles[1], quiet = TRUE)  # Read first shapefile
column_names <- names(example_shp)  # Extract column names
num_rows <- nrow(example_shp)       # Get number of rows

# Extract dates from shapefile names using regex
dates <- str_extract(basename(shapefiles), "\\d{8}")  # Extracts the 8-digit date part
dates <- as.Date(dates, format="%Y%m%d")              # Convert to Date format

# Define the number of shapefiles
num_shapefiles <- length(shapefiles)
# Create a list of 28 empty data frames (one for each column)
df_list <- lapply(column_names, function(col_name) {
  df <- data.frame(matrix(ncol = 1, nrow = num_shapefiles))  # Create empty df with 76 rows
  colnames(df) <- col_name  # Name the single column
  return(df)
})

# Create an empty data frame for each column in the shapefile
for (col_name in column_names) {
  df_list[[col_name]] <- data.frame(matrix(NA, nrow = num_rows, ncol = length(dates)))
  colnames(df_list[[col_name]]) <- dates  # Set column names as the extracted dates
}

# Assign names to the list elements (each data frame is named after a column)
names(df_list) <- column_names

# Fill df_list with data for each column
for (i in seq_along(shp_data_list)) {
  for (col_name in column_names) {
    if (col_name %in% names(shp_data_list[[i]])) {  # Check if column exists in the file
      df_list[[col_name]][, i] <- shp_data_list[[i]][[col_name]]  # Assign all rows
    }
  }
}


extract_date <- function(image_id) {
  if (grepl("LE07|LT05|LC08", image_id)) {
    # Extract last 8 digits for Landsat-style IDs
    return(sub(".*_(\\d{8})$", "\\1", image_id))
  } else {
    # Extract first 8 digits after the first underscore for Sentinel-style IDs
    return(sub("^\\d+_(\\d{8})T.*", "\\1", image_id))
  }
}

# Apply extract_date function to all elements in shp_data_list
shp_data_list <- lapply(shp_data_list, function(df) {
  df$date <- sapply(df$image_id, extract_date)
  return(df)
})

# Print to verify
print(shp_data_list[[1]]$date)
print(shp_data_list[[76]]$date)
shp_data_list[[1]]$swir1
shp_data_list[[76]]$image_id

length(shp_data_list)
ncol(shp_data_list[[1]])
nrow(shp_data_list[[1]])

# Step 1: Create a list of dataframes based on the given conditions
df_list <- lapply(1:ncol(shp_data_list[[1]]), function(i) {
  data.frame(matrix(nrow = length(shp_data_list), 
                    ncol = nrow(shp_data_list[[1]])
                    ))
})

# Fill the rows in df_list with FARM_NAME and the extracted Date
df_list <- lapply(seq_along(df_list), function(i) {
  # Extract the date from image_id for each entry in shp_data_list and assign it to the "Date" column
  df_list[[i]]$Date <- sapply(1:length(shp_data_list), function(i) extract_date(shp_data_list[[i]]$image_id[[1]]))
  return(df_list[[i]])
})
for (i in 1:length(shp_data_list)) {
  for (j in 1:length(shp_data_list[[i]]$ATSAVI)) {
    # Assign the ATSAVI value to the specific cell in df_list[[i]]
    df_list[[1]][i, "NDVId1"] <- shp_data_list[[i]]$NDVI[j]
  }
}

# Convert the 'Date' column to Date format
df_list[[1]]$Date <- as.Date(as.character(df_list[[1]]$Date), format = "%Y%m%d")

# Plot Date vs NDVI using ggplot2
library(ggplot2)
library(signal)

ggplot(df_list[[1]], aes(x = Date, y = NDVId1)) +
  geom_point(size = 3) +  # Use a line plot for continuous data
  labs(title = "Date vs NDVI", x = "Date", y = "NDVI") +
  theme_minimal()

# Load necessary libraries


# Ensure Date is in Date format
df_list[[1]] <- df_list[[1]] %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)  # Sort by Date

# Interpolate missing values in NDVId1
df_list[[1]]$NDVId1_interpolated <- na.approx(df_list[[1]]$NDVId1, na.rm = FALSE)




# Assuming df_list[[1]]$NDVId1 is your data frame with time series for the 'NDVId1' band
# If df_list[[1]]$NDVId1 is a vector, you can still apply the function directly.
# Define the Savitzky-Golay filter parameters
order <- 3         # Polynomial degree
window_size <- 15  # Window size (must be odd and greater than order)

# Apply the Savitzky-Golay filter to the interpolated NDVId1 band
df_list[[1]]$NDVId1_sg <- sgolayfilt(df_list[[1]]$NDVId1_interpolated, p = order, n = window_size)

plot(df_list[[1]]$NDVId1_sg )
df_list[[1]]$Date

library(ggplot2)

ggplot(df_list[[1]], aes(x = Date)) +
  geom_point(aes(y = NDVId1, color = "Original NDVI"), linetype = "dashed", size = 1) +
  geom_point(aes(y = NDVId1_interpolated, color = "Interpolated NDVI"), size = 1) +
  geom_line(aes(y = NDVId1_sg, color = "Smoothed NDVI"), size = 1) +
  labs(
    title = "NDVI Time Series",
    x = "Date",
    y = "NDVI",
    color = "Legend"
  ) +
  scale_color_manual(values = c(
    "Original NDVI" = "#d73027",
    "Interpolated NDVI" = "#1f78b4",
    "Smoothed NDVI" = "#33a02c"
  )) +
  theme_minimal()
plot(df_filtered$NDVId1_sg)

#############################
########INDEXING FOR PCA####
############################
# After filling data, sort the columns by date for each dataframe
# Create a new dataframe with only NDVId1_sg and Date
df_filtered <- df_list[[1]] %>%
  select(NDVId1_sg)
df_filtered
pr.out = prcomp(df_filtered, scale=FALSE)
print(pr.out)
# biplot:
biplot(pr.out,scale=0)

