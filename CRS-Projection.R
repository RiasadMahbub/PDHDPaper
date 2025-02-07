library(sf)

# Function to print CRS and projection of a shapefile
print_crs_projection <- function(shapefile_path) {
  # Read the shapefile
  shapefile <- st_read(shapefile_path)
  
  # Get the CRS and projection
  crs <- st_crs(shapefile)
  
  # Print the shapefile name, CRS and projection information
  cat("Shapefile:", shapefile_path, "\n")
  if (!is.null(crs)) {
    cat("CRS EPSG Code:", crs$epsg, "\n")
    cat("CRS proj4string:", crs$proj4string, "\n\n")
  } else {
    cat("CRS: Not available\n\n")
  }
}

# Directory containing the shapefiles
shapefile_directory <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllShapefile"

# List all shapefiles in the directory
shapefile_list <- list.files(shapefile_directory, pattern = "\\.shp$", full.names = TRUE)

# Apply the function to each shapefile in the list
lapply(shapefile_list, print_crs_projection)





library(sf)

# Function to extract CRS and projection of a shapefile
extract_crs_projection <- function(shapefile_path) {
  # Read the shapefile
  shapefile <- st_read(shapefile_path)
  
  # Get the CRS and projection
  crs <- st_crs(shapefile)
  
  # Extract the year from the shapefile name (assuming the year is in the name)
  shapefile_name <- basename(shapefile_path)
  year <- sub(".*?(\\d{4}).*", "\\1", shapefile_name)
  
  # Return a list with the year, CRS EPSG code, and CRS proj4string
  list(
    Year = year,
    CRS_EPSG_Code = ifelse(!is.null(crs$epsg), crs$epsg, NA),
    CRS_proj4string = ifelse(!is.null(crs$proj4string), crs$proj4string, NA)
  )
}

# Directory containing the shapefiles
shapefile_directory <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllShapefile"

# List all shapefiles in the directory
shapefile_list <- list.files(shapefile_directory, pattern = "\\.shp$", full.names = TRUE)

# Apply the function to each shapefile in the list and store the results in a data frame
shapefile_info <- do.call(rbind, lapply(shapefile_list, function(x) as.data.frame(extract_crs_projection(x))))

# Print the table using knitr
library(knitr)
kable(shapefile_info, format = "markdown", col.names = c("Year", "CRS EPSG Code", "CRS proj4string"))


##### CDL LAYER PROJECTION"""" 

# Install and load necessary packages
# Install and load necessary packages
# Load necessary libraries
library(terra)
library(sf)
library(knitr)

# Function to extract CRS and projection of a raster file
extract_crs_projection <- function(raster_path) {
  # Read the raster file
  raster_data <- rast(raster_path)
  
  # Get the CRS and projection information
  crs_info <- crs(raster_data, describe = TRUE)
  crs_proj4string <- crs(raster_data)
  crs_epsg_code <- crs_info$epsg
  
  # Extract the year from the raster file name (assuming the year is in the name)
  raster_name <- basename(raster_path)
  year <- substr(raster_name, 1, 4)
  
  # Return a list with the year, CRS EPSG code, and CRS proj4string
  list(
    Year = year,
    CRS_EPSG_Code = ifelse(!is.null(crs_epsg_code), crs_epsg_code, NA),
    CRS_proj4string = ifelse(!is.null(crs_proj4string), crs_proj4string, NA)
  )
}

# Directory containing the .tif files
raster_directory <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/RasterData/CDLData"

# List all .tif files in the directory
raster_list <- list.files(raster_directory, pattern = "\\.tif$", full.names = TRUE)

# Apply the function to each .tif file in the list and store the results in a data frame
raster_info <- do.call(rbind, lapply(raster_list, function(x) as.data.frame(extract_crs_projection(x))))

# Print the table using knitr
kable(raster_info, format = "markdown", col.names = c("Year", "CRS EPSG Code", "CRS proj4string"))




###################
library(terra)
library(sf)
library(knitr)

# Function to extract CRS and projection of a raster file
extract_crs_projection <- function(raster_path) {
  # Read the raster file
  raster_data <- rast(raster_path)
  
  # Get the CRS and projection
  crs_info <- crs(raster_data)
  crs_proj4string <- proj4string(raster_data)
  crs_epsg_code <- crs_info$epsg
  
  # Extract the year from the raster file name (assuming the year is in the name)
  raster_name <- basename(raster_path)
  year <- substr(raster_name, 1, 4)
  
  # Return a list with the year, CRS EPSG code, and CRS proj4string
  list(
    Year = year,
    CRS_EPSG_Code = ifelse(!is.null(crs_epsg_code), crs_epsg_code, NA),
    CRS_proj4string = ifelse(!is.null(crs_proj4string), crs_proj4string, NA)
  )
}

# Directory containing the .tif files
raster_directory <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/RasterData/CDLData"

# List all .tif files in the directory
raster_list <- list.files(raster_directory, pattern = "\\.tif$", full.names = TRUE)

# Apply the function to each .tif file and store the results in a data frame
raster_info <- do.call(rbind, lapply(raster_list, extract_crs_projection))

# Print the table using knitr
kable(raster_info, format = "markdown", col.names = c("Year", "CRS EPSG Code", "CRS proj4string"))

