library(terra)

# File path to the raster
raster_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/RasterData/CDLData/2019_30m_cdls.tif"

# Read the raster file
raster_data <- rast(raster_path)

# Print summary of the raster
print(raster_data)

# Path to the Arkansas shapefile
arkansas_shapefile_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/ArkansasBoundaryShp/ArkansasBoundaryshp.shp"


# Read the Arkansas shapefile
arkansas_shapefile <- st_read(arkansas_shapefile_path)

#raster_projected <- project(raster_data, crs(arkansas_shapefile))
raster_clipped <- crop(raster_data, vect(arkansas_shapefile))
raster_clipped <- mask(raster_clipped, vect(arkansas_shapefile))


##################################################################
##################CDL Layer Conversion###########################
#### Reproject raster#####################
# Load required libraries
# Load required libraries
library(terra)

# Directory containing the .tif files
raster_directory <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/RasterData/CDLData"

# List all .tif files in the directory
raster_list <- list.files(raster_directory, pattern = "\\.tif$", full.names = TRUE)

# Function to reproject a raster to EPSG:4326
reproject_to_wgs84 <- function(raster_path) {
  # Read the raster file
  raster_data <- rast(raster_path)
  
  # Reproject the raster to EPSG:4326
  raster_reprojected <- project(raster_data, "EPSG:4326", method = "near")
  
  return(raster_reprojected)
}

# Read and reproject all the raster files
reprojected_rasters <- lapply(raster_list, reproject_to_wgs84)

# Print the reprojected raster objects to confirm they are processed correctly
print(reprojected_rasters)



#########Clip Raster#################


rast2023<-rast("C:/Users/rbmahbub/Documents/Data/GeospatialData/RasterData/CDLData/2023_30m_cdls.tif")
terra::project(rast2023, "EPSG:4326", method = "near")



##########################################################
### Clipping GlobBoundary layer by Rice CDL Layer ##########
###############################################################
# Load required libraries
library(sf)
library(dplyr)
sf_use_s2(FALSE)

# Path to the GloCAB field boundaries shapefile
globcarb_shapefile_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/GloCAB-CroplandFieldBoundary/GloCAB_Field_Boundaries-ClippedbyArkansas/GloCAB_Field_Boundaries_Ark.shp"
# Read the shapefile
globcarb_shapefile <- st_read(globcarb_shapefile_path)

# Directory containing the Arkansas Rice shapefiles
rice_shapefiles_directory <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllShapefile"

# Output directory for cropped shapefiles
output_directory <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/GloCAB-CroplandFieldBoundary/GloCAb_FieldBoundaries-CLippedbyArkRice2008-2020-individual"
# Ensure the output directory exists
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

# Function to process each rice shapefile
process_rice_shapefile <- function(year) {
  rice_shapefile_path <- file.path(rice_shapefiles_directory, paste0("ArkansasRice", year, "_Shapefile_corrected.shp"))
  
  # Check if the file exists
  if (file.exists(rice_shapefile_path)) {
    # Read the rice shapefile
    rice_shapefile <- st_read(rice_shapefile_path)
    
    # Transform rice shapefile CRS to match GloCAB shapefile CRS if necessary
    if (st_crs(globcarb_shapefile) != st_crs(rice_shapefile)) {
      rice_shapefile <- st_transform(rice_shapefile, st_crs(globcarb_shapefile))
    }
    
    # Check if their CRS is the same
    if (st_crs(globcarb_shapefile) == st_crs(rice_shapefile)) {
      # Crop the GloCAB shapefile using the rice shapefile
      cropped <- st_intersection(globcarb_shapefile, rice_shapefile)
      
      # Path to save the cropped shapefile
      output_path <- file.path(output_directory, paste0("GloCAB_Field_Boundaries_Ark_Rice", year, ".shp"))
      
      # Write the cropped shapefile to disk
      st_write(cropped, output_path)
    } else {
      warning(paste("CRS of the shapefiles do not match and transformation failed for year", year))
    }
  } else {
    warning(paste("Shapefile for year", year, "does not exist:", rice_shapefile_path))
  }
}

# Years for which shapefiles are available
years <- 2008:2020

# Apply the function to each year
lapply(years, process_rice_shapefile)

DTM_hill_UTMZ18N_HARV <- project(DTM_hill_HARV,
                                 crs(DTM_HARV))

# Load required libraries
# Load required libraries
library(terra)
library(sf)

# Set terra options for temporary operations
terraOptions(tempdir = "C:/Users/rbmahbub/Documents/RProjects/TemporaryFolderTerraOptions", verbose = TRUE)

# Path to the Arkansas Boundary shapefile
arkansas_shapefile_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/ArkansasBoundaryShp/ArkansasBoundaryshp.shp"

# Read the shapefile
arkansas_shapefile <- st_read(arkansas_shapefile_path)

# Path to the raster file
raster_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/RasterData/CDLData/2023_30m_cdls.tif"

# Read the raster file
rast2023 <- rast(raster_path)

# Function to project raster in chunks
project_raster_in_chunks <- function(raster, shapefile_crs, chunk_size = 1000) {
  # Get raster dimensions
  nrow <- nrow(raster)
  ncol <- ncol(raster)
  
  # Initialize an empty list to store projected chunks
  projected_chunks <- list()
  
  # Loop through the raster in chunks
  for (i in seq(1, nrow, by = chunk_size)) {
    for (j in seq(1, ncol, by = chunk_size)) {
      # Define the extent of the current chunk
      chunk_extent <- ext(raster, row = c(i, min(i + chunk_size - 1, nrow)),
                          col = c(j, min(j + chunk_size - 1, ncol)))
      
      # Crop the current chunk
      chunk <- crop(raster, chunk_extent)
      
      # Project the current chunk
      projected_chunk <- project(chunk, shapefile_crs)
      
      # Add the projected chunk to the list
      projected_chunks <- append(projected_chunks, list(projected_chunk))
    }
  }
  
  # Merge all projected chunks back together
  merged_raster <- do.call(merge, projected_chunks)
  
  return(merged_raster)
}

# Perform the projection in chunks
shapefile_crs <- crs(arkansas_shapefile)
rast2023_projected <- project_raster_in_chunks(rast2023, shapefile_crs)

# Save the projected raster
writeRaster(rast2023_projected, "C:/Users/rbmahbub/Documents/Data/GeospatialData/RasterData/CDLData/2023_30m_cdls_projected.tif", overwrite = TRUE)



