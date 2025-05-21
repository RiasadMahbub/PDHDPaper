### Intersection
library(sf)

# Load the shapefiles
shapefile_2020_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/2020/FieldBorderManual-ReprojectedAreaGreater5000/ArkansasRice2020_Shapefile_corrected.shp"
shapefile_2019_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/2019/RepojectedAreaGreaterthan5000/ArkansasRice2019_Shapefile_corrected.shp"

# Read the shapefiles
a <- st_read(shapefile_2020_path)
b <- st_read(shapefile_2019_path)

if (st_crs(a) != st_crs(b)) {
  b <- st_transform(b, st_crs(a))  # Transform 'b' to have the same CRS as 'a'
}


# Perform the spatial intersection
Clip1 <- st_intersection(a, b)

# Perform the spatial intersection
Clip2 <- st_union(Clip1, b)
# Extract "POLYGON" geometries
Clip1_polygons <- st_collection_extract(Clip2, "POLYGON")
# Optional: Save the resulting shapefile
st_write(Clip1_polygons, "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/2019/2019clippedBy2020/ArkansasRice_Intersection_2019_2020.shp")


#### 20202008-2020 intersection
### Intersection
library(sf)
library(maps)
library(maptools)
library(sp)
library(rgeos)
# Load the shapefiles
shapefile_2020_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/2020/FieldBorderManual-ReprojectedAreaGreater5000/ArkansasRice2020_Shapefile_corrected.shp"
shapefile_20082020_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/RiceArkansas50000820Merged.shp"


# Read the shapefiles
a <- st_read(shapefile_2020_path)
b <- st_read(shapefile_20082020_path)

if (st_crs(a) != st_crs(b)) {
  b <- st_transform(b, st_crs(a))  # Transform 'b' to have the same CRS as 'a'
}

# this is a well known R / GEOS hack (usually combined with the above) to 
# deal with "bad" polygons
b <- st_buffer(b, 5000)
# Perform the spatial intersection
Clip1 <- st_intersection(a, b)

# Perform the spatial intersection
Clip2 <- st_union(Clip1, b)
# Extract "POLYGON" geometries
Clip1_polygons <- st_collection_extract(Clip2, "POLYGON")
# Optional: Save the resulting shapefile
st_write(Clip1_polygons, "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/2020/2020Intersected-shapefile_20082020_path/ArkansasRice_Intersection_2020_20082020.shp")


# Specify the folder path
folder_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllShapefile"

# List all shapefiles in the folder
shapefiles <- list.files(path = folder_path, pattern = "\\.shp$", full.names = TRUE)

# Print the list of shapefiles
print(shapefiles)



##########2008 and 2009##########
###############stst_
library(sf)

# Specify the paths to the shapefiles
shapefile_2008_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllShapefile/ArkansasRice2008_Shapefile_corrected.shp"
shapefile_2009_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllShapefile/ArkansasRice2009_Shapefile_corrected.shp"

# Read the shapefiles
shapefile_2008 <- st_read(shapefile_2008_path)
shapefile_2009 <- st_read(shapefile_2009_path)

# Apply a zero-width buffer to clean up topology issues
shapefile_2008 <- st_buffer(shapefile_2008, 0)
shapefile_2009 <- st_buffer(shapefile_2009, 0)

# Ensure both shapefiles have the same CRS
if (st_crs(shapefile_2008) != st_crs(shapefile_2009)) {
  shapefile_2009 <- st_transform(shapefile_2009, st_crs(shapefile_2008))
}

# Perform the st_union operation
union_result <- st_union(shapefile_2008, shapefile_2009)

# Specify the output path
output_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/STUnionisedShapefile/ArkansasRice_Union_2008_2009.shp"

# Create the output directory if it doesn't exist
dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

# Save the resulting shapefile
st_write(union_result, output_path)

