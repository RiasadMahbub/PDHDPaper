// Define cloud cover thresholds and date range
var maxCloudCoverLandsat = 40;
var maxCloudCoverSentinel = 40;
var startDate = ee.Date('2019-01-01');
var endDate = ee.Date('2019-03-31');

// Load the FeatureCollection
var mergedFC = ee.FeatureCollection('projects/ee-my-riasadbmahbub/assets/PDHD_Merged_2015_2024');

// Filter features where FARM_NAME is not empty
var filteredFC = mergedFC.filter(ee.Filter.neq('FARM_NAME', null))
                         .filter(ee.Filter.neq('FARM_NAME', ''));
                         
var filteredFC = mergedFC                         

// Function to find Sentinel-2 Tile ID
var getSentinelTileID = function(feature) {
  var centroid = feature.geometry().centroid();

  // Find a representative Sentinel-2 image
  var sampleImage = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
    .filterBounds(centroid)
    .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
    .filterDate(startDate, endDate)
    .sort('CLOUDY_PIXEL_PERCENTAGE') // Sort by least cloud cover
    .first(); // Get the first image

  // Extract Tile ID from MGRS_TILE property
  var tileID = sampleImage.get('MGRS_TILE'); // Extract MGRS Tile ID (TXXXXX)

  // Extract Image ID for reference (optional)
  var imageID = sampleImage.get('system:index'); // Get image ID (optional)

  // Return feature with extracted Sentinel Tile ID
  return feature.set({
    'Sentinel_Tile_ID': tileID,
    'Sentinel_Image_ID': imageID // Store the image ID for reference
  });
};

// Apply function to each farm feature
var updatedFC = filteredFC.map(getSentinelTileID);

// Function to find Landsat Path/Row from image metadata
var getPathRowFromImage = function(feature) {
  var centroid = feature.geometry().centroid();

  // Find a representative Landsat 8 image
  var sampleImage = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
    .filterBounds(centroid)
    .filterDate(startDate, endDate)
    .sort('CLOUD_COVER')  // Sort by least cloud cover
    .first(); // Get the first image

  // Extract Path/Row metadata from the image
  var path = sampleImage.get('WRS_PATH');
  var row = sampleImage.get('WRS_ROW');

  // Return feature with extracted Path/Row
  return feature.set({
    'PATH': path,
    'ROW': row,
    'Landsat_Image_ID': sampleImage.get('system:index') // Store the image ID for reference
  });
};

// Apply function to each farm feature
var finalFC = updatedFC.map(getPathRowFromImage);

// Print Results
print('Final FeatureCollection with Sentinel and Landsat Info:', finalFC);

// Function to add PATHROWTILE property
var addPathRowTile = function(feature) {
  var path = ee.String(feature.get('PATH'));
  var row = ee.String(feature.get('ROW'));
  var tile = ee.String(feature.get('Sentinel_Tile_ID'));

  // Concatenate values to form PATHROWTILE
  var pathRowTile = path.cat('_').cat(row).cat('_').cat(tile);

  // Return feature with new property
  return feature.set('PATHROWTILE', pathRowTile);
};

// Apply function to each feature
var updatedFC = finalFC.map(addPathRowTile);

// Print updated FeatureCollection
print('Updated FeatureCollection with PATHROWTILE:', updatedFC);

// Extract unique PATHROWTILE values
var uniquePathRowTiles = updatedFC.distinct(['PATHROWTILE']);

// Print unique values
print('Unique PATHROWTILE values:', uniquePathRowTiles);

// PATHROWTILE: 24.0_36.0_T15SXU
// PATHROWTILE: 23.0_35.0_T16SBE
// PATHROWTILE: 23.0_35.0_T15SYV
// PATHROWTILE: 24.0_36.0_T15SWU
// PATHROWTILE: 23.0_35.0_T15SXV

// Map over the FeatureCollection to create concatenated PATHROW values
var pathRowConcat = uniquePathRowTiles.map(function(feature) {
  var path = ee.Number(feature.get('PATH')).format('%d'); // Ensure it's a string
  var row = ee.Number(feature.get('ROW')).format('%d');   // Ensure it's a string
  return feature.set('PATHROW', path.cat('_').cat(row));  // Concatenate properly
});

// Extract unique values
var uniquePathRows = pathRowConcat.distinct(['PATHROW']).aggregate_array('PATHROW');

// Print the unique PATHROW values
print('Unique PATHROW values:', uniquePathRows);

// Extract unique Sentinel_Tile_ID values
var uniqueSentinelTiles = uniquePathRowTiles.distinct(['Sentinel_Tile_ID']);

// Convert to a list
var sentinelTileList = uniqueSentinelTiles.aggregate_array('Sentinel_Tile_ID');

// Print the unique Sentinel_Tile_ID values as a list
print('Unique Sentinel_Tile_ID values:', sentinelTileList);


// Export the FeatureCollection as a shapefile
Export.table.toDrive({
  collection: updatedFC,
  description: 'PDHDshp2015_2024_POI',
  folder: 'PDHDshp20152024POI',  // Specify a folder in Google Drive
  fileFormat: 'SHP'  // Export as a shapefile
});

print('Export started: Check Google Drive for results.');




