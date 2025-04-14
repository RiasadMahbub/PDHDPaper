var PDHD2015 = ee.FeatureCollection("projects/ee-my-riasadbmahbub/assets/DOPDOHDL/PDHD2015"),
    table = ee.FeatureCollection("projects/ee-my-riasadbmahbub/assets/PDHD_Merged_2015_2024");
var spectral = require('users/dmlmont/spectral:spectral');
// 0: T15SXU
// 1: T16SBE
// 2: T15SWV
// 3: T15SYV
// 4: T15SWU
// 5: T15SXV

// 0: 24_36
// 1: 23_35
// 2: 25_36

// Link to original code: https://code.earthengine.google.com/934b2252c5403a1b3af9a4a0ccb4c9e1
var PointName = 'USHRA';
Map.addLayer(PDHD2015);
// Define date range to search for matching Landsat and Sentinel-2 images
var startDate = ee.Date('2015-01-01');
var endDate = ee.Date('2017-12-31');

// Define cloud cover thresholds (adjust as needed)
var maxCloudCoverLandsat = 40; // Landsat cloud cover threshold
var maxCloudCoverSentinel = 20; // Sentinel-2 cloud cover threshold

// Function to find near-same date images
function findMatchingDates(imageCollection, date, days) {
  return imageCollection.filterDate(date.advance(-days, 'day'), date.advance(days, 'day'));
}
var PoI1 = ee.Geometry.Point([-93.49180267996414, 34.74599160047214]);
var PoI2 = ee.Geometry.Point([-93.4243296195461, 35.62679298187295]);
var PoI3 = ee.Geometry.Point([-92.40056762511915, 34.746603482739836]);
var PoI4 = ee.Geometry.Point([-91.39006192356827, 34.760293712209176]);
var PoI5 = ee.Geometry.Point([-90.18637918959824, 35.61710196401902]);
var PoI6 = ee.Geometry.Point([-89.70641826529712, 35.619559587972994]);
var PoI7 = ee.Geometry.Point([-92.39390504993484, 35.64844808113487]);

// Filter Landsat 7 for path 24, row 36
var L7_col2436 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 24))
  .filter(ee.Filter.eq('WRS_ROW', 36))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
  
  // Filter Landsat 7 for path 23, row 35
var L7_col2335 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 23))
  .filter(ee.Filter.eq('WRS_ROW', 35))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
  
  // Filter Landsat 7 for path 25, row 36
var L7_col2536 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 25))
  .filter(ee.Filter.eq('WRS_ROW', 36))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
  
  
// Filter Landsat 8 for path 24, row 36
var L8_col2436 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 24))
  .filter(ee.Filter.eq('WRS_ROW', 36))
  .filterDate(startDate, endDate)


// Filter Landsat 8 for path 23, row 35
var L8_col2335 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 23))
  .filter(ee.Filter.eq('WRS_ROW', 35))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));

// Filter Landsat 8 for path 25, row 36
var L8_col2536 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 25))
  .filter(ee.Filter.eq('WRS_ROW', 36))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));


// Filter Sentinel-2 for 15SXU
var S2_15SXU = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '15SXU'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);

// Filter Sentinel-2 for 16SBE
var S2_16SBE = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '16SBE'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);

// Filter Sentinel-2 for 15SWV
var S2_15SWV = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '15SWV'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);

// Filter Sentinel-2 for 15SYV
var S2_15SYV = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '15SYV'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);

// Filter Sentinel-2 for 15SWU
var S2_15SWU = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '15SWU'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);

// Filter Sentinel-2 for 15SXV
var S2_15SXV = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '15SXV'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);


// Function to match Landsat and Sentinel-2 images with similar dates
function findClearSkyPairs(LandsatCol, SentinelCol, maxDaysDifference) {
  var LandsatDates = LandsatCol.aggregate_array('system:time_start');
  
  var clearSkyPairs = LandsatDates.map(function(landsatDate) {
    var landsatImage = LandsatCol.filter(ee.Filter.date(ee.Date(landsatDate)));
    
    // Find Sentinel-2 images within the date range
    var sentinelMatch = findMatchingDates(SentinelCol, ee.Date(landsatDate), maxDaysDifference)
      .sort('system:time_start')
      .first();
    
    return ee.Algorithms.If(sentinelMatch, {
      landsatDate: ee.Date(landsatDate).format('YYYY-MM-dd'),
      sentinelDate: ee.Image(sentinelMatch).date().format('YYYY-MM-dd')
    }, null);
  });
  
  return clearSkyPairs;
}

// 0: 24.0_36.0_15SXU L7_col2436 L8_col2436 S2_15SXU  
// 1: 23.0_35.0_16SBE L7_col2335 L8_col2335 S2_16SBE
// 2: 25.0_36.0_15SWV L7_col2536 L8_col2536 S2_15SWV
// 3: 23.0_35.0_15SYV L7_col2335 L8_col2335 S2_15SYV
// 4: 24.0_36.0_15SWU L7_col2436 L8_col2436 S2_15SWU
// 5: 23.0_35.0_15SXV  L7_col2335 L8_col2335 S2_15SXV

// Find matching dates with clear skies for Landsat 7 - Sentinel-2
var clearSkyPairsL72436S215SXU = findClearSkyPairs(L7_col2436.filterDate(startDate, endDate), S2_15SXU.filterDate(startDate, endDate), 1);
//print('1Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL72436S215SXU);
// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S272436S215SXU = findClearSkyPairs(L8_col2436.filterDate(startDate, endDate), S2_15SXU.filterDate(startDate, endDate), 1);
//print('1Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S272436S215SXU);

var clearSkyPairsL7S2233516SBE = findClearSkyPairs(L7_col2335.filterDate(startDate, endDate), S2_16SBE.filterDate(startDate, endDate), 1);
//print('2Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S2233516SBE);
// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S2233516SBE = findClearSkyPairs(L8_col2335.filterDate(startDate, endDate), S2_16SBE.filterDate(startDate, endDate), 1);
//print('2Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S2233516SBE);

var clearSkyPairsL7S2253615SWV = findClearSkyPairs(L7_col2536.filterDate(startDate, endDate), S2_15SWV.filterDate(startDate, endDate), 1);
//print('3Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S2253615SWV);
// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S2253615SWV = findClearSkyPairs(L8_col2536.filterDate(startDate, endDate), S2_15SWV.filterDate(startDate, endDate), 1);
//print('3Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S2253615SWV);

var clearSkyPairsL7S2233515SYV = findClearSkyPairs(L7_col2335.filterDate(startDate, endDate), S2_15SYV.filterDate(startDate, endDate), 1);
//print('4Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S2233515SYV);
// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S2233515SYV = findClearSkyPairs(L8_col2335.filterDate(startDate, endDate), S2_15SYV.filterDate(startDate, endDate), 1);
//print('4Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S2233515SYV);

var clearSkyPairsL7S2243615SWU = findClearSkyPairs(L7_col2436.filterDate(startDate, endDate), S2_15SWU.filterDate(startDate, endDate), 1);
//print('5Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S2243615SWU);
// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S2243615SWU = findClearSkyPairs(L8_col2436.filterDate(startDate, endDate), S2_15SWU.filterDate(startDate, endDate), 1);
//print('5Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S2243615SWU);

var clearSkyPairsL7S2233515SXV = findClearSkyPairs(L7_col2335.filterDate(startDate, endDate), S2_15SXV.filterDate(startDate, endDate), 1);
//print('6Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S2233515SXV);
// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S2233515SXV = findClearSkyPairs(L8_col2335.filterDate(startDate, endDate), S2_15SXV.filterDate(startDate, endDate), 1);
//print('6Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S2233515SXV);

/* -------------- Harmonization of Landsat and Sentinel-2 -------------------
*/
// -- Import the MODULES (functions)
var funcHLS = require('users/geeberra/Modules:HLS_Module_v2');

// -- Selected point of interest and clear sky (near)same dates Landsat-Sentinel observations
// OPTION 1 //// ######### Agriculture in Carazinho - Brazil
// var PoI = ee.Geometry.Point(-91.75189293121653, 34.58672839552607); // Point of Interest. Ground NDVI sensors
// var PointName = 'USHRA';
// - L8-S2 pair 
var Date_L7_PoI1 = ee.Date(ee.Dictionary((clearSkyPairsL72436S215SXU 
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI1 = ee.Date(ee.Dictionary((clearSkyPairsL72436S215SXU 
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI1 = ee.Date(ee.Dictionary((clearSkyPairsL8S272436S215SXU 
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI1 = ee.Date(ee.Dictionary((clearSkyPairsL8S272436S215SXU 
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));

var Date_L7_PoI2 = ee.Date(ee.Dictionary((clearSkyPairsL7S2233516SBE 
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI2 = ee.Date(ee.Dictionary((clearSkyPairsL7S2233516SBE 
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI2 = ee.Date(ee.Dictionary((clearSkyPairsL8S2233516SBE 
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI2 = ee.Date(ee.Dictionary((clearSkyPairsL8S2233516SBE 
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));



var Date_L7_PoI3 = ee.Date(ee.Dictionary((clearSkyPairsL7S2253615SWV  
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI3 = ee.Date(ee.Dictionary((clearSkyPairsL7S2253615SWV  
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI3 = ee.Date(ee.Dictionary((clearSkyPairsL8S2253615SWV  
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI3 = ee.Date(ee.Dictionary((clearSkyPairsL8S2253615SWV  
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));


var Date_L7_PoI4 = ee.Date(ee.Dictionary((clearSkyPairsL7S2233515SYV 
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI4 = ee.Date(ee.Dictionary((clearSkyPairsL7S2233515SYV 
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI4 = ee.Date(ee.Dictionary((clearSkyPairsL8S2233515SYV 
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI4 = ee.Date(ee.Dictionary((clearSkyPairsL8S2233515SYV 
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));


var Date_L7_PoI5 = ee.Date(ee.Dictionary((clearSkyPairsL7S2243615SWU 
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI5 = ee.Date(ee.Dictionary((clearSkyPairsL7S2243615SWU 
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI5 = ee.Date(ee.Dictionary((clearSkyPairsL8S2243615SWU 
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI5 = ee.Date(ee.Dictionary((clearSkyPairsL8S2243615SWU 
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));

// PoI6 - L7-S2 and L8-S2 pair dates
var Date_L7_PoI6 = ee.Date(ee.Dictionary((clearSkyPairsL7S2233515SXV 
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI6 = ee.Date(ee.Dictionary((clearSkyPairsL7S2233515SXV 
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI6 = ee.Date(ee.Dictionary((clearSkyPairsL8S2233515SXV 
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI6 = ee.Date(ee.Dictionary((clearSkyPairsL8S2233515SXV 
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));


// -- Choose which bands to get statistics from
var bandNames_img1_L7 = ee.List(['B3', 'B4']);//Red and NIR: L7
var bandNames_img1_L8 = ee.List(['B4', 'B5']);//Red and NIR: L8
var bandNames_img2_S2 = ee.List(['B4', 'B8']);//Red and NIR: S2
var bandNames_common = ee.List(['red', 'nir']);//Harmonized names

// -- Visualization parameters in an object literal.
var Color_comp_01 = {bands:"B4,B3,B2", min: 0.0, max: 0.4, gamma: 1};
var Color_comp_01_L7 = {bands:"B3,B2,B1", min: 0.0, max: 0.4, gamma: 1};
var Color_comp =    {bands:"B4,B3,B2", min:200, max:2000, gamma: 1};
var viz =  {min:200,max:2000,bands:['red','green','blue']};
var viz2 =  {min:0,max:0.4,bands:['red','green','blue']};


//Identify the equivalent bands across the three sensors
    //For L7:     B1=blue, B2=green, B3=red, B4=nir, B5=swir1, B7=swir2  
    var bandIn_L7 = ['B1',   'B2',    'B3',   'B4',   'B5',    'B7'];
    
    //For L8:      B2=blue, B3=green, B4=red, B5=nir, B6=swir1, B7=swir2
    var bandIn_L8 = ['B2',   'B3',    'B4',    'B5',   'B6',   'B7'];
    
    //For S2:    B2=blue, B3=green, B4=red, B8=nir, B11=swir1, B12=swir2  
    var bandIn_S2 = [  'B2',  'B3',  'B4',    'B8',    'B11',    'B12'];
    
    //BandOut will have the same name for all the sensors
    var bandOut = ['blue','green','red','nir','swir1','swir2'];
    

// ######################################################################################################
// ######################################################################################################
//                                    ### STEP 01: GENERAL FILTERING  ###
// ######################################################################################################

// -- Region of Interest (ROI) and time span 
// var polygon = PoI.buffer(300).bounds();//buffer around the point
var start_date = '2015-01-01';
var end_date   = '2017-12-31';
//print("table", table);
var tablegeometry = PDHD2015.geometry();

// Assuming 'fields' is your shapefile or feature collection with polygon geometries
var fields = ee.FeatureCollection(PDHD2015);
var criteria = ee.Filter.and(
     ee.Filter.date(start_date, end_date));
var cloud_perc = 40;//Max cloud percentile per scene. 
// -- Collections of Landsat 7, 8 and Sentinel 2
var cloud_perc = 20; // Example value for cloud cover percentage

// For PoI1
var L8_col1_PoI1 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 24))
  .filter(ee.Filter.eq('WRS_ROW', 36))
  .filterDate(startDate, endDate)


var L7_col1_PoI1 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 24))
  .filter(ee.Filter.eq('WRS_ROW', 36))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));


var S2_col1_PoI1 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '15SXU'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);


// For PoI2
var L8_col1_PoI2 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 23))
  .filter(ee.Filter.eq('WRS_ROW', 35))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));


var L7_col1_PoI2 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 23))
  .filter(ee.Filter.eq('WRS_ROW', 35))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));


var S2_col1_PoI2 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '16SBE'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);


// Repeat the same structure for PoI3, PoI4, PoI5, and PoI6
var L8_col1_PoI3 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 25))
  .filter(ee.Filter.eq('WRS_ROW', 36))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));


var L7_col1_PoI3 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 25))
  .filter(ee.Filter.eq('WRS_ROW', 36))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));


var S2_col1_PoI3 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '15SWV'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);

// For PoI4
var L8_col1_PoI4 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 23))
  .filter(ee.Filter.eq('WRS_ROW', 35))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));


var L7_col1_PoI4 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 23))
  .filter(ee.Filter.eq('WRS_ROW', 35))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));



var S2_col1_PoI4 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '15SYV'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);


// For PoI5
var L8_col1_PoI5 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 24))
  .filter(ee.Filter.eq('WRS_ROW', 36))
  .filterDate(startDate, endDate);

var L7_col1_PoI5 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 24))
  .filter(ee.Filter.eq('WRS_ROW', 36))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
  
var S2_col1_PoI5 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '15SWU'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);


// For PoI6
var L8_col1_PoI6 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 23))
  .filter(ee.Filter.eq('WRS_ROW', 35))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));


var L7_col1_PoI6 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filter(ee.Filter.eq('WRS_PATH', 23))
  .filter(ee.Filter.eq('WRS_ROW', 35))
  .filterDate(startDate, endDate)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));

var S2_col1_PoI6 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
  .filter(ee.Filter.eq('MGRS_TILE', '15SXV'))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel))
  .filterDate(startDate, endDate);

print("S2_col1_PoI4", S2_col1_PoI4)
/*S2 may have mulitple observations in the same day on some dates. 'funcHLS.FirstDate' selects one scene per date,
but works only for a single path/row every time. It's quite slow. Think wheather to use it.
  */
// S2_col = funcHLS.FirstDate(S2_col);
var footprint_S2_PoI1 = S2_col1_PoI1.first().geometry().bounds();
var footprint_S2_PoI2 = S2_col1_PoI2.first().geometry().bounds();
var footprint_S2_PoI3 = S2_col1_PoI3.first().geometry().bounds();
var footprint_S2_PoI4 = S2_col1_PoI4.first().geometry().bounds();
var footprint_S2_PoI5 = S2_col1_PoI5.first().geometry().bounds();
var footprint_S2_PoI6 = S2_col1_PoI6.first().geometry().bounds();

print("footprint_S2_PoI4", footprint_S2_PoI4)
// ######################################################################################################
// ######################################################################################################
//                                ### STEP 02.CLOUD and SHADOW MASKING  ###
// ######################################################################################################

// Tuning parameters for S2 cloud and cloud shadow detection and masking
var args_PoI1 = {
    'Sentinel2Col': S2_col1_PoI1,
    'FilterCriteria': criteria // For cloud masking
};

var args_PoI2 = {
    'Sentinel2Col': S2_col1_PoI2,
    'FilterCriteria': criteria // For cloud masking
};

var args_PoI3 = {
    'Sentinel2Col': S2_col1_PoI3,
    'FilterCriteria': criteria // For cloud masking
};

var args_PoI4 = {
    'Sentinel2Col': S2_col1_PoI4,
    'FilterCriteria': criteria // For cloud masking
};

var args_PoI5 = {
    'Sentinel2Col': S2_col1_PoI5,
    'FilterCriteria': criteria // For cloud masking
};

var args_PoI6 = {
    'Sentinel2Col': S2_col1_PoI6,
    'FilterCriteria': criteria // For cloud masking
};

var L7_col1_PoI1_masked = L7_col1_PoI1.map(funcHLS.cloudMaskL78_TOAoriginal);
var L8_col1_PoI1_masked = L8_col1_PoI1.map(funcHLS.cloudMaskL78_TOAoriginal);

var L7_col1_PoI2_masked = L7_col1_PoI2.map(funcHLS.cloudMaskL78_TOAoriginal);
var L8_col1_PoI2_masked = L8_col1_PoI2.map(funcHLS.cloudMaskL78_TOAoriginal);

var L7_col1_PoI3_masked = L7_col1_PoI3.map(funcHLS.cloudMaskL78_TOAoriginal);
var L8_col1_PoI3_masked = L8_col1_PoI3.map(funcHLS.cloudMaskL78_TOAoriginal);

var L7_col1_PoI4_masked = L7_col1_PoI4.map(funcHLS.cloudMaskL78_TOAoriginal);
var L8_col1_PoI4_masked = L8_col1_PoI4.map(funcHLS.cloudMaskL78_TOAoriginal);

var L7_col1_PoI5_masked = L7_col1_PoI5.map(funcHLS.cloudMaskL78_TOAoriginal);
var L8_col1_PoI5_masked = L8_col1_PoI5.map(funcHLS.cloudMaskL78_TOAoriginal);

var L7_col1_PoI6_masked = L7_col1_PoI6.map(funcHLS.cloudMaskL78_TOAoriginal);
var L8_col1_PoI6_masked = L8_col1_PoI6.map(funcHLS.cloudMaskL78_TOAoriginal);


var S2_col1_PoI1_masked = funcHLS.Cloud_ScorePlusS2(args_PoI1);
var S2_col1_PoI2_masked = funcHLS.Cloud_ScorePlusS2(args_PoI2);
var S2_col1_PoI3_masked = funcHLS.Cloud_ScorePlusS2(args_PoI3);
var S2_col1_PoI4_masked = funcHLS.Cloud_ScorePlusS2(args_PoI4);
var S2_col1_PoI5_masked = funcHLS.Cloud_ScorePlusS2(args_PoI5);
var S2_col1_PoI6_masked = funcHLS.Cloud_ScorePlusS2(args_PoI6);



// ######################################################################################################
// ######################################################################################################
//                                ### STEP 03. INTER-SENSOR BAND ADJUSTMENT   ###
// ######################################################################################################


//Adjust L7 and L8 to match the S2 TOA spectral bands
var L7_col1_PoI1_adj = L7_col1_PoI1_masked.map(funcHLS.band_adjsut_L7);
var L7_col1_PoI2_adj = L7_col1_PoI2_masked.map(funcHLS.band_adjsut_L7);
var L7_col1_PoI3_adj = L7_col1_PoI3_masked.map(funcHLS.band_adjsut_L7);
var L7_col1_PoI4_adj = L7_col1_PoI4_masked.map(funcHLS.band_adjsut_L7);
var L7_col1_PoI5_adj = L7_col1_PoI5_masked.map(funcHLS.band_adjsut_L7);
var L7_col1_PoI6_adj = L7_col1_PoI6_masked.map(funcHLS.band_adjsut_L7);

var L8_col1_PoI1_adj = L8_col1_PoI1_masked.map(funcHLS.band_adjsut_L8);
var L8_col1_PoI2_adj = L8_col1_PoI2_masked.map(funcHLS.band_adjsut_L8);
var L8_col1_PoI3_adj = L8_col1_PoI3_masked.map(funcHLS.band_adjsut_L8);
var L8_col1_PoI4_adj = L8_col1_PoI4_masked.map(funcHLS.band_adjsut_L8);
var L8_col1_PoI5_adj = L8_col1_PoI5_masked.map(funcHLS.band_adjsut_L8);
var L8_col1_PoI6_adj = L8_col1_PoI6_masked.map(funcHLS.band_adjsut_L8);

var S2_col1_PoI1_adj = S2_col1_PoI1_masked;
var S2_col1_PoI2_adj = S2_col1_PoI2_masked;
var S2_col1_PoI3_adj = S2_col1_PoI3_masked;
var S2_col1_PoI4_adj = S2_col1_PoI4_masked;
var S2_col1_PoI5_adj = S2_col1_PoI5_masked;
var S2_col1_PoI6_adj = S2_col1_PoI6_masked;

// ######################################################################################################
// ######################################################################################################
//                                    ### STEP 04.ATMOSPHERIC CORRECTION  ###
// ######################################################################################################


//Retrieve bottom of atmosphere (BOA) reflectance via SIAC
//Give common names to all bands
var L7_col1_PoI1_boa = L7_col1_PoI1_adj.map(funcHLS.SIAC_L7).select(bandIn_L7, bandOut);
var L8_col1_PoI1_boa = L8_col1_PoI1_adj.map(funcHLS.SIAC_L8).select(bandIn_L8, bandOut);

var L7_col1_PoI2_boa = L7_col1_PoI2_adj.map(funcHLS.SIAC_L7).select(bandIn_L7, bandOut);
var L8_col1_PoI2_boa = L8_col1_PoI2_adj.map(funcHLS.SIAC_L8).select(bandIn_L8, bandOut);

var L7_col1_PoI3_boa = L7_col1_PoI3_adj.map(funcHLS.SIAC_L7).select(bandIn_L7, bandOut);
var L8_col1_PoI3_boa = L8_col1_PoI3_adj.map(funcHLS.SIAC_L8).select(bandIn_L8, bandOut);

var L7_col1_PoI4_boa = L7_col1_PoI4_adj.map(funcHLS.SIAC_L7).select(bandIn_L7, bandOut);
var L8_col1_PoI4_boa = L8_col1_PoI4_adj.map(funcHLS.SIAC_L8).select(bandIn_L8, bandOut);

var L7_col1_PoI5_boa = L7_col1_PoI5_adj.map(funcHLS.SIAC_L7).select(bandIn_L7, bandOut);
var L8_col1_PoI5_boa = L8_col1_PoI5_adj.map(funcHLS.SIAC_L8).select(bandIn_L8, bandOut);

var L7_col1_PoI6_boa = L7_col1_PoI6_adj.map(funcHLS.SIAC_L7).select(bandIn_L7, bandOut);
var L8_col1_PoI6_boa = L8_col1_PoI6_adj.map(funcHLS.SIAC_L8).select(bandIn_L8, bandOut);


// Remove the problematic image by filtering out its ID
//var S2_col1_PoI3_adj = S2_col1_PoI3_adj.filter(ee.Filter.neq('system:index', '20231207T170709_20231207T171334_T15SWV'));

var S2_col1_PoI1_boa = S2_col1_PoI1_adj.map(funcHLS.SIAC_S2).select(bandIn_S2, bandOut);
var S2_col1_PoI2_boa = S2_col1_PoI2_adj.map(funcHLS.SIAC_S2).select(bandIn_S2, bandOut);
var S2_col1_PoI3_boa = S2_col1_PoI3_adj.map(funcHLS.SIAC_S2).select(bandIn_S2, bandOut);
var S2_col1_PoI4_boa = S2_col1_PoI4_adj.map(funcHLS.SIAC_S2).select(bandIn_S2, bandOut);
var S2_col1_PoI5_boa = S2_col1_PoI5_adj.map(funcHLS.SIAC_S2).select(bandIn_S2, bandOut);
var S2_col1_PoI6_boa = S2_col1_PoI6_adj.map(funcHLS.SIAC_S2).select(bandIn_S2, bandOut);

// ######################################################################################################
// ######################################################################################################
//                              ### STEP 05. BRDF  ###
// ######################################################################################################
/*Code from <https://github.com/ndminhhus/geeguide/blob/master/04.topo_correction.md>
In a first test, the topographic correction seemed very computing power demanding. 
Think whether is really necessary to apply it or not.
*/

//Apply BRDF
var imgL7_col1_PoI1_SR_BRDF = L7_col1_PoI1_boa.map(funcHLS.applyBRDF); 
var imgL8_col1_PoI1_SR_BRDF = L8_col1_PoI1_boa.map(funcHLS.applyBRDF); 

var imgL7_col1_PoI2_SR_BRDF = L7_col1_PoI2_boa.map(funcHLS.applyBRDF); 
var imgL8_col1_PoI2_SR_BRDF = L8_col1_PoI2_boa.map(funcHLS.applyBRDF); 

var imgL7_col1_PoI3_SR_BRDF = L7_col1_PoI3_boa.map(funcHLS.applyBRDF); 
var imgL8_col1_PoI3_SR_BRDF = L8_col1_PoI3_boa.map(funcHLS.applyBRDF); 

var imgL7_col1_PoI4_SR_BRDF = L7_col1_PoI4_boa.map(funcHLS.applyBRDF); 
var imgL8_col1_PoI4_SR_BRDF = L8_col1_PoI4_boa.map(funcHLS.applyBRDF); 

var imgL7_col1_PoI5_SR_BRDF = L7_col1_PoI5_boa.map(funcHLS.applyBRDF); 
var imgL8_col1_PoI5_SR_BRDF = L8_col1_PoI5_boa.map(funcHLS.applyBRDF); 

var imgL7_col1_PoI6_SR_BRDF = L7_col1_PoI6_boa.map(funcHLS.applyBRDF); 
var imgL8_col1_PoI6_SR_BRDF = L8_col1_PoI6_boa.map(funcHLS.applyBRDF);



var imgS2_col1_PoI1_SR_BRDF = S2_col1_PoI1_boa.map(funcHLS.applyBRDF); 
var imgS2_col1_PoI2_SR_BRDF = S2_col1_PoI2_boa.map(funcHLS.applyBRDF); 
var imgS2_col1_PoI3_SR_BRDF = S2_col1_PoI3_boa.map(funcHLS.applyBRDF); 
var imgS2_col1_PoI4_SR_BRDF = S2_col1_PoI4_boa.map(funcHLS.applyBRDF); 
var imgS2_col1_PoI5_SR_BRDF = S2_col1_PoI5_boa.map(funcHLS.applyBRDF); 
var imgS2_col1_PoI6_SR_BRDF = S2_col1_PoI6_boa.map(funcHLS.applyBRDF);


print("imgS2_col1_PoI4_SR_BRDF", imgS2_col1_PoI4_SR_BRDF)
// ######################################################################################################
// ######################################################################################################
//                                ### STEP 06. SPATIAL CO-REGISTRATION  ###
// ######################################################################################################
// 1st) Select the cloud-free image pairs.
// 2nd) Choose which band to calculate the displacement from.
var Bd = 'nir';
// PoI1
var Cloud_free_L7_col1_PoI1_Bd = funcHLS.selDate(L7_col1_PoI1_masked.select(bandIn_L7, bandOut), Date_L7_PoI1).select(Bd);
var Cloud_free_S2_col1_L7_col1_PoI1_Bd = funcHLS.selDate(S2_col1_PoI1_masked.select(bandIn_S2, bandOut), Date_S2_L7_PoI1).select(Bd);
var Cloud_free_L8_col1_PoI1_Bd = funcHLS.selDate(L8_col1_PoI1_masked.select(bandIn_L8, bandOut), Date_L8_PoI1).select(Bd);
var Cloud_free_S2_col1_L8_col1_PoI1_Bd = funcHLS.selDate(S2_col1_PoI1_masked.select(bandIn_S2, bandOut), Date_S2_L8_PoI1).select(Bd);

// PoI2
var Cloud_free_L7_col1_PoI2_Bd = funcHLS.selDate(L7_col1_PoI2_masked.select(bandIn_L7, bandOut), Date_L7_PoI2).select(Bd);
var Cloud_free_S2_col1_L7_col1_PoI2_Bd = funcHLS.selDate(S2_col1_PoI2_masked.select(bandIn_S2, bandOut), Date_S2_L7_PoI2).select(Bd);
var Cloud_free_L8_col1_PoI2_Bd = funcHLS.selDate(L8_col1_PoI2_masked.select(bandIn_L8, bandOut), Date_L8_PoI2).select(Bd);
var Cloud_free_S2_col1_L8_col1_PoI2_Bd = funcHLS.selDate(S2_col1_PoI2_masked.select(bandIn_S2, bandOut), Date_S2_L8_PoI2).select(Bd);

// PoI3
var Cloud_free_L7_col1_PoI3_Bd = funcHLS.selDate(L7_col1_PoI3_masked.select(bandIn_L7, bandOut), Date_L7_PoI3).select(Bd);
var Cloud_free_S2_col1_L7_col1_PoI3_Bd = funcHLS.selDate(S2_col1_PoI3_masked.select(bandIn_S2, bandOut), Date_S2_L7_PoI3).select(Bd);
var Cloud_free_L8_col1_PoI3_Bd = funcHLS.selDate(L8_col1_PoI3_masked.select(bandIn_L8, bandOut), Date_L8_PoI3).select(Bd);
var Cloud_free_S2_col1_L8_col1_PoI3_Bd = funcHLS.selDate(S2_col1_PoI3_masked.select(bandIn_S2, bandOut), Date_S2_L8_PoI3).select(Bd);

// PoI4
var Cloud_free_L7_col1_PoI4_Bd = funcHLS.selDate(L7_col1_PoI4_masked.select(bandIn_L7, bandOut), Date_L7_PoI4).select(Bd);
var Cloud_free_S2_col1_L7_col1_PoI4_Bd = funcHLS.selDate(S2_col1_PoI4_masked.select(bandIn_S2, bandOut), Date_S2_L7_PoI4).select(Bd);
var Cloud_free_L8_col1_PoI4_Bd = funcHLS.selDate(L8_col1_PoI4_masked.select(bandIn_L8, bandOut), Date_L8_PoI4).select(Bd);
var Cloud_free_S2_col1_L8_col1_PoI4_Bd = funcHLS.selDate(S2_col1_PoI4_masked.select(bandIn_S2, bandOut), Date_S2_L8_PoI4).select(Bd);

// PoI5
var Cloud_free_L7_col1_PoI5_Bd = funcHLS.selDate(L7_col1_PoI5_masked.select(bandIn_L7, bandOut), Date_L7_PoI5).select(Bd);
var Cloud_free_S2_col1_L7_col1_PoI5_Bd = funcHLS.selDate(S2_col1_PoI5_masked.select(bandIn_S2, bandOut), Date_S2_L7_PoI5).select(Bd);
var Cloud_free_L8_col1_PoI5_Bd = funcHLS.selDate(L8_col1_PoI5_masked.select(bandIn_L8, bandOut), Date_L8_PoI5).select(Bd);
var Cloud_free_S2_col1_L8_col1_PoI5_Bd = funcHLS.selDate(S2_col1_PoI5_masked.select(bandIn_S2, bandOut), Date_S2_L8_PoI5).select(Bd);

// PoI6
var Cloud_free_L7_col1_PoI6_Bd = funcHLS.selDate(L7_col1_PoI6_masked.select(bandIn_L7, bandOut), Date_L7_PoI6).select(Bd);
var Cloud_free_S2_col1_L7_col1_PoI6_Bd = funcHLS.selDate(S2_col1_PoI6_masked.select(bandIn_S2, bandOut), Date_S2_L7_PoI6).select(Bd);
var Cloud_free_L8_col1_PoI6_Bd = funcHLS.selDate(L8_col1_PoI6_masked.select(bandIn_L8, bandOut), Date_L8_PoI6).select(Bd);
var Cloud_free_S2_col1_L8_col1_PoI6_Bd = funcHLS.selDate(S2_col1_PoI6_masked.select(bandIn_S2, bandOut), Date_S2_L8_PoI6).select(Bd);


// Add the first image from L7_col1_PoI3_masked collection
var L7_firstImage = ee.Image(L7_col1_PoI3_masked.first());
Map.addLayer(L7_firstImage, 
  {bands: ['B4', 'B3', 'B2'], min: 0, max: 0.3}, 
  'L7 First Image PoI3');

// Add the first image from L8_col1_PoI3_masked collection
var L8_firstImage = ee.Image(L8_col1_PoI3_masked.first());
Map.addLayer(L8_firstImage, 
  {bands: ['B4', 'B3', 'B2'], min: 0, max: 0.3}, 
  'L8 First Image PoI3');
  
// Add the first image from S2_col1_PoI4_masked collection
var S2_firstImage = ee.Image(S2_col1_PoI4_masked.first());
Map.addLayer(S2_firstImage, 
  {bands: ['B4', 'B3', 'B2'], min: 0, max: 0.3}, 
  'S2 First Image PoI4');
  
print("S2_col1_PoI4_masked", S2_col1_PoI4_masked)

// PoI1
var iterpolation_method = "bilinear"; // The interpolation mode to use: 'nearest_neighbor', 'bilinear', or 'bicubic'.
var L7_regist_col_PoI1 = imgL7_col1_PoI1_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L7_col1_PoI1_Bd, Cloud_free_S2_col1_L7_col1_PoI1_Bd, iterpolation_method));
var L8_regist_col_PoI1 = imgL8_col1_PoI1_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L8_col1_PoI1_Bd, Cloud_free_S2_col1_L8_col1_PoI1_Bd, iterpolation_method));
var S2_regist_col_PoI1 = imgS2_col1_PoI1_SR_BRDF; // Rename the S2 collection to keep consistency with L7-8 naming

// PoI2
var L7_regist_col_PoI2 = imgL7_col1_PoI2_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L7_col1_PoI2_Bd, Cloud_free_S2_col1_L7_col1_PoI2_Bd, iterpolation_method));
var L8_regist_col_PoI2 = imgL8_col1_PoI2_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L8_col1_PoI2_Bd, Cloud_free_S2_col1_L8_col1_PoI2_Bd, iterpolation_method));
var S2_regist_col_PoI2 = imgS2_col1_PoI2_SR_BRDF;

// PoI3
var L7_regist_col_PoI3 = imgL7_col1_PoI3_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L7_col1_PoI3_Bd, Cloud_free_S2_col1_L7_col1_PoI3_Bd, iterpolation_method));
var L8_regist_col_PoI3 = imgL8_col1_PoI3_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L8_col1_PoI3_Bd, Cloud_free_S2_col1_L8_col1_PoI3_Bd, iterpolation_method));
var S2_regist_col_PoI3 = imgS2_col1_PoI3_SR_BRDF;



// PoI4
var L7_regist_col_PoI4 = imgL7_col1_PoI4_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L7_col1_PoI4_Bd, Cloud_free_S2_col1_L7_col1_PoI4_Bd, iterpolation_method));
var L8_regist_col_PoI4 = imgL8_col1_PoI4_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L8_col1_PoI4_Bd, Cloud_free_S2_col1_L8_col1_PoI4_Bd, iterpolation_method));
var S2_regist_col_PoI4 = imgS2_col1_PoI4_SR_BRDF;

// PoI5
var L7_regist_col_PoI5 = imgL7_col1_PoI5_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L7_col1_PoI5_Bd, Cloud_free_S2_col1_L7_col1_PoI5_Bd, iterpolation_method));
var L8_regist_col_PoI5 = imgL8_col1_PoI5_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L8_col1_PoI5_Bd, Cloud_free_S2_col1_L8_col1_PoI5_Bd, iterpolation_method));
var S2_regist_col_PoI5 = imgS2_col1_PoI5_SR_BRDF;

// PoI6
var L7_regist_col_PoI6 = imgL7_col1_PoI6_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L7_col1_PoI6_Bd, Cloud_free_S2_col1_L7_col1_PoI6_Bd, iterpolation_method));
var L8_regist_col_PoI6 = imgL8_col1_PoI6_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L8_col1_PoI6_Bd, Cloud_free_S2_col1_L8_col1_PoI6_Bd, iterpolation_method));
var S2_regist_col_PoI6 = imgS2_col1_PoI6_SR_BRDF;


// ######################################################################################################
// ######################################################################################################
//                                ### STEP 07. REPROJECT and RESAMPLE  ###
// ######################################################################################################

// ########## OPTION 1 ##########
      //1st: reproject S2 data to 30 m scale
      // var output_Scale = 30;//scale in meters
      // var imgS2_30 = S2_regist_col.map(funcHLS.reproj_S2(output_Scale));
      // var selected_S2_crs_L = imgS2_30.first().select('red').projection();

      // //2nd: Reproject and rescale L7-L8 images according to the S2 30m grid.
      // //No resample() was set: the default nearest-neighbor is used  to compute pixels in the chosen projection 
      // var imgL7_30 = L7_regist_col.map(funcHLS.reproj_L78(selected_S2_crs_L));
      // var imgL8_30 = L8_regist_col.map(funcHLS.reproj_L78(selected_S2_crs_L));
      
// ########## OPTION 2 ##########
/*Note: After tests, as described in the code 'HLS_test_2'(https://code.earthengine.google.com/f9375eab80b8479c15c773522a058497), this option seems
to result in least uncertainty as plot analysis revealed ('ImagePairs_Correl_St06_07.ipynb', https://colab.research.google.com/drive/15hv46w82cKoD9-3hdTr4gsqCxHlfn0zt?usp=drive_link)
*/
        //1st: Select the Landsat crs to reproject and re-scale the S2 data to
// Define the processing for each point (PoI1 to PoI6)

// PoI1
var selected_L_crs_S2_PoI1 = L8_regist_col_PoI1.first().select('red').projection();
var imgS2_30_PoI1 = S2_regist_col_PoI1.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI1));
var imgL7_30_PoI1 = L7_regist_col_PoI1;
var imgL8_30_PoI1 = L8_regist_col_PoI1;

// PoI2
var selected_L_crs_S2_PoI2 = L8_regist_col_PoI2.first().select('red').projection();
var imgS2_30_PoI2 = S2_regist_col_PoI2.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI2));
var imgL7_30_PoI2 = L7_regist_col_PoI2;
var imgL8_30_PoI2 = L8_regist_col_PoI2;

// PoI3
var selected_L_crs_S2_PoI3 = L8_regist_col_PoI3.first().select('red').projection();
var imgS2_30_PoI3 = S2_regist_col_PoI3.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI3));
var imgL7_30_PoI3 = L7_regist_col_PoI3;
var imgL8_30_PoI3 = L8_regist_col_PoI3;



// PoI4
var selected_L_crs_S2_PoI4 = L8_regist_col_PoI4.first().select('red').projection();
var imgS2_30_PoI4 = S2_regist_col_PoI4.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI4));
var imgL7_30_PoI4 = L7_regist_col_PoI4;
var imgL8_30_PoI4 = L8_regist_col_PoI4;

// PoI5
var selected_L_crs_S2_PoI5 = L8_regist_col_PoI5.first().select('red').projection();
var imgS2_30_PoI5 = S2_regist_col_PoI5.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI5));
var imgL7_30_PoI5 = L7_regist_col_PoI5;
var imgL8_30_PoI5 = L8_regist_col_PoI5;

// PoI6
var selected_L_crs_S2_PoI6 = L8_regist_col_PoI6.first().select('red').projection();
var imgS2_30_PoI6 = S2_regist_col_PoI6.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI6));
var imgL7_30_PoI6 = L7_regist_col_PoI6;
var imgL8_30_PoI6 = L8_regist_col_PoI6;





// // ######################################################################################################
// // ######################################################################################################
// //                                    ### STEP 08: PRINT & EXPORT COLLECTIONS ###
// // ######################################################################################################


// Define start and end dates
var s_date = ee.Date('YYYY-MM-DD'); // Replace with your start date
var e_date = ee.Date('YYYY-MM-DD'); // Replace with your end date

// PoI1
var L7_NDVI_PoI1 = imgL7_30_PoI1.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI1 = imgL8_30_PoI1.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI1 = imgS2_30_PoI1.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});

// PoI2
var L7_NDVI_PoI2 = imgL7_30_PoI2.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI2 = imgL8_30_PoI2.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI2 = imgS2_30_PoI2.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});


// PoI3
var L7_NDVI_PoI3 = imgL7_30_PoI3.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI3 = imgL8_30_PoI3.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI3 = imgS2_30_PoI3.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});

// PoI4
var L7_NDVI_PoI4 = imgL7_30_PoI4.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI4 = imgL8_30_PoI4.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI4 = imgS2_30_PoI4.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});


// PoI5
var L7_NDVI_PoI5 = imgL7_30_PoI5.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI5 = imgL8_30_PoI5.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI5 = imgS2_30_PoI5.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});


// PoI6
var L7_NDVI_PoI6 = imgL7_30_PoI6.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI6 = imgL8_30_PoI6.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI6 = imgS2_30_PoI6.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});

                
           
              


// print("L7_NDVI_PoI1", L7_NDVI_PoI1);
// print("L7_NDVI_PoI2", L7_NDVI_PoI2);
// print("L7_NDVI_PoI3", L7_NDVI_PoI3);
// print("L7_NDVI_PoI4", L7_NDVI_PoI4);
// print("L7_NDVI_PoI5", L7_NDVI_PoI5);
// print("L7_NDVI_PoI6", L7_NDVI_PoI6);
// print("L8_NDVI_PoI1", L8_NDVI_PoI1);
// print("L8_NDVI_PoI2", L8_NDVI_PoI2);
// print("L8_NDVI_PoI3", L8_NDVI_PoI3);
// print("L8_NDVI_PoI4", L8_NDVI_PoI4);
// print("L8_NDVI_PoI5", L8_NDVI_PoI5);
// print("L8_NDVI_PoI6", L8_NDVI_PoI6);
// print("S2_NDVI2_PoI4", S2_NDVI2_PoI4);
// print("S2_NDVI2_PoI5", S2_NDVI2_PoI5);
// print("S2_NDVI2_PoI1", S2_NDVI2_PoI1);
// print("S2_NDVI2_PoI6", S2_NDVI2_PoI6);
// print("S2_NDVI2_PoI3", S2_NDVI2_PoI3);
// print("S2_NDVI2_PoI2", S2_NDVI2_PoI2);

// Function to add DATE_ACQUIRED and SPACECRAFT_ID as bands
var addDateAndSpacecraftBand = function(image) {// Get DATE_ACQUIRED property
  var dateStr = image.get('DATE_ACQUIRED');  // Get the DATE_ACQUIRED as string
  var dateMillis = ee.Image(ee.Date(dateStr).millis()).rename('Date');  // Convert the DATE_ACQUIRED to an ee.Image (in milliseconds)
  var spacecraftID = image.get('SPACECRAFT_ID');  // Get SPACECRAFT_ID property
  var spacecraftMap = { 
    'LANDSAT_7': 7,
    'LANDSAT_8': 8,
    'LANDSAT_9': 9,
    // Add other spacecraft IDs as needed
  };
  var spacecraftNumber = ee.Number(ee.Dictionary(spacecraftMap).get(spacecraftID, -1));// Get the numeric value from the dictionary, or use -1 if not found.
  var spacecraftBand = ee.Image.constant(spacecraftNumber).rename('Spacecraft_ID');// Create a constant image with the numeric spacecraft ID
  return image.addBands(dateMillis).addBands(spacecraftBand); // Add these properties as bands to the image
};

var L7_NDVI_PoI1 = L7_NDVI_PoI1.map(addDateAndSpacecraftBand);
var L8_NDVI_PoI1 = L8_NDVI_PoI1.map(addDateAndSpacecraftBand);
var L7_NDVI_PoI2 = L7_NDVI_PoI2.map(addDateAndSpacecraftBand);
var L8_NDVI_PoI2 = L8_NDVI_PoI2.map(addDateAndSpacecraftBand);
var L7_NDVI_PoI3 = L7_NDVI_PoI3.map(addDateAndSpacecraftBand);
var L8_NDVI_PoI3 = L8_NDVI_PoI3.map(addDateAndSpacecraftBand);
var L7_NDVI_PoI4 = L7_NDVI_PoI4.map(addDateAndSpacecraftBand);
var L8_NDVI_PoI4 = L8_NDVI_PoI4.map(addDateAndSpacecraftBand);
var L7_NDVI_PoI5 = L7_NDVI_PoI5.map(addDateAndSpacecraftBand);
var L8_NDVI_PoI5 = L8_NDVI_PoI5.map(addDateAndSpacecraftBand);
var L7_NDVI_PoI6 = L7_NDVI_PoI6.map(addDateAndSpacecraftBand);
var L8_NDVI_PoI6 = L8_NDVI_PoI6.map(addDateAndSpacecraftBand);

var addTimeStampAndSpacecraftBand = function(image) {
  var timeStampMillis = ee.Date(image.get('system:time_start')).millis();
  var timeStampImage = ee.Image.constant(timeStampMillis).rename('Date');
  var spacecraftImage = ee.Image.constant(2).rename('Spacecraft_ID'); // 21 represents Sentinel_2A
  return image.addBands(timeStampImage).addBands(spacecraftImage);
};

var S2_NDVI_PoI1 = S2_NDVI_PoI1.map(addTimeStampAndSpacecraftBand);
var S2_NDVI_PoI2 = S2_NDVI_PoI2.map(addTimeStampAndSpacecraftBand);
var S2_NDVI_PoI3 = S2_NDVI_PoI3.map(addTimeStampAndSpacecraftBand);
var S2_NDVI_PoI4 = S2_NDVI_PoI4.map(addTimeStampAndSpacecraftBand);
var S2_NDVI_PoI5 = S2_NDVI_PoI5.map(addTimeStampAndSpacecraftBand);
var S2_NDVI_PoI6 = S2_NDVI_PoI6.map(addTimeStampAndSpacecraftBand);

// //Merge the series
// //<https://gis.stackexchange.com/questions/354961/plotting-time-series-of-different-products-in-google-earth-engine>
// Merge all points' NDVI collections into Merged_Series
var Merged_Series = L7_NDVI_PoI1
                    .merge(L7_NDVI_PoI2)
                    .merge(L7_NDVI_PoI3)
                    .merge(L7_NDVI_PoI4)
                    .merge(L7_NDVI_PoI5)
                    .merge(L7_NDVI_PoI6)
                    .merge(L8_NDVI_PoI1)
                    .merge(L8_NDVI_PoI2)
                    .merge(L8_NDVI_PoI3)
                    .merge(L8_NDVI_PoI4)
                    .merge(L8_NDVI_PoI5)
                    .merge(L8_NDVI_PoI6)
                    .merge(S2_NDVI_PoI4)
                    .merge(S2_NDVI_PoI5)
                    .merge(S2_NDVI_PoI1)
                    .merge(S2_NDVI_PoI6)
                    .merge(S2_NDVI_PoI3)
                    .merge(S2_NDVI_PoI2);

// Convert to an ImageCollection
Merged_Series = ee.ImageCollection(Merged_Series);
//print('Merged_Series', Merged_Series);
var sorted = ee.ImageCollection(Merged_Series).sort('system:time_start');
print('First and Last image years:', ee.Date(sorted.first().get('system:time_start')).format('YYYY'), ee.Date(sorted.sort('system:time_start', false).first().get('system:time_start')).format('YYYY'));

// Merging the image collections for each POI
// Convert each merged series to an ImageCollection
var Merged_SeriesPOI1 = ee.ImageCollection(L7_NDVI_PoI1.merge(L8_NDVI_PoI1).merge(S2_NDVI_PoI1));
var Merged_SeriesPOI2 = ee.ImageCollection(L7_NDVI_PoI2.merge(L8_NDVI_PoI2).merge(S2_NDVI_PoI2));
var Merged_SeriesPOI3 = ee.ImageCollection(L7_NDVI_PoI3.merge(L8_NDVI_PoI3).merge(S2_NDVI_PoI3));
var Merged_SeriesPOI4 = ee.ImageCollection(L7_NDVI_PoI4.merge(L8_NDVI_PoI4).merge(S2_NDVI_PoI4));
var Merged_SeriesPOI5 = ee.ImageCollection(L7_NDVI_PoI5.merge(L8_NDVI_PoI5).merge(S2_NDVI_PoI5));
var Merged_SeriesPOI6 = ee.ImageCollection(L7_NDVI_PoI6.merge(L8_NDVI_PoI6).merge(S2_NDVI_PoI6));


// // ######################################################################################################
// //                                    ### VI  calculation  ###
// // ######################################################################################################
// Function to calculate indices
var calculateIndices = function(image) {
  var N = image.select("nir");
  var R = image.select("red");
  var parameters = {
    "A": image.select("SR_B1"),
    "B": image.select("blue"),
    "G": image.select("green"),
    "R": image.select("red"),
    "N": image.select("nir"),
    "S1": image.select("swir1"),
    "S2": image.select("swir2"),
    "T1": image.select("B10"),
    "T2": image.select("B11"),
    "g": 2.5,
    "L": 0.5,
    "C1": 6.0,
    "C2": 7.5,
    "cexp": 1.16,
    "nexp": 2.0,
    "alpha": 0.1,
    "beta": 0.05,
    "gamma": 1.0,
    "omega": 2.0,
    "sla": 1.0,
    "slb": 0.0,
    'fdelta': 0.581,
    "kNN": 1.0,
    "kNR": spectral.computeKernel(image, "RBF", {
      "a": image.select("nir"),
      "b": image.select("red"),
      "sigma": N.add(R).divide(2),
    })
  };

// Compute indices
  var indices = spectral.computeIndex(image, [
"AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "AVI", "BCC", "BNDVI", "BWDRVI", "CIG", "CVI",//"CRI550",
 "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI",//, "DVIplus", "EBI", 
 "ENDVI", "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR","FCVI", 
 "GARI", "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", "GOSAVI", "GRNDVI", "GRVI", "GSAVI", 
 "GVMI","IAVI", "IKAW", "IPVI",
 "MCARI1", "MCARI2", "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2",
 "NDDI",  "NDII", "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv", //"NDGI",
  "NLI", "NMDI", "NRFIg", "NRFIr", "NormG", "NormNIR", "NormR", //"NIRvP","NIRvH2",
  "OCVI", "OSAVI", "RCC", "RDVI", "RGBVI", "RGRI", "RI",
  "SARVI", "SAVI", "SAVI2", "SEVI", "SI",  "SLAVI", "SR", "SR2", //"SIPI",
  "TDVI", "TGI", "TSAVI", "TVI", "TriVI", "VARI", "VIG", "WDRVI", "WDVI",
  "bNIRv", "sNIRvLSWI", "sNIRvNDPI", "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR",
  "ANDWI", "AWEInsh", "AWEIsh",  "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", "MuWIR", //"FAI",
   "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI",
   "RNDVI", "SWM", "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS",
   "BaI", "DBSI", "EMBI", "MBI", "NDSoI", "NSDS", "NSDSI1", "NSDSI2", "NSDSI3",
    "RI4XS", "kIPVI", "kNDVI", "kRVI"//, "kVARI","kEVI"
 ], parameters);

  // Add the computed indices to the image
  var imageWithIndices = image.addBands(indices);

  // Remove unwanted bands (e.g., blue_1, red_1, etc.)
  var bandsToKeep = imageWithIndices.bandNames().removeAll(["blue_1", "green_1", "red_1", "nir_1", "swir1_1", "swir2_1"]);
  var cleanedImage = imageWithIndices.select(bandsToKeep);

  // Return the cleaned image
  return cleanedImage;
};

// Apply the function to all collections
var processedPOI1 = Merged_SeriesPOI1.map(calculateIndices);
var processedPOI2 = Merged_SeriesPOI2.map(calculateIndices);
var processedPOI3 = Merged_SeriesPOI3.map(calculateIndices);
var processedPOI4 = Merged_SeriesPOI4.map(calculateIndices);
var processedPOI5 = Merged_SeriesPOI5.map(calculateIndices);
var processedPOI6 = Merged_SeriesPOI6.map(calculateIndices);


// Optionally, print or export the results for verification
//print(processedPOI1, "Processed POI1");


// Function to clip each image by the polygon
// var clipImage = function(image) {
//   return image.clipToCollection(PDHD2015);
// };
// Apply the clipping function to each collection
// var clippedPOI1 = processedPOI1.map(clipImage);
// var clippedPOI2 = processedPOI2.map(clipImage);
// var clippedPOI3 = processedPOI3.map(clipImage);
// var clippedPOI4 = processedPOI4.map(clipImage);
// var clippedPOI5 = processedPOI5.map(clipImage);
// var clippedPOI6 = processedPOI6.map(clipImage);

//Clipping Images:It is often desirable to clip the images to your area of interest. You can use the clip() function to mask out an image using a geometry.While in a Desktop software, clipping is desirable to remove unnecessary portion of a large image and save computation time, in Earth Engine clipping can actually increase the computation time. As described in the Earth Engine Coding Best Practices guide, avoid clipping the images or do it at the end of your script.
// Define the start and end dates for each year for POI1 to POI6

var processedPOI1 = processedPOI1.filterDate('2015-01-01', '2015-12-31');
var processedPOI2 = processedPOI2.filterDate('2015-01-01', '2015-12-31');
var processedPOI3 = processedPOI3.filterDate('2015-01-01', '2015-12-31');
var processedPOI4 = processedPOI4.filterDate('2015-01-01', '2015-12-31');
var processedPOI5 = processedPOI5.filterDate('2015-01-01', '2015-12-31');
var processedPOI6 = processedPOI6.filterDate('2015-01-01', '2015-12-31');

// Extract and print the FIELD_NAME values
var fieldNames = PDHD2015.aggregate_array('FIELD_NAME');

// ######################################################################################################
//                              ### AUTOMATED VI EXPORT BY PATHROWTIL ###
// ######################################################################################################
/// Load the PDHD2015 FeatureCollection
var PDHD2015 = ee.FeatureCollection('projects/ee-my-riasadbmahbub/assets/DOPDOHDL/PDHD2015');
print("PDHD2015",PDHD2015)
var F_8256_5_M12 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8256_5_M12'));
var F_8248_10_M6 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8248_10_M6'));
var F_8252_7_HF = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8252_7_HF'));
var F_8259_3_M3 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8259_3_M3'));
var F_8257_2_M2 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8257_2_M2'));
var F_8246_11_M4 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8246_11_M4'));
var F_8254_4_M10 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8254_4_M10'));
var F_8264_16_SHM = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8264_16_SHM'));
var F_8245_23_Cr = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8245_23_Cr'));
var F_8268_30_BS = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8268_30_BS'));
var F_8287_42_M30 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8287_42_M30'));
var F_8266_33_AP_1 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8266_33_AP_1'));
var F_8278_38_M18 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8278_38_M18'));
var F_8295_36_Ryan_W = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8295_36_Ryan_W'));
var F_8290_27_Pn_1 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8290_27_Pn_1'));
var F_8292_29_Pn_3 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8292_29_Pn_3'));
var F_8291_28_Pn_2 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8291_28_Pn_2'));
var F_8307_53_1 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8307_53_1'));
var F_8308_54_2 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8308_54_2'));
var F_8320_66_6 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8320_66_6'));
var F_8315_61_1 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8315_61_1'));
var F_8318_64_4 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8318_64_4'));
var F_8319_65_5 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8319_65_5'));
var F_8317_63_3 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8317_63_3'));
var F_8316_62_2 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_8316_62_2'));
var F_20581_68_MF = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_20581_68_MF'));
var F_20582_69_Res_N = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_20582_69_Res_N'));
var F_20583_70_Res_S = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'F_20583_70_Res_S'));
var Judys = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Judys'));
var East_Harvey = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_Harvey'));
var West_Harvey = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'West_Harvey'));
var Flat = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Flat'));
var Pops = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Pops'));
var Seed_Rice = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Seed_Rice'));
var Shanes = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Shanes'));
var Haley = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Haley'));
var Morris = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Morris'));
var Carr_North = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Carr_North'));
var Carr_South = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Carr_South'));
var West_Joe_T = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'West_Joe_T'));
var The_90 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'The_90'));
var Hole_40 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Hole_40'));
var Frog_40 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Frog_40'));
var Mid_40_Hwy = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Mid_40_Hwy'));
var Wst_Shop_40 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Wst_Shop_40'));
var HQ_Shop = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'HQ_Shop'));
var Lyntz = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Lyntz'));
var Cotton_Patch = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Cotton_Patch'));
var Way_01 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Way_01'));
var Way_02 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Way_02'));
var Way_03 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Way_03'));
var Way_04 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Way_04'));
var Way_05 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Way_05'));
var Baker_30 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Baker_30'));
var New_Ground = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'New_Ground'));
var Top_of_Res = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Top_of_Res'));
var Bott_of_Res = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Bott_of_Res'));
var Baker_40 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Baker_40'));
var Baker_20 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Baker_20'));
var Baker_50 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Baker_50'));
var Kelly_01 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Kelly_01'));
var Kelly_02 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Kelly_02'));
var Kelly_03 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Kelly_03'));
var Kelly_04 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Kelly_04'));
var Kelly_05 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Kelly_05'));
var Kelly_06 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Kelly_06'));
var Kelly_07 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Kelly_07'));
var Kelly_08 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Kelly_08'));
var Kelly_09 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Kelly_09'));
var Kelly_10 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Kelly_10'));
var Kelly_11 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Kelly_11'));
var Bransford_Est = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Bransford_Est'));
var Mid_Bransford = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Mid_Bransford'));
var Wst_Bransford = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Wst_Bransford'));
var Walls_01 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Walls_01'));
var Walls_02 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Walls_02'));
var Walls_03 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Walls_03'));
var Walls_04 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Walls_04'));
var Walls_05 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Walls_05'));
var Walls_08 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Walls_08'));
var Walls_09 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Walls_09'));
var Walls_10 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Walls_10'));
var Walls_11 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Walls_11'));
var Walls_12 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Walls_12'));
var Walls_13 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Walls_13'));
var East_01 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_01'));
var East_02 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_02'));
var East_03 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_03'));
var East_04 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_04'));
var East_05 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_05'));
var East_06 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_06'));
var East_07 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_07'));
var East_08 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_08'));
var East_09 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_09'));
var East_10 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_10'));
var East_11 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_11'));
var East_12 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_12'));
var East_13 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_13'));
var East_14 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'East_14'));
var Cattlet_01 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Cattlet_01'));
var Cattlet_02 = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', 'Cattlet_02'));


// Function to reduce an ImageCollection by a given FeatureCollection
var reduceByFeatureCollection = function(imageCollection, featureCollection) {
  return imageCollection.map(function(image) {
    var meanStats = featureCollection.map(function(feature) {
      var stats = image.reduceRegion({
        reducer: ee.Reducer.mean(),
        geometry: feature.geometry(),
        scale: 30,
        crs: 'EPSG:4326'
      });
      return feature.set(stats).set('image_id', image.id());
    });
    return meanStats;
  }).flatten();
};

var results = {
  'F_8256_5_M12': reduceByFeatureCollection(processedPOI1, F_8256_5_M12),
  'F_8248_10_M6': reduceByFeatureCollection(processedPOI1, F_8248_10_M6),
  'F_8252_7_HF': reduceByFeatureCollection(processedPOI1, F_8252_7_HF),
  'F_8259_3_M3': reduceByFeatureCollection(processedPOI1, F_8259_3_M3),
  'F_8257_2_M2': reduceByFeatureCollection(processedPOI1, F_8257_2_M2),
  'F_8246_11_M4': reduceByFeatureCollection(processedPOI1, F_8246_11_M4),
  'F_8254_4_M10': reduceByFeatureCollection(processedPOI1, F_8254_4_M10),
  'F_8264_16_SHM': reduceByFeatureCollection(processedPOI1, F_8264_16_SHM),
  'F_8245_23_Cr': reduceByFeatureCollection(processedPOI1, F_8245_23_Cr),
  'F_8268_30_BS': reduceByFeatureCollection(processedPOI1, F_8268_30_BS),
  'F_8287_42_M30': reduceByFeatureCollection(processedPOI1, F_8287_42_M30),
  'F_8266_33_AP_1': reduceByFeatureCollection(processedPOI1, F_8266_33_AP_1),
  'F_8278_38_M18': reduceByFeatureCollection(processedPOI1, F_8278_38_M18),
  'F_8295_36_Ryan_W': reduceByFeatureCollection(processedPOI1, F_8295_36_Ryan_W),
  'F_8290_27_Pn_1': reduceByFeatureCollection(processedPOI1, F_8290_27_Pn_1),
  'F_8292_29_Pn_3': reduceByFeatureCollection(processedPOI1, F_8292_29_Pn_3),
  'F_8291_28_Pn_2': reduceByFeatureCollection(processedPOI1, F_8291_28_Pn_2),
  'F_8307_53_1': reduceByFeatureCollection(processedPOI1, F_8307_53_1),
  'F_8308_54_2': reduceByFeatureCollection(processedPOI1, F_8308_54_2),
  'F_8320_66_6': reduceByFeatureCollection(processedPOI1, F_8320_66_6),
  'F_8315_61_1': reduceByFeatureCollection(processedPOI1, F_8315_61_1),
  'F_8318_64_4': reduceByFeatureCollection(processedPOI1, F_8318_64_4),
  'F_8319_65_5': reduceByFeatureCollection(processedPOI1, F_8319_65_5),
  'F_8317_63_3': reduceByFeatureCollection(processedPOI1, F_8317_63_3),
  'F_8316_62_2': reduceByFeatureCollection(processedPOI1, F_8316_62_2),
  'F_20581_68_MF': reduceByFeatureCollection(processedPOI1, F_20581_68_MF),
  'F_20582_69_Res_N': reduceByFeatureCollection(processedPOI1, F_20582_69_Res_N),
  'F_20583_70_Res_S': reduceByFeatureCollection(processedPOI1, F_20583_70_Res_S),
  'Judys': reduceByFeatureCollection(processedPOI1, Judys),
  'East_Harvey': reduceByFeatureCollection(processedPOI1, East_Harvey),
  'West_Harvey': reduceByFeatureCollection(processedPOI1, West_Harvey),
  'Flat': reduceByFeatureCollection(processedPOI1, Flat),
  'Pops': reduceByFeatureCollection(processedPOI1, Pops),
  'Seed_Rice': reduceByFeatureCollection(processedPOI1, Seed_Rice),
  'Shanes': reduceByFeatureCollection(processedPOI1, Shanes),
  'Haley': reduceByFeatureCollection(processedPOI1, Haley),
  'Morris': reduceByFeatureCollection(processedPOI1, Morris),
  'Carr_North': reduceByFeatureCollection(processedPOI1, Carr_North),
  'Carr_South': reduceByFeatureCollection(processedPOI1, Carr_South),
  'West_Joe_T': reduceByFeatureCollection(processedPOI1, West_Joe_T),
  'The_90': reduceByFeatureCollection(processedPOI1, The_90),
  'Hole_40': reduceByFeatureCollection(processedPOI1, Hole_40),
  'Frog_40': reduceByFeatureCollection(processedPOI1, Frog_40),
  'Mid_40_Hwy': reduceByFeatureCollection(processedPOI1, Mid_40_Hwy),
  'Wst_Shop_40': reduceByFeatureCollection(processedPOI1, Wst_Shop_40),
  'HQ_Shop': reduceByFeatureCollection(processedPOI1, HQ_Shop),
  'Lyntz': reduceByFeatureCollection(processedPOI1, Lyntz),
  'Cotton_Patch': reduceByFeatureCollection(processedPOI1, Cotton_Patch),
  'Way_01': reduceByFeatureCollection(processedPOI1, Way_01),
  'Way_02': reduceByFeatureCollection(processedPOI1, Way_02),
  'Way_03': reduceByFeatureCollection(processedPOI1, Way_03),
  'Way_04': reduceByFeatureCollection(processedPOI1, Way_04),
  'Way_05': reduceByFeatureCollection(processedPOI1, Way_05),
  'Baker_30': reduceByFeatureCollection(processedPOI1, Baker_30),
  'New_Ground': reduceByFeatureCollection(processedPOI1, New_Ground),
  'Top_of_Res': reduceByFeatureCollection(processedPOI1, Top_of_Res),
  'Bott_of_Res': reduceByFeatureCollection(processedPOI1, Bott_of_Res),
  'Baker_40': reduceByFeatureCollection(processedPOI1, Baker_40),
  'Baker_20': reduceByFeatureCollection(processedPOI1, Baker_20),
  'Baker_50': reduceByFeatureCollection(processedPOI1, Baker_50),
  'Kelly_01': reduceByFeatureCollection(processedPOI1, Kelly_01),
  'Kelly_02': reduceByFeatureCollection(processedPOI1, Kelly_02),
  'Kelly_03': reduceByFeatureCollection(processedPOI1, Kelly_03),
  'Kelly_04': reduceByFeatureCollection(processedPOI1, Kelly_04),
  'Kelly_05': reduceByFeatureCollection(processedPOI1, Kelly_05),
  'Kelly_06': reduceByFeatureCollection(processedPOI1, Kelly_06),
  'Kelly_07': reduceByFeatureCollection(processedPOI1, Kelly_07),
  'Kelly_08': reduceByFeatureCollection(processedPOI1, Kelly_08),
  'Kelly_09': reduceByFeatureCollection(processedPOI1, Kelly_09),
  'Kelly_10': reduceByFeatureCollection(processedPOI1, Kelly_10),
  'Kelly_11': reduceByFeatureCollection(processedPOI1, Kelly_11),
  'Bransford_Est': reduceByFeatureCollection(processedPOI1, Bransford_Est),
  'Mid_Bransford': reduceByFeatureCollection(processedPOI1, Mid_Bransford),
  'Wst_Bransford': reduceByFeatureCollection(processedPOI1, Wst_Bransford),
  'Walls_01': reduceByFeatureCollection(processedPOI1, Walls_01),
  'Walls_02': reduceByFeatureCollection(processedPOI1, Walls_02),
  'Walls_03': reduceByFeatureCollection(processedPOI1, Walls_03),
  'Walls_04': reduceByFeatureCollection(processedPOI1, Walls_04),
  'Walls_05': reduceByFeatureCollection(processedPOI1, Walls_05),
  'Walls_08': reduceByFeatureCollection(processedPOI1, Walls_08),
  'Walls_09': reduceByFeatureCollection(processedPOI1, Walls_09),
  'Walls_10': reduceByFeatureCollection(processedPOI1, Walls_10),
  'Walls_11': reduceByFeatureCollection(processedPOI1, Walls_11),
  'Walls_12': reduceByFeatureCollection(processedPOI1, Walls_12),
  'Walls_13': reduceByFeatureCollection(processedPOI1, Walls_13),
  'East_01': reduceByFeatureCollection(processedPOI1, East_01),
  'East_02': reduceByFeatureCollection(processedPOI1, East_02),
  'East_03': reduceByFeatureCollection(processedPOI1, East_03),
  'East_04': reduceByFeatureCollection(processedPOI1, East_04),
  'East_05': reduceByFeatureCollection(processedPOI1, East_05),
  'East_06': reduceByFeatureCollection(processedPOI1, East_06),
  'East_07': reduceByFeatureCollection(processedPOI1, East_07),
  'East_08': reduceByFeatureCollection(processedPOI1, East_08),
  'East_09': reduceByFeatureCollection(processedPOI1, East_09),
  'East_10': reduceByFeatureCollection(processedPOI1, East_10),
  'East_11': reduceByFeatureCollection(processedPOI1, East_11),
  'East_12': reduceByFeatureCollection(processedPOI1, East_12),
  'East_13': reduceByFeatureCollection(processedPOI1, East_13),
  'East_14': reduceByFeatureCollection(processedPOI1, East_14),
  'Cattlet_01': reduceByFeatureCollection(processedPOI1, Cattlet_01),
  'Cattlet_02': reduceByFeatureCollection(processedPOI1, Cattlet_02)
};

print("results", results)

// List of properties to drop
var propertiesToDrop = [
  'altitudeMo', 'begin', 'descriptio', 'end', 'extrude', 
  'icon', 'tessellate', 'timestamp', 'visibility'
];

// Function to remove unwanted properties
var removeProperties = function(feat) {
  var properties = feat.propertyNames();
  var selectedProperties = properties.removeAll(propertiesToDrop);
  return feat.select(selectedProperties);
};

// Function to clean a FeatureCollection so that the first element i snot null for export or else export four columns only
var cleanFeatureCollection = function(featureCollection) {
  // Map through the FeatureCollection and calculate property size for each feature
  var filteredFeatures = featureCollection.filter(ee.Filter.gte('property_size', 144));
  // Calculate property size for each feature and store it as a new property 'property_size'
  var updatedCollection = featureCollection.map(function(feat) {
    var propertyCount = feat.propertyNames().size();  // Get the property count of the feature
    return feat.set('property_size', propertyCount);   // Store it as a new property
  });
  // Filter out features that have fewer than 144 properties
  var resultCollection = updatedCollection.filter(ee.Filter.gte('property_size', 144));
  var cleanedCollection = resultCollection.map(removeProperties);
  return cleanedCollection;
};


// Apply property cleanup and export each FeatureCollection
Object.keys(results).forEach(function(site) {
  var cleanedCollection = cleanFeatureCollection(results[site]);
  
  Export.table.toDrive({
    collection: cleanedCollection,
    description: site + '_VI_2015',
    folder: 'PDHDVI',
    fileFormat: 'CSV'
  });
});


// Apply property cleanup and store the results in a new object
var cleanedResults = {};
Object.keys(results).forEach(function(site) {
  cleanedResults[site] = cleanFeatureCollection(results[site]);
});

// Print the cleaned results in two lines
print("Cleaned results (FeatureCollections):", cleanedResults);


// // ######################################################################################################
// //                                    ### Meteo  Export  ###
// // ######################################################################################################
// Function to join images by nearest timestamp (in case of mismatch)

// Function to extract date from etDatasetPML system:index and format it
function extractDateFromPML(image) {
    var systemIndex = ee.String(image.get('system:index'));
    var datePart = systemIndex.split('PML_V2_8d_v018_').get(1); // Extract 'YYYY-MM-DD'
    return image.set('formatted_date', datePart);
}

// Function to extract and format date from system:index
function extractDateForVPDayET(image) {
    var systemIndex = ee.String(image.get('system:index'));
    // Extract year, month, and day from the 8-digit string
    var year = systemIndex.slice(0, 4);
    var month = systemIndex.slice(4, 6);
    var day = systemIndex.slice(6, 8);
    // Format as "YYYY-MM-DD"
    var formattedDate = year.cat('-').cat(month).cat('-').cat(day);
    return image.set('formatted_date', formattedDate);
}

// Function to extract and format date from system:index
function extractDateForLSTLAINight(image) {
    var systemIndex = ee.String(image.get('system:index'));
    // Extract year, month, and day from "YYYY_MM_dd"
    var formattedDate = systemIndex.replace("_", "-").replace("_", "-");
    return image.set('formatted_date', formattedDate);
}

// Function to compute avgRH (Average Relative Humidity)
function computeAvgRH(image) {
    var rmax = image.select('rmax'); // Maximum relative humidity
    var rmin = image.select('rmin'); // Minimum relative humidity
    var avgRH = rmax.add(rmin).divide(2).rename('avgRH'); // Compute avg and rename band
    
    return image.addBands(avgRH);
}

var startDate = ee.Date('2015-01-01');
var endDate = ee.Date('2015-12-31');

// Load PRISM LST dataset (Daily)
var lstDataset = ee.ImageCollection('OREGONSTATE/PRISM/AN81d')
    .select('tmean', 'ppt')
    .filterDate(startDate, endDate)
    .filterBounds(PDHD2015)
    .map(function(image) { 
        return image.set('date', image.date());
    });
    
print("lstDataset", lstDataset);

// Load LAI dataset (VIIRS, 8-day)
var LAIdataset = ee.ImageCollection('NASA/VIIRS/002/VNP15A2H')
    .select('Lai')
    .filterDate(startDate, endDate)
    .filterBounds(PDHD2015)
    .map(extractDateForLSTLAINight)
    .map(function(image) {
        return image.set('date', image.date());
    });

print("LAIdataset", LAIdataset);

// Load Nighttime Temperature dataset (VIIRS, 8-day)
var NTTempdataset = ee.ImageCollection('NASA/VIIRS/002/VNP21A1N')
    .select("LST_1KM") 
    .filterDate(startDate, endDate)
    .filterBounds(PDHD2015)
    .map(extractDateForLSTLAINight)
    .map(function(image) {
        // Convert from Kelvin to Celsius and rename band
        var celsius = image.select("LST_1KM").subtract(273.15).rename("NTmean");
        return image.addBands(celsius, null, true)
                   .set('date', image.date());
    });

print("NTTempdataset", NTTempdataset);

// Function to convert system:index to YYYY_MM_dd
function formatDateFromIndex(image) {
    var index = image.getString('system:index'); // Get the system:index
    var formattedDate = ee.Date.parse('yyyyMMdd', index).format('YYYY_MM_dd'); // Convert to YYYY_MM_dd
    return image.set('date', formattedDate); // Set new formatted date property
}

// Apply functions to vpdDataset
var vpdDataset = ee.ImageCollection('IDAHO_EPSCOR/GRIDMET')
    .select(['vpd', 'rmax', 'rmin', 'srad']) // Select necessary bands
    .filterDate(startDate, endDate)
    .filterBounds(PDHD2015)
    .map(extractDateForVPDayET) // Format system:index date
    .map(computeAvgRH); // Compute avgRH
  
print("vpdDataset", vpdDataset)

var daylDataset = ee.ImageCollection('NASA/ORNL/DAYMET_V4')
  .select('dayl')
  .filterDate(startDate, endDate)
  .filterBounds(PDHD2015)
  .map(extractDateForVPDayET)
  .map(formatDateFromIndex); // Convert index format
print("daylDataset", daylDataset)
  
var etDataset = ee.ImageCollection('MODIS/061/MOD16A2')
  .select('ET')
  .filterDate(startDate, endDate)
  .filterBounds(PDHD2015)
  .map(extractDateForVPDayET)
  .map(formatDateFromIndex); // Convert index format
print("etDataset", etDataset)
  
var etDatasetPML = ee.ImageCollection('CAS/IGSNRR/PML/V2_v018')
  .select('Ei', 'Ec', 'Es')
  .filterDate(startDate, endDate)
  .map(extractDateFromPML)
  .filterBounds(PDHD2015)
  
print("etDatasetPML", etDatasetPML)


// Apply the function to store extracted dates in etDatasetPML
var etDatasetPMLFormatted = etDatasetPML.map(extractDateFromPML);
print("etDatasetPMLFormatted", etDatasetPMLFormatted);

// Function to get the value if system:index matches; otherwise, return an empty image
function getMatchedValue(index, dataset, bandName) {
    var matchedImage = dataset.filter(ee.Filter.eq('formatted_date', index)).first();
    return ee.Algorithms.If(
        matchedImage, 
        matchedImage.select(bandName), 
        ee.Image([]) // Returns an empty image instead of -9999
    );
}


// Merge datasets
var mergedCollection = lstDataset.map(function(lstImage) {
    var date = lstImage.get('date');

    // Two formats: One for LST/LAI/Nighttime, One for VPD/Dayl/ET
    var formattedDate1 = ee.Date(date).format('YYYY-MM-dd'); // For LST, LAI, Night
    var formattedDate2 = ee.Date(date).format('YYYY-MM-dd');   // For VPD, Dayl, ET
    
    var matchingNTT = ee.Image(getMatchedValue(formattedDate1, NTTempdataset, "LST_1KM"));
    var matchingLAI = ee.Image(getMatchedValue(formattedDate1, LAIdataset, "Lai"));
    var matchingVPD = ee.Image(getMatchedValue(formattedDate2, vpdDataset, "vpd"));
    var matchingDayl = ee.Image(getMatchedValue(formattedDate2, daylDataset, "dayl"));
    //var matchingET = ee.Image(getMatchedValue(formattedDate2, etDataset, "ET"));
    var matchingAvgRH = ee.Image(getMatchedValue(formattedDate2, vpdDataset, "avgRH"));
    var matchingsrad = ee.Image(getMatchedValue(formattedDate2, vpdDataset, "srad"));// Add avgRH
    // Match PML dataset bands (Ei, Ec, Es)
    var matchingEi = ee.Image(getMatchedValue(formattedDate1, etDatasetPML, "Ei"));
    var matchingEc = ee.Image(getMatchedValue(formattedDate1, etDatasetPML, "Ec"));
    var matchingEs = ee.Image(getMatchedValue(formattedDate1, etDatasetPML, "Es"));

    // Convert date to Unix timestamp (seconds since 1970-01-01) and name it "Date"
    var unixTime = ee.Image(ee.Date(date).millis().divide(1000)).rename("Date");

    return lstImage
        .addBands(matchingNTT)
        .addBands(matchingLAI)
        .addBands(matchingVPD)
        .addBands(matchingDayl)
       // .addBands(matchingET)
        .addBands(matchingAvgRH)
        .addBands(matchingsrad)
        .addBands(matchingEi) // Add Ei band from PML
        .addBands(matchingEc) // Add Ec band from PML
        .addBands(matchingEs) // Add Es band from PML
        .addBands(unixTime); // Add Unix timestamp as "Date" band
});

//print("mergedCollection", mergedCollection);


// // Convert merged images into a FeatureCollection
// var features = mergedCollection.map(function(image) {
//     var date = image.get('date');
//     var dict = image.reduceRegion({
//         reducer: ee.Reducer.mean(),
//         geometry: point,
//         scale: 1000
//     });
//     return ee.Feature(point, dict).set('date', date);
// });



// Create a time series chart for nighttime temperature
var chart = ui.Chart.image.series({
    imageCollection: NTTempdataset,
    region: PoI3,
    reducer: ee.Reducer.mean(),
    scale: 1000
}).setOptions({
    title: 'Nighttime Temperature Time Series',
    hAxis: {title: 'Date'},
    vAxis: {title: 'Temperature (Kelvin)'}
});

print(chart);



// Function to reduce an ImageCollection by a given FeatureCollection
var reduceByFeatureCollection = function(imageCollection, featureCollection) {
  return imageCollection.map(function(image) {
    var meanStats = featureCollection.map(function(feature) {
      var stats = image.reduceRegion({
        reducer: ee.Reducer.mean(),
        geometry: feature.geometry(),
        scale: 30,
        crs: 'EPSG:4326'
      });
      return feature.set(stats).set('image_id', image.id());
    });
    return meanStats;
  }).flatten();
};

var copyNTmeanByIndex = function(featureCollection) {
  var features = featureCollection.toList(featureCollection.size());
  var source = ee.Feature(features.get(1));
  var target = ee.Feature(features.get(0));
  var updated = target.set('NTmean', source.get('NTmean'));
  // Build new list by:
  // 1. Taking the updated feature
  // 2. Adding features from index 1 onward
  var newFeatures = ee.List([updated]).cat(features.slice(1));
  return ee.FeatureCollection(newFeatures);
};
var meteoResults = {
  'F_8256_5_M12': reduceByFeatureCollection(mergedCollection, F_8256_5_M12),
  'F_8248_10_M6': reduceByFeatureCollection(mergedCollection, F_8248_10_M6),
  'F_8252_7_HF': reduceByFeatureCollection(mergedCollection, F_8252_7_HF),
  'F_8259_3_M3': reduceByFeatureCollection(mergedCollection, F_8259_3_M3),
  'F_8257_2_M2': reduceByFeatureCollection(mergedCollection, F_8257_2_M2),
  'F_8246_11_M4': reduceByFeatureCollection(mergedCollection, F_8246_11_M4),
  'F_8254_4_M10': reduceByFeatureCollection(mergedCollection, F_8254_4_M10),
  'F_8264_16_SHM': reduceByFeatureCollection(mergedCollection, F_8264_16_SHM),
  'F_8245_23_Cr': reduceByFeatureCollection(mergedCollection, F_8245_23_Cr),
  'F_8268_30_BS': reduceByFeatureCollection(mergedCollection, F_8268_30_BS),
  'F_8287_42_M30': reduceByFeatureCollection(mergedCollection, F_8287_42_M30),
  'F_8266_33_AP_1': reduceByFeatureCollection(mergedCollection, F_8266_33_AP_1),
  'F_8278_38_M18': reduceByFeatureCollection(mergedCollection, F_8278_38_M18),
  'F_8295_36_Ryan_W': reduceByFeatureCollection(mergedCollection, F_8295_36_Ryan_W),
  'F_8290_27_Pn_1': reduceByFeatureCollection(mergedCollection, F_8290_27_Pn_1),
  'F_8292_29_Pn_3': reduceByFeatureCollection(mergedCollection, F_8292_29_Pn_3),
  'F_8291_28_Pn_2': reduceByFeatureCollection(mergedCollection, F_8291_28_Pn_2),
  'F_8307_53_1': reduceByFeatureCollection(mergedCollection, F_8307_53_1),
  'F_8308_54_2': reduceByFeatureCollection(mergedCollection, F_8308_54_2),
  'F_8320_66_6': reduceByFeatureCollection(mergedCollection, F_8320_66_6),
  'F_8315_61_1': reduceByFeatureCollection(mergedCollection, F_8315_61_1),
  'F_8318_64_4': reduceByFeatureCollection(mergedCollection, F_8318_64_4),
  'F_8319_65_5': reduceByFeatureCollection(mergedCollection, F_8319_65_5),
  'F_8317_63_3': reduceByFeatureCollection(mergedCollection, F_8317_63_3),
  'F_8316_62_2': reduceByFeatureCollection(mergedCollection, F_8316_62_2),
  'F_20581_68_MF': reduceByFeatureCollection(mergedCollection, F_20581_68_MF),
  'F_20582_69_Res_N': reduceByFeatureCollection(mergedCollection, F_20582_69_Res_N),
  'F_20583_70_Res_S': reduceByFeatureCollection(mergedCollection, F_20583_70_Res_S),
  'Judys': reduceByFeatureCollection(mergedCollection, Judys),
  'East_Harvey': reduceByFeatureCollection(mergedCollection, East_Harvey),
  'West_Harvey': reduceByFeatureCollection(mergedCollection, West_Harvey),
  'Flat': reduceByFeatureCollection(mergedCollection, Flat),
  'Pops': reduceByFeatureCollection(mergedCollection, Pops),
  'Seed_Rice': reduceByFeatureCollection(mergedCollection, Seed_Rice),
  'Shanes': reduceByFeatureCollection(mergedCollection, Shanes),
  'Haley': reduceByFeatureCollection(mergedCollection, Haley),
  'Morris': reduceByFeatureCollection(mergedCollection, Morris),
  'Carr_North': reduceByFeatureCollection(mergedCollection, Carr_North),
  'Carr_South': reduceByFeatureCollection(mergedCollection, Carr_South),
  'West_Joe_T': reduceByFeatureCollection(mergedCollection, West_Joe_T),
  'The_90': reduceByFeatureCollection(mergedCollection, The_90),
  'Hole_40': reduceByFeatureCollection(mergedCollection, Hole_40),
  'Frog_40': reduceByFeatureCollection(mergedCollection, Frog_40),
  'Mid_40_Hwy': reduceByFeatureCollection(mergedCollection, Mid_40_Hwy),
  'Wst_Shop_40': reduceByFeatureCollection(mergedCollection, Wst_Shop_40),
  'HQ_Shop': reduceByFeatureCollection(mergedCollection, HQ_Shop),
  'Lyntz': reduceByFeatureCollection(mergedCollection, Lyntz),
  'Cotton_Patch': reduceByFeatureCollection(mergedCollection, Cotton_Patch),
  'Way_01': reduceByFeatureCollection(mergedCollection, Way_01),
  'Way_02': reduceByFeatureCollection(mergedCollection, Way_02),
  'Way_03': reduceByFeatureCollection(mergedCollection, Way_03),
  'Way_04': reduceByFeatureCollection(mergedCollection, Way_04),
  'Way_05': reduceByFeatureCollection(mergedCollection, Way_05),
  'Baker_30': reduceByFeatureCollection(mergedCollection, Baker_30),
  'New_Ground': reduceByFeatureCollection(mergedCollection, New_Ground),
  'Top_of_Res': reduceByFeatureCollection(mergedCollection, Top_of_Res),
  'Bott_of_Res': reduceByFeatureCollection(mergedCollection, Bott_of_Res),
  'Baker_40': reduceByFeatureCollection(mergedCollection, Baker_40),
  'Baker_20': reduceByFeatureCollection(mergedCollection, Baker_20),
  'Baker_50': reduceByFeatureCollection(mergedCollection, Baker_50),
  'Kelly_01': reduceByFeatureCollection(mergedCollection, Kelly_01),
  'Kelly_02': reduceByFeatureCollection(mergedCollection, Kelly_02),
  'Kelly_03': reduceByFeatureCollection(mergedCollection, Kelly_03),
  'Kelly_04': reduceByFeatureCollection(mergedCollection, Kelly_04),
  'Kelly_05': reduceByFeatureCollection(mergedCollection, Kelly_05),
  'Kelly_06': reduceByFeatureCollection(mergedCollection, Kelly_06),
  'Kelly_07': reduceByFeatureCollection(mergedCollection, Kelly_07),
  'Kelly_08': reduceByFeatureCollection(mergedCollection, Kelly_08),
  'Kelly_09': reduceByFeatureCollection(mergedCollection, Kelly_09),
  'Kelly_10': reduceByFeatureCollection(mergedCollection, Kelly_10),
  'Kelly_11': reduceByFeatureCollection(mergedCollection, Kelly_11),
  'Bransford_Est': reduceByFeatureCollection(mergedCollection, Bransford_Est),
  'Mid_Bransford': reduceByFeatureCollection(mergedCollection, Mid_Bransford),
  'Wst_Bransford': reduceByFeatureCollection(mergedCollection, Wst_Bransford),
  'Walls_01': reduceByFeatureCollection(mergedCollection, Walls_01),
  'Walls_02': reduceByFeatureCollection(mergedCollection, Walls_02),
  'Walls_03': reduceByFeatureCollection(mergedCollection, Walls_03),
  'Walls_04': reduceByFeatureCollection(mergedCollection, Walls_04),
  'Walls_05': reduceByFeatureCollection(mergedCollection, Walls_05),
  'Walls_08': reduceByFeatureCollection(mergedCollection, Walls_08),
  'Walls_09': reduceByFeatureCollection(mergedCollection, Walls_09),
  'Walls_10': reduceByFeatureCollection(mergedCollection, Walls_10),
  'Walls_11': reduceByFeatureCollection(mergedCollection, Walls_11),
  'Walls_12': reduceByFeatureCollection(mergedCollection, Walls_12),
  'Walls_13': reduceByFeatureCollection(mergedCollection, Walls_13),
  'East_01': reduceByFeatureCollection(mergedCollection, East_01),
  'East_02': reduceByFeatureCollection(mergedCollection, East_02),
  'East_03': reduceByFeatureCollection(mergedCollection, East_03),
  'East_04': reduceByFeatureCollection(mergedCollection, East_04),
  'East_05': reduceByFeatureCollection(mergedCollection, East_05),
  'East_06': reduceByFeatureCollection(mergedCollection, East_06),
  'East_07': reduceByFeatureCollection(mergedCollection, East_07),
  'East_08': reduceByFeatureCollection(mergedCollection, East_08),
  'East_09': reduceByFeatureCollection(mergedCollection, East_09),
  'East_10': reduceByFeatureCollection(mergedCollection, East_10),
  'East_11': reduceByFeatureCollection(mergedCollection, East_11),
  'East_12': reduceByFeatureCollection(mergedCollection, East_12),
  'East_13': reduceByFeatureCollection(mergedCollection, East_13),
  'East_14': reduceByFeatureCollection(mergedCollection, East_14),
  'Cattlet_01': reduceByFeatureCollection(mergedCollection, Cattlet_01),
  'Cattlet_02': reduceByFeatureCollection(mergedCollection, Cattlet_02)
};
// List of properties to drop
var propertiesToDrop = [
  'altitudeMo', 'begin', 'descriptio', 'end', 'extrude', 
  'icon', 'tessellate', 'timestamp', 'visibility'
];

// Function to remove unwanted properties
var removeProperties = function(feat) {
  var properties = feat.propertyNames();
  var selectedProperties = properties.removeAll(propertiesToDrop);
  return feat.select(selectedProperties);
};

// Apply both functions to each FeatureCollection in meteoResults
Object.keys(meteoResults).forEach(function(site) {
  // Apply copyNTmeanByIndex to update NTmean for Feature 0
  var updatedCollection = copyNTmeanByIndex(meteoResults[site]);
  
  // Apply removeProperties to remove unwanted properties
  var cleanedCollection = updatedCollection.map(removeProperties);
  
  // Export the cleaned and updated FeatureCollection as CSV
  Export.table.toDrive({
    collection: cleanedCollection,
    description: site + '_Meteo2015', // Updated filename
    folder: 'PDHDVI', // Updated folder
    fileFormat: 'CSV'
  });
});