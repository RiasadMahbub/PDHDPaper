var PDHD2019 = ee.FeatureCollection("projects/ee-my-riasadbmahbub/assets/DOPDOHDL/PDHD2019"),
    table = ee.FeatureCollection("projects/ee-my-riasadbmahbub/assets/PDHD_Merged_2015_2024");
var spectral = require('users/dmlmont/spectral:spectral');

// Link to original code: https://code.earthengine.google.com/934b2252c5403a1b3af9a4a0ccb4c9e1
var PointName = 'USHRA';
Map.addLayer(table)
// Define date range to search for matching Landsat and Sentinel-2 images
var startDate = ee.Date('2019-01-01');
var endDate = ee.Date('2019-12-31');
// Define cloud cover thresholds (adjust as needed)
var maxCloudCoverLandsat = 20; // Landsat cloud cover threshold
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

// Filter Landsat and Sentinel-2 collections for each PoI
var L7_col1 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filterBounds(PoI1)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var L8_col1 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filterBounds(PoI1)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var S2_col1 = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
  .filterBounds(PoI1)
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel));

// Repeat for PoI2
var L7_col2 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filterBounds(PoI2)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var L8_col2 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filterBounds(PoI2)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var S2_col2 = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
  .filterBounds(PoI2)
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel));

// Repeat similarly for PoI3, PoI4, PoI5, and PoI6
var L7_col3 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filterBounds(PoI3)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var L8_col3 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filterBounds(PoI3)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var S2_col3 = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
  .filterBounds(PoI3)
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel));

var L7_col4 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filterBounds(PoI4)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var L8_col4 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filterBounds(PoI4)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var S2_col4 = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
  .filterBounds(PoI4)
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel));

var L7_col5 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filterBounds(PoI5)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var L8_col5 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filterBounds(PoI5)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var S2_col5 = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
  .filterBounds(PoI5)
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel));

var L7_col6 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
  .filterBounds(PoI6)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var L8_col6 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
  .filterBounds(PoI6)
  .filter(ee.Filter.lt('CLOUD_COVER', maxCloudCoverLandsat));
var S2_col6 = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
  .filterBounds(PoI6)
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', maxCloudCoverSentinel));

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

// Find matching dates with clear skies for Landsat 7 - Sentinel-2
var clearSkyPairsL7S21 = findClearSkyPairs(L7_col1.filterDate(startDate, endDate), S2_col1.filterDate(startDate, endDate), 1);
print('1Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S21);

// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S21 = findClearSkyPairs(L8_col1.filterDate(startDate, endDate), S2_col1.filterDate(startDate, endDate), 1);
print('1Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S21);

var clearSkyPairsL7S22 = findClearSkyPairs(L7_col2.filterDate(startDate, endDate), S2_col2.filterDate(startDate, endDate), 1);
print('2Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S22);

// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S22 = findClearSkyPairs(L8_col2.filterDate(startDate, endDate), S2_col2.filterDate(startDate, endDate), 1);
print('2Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S22);

var clearSkyPairsL7S23 = findClearSkyPairs(L7_col3.filterDate(startDate, endDate), S2_col3.filterDate(startDate, endDate), 1);
print('3Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S23);

// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S23 = findClearSkyPairs(L8_col3.filterDate(startDate, endDate), S2_col3.filterDate(startDate, endDate), 1);
print('3Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S23);

var clearSkyPairsL7S24 = findClearSkyPairs(L7_col4.filterDate(startDate, endDate), S2_col4.filterDate(startDate, endDate), 1);
print('4Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S24);

// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S24 = findClearSkyPairs(L8_col4.filterDate(startDate, endDate), S2_col4.filterDate(startDate, endDate), 1);
print('4Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S24);

var clearSkyPairsL7S25 = findClearSkyPairs(L7_col5.filterDate(startDate, endDate), S2_col5.filterDate(startDate, endDate), 1);
print('5Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S25);

// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S25 = findClearSkyPairs(L8_col5.filterDate(startDate, endDate), S2_col5.filterDate(startDate, endDate), 1);
print('5Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S25);

var clearSkyPairsL7S26 = findClearSkyPairs(L7_col6.filterDate(startDate, endDate), S2_col6.filterDate(startDate, endDate), 1);
print('6Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S26);

// Find matching dates with clear skies for Landsat 8 - Sentinel-2
var clearSkyPairsL8S26 = findClearSkyPairs(L8_col6.filterDate(startDate, endDate), S2_col6.filterDate(startDate, endDate), 1);
print('6Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S26);

var clearSkyPairsL7S234 = findClearSkyPairs(L7_col3.filterDate(startDate, endDate), S2_col4.filterDate(startDate, endDate), 1);
print('34Landsat 7 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL7S234);

var clearSkyPairsL8S234 = findClearSkyPairs(L8_col3.filterDate(startDate, endDate), S2_col4.filterDate(startDate, endDate), 1);
print('34Landsat 8 - Sentinel-2 Clear Sky Date Pairs:', clearSkyPairsL8S234);

/* -------------- Harmonization of Landsat and Sentinel-2 -------------------
*/
// -- Import the MODULES (functions)
var funcHLS = require('users/geeberra/Modules:HLS_Module_v2');

// -- Selected point of interest and clear sky (near)same dates Landsat-Sentinel observations
// OPTION 1 //// ######### Agriculture in Carazinho - Brazil
// var PoI = ee.Geometry.Point(-91.75189293121653, 34.58672839552607); // Point of Interest. Ground NDVI sensors
// var PointName = 'USHRA';
// - L8-S2 pair 
// PoI1 - L7-S2 and L8-S2 pair dates
var Date_L7_PoI1 = ee.Date(ee.Dictionary((clearSkyPairsL7S21
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI1 = ee.Date(ee.Dictionary((clearSkyPairsL7S21
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI1 = ee.Date(ee.Dictionary((clearSkyPairsL8S21
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI1 = ee.Date(ee.Dictionary((clearSkyPairsL8S21
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));

var Date_L7_PoI2 = ee.Date(ee.Dictionary((clearSkyPairsL7S22
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI2 = ee.Date(ee.Dictionary((clearSkyPairsL7S22
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI2 = ee.Date(ee.Dictionary((clearSkyPairsL8S22
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI2 = ee.Date(ee.Dictionary((clearSkyPairsL8S22
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));



var Date_L7_PoI3 = ee.Date(ee.Dictionary((clearSkyPairsL7S23
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI3 = ee.Date(ee.Dictionary((clearSkyPairsL7S23
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI3 = ee.Date(ee.Dictionary((clearSkyPairsL8S23
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI3 = ee.Date(ee.Dictionary((clearSkyPairsL8S23
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));


var Date_L7_PoI4 = ee.Date(ee.Dictionary((clearSkyPairsL7S24
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI4 = ee.Date(ee.Dictionary((clearSkyPairsL7S24
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI4 = ee.Date(ee.Dictionary((clearSkyPairsL8S24
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI4 = ee.Date(ee.Dictionary((clearSkyPairsL8S24
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));


var Date_L7_PoI5 = ee.Date(ee.Dictionary((clearSkyPairsL7S25
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI5 = ee.Date(ee.Dictionary((clearSkyPairsL7S25
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI5 = ee.Date(ee.Dictionary((clearSkyPairsL8S25
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI5 = ee.Date(ee.Dictionary((clearSkyPairsL8S25
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));

// PoI6 - L7-S2 and L8-S2 pair dates
var Date_L7_PoI6 = ee.Date(ee.Dictionary((clearSkyPairsL7S26
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI6 = ee.Date(ee.Dictionary((clearSkyPairsL7S26
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI6 = ee.Date(ee.Dictionary((clearSkyPairsL8S26
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI6 = ee.Date(ee.Dictionary((clearSkyPairsL8S26
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));

// PoI34 - L7-S2 and L8-S2 pair dates
var Date_L7_PoI34 = ee.Date(ee.Dictionary((clearSkyPairsL7S234
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L7_PoI34 = ee.Date(ee.Dictionary((clearSkyPairsL7S234
  .filter(ee.Filter.neq('item', null))).get(0)).get('sentinelDate'));
var Date_L8_PoI34 = ee.Date(ee.Dictionary((clearSkyPairsL8S234
  .filter(ee.Filter.neq('item', null))).get(0)).get('landsatDate'));
var Date_S2_L8_PoI34 = ee.Date(ee.Dictionary((clearSkyPairsL8S234
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
var start_date = '2019-01-01';
var end_date   = '2019-12-31';
print("table", table);
var tablegeometry = table.geometry();
print("tablegeometry", tablegeometry);
// Assuming 'fields' is your shapefile or feature collection with polygon geometries
var fields = ee.FeatureCollection(table);

var criteria = ee.Filter.and(
     ee.Filter.date(start_date, end_date));
var cloud_perc = 40;//Max cloud percentile per scene. 

// -- Collections of Landsat 7, 8 and Sentinel 2
var cloud_perc = 20; // Example value for cloud cover percentage

// For PoI1
var L8_col1_PoI1 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
    .filterBounds(PoI1)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var L7_col1_PoI1 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
    .filterBounds(PoI1)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var S2_col1_PoI1 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
    .filterBounds(PoI1)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', cloud_perc));

// For PoI2
var L8_col1_PoI2 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
    .filterBounds(PoI2)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var L7_col1_PoI2 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
    .filterBounds(PoI2)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var S2_col1_PoI2 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
    .filterBounds(PoI2)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', cloud_perc));

// Repeat the same structure for PoI3, PoI4, PoI5, and PoI6
var L8_col1_PoI3 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
    .filterBounds(PoI3)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var L7_col1_PoI3 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
    .filterBounds(PoI3)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var S2_col1_PoI3 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
    .filterBounds(PoI3)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', cloud_perc));

// For PoI4
var L8_col1_PoI4 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
    .filterBounds(PoI4)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var L7_col1_PoI4 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
    .filterBounds(PoI4)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var S2_col1_PoI4 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
    .filterBounds(PoI4)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', cloud_perc));

// For PoI5
var L8_col1_PoI5 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
    .filterBounds(PoI5)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var L7_col1_PoI5 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
    .filterBounds(PoI5)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var S2_col1_PoI5 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
    .filterBounds(PoI5)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', cloud_perc));

// For PoI6
var L8_col1_PoI6 = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
    .filterBounds(PoI6)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var L7_col1_PoI6 = ee.ImageCollection("LANDSAT/LE07/C02/T1_TOA")
    .filterBounds(PoI6)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUD_COVER', cloud_perc));

var S2_col1_PoI6 = ee.ImageCollection("COPERNICUS/S2_HARMONIZED")
    .filterBounds(PoI6)
    .filterDate(start_date, end_date)
    .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', cloud_perc));

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


// PoI34
var Cloud_free_L7_col1_PoI34_Bd = funcHLS.selDate(L7_col1_PoI3_masked.select(bandIn_L7, bandOut), Date_L7_PoI34).select(Bd);
var Cloud_free_S2_col1_L7_col1_PoI34_Bd = funcHLS.selDate(S2_col1_PoI4_masked.select(bandIn_S2, bandOut), Date_S2_L7_PoI34).select(Bd);
var Cloud_free_L8_col1_PoI34_Bd = funcHLS.selDate(L8_col1_PoI3_masked.select(bandIn_L8, bandOut), Date_L8_PoI34).select(Bd);
var Cloud_free_S2_col1_L8_col1_PoI34_Bd = funcHLS.selDate(S2_col1_PoI4_masked.select(bandIn_S2, bandOut), Date_S2_L8_PoI34).select(Bd);


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

//PoI34
var L7_regist_col_PoI34 = imgL7_col1_PoI3_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L7_col1_PoI34_Bd, Cloud_free_S2_col1_L7_col1_PoI34_Bd, iterpolation_method));
var L8_regist_col_PoI34 = imgL8_col1_PoI3_SR_BRDF.map(funcHLS.Co_reg(Cloud_free_L8_col1_PoI34_Bd, Cloud_free_S2_col1_L8_col1_PoI34_Bd, iterpolation_method));


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
var imgS2_302_PoI1 = S2_regist_col_PoI1.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI1));
var imgL7_30_PoI1 = L7_regist_col_PoI1;
var imgL8_30_PoI1 = L8_regist_col_PoI1;

// PoI2
var selected_L_crs_S2_PoI2 = L8_regist_col_PoI2.first().select('red').projection();
var imgS2_30_PoI2 = S2_regist_col_PoI2.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI2));
var imgS2_302_PoI2 = S2_regist_col_PoI2.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI2));
var imgL7_30_PoI2 = L7_regist_col_PoI2;
var imgL8_30_PoI2 = L8_regist_col_PoI2;

// PoI3
var selected_L_crs_S2_PoI3 = L8_regist_col_PoI3.first().select('red').projection();
var imgS2_30_PoI3 = S2_regist_col_PoI3.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI3));
var imgS2_302_PoI3 = S2_regist_col_PoI3.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI3));
var imgL7_30_PoI3 = L7_regist_col_PoI3;
var imgL8_30_PoI3 = L8_regist_col_PoI3;

// PoI4
var selected_L_crs_S2_PoI4 = L8_regist_col_PoI4.first().select('red').projection();
var imgS2_30_PoI4 = S2_regist_col_PoI4.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI4));
var imgS2_302_PoI4 = S2_regist_col_PoI4.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI4));
var imgL7_30_PoI4 = L7_regist_col_PoI4;
var imgL8_30_PoI4 = L8_regist_col_PoI4;

// PoI5
var selected_L_crs_S2_PoI5 = L8_regist_col_PoI5.first().select('red').projection();
var imgS2_30_PoI5 = S2_regist_col_PoI5.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI5));
var imgS2_302_PoI5 = S2_regist_col_PoI5.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI5));
var imgL7_30_PoI5 = L7_regist_col_PoI5;
var imgL8_30_PoI5 = L8_regist_col_PoI5;

// PoI6
var selected_L_crs_S2_PoI6 = L8_regist_col_PoI6.first().select('red').projection();
var imgS2_30_PoI6 = S2_regist_col_PoI6.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI6));
var imgS2_302_PoI6 = S2_regist_col_PoI6.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI6));
var imgL7_30_PoI6 = L7_regist_col_PoI6;
var imgL8_30_PoI6 = L8_regist_col_PoI6;


// PoI34
var selected_L_crs_S2_PoI34 = L8_regist_col_PoI34.first().select('red').projection();
var imgS2_30_PoI4 = S2_regist_col_PoI4.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI34));
var imgS2_302_PoI4 = S2_regist_col_PoI4.map(funcHLS.reproj_L78(selected_L_crs_S2_PoI34));
var imgL7_30_PoI34 = L7_regist_col_PoI34;
var imgL8_30_PoI34= L8_regist_col_PoI34;


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
var S2_NDVI2_PoI1 = imgS2_302_PoI1.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});

// PoI2
var L7_NDVI_PoI2 = imgL7_30_PoI2.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI2 = imgL8_30_PoI2.filterDate(start_date, end_date).map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI2 = imgS2_30_PoI2.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});
var S2_NDVI2_PoI2 = imgS2_302_PoI2.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});

// PoI3
var L7_NDVI_PoI3 = imgL7_30_PoI3.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI3 = imgL8_30_PoI3.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI3 = imgS2_30_PoI3.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});
var S2_NDVI2_PoI3 = imgS2_302_PoI3.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});

// PoI4
var L7_NDVI_PoI4 = imgL7_30_PoI4.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI4 = imgL8_30_PoI4.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI4 = imgS2_30_PoI4.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});
var S2_NDVI2_PoI4 = imgS2_302_PoI4.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});

// PoI5
var L7_NDVI_PoI5 = imgL7_30_PoI5.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI5 = imgL8_30_PoI5.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI5 = imgS2_30_PoI5.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});
var S2_NDVI2_PoI5 = imgS2_302_PoI5.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});

// PoI6
var L7_NDVI_PoI6 = imgL7_30_PoI6.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI6 = imgL8_30_PoI6.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI6 = imgS2_30_PoI6.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});
var S2_NDVI2_PoI6 = imgS2_302_PoI6.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});
                
                
var L7_NDVI_PoI34 = imgL7_30_PoI34.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_7'})});
var L8_NDVI_PoI34 = imgL8_30_PoI34.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'LANDSAT_8'})});
var S2_NDVI_PoI4 = imgS2_30_PoI4.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});
var S2_NDVI2_PoI4 = imgS2_302_PoI4.filterDate(start_date, end_date)//.map(funcHLS.calc_NDVI)
                .map(function(img){return img.set({'SATELLITE': 'SENTINEL_2'})});                


// //Merge the series
// //<https://gis.stackexchange.com/questions/354961/plotting-time-series-of-different-products-in-google-earth-engine>
// Merge all points' NDVI collections into Merged_Series
var Merged_Series = L7_NDVI_PoI1
                    .merge(L7_NDVI_PoI2)
                    .merge(L7_NDVI_PoI3)
                    .merge(L7_NDVI_PoI4)
                    .merge(L7_NDVI_PoI5)
                    .merge(L7_NDVI_PoI6)
                    .merge(L7_NDVI_PoI34)
                    .merge(L8_NDVI_PoI1)
                    .merge(L8_NDVI_PoI2)
                    .merge(L8_NDVI_PoI3)
                    .merge(L8_NDVI_PoI4)
                    .merge(L8_NDVI_PoI5)
                    .merge(L8_NDVI_PoI6)
                    .merge(L8_NDVI_PoI34)
                    .merge(S2_NDVI2_PoI4)
                    .merge(S2_NDVI2_PoI5)
                    .merge(S2_NDVI2_PoI1)
                    .merge(S2_NDVI2_PoI6)
                    .merge(S2_NDVI2_PoI3)
                    .merge(S2_NDVI2_PoI2);

// Convert to an ImageCollection
Merged_Series = ee.ImageCollection(Merged_Series);
print('Merged_Series', Merged_Series);


// Merging the image collections for each POI
// Convert each merged series to an ImageCollection
var Merged_SeriesPOI1 = ee.ImageCollection(L7_NDVI_PoI1.merge(L8_NDVI_PoI1).merge(S2_NDVI2_PoI1));
var Merged_SeriesPOI2 = ee.ImageCollection(L7_NDVI_PoI2.merge(L8_NDVI_PoI2).merge(S2_NDVI2_PoI2));
var Merged_SeriesPOI3 = ee.ImageCollection(L7_NDVI_PoI3.merge(L8_NDVI_PoI3).merge(S2_NDVI2_PoI3));
var Merged_SeriesPOI4 = ee.ImageCollection(L7_NDVI_PoI4.merge(L8_NDVI_PoI4).merge(S2_NDVI2_PoI4));
var Merged_SeriesPOI5 = ee.ImageCollection(L7_NDVI_PoI5.merge(L8_NDVI_PoI5).merge(S2_NDVI2_PoI5));
var Merged_SeriesPOI6 = ee.ImageCollection(L7_NDVI_PoI6.merge(L8_NDVI_PoI6).merge(S2_NDVI2_PoI6));
var Merged_SeriesPOI34 = ee.ImageCollection(L7_NDVI_PoI34.merge(L8_NDVI_PoI34).merge(S2_NDVI2_PoI4));


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
var processedPOI34 = Merged_SeriesPOI34.map(calculateIndices);

// Optionally, print or export the results for verification
print(processedPOI1, "Processed POI1");


// // Function to clip each image by the polygon
// var clipImage = function(image) {
//   return image.clipToCollection(table);
// };

// // // Apply the clipping function to each collection
// // var clippedPOI1 = processedPOI1.map(clipImage);
// // var clippedPOI2 = processedPOI2.map(clipImage);
// // var clippedPOI3 = processedPOI3.map(clipImage);
// // var clippedPOI4 = processedPOI4.map(clipImage);
// // var clippedPOI5 = processedPOI5.map(clipImage);
// // var clippedPOI6 = processedPOI6.map(clipImage);
// // var clippedPOI34 = processedPOI34.map(clipImage);


//////////////////////////////////////////////////////
// Exporting 34 collection ///////////////////////////
//////////////////////////////////////////////////////

var clippedPOI34collection = ee.ImageCollection(clippedPOI34);
// Feature collection PoI for reduction
var featureCollection = ee.FeatureCollection(table);

// Function to reduce the image by each feature in the collection
var reduceByFeature = function(image) {
  // Apply the reduction function to each feature in the collection
  var meanStats = featureCollection.map(function(feature) {
    var meanStats = image.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: feature.geometry(),
      scale: 30,  // Scale in meters, adjust as needed
      crs: 'EPSG:4326'  // Coordinate Reference System, adjust if needed
    });
    return feature.set(meanStats);
  });
  
  // Set the image ID as a property for the mean stats
  return meanStats.map(function(f) {
    return f.set('image_id', image.id());
  });
};
var allMeanStatsnonflat34 = clippedPOI34collection.map(reduceByFeature);
print("allMeanStatsnonflat34", allMeanStatsnonflat34)

// Get the feature IDs from the feature collections
var featureIDs = allMeanStatsnonflat34.aggregate_array('system:index');
// Evaluate and export each feature collection
featureIDs.evaluate(function(list) {
  list.map(function(id) {
    // Filter the feature collections by system:index
    var filteredFeatures = allMeanStatsnonflat34.filter(ee.Filter.eq('system:index', id));
    var filteredFeatures = ee.FeatureCollection(filteredFeatures.flatten());
    // Filter to retain only features with Polygon geometry and convert to FeatureCollection
    var polygonFeatureCollection = ee.FeatureCollection(filteredFeatures
      .map(function (f) { 
        return ee.Feature(f).set('geometry_type', ee.Feature(f).geometry().type()); 
      })
      .filter(ee.Filter.or(
        ee.Filter.equals('geometry_type', 'Polygon'),
        ee.Filter.equals('geometry_type', 'MultiPolygon'))));
    

    // Add Landsat 7, Landsat 8, or Sentinel-2 label to the export name using Earth Engine string methods
    var sensorLabel = '';
    if (id.indexOf('LE07') !== -1) {
      sensorLabel = 'Landsat7_';
    } else if (id.indexOf('LC08') !== -1) {
      sensorLabel = 'Landsat8_';
    } else if (id.indexOf('T') !== -1) {  // Sentinel IDs often have a "T" in their index
      sensorLabel = 'Sentinel2_';
    }

    // Export the feature collection as a shapefile to Google Drive
    Export.table.toDrive({
      collection: polygonFeatureCollection,
      description: sensorLabel + id,
      folder: 'HarmonizedLandsatSentinel',
      fileFormat: 'SHP'
    });
  });
});





// // Inspect the clipped results
// // Mosaic Landsat 7 images
// // Define the merged collection
// var mergedSeries = ee.ImageCollection('Merged_Series');

// // Filter for Landsat 7 images
// var landsat7 = mergedSeries.filterMetadata('system:id', 'contains', 'LANDSAT/LE07');

// // Get unique names (replace 'name' with the property containing the names)
// var uniqueNames = landsat7.aggregate_array('name').distinct();

// // Function to mosaic by name
// var mosaicByName = uniqueNames.map(function(name) {
//   name = ee.String(name);
//   var filtered = landsat7.filter(ee.Filter.eq('name', name));
//   var mosaic = filtered.mosaic();
//   return mosaic.set('name', name); // Set name as a property for identification
// });

// // Convert to ImageCollection
// var mosaickedCollection = ee.ImageCollection(mosaicByName);

// print("mosaickedCollection", mosaickedCollection)




// // // ######################################################################################################
// // //                                    ### Duplicate Images  ###
// // // ######################################################################################################

// // Step 1: Define the image collection 'Merged_Series'
// var mergedSeries = ee.ImageCollection(Merged_Series);
// // Step 2: Extract the image names (IDs)
// var imageNames = mergedSeries.aggregate_array('system:id');

// // Step 3: Find unique image names
// var uniqueNames = imageNames.distinct();

// // Step 4: Count the duplicates by comparing the size of the original collection and the distinct collection
// var numDuplicates = imageNames.length().subtract(uniqueNames.length());

// // Step 5: Print the number of duplicate images
// print('Number of duplicate images:', numDuplicates);



// // Step 2: Extract the image names (IDs)
// var imageNames = mergedSeries.aggregate_array('system:id');

// // Step 3: Find the frequency of each image name using the 'reduce' method
// var nameCounts = imageNames.reduce(ee.Reducer.frequencyHistogram());

// // Step 4: Filter the names that appear more than once (duplicates)
// var duplicates = nameCounts.filter(ee.Filter.gt('item', 1));

// // Step 5: Get the list of duplicate image names
// var duplicateNames = ee.List(duplicates.keys());

// // Step 6: Print the duplicate image names
// print('Duplicate image names:', duplicateNames);
