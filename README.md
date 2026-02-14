# PDHDPaper
This repository contains the code, blueprint and dataset of my PD and HD paper

Here are some step by step workflow of the code and the paper

Writing the code to extract the NDVI, EVI and everything from google earth engine
What is the code like 
1. Load the dataset <br> 
    Latasat 8 Dataset collection 2 ebLANDSAT/LC08/C02/T1_L2<br>
    GEE is now taking collection 2 so the band names need to be changed <br>
    Previous collection: Landsat 8 Dataset collection 1 (LANDSAT/LC08/C01/T1_SR)<br> 
    Change in collection of bands and scaling factors: https://developers.google.com/earth-engine/landsat_c1_to_c2 <br>
    Nass Cropscape Dataset (USDA/NASS/CDL)<br> 
    Tiger State collection (TIGER/2016/States)<br> 
2. Load the packages <br> 
    Spectral (Used to calculate the different vegetation indices)<br> 
    Palettes (Used to map divergent color in google earth engine)<br> 
3. Filtering Data<br> 
    Spatial Filter <br> 
        For spatial cover we want the Arkansas region which is obtained from the Tiger dataset<br> 
        From the NASS cropscape dataset choose rice which is crop land cover =3<br> 
    Temporal Filter <br> 
    The minimum day of planting from the histogram of field collected DOP and the maximum day of harvesting from the histogram of field collected DOH<br> 
4. Masking out the bad pixels <br> 
    Cloud shadow<br> 
    Cloud cover <br> 
    Filter bound the <br> 


- Manual export
- Package Loading:
  - spectral and palettes
  - viridis palettes
- Load the geospatial data 
  - the arkansas shapefile file from FAO Gaul 2015
    - Convert the geometry
  - NASS cropland dataset filter date 2015
    - Select the rice cropland class (code 4) from the filtered dataset


## Landsat Images
List  the band names



Load function libraries
Load the image collections
Split Arkansas into 75 counties
Get the distinct batch numbers so that the whole analysis of Arkansas is done using different batch numbers and the whole work in split up. Each county is a batch
Get the images into the batch

Load the NASSCropscape data
using map function filter the rice layer and clip each images by the arkansas shapefile

1. Check if you can make the code in google earth engine more faster
2. CDL layer (raster download from website)
  1. Reproject the raster layers.
    Raster layer location: 


script: ConvertRasterandFilter.R
ArkansasRiceClippedCrospcapedata: C:\Users\rbmahbub\Documents\Data\GeospatialData\Shapefile\ArkansasRiceClipped\AllShapefile
Globcarb directory: C:\Users\rbmahbub\Documents\Data\GeospatialData\Shapefile\GloCAB-CroplandFieldBoundary\GloCAB_Field_Boundaries-ClippedbyArkansas\
GloCAB_Field_Boundaries_Ark.shp
GlobcarbexportedDirectory: 
1. Clip GlobCarb using the shapefile of each NassCropscape data using R. Same projection and same crs, same epsg, terra file
  check crs st_crs

2. Upload Globcarbexported shapefile online 

1. CDL Data: USA different projection
Project them 
Clip them with Arkansas

2. 

Used google earth engine to export the images: Too much data failed to export
Used R to clip and reproject them in same projection: Lack of memory
Now using AHPCC computing resources to clip 

Data collection can be done for 2008-2020


Harvesting date data creation
1.Manual data collection:
The directory of the folders: C:\Users\rbmahbub\Documents\Data\GeospatialData\Shapefile\GloCAB-CroplandFieldBoundary\GloCAb_FieldBoundaries-CLippedbyArkRice2008-2020-individual
Save the edited files in a different directory



# Collection of the ground truth data:
1. Whittaker farm has the information 
2. Isbell farm has the information
3. Chris Henry has 60-70 farms data


Going for trips and collecting the data
1. Digitizing the data


# Temporal duration of the images
1. 100 features and 365 images, 100*365 = 36500. For 5 years 36500*5 =
2. Different scripts for different temporal aggregation
  1. Four months aggreation
  2. Yearly mean aggregation
  3. Daily aggregation
  4. 

1. Spatial aggregation
  1. Pixel based prediction
  2. Field -scale mean prediction
    1. Use mean function in R


Post processing of data in R
1. Convert them to same CRS
2. Convert them to same projection
3. Same resolution
4. Multidimension array


R code 
1. OrganizeGroundData: 
2. OrganizingHarvestDate
3. ShapefilePDHD: Matching and Saving Shapefiles by Year 


OrganizingHarvestDate.R reads multi-year Isbell Farm Excel yield files, standardizes harvest date and field names, cleans inconsistencies, and merges them into a unified dataset with consistent FIELD_NAME, year, and FIELDNAME_YEAR identifiers for analysis.

GroundTruth_PlantingHarvest_Processing.R ingests multi-source rice field datasets (ARVA, DWMRU, Matt Morris, Sullivan, Scott Matthews, Unilever, and Isbell Flying Records), standardizes field names and date formats, harmonizes planting (PD) and harvest (HD) dates into YEAR and DOY metrics, and outputs cleaned, source-labeled datasets ready for multi-year comparative analysis (2015–2024).

Field_Shapefile_PDHD_Matching.R Standardizes naming and matches multi-farm field geometries (2015–2024) with ground-truth planting and harvest dates.
Resolves spatial inconsistencies and geometry errors to produce harmonized datasets for Earth Engine and rice productivity modeling.

InterSGPCA_Features.R
A parallel-processed R pipeline for cleaning, daily interpolation, and Savitzky-Golay smoothing of paired meteorological and satellite-derived vegetation indices.

DeinesminMaxExtraction2.R
The model fits the kNDVI signal using the harmonic function $y = c + \sum (a_n \cos + b_n \sin)$.

Phenofit.R
Utilizes the phenofit package to apply multiple curve-fitting methods (AG, Beck, Elmore, Gu, Zhang) to kNDVI data, extracting critical transition dates like SOS (Start of Season) and EOS (End of Season).

phenofitSequential.R
If the parallel multisession approach encounters environment or memory issues, the script is designed to fall back to the sequential method (plan(sequential))


RFE_OptimumNumberOfPlots.R
Executes Recursive Feature Elimination (RFE) to isolate the most influential phenological and environmental predictors for rice management events. RFE with 5-fold repeated cross-validation to minimize RMSE.

rf_phenology_method_KFOLDVALIDATION.R
The modeling framework utilizes a Monte Carlo design that executes 100 randomized iterations using a 60/20/20 split (Train/Validation/Fixed Test) to ensure that all performance metrics are statistically stable and free from sampling bias. This stability-driven approach specifically tracks the convergence of cumulative RMSE and MAE across every run, confirming that the consensus model reaches a reliable steady-state for Arkansas rice fields.  Finally, the script identifies aggregate importance by ranking predictors based on their average %IncMSE over the entire 100-run ensemble, effectively isolating the most robust environmental and phenological drivers of rice management.

rf_deines_method_KFOLDVALIDATION.R
This script automates the Deines Method implementation by calculating the Green Chlorophyll Vegetation Index (GCVI) and fitting robust harmonic regressions to multiple spectral bands. By integrating multi-band Fourier coefficients (a1​,b1​,c) with season-specific meteorological aggregates (e.g., April–June precipitation and GDD), it generates the high-dimensional feature matrix required for Random Forest yield modeling. The workflow ensures spatial-temporal consistency by merging satellite phenology with climate drivers into a single, wide-format dataset ready for Arkansas-wide rice productivity analysis.

rf_deines_method_KFOLDVALIDATION.R
This Monte Carlo validation framework executes 100 randomized iterations with a 60/20/20 data split to ensure performance metrics are statistically stable and free from sampling bias. By tracking the convergence of cumulative RMSE and MAE, the pipeline confirms that the consensus model reaches a reliable steady-state for Arkansas rice fields.  Finally, it isolates the most robust environmental and phenological drivers of rice management by ranking predictors based on their average %IncMSE across the entire 100-run ensemble.

Presentation_GeneralPlottingDataDOPDOH.R
AnimationsForPresentation.R
Creates graphs for presentation 

GeneralPlottingDataDOPDOH.R
Creates graphs for manuscript file

writingFromCodePDHD.R
exports lines for the manuscript paper

ModelExplanation.R
Extra codes to understand the model

LagRandomForest.R

Doub_Logis_First.R

optimalcurvepddoywithcummeteo.R

optimalcurvekndvigdd.R

optimalcurveHDDOY.R

Doub_Logis_First_KFOLD.R

Doub_Logis_First_KFOLD_Phenotfit.R
