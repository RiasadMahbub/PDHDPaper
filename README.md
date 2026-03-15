# PDHDPaper

This repository contains the **code, workflow, and datasets** used in the study for estimating **rice Planting Date (PD) and Harvest Date (HD)** using satellite imagery, field boundaries, and ground-truth agricultural data in Arkansas.

The project integrates **Google Earth Engine (GEE), R-based geospatial processing, and machine learning models** to analyze rice phenology from **2008–2020**.

---

# Project Overview

The workflow integrates multiple datasets and methods to estimate rice phenology and management timing:

- Satellite remote sensing
- Crop classification datasets
- Field boundary datasets
- Ground-truth farm records
- Phenological signal extraction
- Machine learning modeling

Final outputs include **rice phenology estimates and predictive models for planting and harvesting timing across Arkansas**.

---

# Data Sources

## Satellite Data
- **Landsat 8 Surface Reflectance – Collection 2**
- GEE dataset ID:

```
LANDSAT/LC08/C02/T1_L2
```

Note: Google Earth Engine now uses **Collection 2**, which requires updated band names and scaling factors.

Reference:
https://developers.google.com/earth-engine/landsat_c1_to_c2

---

## Crop Classification
- **USDA Cropland Data Layer (CDL)**

```
USDA/NASS/CDL
```

Rice crop class used:

```
Rice = 4
```

---

## Administrative Boundaries
- **US Census TIGER dataset**

```
TIGER/2016/States
```

Used for extracting **Arkansas boundary**.

---

## Field Boundaries
- **GloCAB Cropland Field Boundaries**

These field geometries were clipped to Arkansas and matched with ground truth farm data.

---

## Ground Truth Data Sources

Farm-level planting and harvesting information collected from:

- Whittaker Farm
- Isbell Farm
- Chris Henry farm dataset (60–70 farms)

Ground truth datasets include:

- Planting date (PD)
- Harvest date (HD)
- Field identifiers
- Farm locations

---

# Workflow Overview

The full pipeline contains **five major stages**:

1. Google Earth Engine data extraction  
2. Spatial processing in R  
3. Ground-truth dataset processing  
4. Phenological signal extraction  
5. Machine learning modeling  

---

# 1. Google Earth Engine Processing

## Load datasets

- Landsat 8 Collection 2
- USDA Cropland Data Layer
- Arkansas administrative boundary

---

## Load packages

Used within the GEE environment:

- **Spectral**  
  Used for calculating vegetation indices.

- **Palettes**  
  Used for visualization of color scales.

---

## Spatial Filtering

Filtering applied to isolate Arkansas rice fields.

Steps:

1. Extract Arkansas boundary from TIGER dataset
2. Load NASS Cropland Data Layer
3. Select rice cropland pixels

```
Rice = class 4
```

---

## Temporal Filtering

Temporal window defined using **ground truth planting and harvesting histograms**:

- Minimum planting DOY
- Maximum harvesting DOY

This reduces noise and focuses analysis on rice growing season.

---

## Cloud Masking

Poor quality pixels removed using:

- Cloud mask
- Cloud shadow mask
- Quality assessment bands

---

## Vegetation Indices

Vegetation indices extracted from Landsat bands:

- NDVI
- EVI
- kNDVI
- GCVI

These indices form the **time-series phenological signal** used in modeling.

---

# 2. Spatial Processing in R

Due to data volume, additional spatial processing was performed in **R and HPC environments**.

Tasks include:

- Raster reprojection
- Clipping datasets
- Harmonizing coordinate systems
- Spatial matching

---

## Key Script

### `ConvertRasterandFilter.R`

Functions:

- Reproject CDL raster layers
- Clip rasters to Arkansas boundary
- Align projections with field shapefiles

Example directories used:

```
ArkansasRiceClippedCrospcapedata
C:\Users\rbmahbub\Documents\Data\GeospatialData\Shapefile\ArkansasRiceClipped
```

```
GlobCarb Directory
C:\Users\rbmahbub\Documents\Data\GeospatialData\Shapefile\GloCAB-CroplandFieldBoundary
```

CRS checks performed using:

```R
st_crs()
```

---

# 3. Ground Truth Data Processing

Ground-truth datasets were collected from multiple farms and standardized.

Manual data editing performed on field boundary datasets located at:

```
GloCAB_FieldBoundaries-CLippedbyArkRice2008-2020-individual
```

---

## Harvest Data Processing

### `OrganizingHarvestDate.R`

This script:

- Reads multi-year Isbell farm harvest records
- Standardizes field names
- Cleans date formats
- Generates unified identifiers

Key fields created:

- `FIELD_NAME`
- `YEAR`
- `FIELDNAME_YEAR`

---

## Multi-Source Dataset Integration

### `GroundTruth_PlantingHarvest_Processing.R`

Combines multiple farm datasets:

Sources include:

- ARVA
- DWMRU
- Matt Morris
- Sullivan
- Scott Matthews
- Unilever
- Isbell Flying Records

Outputs include:

- standardized planting dates
- standardized harvest dates
- DOY metrics
- unified multi-year dataset (2015–2024)

---

# 4. Field Boundary Matching

### `Field_Shapefile_PDHD_Matching.R`

This script:

- Standardizes field naming
- Matches farm polygons with ground truth PD/HD records
- Fixes geometry issues
- Produces harmonized spatial datasets ready for Earth Engine processing.

---

# 5. Time Series Processing

### `InterSGPCA_Features.R`

Parallel processing pipeline for:

- data cleaning
- daily interpolation
- Savitzky-Golay smoothing
- merging meteorological and vegetation index datasets

---

# Phenology Extraction Methods

## Harmonic Regression

Script:

```
DeinesminMaxExtraction2.R
```

The vegetation signal is modeled using harmonic functions:

```
y = c + Σ (a_n cos + b_n sin)
```

This captures seasonal vegetation patterns.

---

## Phenofit Curve Fitting

Script:

```
Phenofit.R
```

Curve fitting methods applied:

- AG
- Beck
- Elmore
- Gu
- Zhang

Phenological metrics extracted:

- SOS (Start of Season)
- EOS (End of Season)

Fallback script if parallel processing fails:

```
phenofitSequential.R
```

---

# Feature Selection

### `RFE_OptimumNumberOfPlots.R`

Recursive Feature Elimination used to identify the most important predictors.

Method:

- 5-fold repeated cross-validation
- RMSE minimization

---

# Machine Learning Models

## Random Forest Modeling

A Monte Carlo validation framework was used.

Parameters:

- 100 randomized iterations
- 80(75/25)/20 train-validation-test split

Performance evaluated using:

- RMSE
- MAE convergence

---

## Phenology Prediction Model

Script:

```
rf_phenology_method_KFOLDVALIDATION.R
```

Predicts planting and harvest timing using:

- vegetation indices
- phenological indicators
- environmental variables

---

## Deines Method Modeling

Script:

```
rf_deines_method_KFOLDVALIDATION.R
```

Features include:

- GCVI
- harmonic Fourier coefficients
- precipitation aggregates
- growing degree days

This generates a **high-dimensional feature matrix** for rice productivity modeling.

---

# Visualization and Figures

Scripts used for manuscript and presentation figures.

### Presentation Figures

```
Presentation_GeneralPlottingDataDOPDOH.R
```

---

### Animations

```
AnimationsForPresentation.R
```

---

### Manuscript Figures

```
GeneralPlottingDataDOPDOH.R
```

---

# Manuscript Integration

### `writingFromCodePDHD.R`

Exports:

- model outputs
- statistics
- formatted results

for direct integration into the manuscript text.

---

# Additional Model Experiments

Exploratory modeling scripts include:

```
LagRandomForest.R
Doub_Logis_First.R
optimalcurvepddoywithcummeteo.R
optimalcurvekndvigdd.R
optimalcurveHDDOY.R
Doub_Logis_First_KFOLD.R
Doub_Logis_First_KFOLD_Phenotfit.R
```

---

# Computing Infrastructure

Due to large datasets:

- GEE exports occasionally failed
- Local R processing encountered memory limitations

Final large-scale processing performed using:

**AHPCC (Arkansas High Performance Computing Center)**

Used for:

- raster clipping
- batch processing
- large-scale modeling

---

# Temporal Coverage

Data used in this study covers:

```
2008–2020
```
---

# Author

**Riasad Bin Mahbub**  
PhD Candidate – Environmental Dynamics  
University of Arkansas

GitHub  
LinkedIn  
Google Scholar

---