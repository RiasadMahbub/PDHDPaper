# PDHDPaper

Code, workflow, and datasets for estimating **rice Planting Date (PD) and Harvest Date (HD)**
using satellite imagery, field boundaries, and ground-truth agricultural data in Arkansas.

The project integrates Google Earth Engine (GEE), R-based geospatial processing, phenological
curve fitting, and machine learning models to analyze rice phenology from **2015–2024**.

---

## Project Overview

- Satellite remote sensing (Landsat 8, Sentinel)
- Crop classification (USDA CDL)
- Field boundary datasets (GloCAB)
- Ground-truth farm records (Whittaker, Isbell, Chris Henry — 60–70 farms)
- Phenological signal extraction (Phenofit double-logistic)
- Machine learning modeling (Random Forest with Leave-2-Years-Out validation)

---

## Data Sources

| Source | GEE Dataset ID / Notes |
|---|---|
| Landsat 8 SR C2 | `LANDSAT/LC08/C02/T1_L2` |
| USDA CDL | `USDA/NASS/CDL` — Rice = class 4 |
| Arkansas boundary | `TIGER/2016/States` |
| Field boundaries | GloCAB Cropland Field Boundaries |
| Ground truth | Whittaker, Isbell, Chris Henry farm records |

---

## Full Pipeline — Run Order

### Step 1 — Harvest Date Preparation
**`OrganizingHarvestDate.R`**
- Reads multi-year Isbell farm harvest records
- Standardizes field names, cleans date formats
- Generates `FIELD_NAME`, `YEAR`, `FIELDNAME_YEAR` identifiers

---

### Step 2 — Ground Truth Data Processing
**`OrganizeGroundData.R`**
- Combines ARVA, DWMRU, Matt Morris, Sullivan, Scott Matthews, Unilever, Isbell sources
- Outputs standardized PD/HD DOY metrics and unified multi-year dataset (2015–2024)

---

### Step 3 — Field Boundary Matching
**`ShapefilePDHD.R`** *(not uploaded — referenced in workflow)*
- Standardizes field naming
- Matches farm polygons with ground truth PD/HD records
- Produces harmonized spatial datasets for Earth Engine

---

### Step 4 — Time Series Processing
**`InterSGPCA_Features.R`**
- Parallel pipeline: data cleaning, daily interpolation, Savitzky-Golay smoothing
- Merges meteorological and vegetation index datasets per field
- Outputs `merged_list` — one dataframe per Field_Year

---

### Step 5 — Harmonic Feature Extraction (Deines Features)
**`DeinesminMaxExtraction2.R`**
- Fits harmonic regression to GCVI and NIR time series
- Extracts Fourier coefficients: `a1, a2, b1, b2`, NIR params
- Outputs `deinesharmonicminmaxdf`

---

### Step 6 — Phenological Curve Fitting
**`Phenofit.R`** *(use this — the older version does not work)*

Key decisions and notes:
- Uses `phenofit` double-logistic curve fitting in parallel (`furrr`)
- If stuck at 99%, stop and run manually:
  ```r
  phenology_df <- bind_rows(phenology_list)
  ```
- **Window mode switch** — choose one before running meteo summary:
  ```r
  window_mode <- "sos_deriv"   # field-specific anchor at SOS_deriv.sos
  window_mode <- "fixed"       # fixed DOY 163–193 for all fields
  ```
  The `DOY_max_fit` column records which anchor was used per field.

- Outputs `phenology_df` — one row per Field_Year with SOS, EOS, curve parameters

> **Important:** `Greenup.Greenup` and `Dormancy.Dormancy` should **not** be in
> `df_planting` or `dfharvest` feature sets — check column names before modeling.

If parallel processing fails use the fallback:
**`phenofitSequential.R`** (PhenofitFIxmultisession)

---

### Step 7 — Meteo Summary Tables
*(run inside `Phenofit.R` or standalone)*

Two separate summary tables are built — they use **different time windows** and must
not be confused:

| Object | Source script section | Window | Used for |
|---|---|---|---|
| `meteo_summary_df` → `df` | `meteo_summary_list` | `SOS_deriv - 30` to `SOS_deriv` (or fixed 193) | PDDOY modeling |
| `meteo_summary_df_harvest` → `dfharvest` | `meteo_summary_listharvest` | `DD.DD` to `DD.DD + 60` | HDDOY modeling |

Both tables now include `yearcumgdd`, `yearcumvpd`, `yearcumtmin`, `yearcumrh`,
`yearcumrad` (full-year sums). These are required for year-anomaly features in
the RF models.

---

### Step 8 — Deines Data Preparation
**`DeinesDataPreparation.R`**
- Assembles `deines_results_df` with GCVI harmonic features, Apr–Jun climate,
  NIR params, soil temperature
- Used as input to the Deines RF comparison model

---

### Step 9 — Window Optimisation (Optional / Diagnostic)
**`optimalcurvepddoywithcummeteo.R`**
- Parallel search over window size (30–60 days) and end DOY (46–250)
- Finds window that maximises correlation between cumulative features and PDDOY

**`optimalcurvekndvigdd.R`**
- Finds window that minimises correlation between cumulative kNDVI and cumulative GDD
- Used to justify the chosen pre-planting window

---

### Step 10 — Interannual Variability Diagnostic
**`INTERANNUAL_VARIABILITY_CHECK.R`**
- Checks year-level signal in planting features
- Reports z-score anomalies per year for VPD, GDD, Tmin, RH, radiation
- Run before modeling to confirm `yearcum*` columns are populated

---

### Step 11 — Hyperparameter Tuning (LIMP RF)
**`HyperparameterTuning_rf_phenology_method_KFOLDVALIDATIONold.R`**

*(Note: "old" refers to the random-split version — kept for comparison)*

The current tuning pipeline uses **Leave-2-Years-Out (L2YO)** cross-validation.
Grid search over `ntree × mtry × n_features`:

- PDDOY winner: `ntree=350, mtry=8, feat=10`
- HDDOY winner: `ntree=500, mtry=3, feat=15`

**Critical data source rule:**
```
df_pd  ← built from df          (planting window cumulatives)
df_hd  ← built from dfharvest   (harvest window cumulatives)
```
Do NOT use `df_sos` for both targets — the windows are different.

---

### Step 12 — Full RF Pipeline
**`RFPDHDFULLPIPELINE.R`** *(referenced — not uploaded)*

Self-contained pipeline:
1. Builds `df_master_pd` from `df`, `df_master_hd` from `dfharvest`
2. Joins `year_signal` (full-year climate anomalies) to both
3. Builds interaction features (`gdd_x_sos`, `tmin_gdd`, `eos_x_sos`, etc.)
4. Runs L2YO ntree search → feature ranking → grid search → final models
5. Outputs `pd_final`, `hd_final` with RMSE, MAE, R², MBE per pair

---

### Step 13 — RF with Importance Graphs
**`RFLOYOfulloutlineimportance.R`** *(referenced — not uploaded)*

Same as RFPDHDFULLPIPELINE but adds:
- Variable importance plots (`%IncMSE`)
- Per-year RMSE and MBE heatmaps
- Climate anomaly vs RMSE scatter plots

---

### Step 14 — Double Logistic Models
**`Doub_Logis_First_KFOLD_Phenotfit.R`**
- Phenofit-based double logistic model with k-fold validation

**`Doub_Logis_LOYO2.R`** (PDSOSDER model)
- Leave-2-Years-Out version of the SOS-derivative GDD lag model
- Computes `mean_gdd_lag` from training data only per pair
- L2YO GDD lag: 781.23 ± 13.06 °C (vs 763.35 ± 5.55 from random split)

---

### Step 15 — Deines L2YO
**`Deineskfold_LOYO.R`**
- Runs Deines RF features under honest L2YO evaluation
- Demonstrates temporal leakage in random split:
  - Random split PDDOY Test RMSE: 8.11 → L2YO: 17.52
  - Random split HDDOY Test RMSE: 5.67 → L2YO: 14.75

---

### Step 16 — Master Metrics Table
**`allmetricsOne.R`**
- Consolidates all model results into two paper-ready tables:
  - `PDDOY_final_table_with_MBE.csv` (6 rows: 3 models × 2 splits)
  - `HDDOY_final_table_with_MBE.csv` (4 rows: 2 models × 2 splits)
- Metrics reported: RMSE, MAE, R², MBE for Train / Validation / Test
- Fixes any missing Bias/MBE values from summary objects before building tables
- Generates comparison bar plots with ±SD error bars

---

### Step 17 — General Plotting
**`GeneralPlottingDataDOPDOH.R`**
- Manuscript figures: observed vs predicted, residual plots, year difficulty
- Box plots: not needed (commented out)

---

### Step 18 — Model Explanation
**`ModelExplanation.R`**
- SHAP-style feature importance
- Partial dependence plots for top predictors
- Growing season length and phenology diagnostics

---

### Step 19 — Animations
**`AnimationsForPresentation.R`**
- Animated figures for conference presentations
- Time-lapse of phenology across years

---

### Step 20 — Presentation Figures
**`Presentation_GeneralPlottingDataDOPDOH.R`** *(referenced — not uploaded)*
- Simplified versions of manuscript figures for slides

---

## Model Architecture Summary

### LIMP RF (this study)
| | PDDOY | HDDOY |
|---|---|---|
| Evaluation | L2YO honest | L2YO honest |
| ntree | 350 | 500 |
| mtry | 8 | 3 |
| Features | 10 | 15 |
| Train RMSE | 6.35 | 4.25 |
| Test RMSE | 12.12 ± 1.55 | 9.07 ± 2.90 |
| Test R² | 0.612 | 0.540 |
| Test MBE | −0.21 | 0.35 |

### Comparison Models
| Model | Evaluation | PDDOY Test RMSE | HDDOY Test RMSE |
|---|---|---|---|
| Deines RF | Random Split ❌ | 8.11 | 5.67 |
| Deines RF | L2YO ✅ | 17.52 | 14.75 |
| PDSOSDER | Random Split ❌ | 10.45 | — |
| PDSOSDER | L2YO ✅ | 12.29 | — |
| LIMP RF | Random Split ❌ | 9.77 | 7.14 |
| LIMP RF | L2YO ✅ | **12.12** | **9.07** |

> Random split results are inflated due to temporal data leakage.
> L2YO is the only valid evaluation for year-structured agricultural data.

---

## Key Climate Findings (L2YO Error Analysis)

**PDDOY hardest years:** 2021, 2022, 2019, 2018, 2020
- Cold years (low `tmin`) are hardest to predict: r = −0.657 (p = 0.039)

**HDDOY hardest years:** 2019, 2021, 2020, 2022, 2018
- High VPD years are hardest: r = −0.704 (p = 0.023)
- High humidity years also difficult: r = +0.667 (p = 0.035)

---

## Output Files

All outputs saved to:
```
C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure/
```

Key CSVs:
- `PDDOY_final_table_with_MBE.csv`
- `HDDOY_final_table_with_MBE.csv`
- `PDDOY_year_difficulty_RMSE_MBE_climate.csv`
- `HDDOY_year_difficulty_RMSE_MBE_climate.csv`
- `LIMP_RandomVsL2YO_allmetrics.csv`

---

## Computing Infrastructure

Large-scale processing performed at:
**AHPCC (Arkansas High Performance Computing Center)**

Used for raster clipping, batch VI extraction, and large-scale phenofit fitting.

---

## Author

**Riasad Bin Mahbub**
PhD Candidate — Environmental Dynamics
University of Arkansas