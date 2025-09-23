#==============================================================================
# 1. SETUP: Install and Load Necessary R Packages
#==============================================================================
# If you don't have these packages installed, uncomment and run the lines below.
# install.packages("dplyr")
# install.packages("randomForest")
# install.packages("caret")
# install.packages("Metrics")
# install.packages("hydroGOF")
# install.packages("tidyr")
# install.packages("ggplot2")

# Load all required libraries
library(dplyr)
library(randomForest)
library(caret)
library(Metrics)
library(hydroGOF)
library(tidyr)
library(ggplot2)

#==============================================================================
# 2. DATA PREPARATION: Filter and Split the Data
#==============================================================================
# Assume 'df' is your loaded data frame.
# If 'df' is not in your environment, you need to load it here, e.g.:
# df <- read.csv("your_data_file.csv")

# Filter valid rows and select the initial set of variables for modeling.
# The `select()` function is used to include all potential predictors and the response.
df_planting <- df %>%
  dplyr::select(
    # --- Top RFE variables from initial analysis ---
    a1, DOY_min_fit, 
    #Greenup.Greenup, 
    SOS_trs.sos, a2, RD.RD,
    EOS_trs.eos, Value_max_obs, b1, b2, a3.a3,
    # --- Other predictor variables ---
    avgsoilclay, avgsoilorg, cum_meansrad, cum_tmin, cum_tmax,
    cum_soiltemp, cum_RH, mean_ExG, mean_MCARI1, mean_MTVI1, mean_OSAVI,mean_GCC,
    
    UD.UD, 
    #Dormancy.Dormancy, 
    DOY_max_before_min_fit, DD.DD,mx.mx, 
    EOS_deriv.eos, SOS_deriv.sos, 
    #lagtrsgreenup, 
    #Senescence.Senescence, 
    rsp.rsp, lagtrsupdate,POS.pos,cumGDVI,Laglocalmaxglomax,
    Laglocalminglomax, Laglocalmaxlocalmin,cum_gdd,cum_vpd,
    mean_nir, DOY_maxROC_nir,
    # --- Response variable ---
    PDDOY
  ) %>%
  dplyr::filter(!is.na(PDDOY)) %>% # Remove rows where the response is missing
  drop_na() # This removes any row with missing values in the selected columns


df_planting <- df %>%
  dplyr::select(
    # --- Top RFE variables from initial analysis ---
    a1, DOY_min_fit, 
    SOS_trs.sos, a2, RD.RD,
    EOS_trs.eos, Value_max_obs, b1, b2, a3.a3,
    # --- Other predictor variables ---
    avgsoilclay, avgsoilorg, cum_meansrad, cum_tmin, cum_tmax,
    cum_soiltemp, cum_RH, mean_ExG, mean_MCARI1, mean_MTVI1, mean_OSAVI, mean_GCC,
    UD.UD, , DD.DD, mx.mx, 
    EOS_deriv.eos, SOS_deriv.sos, 
    #Senescence.Senescence, 
    rsp.rsp, lagtrsupdate, POS.pos, cumGDVI,
    
    Laglocalmaxglomax,Laglocalminglomax, Laglocalmaxlocalmin, DOY_max_before_min_fit,
    cum_gdd, cum_vpd,
    mean_nir, DOY_maxROC_nir,
    # --- Explicitly include additional DOY_maxROC variables ---
    DOY_maxROC_EVI, DOY_maxROC_EVIv, DOY_maxROC_ExG, DOY_maxROC_ExGR,
    DOY_maxROC_IAVI, DOY_maxROC_MCARI1, DOY_maxROC_MCARI2, DOY_maxROC_MTVI1,
    DOY_maxROC_MTVI2, DOY_maxROC_NDDI, DOY_maxROC_NIRv, DOY_maxROC_NMDI,
    DOY_maxROC_SARVI, DOY_maxROC_TriVI, DOY_maxROC_VARI, 
    DOY_maxROC_sNIRvNDVILSWIS, DOY_maxROC_AWEInsh,
    # --- Response variable ---
    PDDOY
  ) %>%
  dplyr::filter(!is.na(PDDOY))  # Remove rows where the response is missing

df_planting <- df %>%
  dplyr::select(
    # --- Top 20 RFE variables ---
    SOS_trs.sos, SOS_deriv.sos, cum_RH, cum_tmin, avgsoilorg, a2, UD.UD, DD.DD, 
    EOS_trs.eos, RD.RD, cum_soiltemp, mean_GCC, EOS_deriv.eos, mx.mx, mean_ExG, cum_meansrad, 
    cum_vpd, cum_gdd, mean_nir, DOY_maxROC_ExG, DOY_maxROC_NMDI, Value_max_obs, DOY_maxROC_EVI, 
    DOY_min_fit, DOY_maxROC_sNIRvNDVILSWIS,
    
    Laglocalmaxglomax,Laglocalminglomax, Laglocalmaxlocalmin, DOY_max_before_min_fit,
    
    # --- Additional features to include ---
    mean_AFRI1600, mean_AFRI2100, mean_DSI, mean_DSWI5, mean_ExGR, 
    mean_GVMI, mean_MNDVI, mean_MNLI, mean_MSI, mean_NDII,
    mean_NDMI, mean_NDPI, mean_NDVI, mean_NRFIg, mean_NRFIr,
    mean_SLAVI, mean_sNIRvLSWI, mean_sNIRvNDPI, mean_sNIRvNDVILSWIP, mean_sNIRvNDVILSWIS,
    mean_sNIRvSWIR, mean_LSWI, mean_MBWI, mean_MLSWI27, mean_WI1,
    mean_WI2015, mean_BaI, mean_NDSoI, mean_NSDS, mean_NSDSI2,
    mean_NSDSI3, mean_kIPVI, mean_kNDVI,
    
    PDDOY
  ) %>%
  dplyr::filter(!is.na(PDDOY))
# Remove rows with any NA in predictor columns
df_planting <- df_planting %>%
  tidyr::drop_na()  # removes all rows that have NA in any column
# Check which columns have missing values
na_counts <- colSums(is.na(df_planting))
na_counts[na_counts > 0]  # Show only columns with at least one NA


# Set a random seed for reproducible data splitting
set.seed(42)
n_planting <- nrow(df_planting)
# Create a fixed 20% test set that will be used for final evaluation
test_idx_planting <- sample(1:n_planting, size = 0.2 * n_planting)
test_set_planting <- df_planting[test_idx_planting, ]
remaining_df_planting <- df_planting[-test_idx_planting, ]

# Separate predictors (x) and response (y) for the RFE function
x_rfe <- remaining_df_planting %>% select(-PDDOY)
y_rfe <- remaining_df_planting$PDDOY

#==============================================================================
# 3. FEATURE SELECTION: Find Optimal Features with RFE
#==============================================================================
cat("Starting Recursive Feature Elimination (RFE) to find the optimal feature subset...\n")

# Define the control parameters for RFE using repeated cross-validation.
# We will optimize for minimum RMSE.
rfe_control <- rfeControl(
  functions = rfFuncs,       # Use Random Forest functions
  method = "repeatedcv",     # Use repeated cross-validation
  number = 5,                # 5-fold cross-validation
  repeats = 3,               # Repeat 3 times for stability
  # NOTE: The 'seeds' argument has been removed to avoid a specific R-version error.
  # The set.seed() call before rfe() is sufficient for reproducibility.
  verbose = TRUE             # Print progress updates during the process
)

# Set a seed for RFE for overall reproducibility
set.seed(123)
# Run the RFE process. Note: 'metric' is now an argument of the rfe() function.
rfe_results_planting <- rfe(
  x = x_rfe,
  y = y_rfe,
  sizes = 2:ncol(x_rfe),  # Test feature subsets from 2 up to all predictors
  rfeControl = rfe_control,
  metric = "RMSE"         # Pass the metric directly to the rfe() call
)

# Extract the optimal features from the RFE results
optimal_features <- predictors(rfe_results_planting)
optimal_num_features <- rfe_results_planting$bestSubset
optimal_rmse_rfe <- min(rfe_results_planting$results$RMSE)

cat("\n--- RFE Results Summary ---\n")
cat("Optimal Number of Features:", optimal_num_features, "\n")
cat("Optimal Features Selected:\n", paste(optimal_features, collapse = ", "), "\n")
cat("RMSE with Optimal Features (from CV):", round(optimal_rmse_rfe, 4), "\n")

#==============================================================================
# 4. FINAL MODELING: Train and Evaluate with Optimal Features
#==============================================================================
# Create new data frames using only the features identified by RFE
remaining_df_optimal <- remaining_df_planting %>%
  select(PDDOY, all_of(optimal_features))

test_set_optimal <- test_set_planting %>%
  select(PDDOY, all_of(optimal_features))

# Train the final Random Forest model on the 80% remaining data
cat("\nTraining final Random Forest model with optimal features...\n")
final_model_optimal <- randomForest(PDDOY ~ ., data = remaining_df_optimal, ntree = 100)

# Make predictions on the fixed, held-out test set
test_pred_optimal <- predict(final_model_optimal, newdata = test_set_optimal)
obs_test_optimal <- test_set_optimal$PDDOY

# Compute and print the final test set metrics
test_results_optimal <- data.frame(
  phase = "Test (Optimal Features)",
  R2 = summary(stats::lm(obs_test_optimal ~ test_pred_optimal))$r.squared,
  MAE = Metrics::mae(obs_test_optimal, test_pred_optimal),
  RMSE = Metrics::rmse(obs_test_optimal, test_pred_optimal),
  NSE = hydroGOF::NSE(test_pred_optimal, obs_test_optimal),
  Bias = Metrics::bias(test_pred_optimal, obs_test_optimal)
)

cat("\n--- Final Model Test Results with RFE-Selected Features ---\n")
print(test_results_optimal)




#==============================================================================
# 1. SETUP: Install and Load Necessary R Packages
#==============================================================================
# Load all required libraries
library(dplyr)
library(randomForest)
library(caret)
library(Metrics)
library(hydroGOF)
library(tidyr)
library(ggplot2)

#==============================================================================
# 2. DATA PREPARATION: Filter and Split the Data
#==============================================================================

# Filter valid rows and select the initial set of variables for modeling.
# The `select()` function is used to include all potential predictors and the response.
df_harvest <- df %>%
  dplyr::select(
    a1, 
    UD.UD, 
    DOY_min_fit, 
    #Greenup.Greenup, 
    DOY_min_fit,
    SOS_trs.sos, 
    a2, 
    Dormancy.Dormancy, 
    RD.RD, 
    EOS_trs.eos, 
    DOY_max_before_min_fit, 
    DD.DD, 
    b1, 
    mx.mx, 
    EOS_deriv.eos, 
    SOS_deriv.sos, 
    #lagtrsgreenup, 
    #Senescence.Senescence, 
    rsp.rsp, 
    lagtrsupdate, 
    a3.a3,
    
    # --- Response variable ---
    HDDOY, 
    avgsoilclay, avgsoilorg, 
    # --- Commented out variables ---
    POS.pos,
    cumGDVI,
    #Laglocalmaxglomax, Laglocalminglomax, Laglocalmaxlocalmin,
    cum_gdd, 
    cum_meansrad, 
    cum_vpd, 
    cum_tmin, 
    DOY_maxROC_IAVI, DOY_maxROC_MSI, DOY_maxROC_TGI,DOY_maxROC_ExGR,
    #DOY_maxROC_DSI, 
    DOY_maxROC_EVI,  DOY_maxROC_IAVI,  DOY_maxROC_MSI,
    DOY_maxROC_ExGR,DOY_maxROC_MRBVI,
    DOY_maxROC_NMDI,
    cum_tmax, cum_soiltemp, cum_RH
  ) %>%
  dplyr::filter(!is.na(HDDOY)) %>% # Remove rows where the response is missing
  drop_na() # This removes any row with missing values in the selected columns

# Set a random seed for reproducible data splitting
set.seed(42)
n_harvest <- nrow(df_harvest)
# Create a fixed 20% test set that will be used for final evaluation
test_idx_harvest <- sample(1:n_harvest, size = 0.2 * n_harvest)
test_set_harvest <- df_harvest[test_idx_harvest, ]
remaining_df_harvest <- df_harvest[-test_idx_harvest, ]

# Separate predictors (x) and response (y) for the RFE function
x_rfe <- remaining_df_harvest %>% select(-HDDOY)
y_rfe <- remaining_df_harvest$HDDOY

#==============================================================================
# 3. FEATURE SELECTION: Find Optimal Features with RFE
#==============================================================================
cat("Starting Recursive Feature Elimination (RFE) to find the optimal feature subset...\n")

# Define the control parameters for RFE using repeated cross-validation.
# We will optimize for minimum RMSE.
rfe_control <- rfeControl(
  functions = rfFuncs,       # Use Random Forest functions
  method = "repeatedcv",     # Use repeated cross-validation
  number = 5,                # 5-fold cross-validation
  repeats = 3,               # Repeat 3 times for stability
  # NOTE: The 'seeds' argument has been removed to avoid a specific R-version error.
  # The set.seed() call before rfe() is sufficient for reproducibility.
  verbose = TRUE             # Print progress updates during the process
)

# Set a seed for RFE for overall reproducibility
set.seed(123)
# Run the RFE process. Note: 'metric' is now an argument of the rfe() function.
rfe_results_harvest <- rfe(
  x = x_rfe,
  y = y_rfe,
  sizes = 2:ncol(x_rfe),  # Test feature subsets from 2 up to all predictors
  rfeControl = rfe_control,
  metric = "RMSE"         # Pass the metric directly to the rfe() call
)

# Extract the optimal features from the RFE results
optimal_features <- predictors(rfe_results_harvest)
optimal_num_features <- rfe_results_harvest$bestSubset
optimal_rmse_rfe <- min(rfe_results_harvest$results$RMSE)

cat("\n--- RFE Results Summary ---\n")
cat("Optimal Number of Features:", optimal_num_features, "\n")
cat("Optimal Features Selected:\n", paste(optimal_features, collapse = ", "), "\n")
cat("RMSE with Optimal Features (from CV):", round(optimal_rmse_rfe, 4), "\n")

#==============================================================================
# 4. FINAL MODELING: Train and Evaluate with Optimal Features
#==============================================================================
# Create new data frames using only the features identified by RFE
remaining_df_optimal <- remaining_df_harvest %>%
  select(HDDOY, all_of(optimal_features))

test_set_optimal <- test_set_harvest %>%
  select(HDDOY, all_of(optimal_features))

# Train the final Random Forest model on the 80% remaining data
cat("\nTraining final Random Forest model with optimal features...\n")
final_model_optimal <- randomForest(HDDOY ~ ., data = remaining_df_optimal, ntree = 100)

# Make predictions on the fixed, held-out test set
test_pred_optimal <- predict(final_model_optimal, newdata = test_set_optimal)
obs_test_optimal <- test_set_optimal$HDDOY

# Compute and print the final test set metrics
test_results_optimal <- data.frame(
  phase = "Test (Optimal Features)",
  R2 = summary(stats::lm(obs_test_optimal ~ test_pred_optimal))$r.squared,
  MAE = Metrics::mae(obs_test_optimal, test_pred_optimal),
  RMSE = Metrics::rmse(obs_test_optimal, test_pred_optimal),
  NSE = hydroGOF::NSE(test_pred_optimal, obs_test_optimal),
  Bias = Metrics::bias(test_pred_optimal, obs_test_optimal)
)

cat("\n--- Final Model Test Results with RFE-Selected Features ---\n")
print(test_results_optimal)

