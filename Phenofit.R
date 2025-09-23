#Link: https://github.com/eco-hydro/phenofit/blob/master/R/examples/ex-PhenoTrs.R
library(ggplot2)
library(Metrics)  # for mae
library(hydroGOF) # for rmse and NSE
library(caret)
library(phenofit)
library(furrr)
library(future)
library(progressr)
library(dplyr)
library(ggplot2)
library(gridExtra)  # For arranging multiple plots


# Helper function to check, install, and load packages
ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Install and load the main packages
ensure_package("dplyr")
ensure_package("lubridate")
ensure_package("phenofit")



# Extract data
dates <- as.Date(vi_list_gt20$F_8316_62_2_2015$Date)
ndvi  <- vi_list_gt20$F_8316_62_2_2015$kNDVI

# Convert dates to DOY (day-of-year)
t <- as.numeric(format(dates, "%j"))
t
# Create full output timeline (daily from Jan 1 to Dec 31)
tout <- 1:365

# Choose phenology fitting methods
methods <- c("AG", "Beck", "Elmore", "Gu", "Zhang")
fit <- curvefit(ndvi, t, tout, methods)

# Extract fitted data for AG model
x <- fit$model$AG

# Extract phenology metrics
trs <- PhenoTrs(x)
der <- PhenoDeriv(x)
gu  <- PhenoGu(x)
kl  <- PhenoKl(x)


op <- par(mfrow = c(2, 2), mar = c(3,4,2,1), oma = c(0,0,2,0))
on.exit(par(op), add = TRUE)

# Panel 1
PhenoTrs(x)
box()
#title("PhenoTrs", line = 0.5, cex.main = 1.2)
axis(1, labels = FALSE, tick = FALSE)
axis(2, labels = FALSE, tick = FALSE)

# Panel 2
PhenoDeriv(x)
box()
#title("PhenoDeriv", line = 0.5, cex.main = 1.2)
axis(1, labels = FALSE, tick = FALSE)
axis(2, labels = FALSE, tick = FALSE)

# Panel 3
PhenoGu(x)
box()
#title("PhenoGu", line = 0.5, cex.main = 1.2)
axis(1, labels = FALSE, tick = FALSE)
axis(2, labels = FALSE, tick = FALSE)

# Panel 4
PhenoKl(x)
box()
#title("PhenoKl", line = 0.5, cex.main = 1.2)
axis(1, labels = FALSE, tick = FALSE)
axis(2, labels = FALSE, tick = FALSE)

# Global title
mtext("Phenology Metrics", outer = TRUE, cex = 1.3, font = 2)



# Extract AG model parameters
params <- x$par["nlminb", ]  # extract parameter row as named vector

# Combine phenology metrics + model parameters into one named vector
phenology_row <- c(
  Model        = "AG",
  SOS_trs      = trs["sos"],
  EOS_trs      = trs["eos"],
  SOS_deriv    = der["sos"],
  POS          = der["pos"],
  EOS_deriv    = der["eos"],
  UD           = gu["UD"],
  SD           = gu["SD"],
  DD           = gu["DD"],
  RD           = gu["RD"],
  Greenup      = kl["Greenup"],
  Maturity     = kl["Maturity"],
  Senescence   = kl["Senescence"],
  Dormancy     = kl["Dormancy"],
  t0           = params["t0"],
  mn           = params["mn"],
  mx           = params["mx"],
  rsp          = params["rsp"],
  a3           = params["a3"],
  rau          = params["rau"],
  a5           = params["a5"]
)

# Convert to dataframe
phenology_df <- as.data.frame(t(phenology_row), stringsAsFactors = FALSE)

# Convert all columns except Model to numeric
phenology_df[ , -1] <- lapply(phenology_df[ , -1], as.numeric)

# View result
print(phenology_df)



####################################################

#----------------------------------------------------

# Optional: use more threads
plan(multisession, workers = parallel::detectCores() - 1)

# Start timing
start_time <- Sys.time()

# Enable progress bar globally
handlers(global = TRUE)
handlers("progress")

progressr::with_progress({
  p <- progressor(steps = length(vi_list_gt20))
  extract_phenology <- function(vi) {
    p()  # Update progress bar
    dates <- as.Date(vi$Date)
    ndvi  <- vi$kNDVI
    if (all(is.na(ndvi)) || length(ndvi) < 5) return(NULL)
    t <- as.numeric(format(dates, "%j"))
    tout <- 1:300
    methods <- c("AG", "Beck", "Elmore", "Gu", "Zhang")
    fit <- tryCatch({
      curvefit(ndvi, t, tout, methods)
    }, error = function(e) return(NULL))
    x <- fit$model$AG
    if (is.null(x)) return(NULL)
    safe_extract <- function(f, x) tryCatch(f(x), error = function(e) NA)
    trs <- safe_extract(PhenoTrs, x)
    der <- safe_extract(PhenoDeriv, x)
    gu  <- safe_extract(PhenoGu, x)
    kl  <- safe_extract(PhenoKl, x)
    # Try extracting model parameters
    params <- tryCatch(x$par["nlminb", ], error = function(e) rep(NA, 7))
    phenology_row <- c(
      Model        = "AG",
      SOS_trs      = trs["sos"],
      EOS_trs      = trs["eos"],
      SOS_deriv    = der["sos"],
      POS          = der["pos"],
      EOS_deriv    = der["eos"],
      UD           = gu["UD"],
      SD           = gu["SD"],
      DD           = gu["DD"],
      RD           = gu["RD"],
      Greenup      = kl["Greenup"],
      Maturity     = kl["Maturity"],
      Senescence   = kl["Senescence"],
      Dormancy     = kl["Dormancy"],
      t0           = params["t0"],
      mn           = params["mn"],
      mx           = params["mx"],
      rsp          = params["rsp"],
      a3           = params["a3"],
      rau          = params["rau"],
      a5           = params["a5"]
    )
    
    df <- as.data.frame(t(phenology_row), stringsAsFactors = FALSE)
    df[ , -1] <- lapply(df[ , -1], as.numeric)
    
    # Add extra metadata
    df$PDDOY      <- if ("PDDOY" %in% names(vi)) vi$PDDOY[1] else NA
    df$HDDOY      <- if ("HDDOY" %in% names(vi)) vi$HDDOY[1] else NA
    df$Field_Year <- if ("Field_Year" %in% names(vi)) vi$Field_Year[1] else NA
    return(df)
  }
  
  # Parallel apply across all sites
  phenology_list <- future_map(vi_list_gt20, extract_phenology, .options = furrr_options(seed = TRUE))
})

# Combine results into single dataframe
phenology_df <- bind_rows(phenology_list)

# End timing
end_time <- Sys.time()
cat("✅ Completed in", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

# View summary
print(dim(phenology_df))
has_UD <- "UD.UD" %in% colnames(phenology_df)
has_DD <- "DD.DD" %in% colnames(phenology_df)

# Print results
cat("UD.UD exists:", has_UD, "\n")
cat("DD.DD exists:", has_DD, "\n")


merged_list <- merged_list[names(merged_list) != "Pond_South_2023"]
merged_list <- lapply(merged_list, function(df) {  # Make sure Field_Year exists in both
  if("Field_Year" %in% colnames(df)) {
    df <- df %>%   # Join phenology_df columns UD.UD and DD.DD by Field_Year
      left_join(
        phenology_df %>% select(Field_Year, UD.UD, DD.DD),
        by = "Field_Year"
      )
  }
  return(df)
})
# Check if UD.UD and DD.DD exist in each dataframe inside merged_list
check_cols <- lapply(names(merged_list), function(field_id) {
  df <- merged_list[[field_id]]
  cols <- colnames(df)
  
  list(
    Field_ID = field_id,
    has_UD = "UD.UD" %in% cols,
    has_DD = "DD.DD" %in% cols
  )
})

# Convert to data frame for easier viewing
check_cols_df <- do.call(rbind, lapply(check_cols, as.data.frame))

print(check_cols_df)


# list of soil indices
soil_indices <- c("AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "BCC", "BNDVI", "BWDRVI", "CIG", "CVI",
                  "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI",
                  "ENDVI", "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR","FCVI", 
                  "GARI", "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", "GOSAVI", "GRNDVI", "GRVI", "GSAVI", 
                  "GVMI","IAVI", "IKAW", "IPVI",
                  "MCARI1", "MCARI2", "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2",
                  "NDDI",  "NDII", "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv", 
                  "NLI", "NMDI", "NRFIg", "NRFIr", "NormG", "NormNIR", "NormR", 
                  "OCVI", "OSAVI", "RCC", "RDVI", "RGBVI", "RGRI", "RI",
                  "SARVI", "SAVI", "SAVI2", "SEVI", "SI",  "SLAVI", "SR", "SR2", 
                  "TDVI", "TGI", "TSAVI", "TVI", "TriVI", "VARI", "VIG", "WDRVI", "WDVI",
                  "bNIRv", "sNIRvLSWI", "sNIRvNDPI", "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR",
                  "ANDWI", "AWEInsh", "AWEIsh",  "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", "MuWIR", 
                  "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI",
                  "RNDVI", "SWM", "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS",
                  "BaI", "DBSI", "EMBI", "MBI", "NDSoI", "NSDS", "NSDSI1", "NSDSI2", "NSDSI3",
                  "RI4XS", "kIPVI", "kNDVI", "kRVI", "nir")

# Remove "Dewitt_2_2023" from merged_list
merged_list[["Dewitt_2_2023"]] <- NULL
meteo_summary_list <- lapply(names(merged_list), function(field_id) {
  df <- merged_list[[field_id]]
  df$Date <- as.Date(df$Date)
  df$DOY <- yday(df$Date)
  # Get DOY_max_fit from the same dataframe
  doy_max <- unique(df$UD.UD)
  if(length(doy_max) != 1 || is.na(doy_max)) {
    doy_max <- max(df$DOY, na.rm = TRUE) # fallback if missing
  }
  # Define 45-day window
  start_doy <- doy_max - 45
  end_doy <- doy_max
  df_window <- df %>%
    dplyr::filter(DOY >= start_doy & DOY <= end_doy)
  
  # ---- compute soil index means within the window ----
  soil_means <- df_window %>%
    dplyr::summarise(across(all_of(soil_indices), ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"))
  
  # ---- compute DOY of maximum rate of change within the window ----
  soil_doys <- lapply(soil_indices, function(idx) {
    vals <- df_window[[idx]]
    if (all(is.na(vals))) return(NA)  # skip if missing
    rate <- diff(vals) / diff(df_window$DOY)  # approximate derivative
    max_doy <- df_window$DOY[-1][which.max(abs(rate))]  # DOY where max change occurs
    return(max_doy)
  }) %>%
    setNames(paste0("DOY_maxROC_", soil_indices)) %>%
    as_tibble()
  
  # ---- weather cumulative + soil averages ----
  df_summary <- df_window %>%
    dplyr::summarise(
      cum_tmean     = sum(tmean, na.rm = TRUE),
      cum_gdd       = sum(gdd, na.rm = TRUE),
      cum_meansrad  = sum(srad, na.rm = TRUE),
      cumGDVI       = sum(GDVI, na.rm = TRUE),
      cumRNDVI      = sum(Lai, na.rm = TRUE),
      cumkNDVI       = sum(kNDVI, na.rm = TRUE),
      cum_vpd       = sum(vpd, na.rm = TRUE),
      cum_tmin      = sum(tmin, na.rm = TRUE),
      cum_tmax      = sum(tmax, na.rm = TRUE),
      cum_RH        = sum(avgRH, na.rm = TRUE),
      cum_soiltemp  = sum(SoilTMP0_10cm_inst, na.rm = TRUE),
      avgsoilorg    = mean(SOC_avg_0_30cm, na.rm = TRUE),
      avgsoilclay   = mean(Clay_avg_0_30cm, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      Field_ID = field_id,
      DOY_max_fit = doy_max
    ) %>%
    dplyr::select(Field_ID, DOY_max_fit, everything()) %>%
    bind_cols(soil_means, soil_doys) %>%  # attach soil stats
    as_tibble()
  
  return(df_summary)
})

meteo_summary_listharvest <- lapply(names(merged_list), function(field_id) {
  df <- merged_list[[field_id]]
  
  df$Date <- as.Date(df$Date)
  df$DOY <- yday(df$Date)
  
  # Get DOY_max_fit from the same dataframe
  doy_max <- unique(df$DD.DD)
  if(length(doy_max) != 1 || is.na(doy_max)) {
    doy_max <- max(df$DOY, na.rm = TRUE) # fallback if missing
  }
  
  # Define 45-day window
  start_doy <- doy_max
  end_doy <- doy_max + 60
  df_window <- df %>%
    dplyr::filter(DOY >= start_doy & DOY <= end_doy)
  
  # ---- compute soil index means within the window ----
  soil_means <- df_window %>%
    dplyr::summarise(across(all_of(soil_indices), ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"))
  
  # ---- compute DOY of maximum rate of change within the window ----
  soil_doys <- lapply(soil_indices, function(idx) {
    vals <- df_window[[idx]]
    if (all(is.na(vals))) return(NA)  # skip if missing
    rate <- diff(vals) / diff(df_window$DOY)  # approximate derivative
    max_doy <- df_window$DOY[-1][which.max(abs(rate))]  # DOY where max change occurs
    return(max_doy)
  }) %>%
    setNames(paste0("DOY_maxROC_", soil_indices)) %>%
    as_tibble()
  
  # ---- weather cumulative + soil averages ----
  df_summary <- df_window %>%
    dplyr::summarise(
      cum_tmean     = sum(tmean, na.rm = TRUE),
      cum_gdd       = sum(gdd, na.rm = TRUE),
      cum_meansrad  = sum(srad, na.rm = TRUE),
      cumGDVI       = sum(GDVI, na.rm = TRUE),
      cumRNDVI      = sum(Lai, na.rm = TRUE),
      cum_vpd       = sum(vpd, na.rm = TRUE),
      cum_tmin      = sum(tmin, na.rm = TRUE),
      cum_tmax      = sum(tmax, na.rm = TRUE),
      cum_RH        = sum(avgRH, na.rm = TRUE),
      cum_soiltemp  = sum(SoilTMP0_10cm_inst, na.rm = TRUE),
      avgsoilorg    = mean(SOC_avg_0_30cm, na.rm = TRUE),
      avgsoilclay   = mean(Clay_avg_0_30cm, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      Field_ID = field_id,
      DOY_max_fit = doy_max
    ) %>%
    dplyr::select(Field_ID, DOY_max_fit, everything()) %>%
    bind_cols(soil_means, soil_doys) %>%  # attach soil stats
    as_tibble()
  
  return(df_summary)
})


meteo_summary_df <- bind_rows(meteo_summary_list)
meteo_summary_df <- meteo_summary_df %>%
  mutate(Field_ID = gsub("_VI", "", Field_ID)) %>%
  rename(Field_Year = Field_ID)

meteo_summary_df_harvest <- bind_rows(meteo_summary_listharvest)
meteo_summary_df_harvest <- meteo_summary_df_harvest %>%
  mutate(Field_ID = gsub("_VI", "", Field_ID)) %>%
  rename(Field_Year = Field_ID)

#df <- phenology_df %>%
  #left_join(meteo_summary_df, by = "Field_Year")

df <- deinesharmonicminmaxdf %>%
  left_join(phenology_df, by = "Field_Year")
df <- df %>%
  left_join(meteo_summary_df, by = "Field_Year")
df <- df %>%
  rename(DOY_max_fit = DOY_max_fit.x) %>%  # remove .x suffix
  select(-DOY_max_fit.y)                   # drop the .y column
df <- df %>%
  rename(
    PDDOY = PDDOY.x,
    HDDOY = HDDOY.x
  ) %>%
  select(
    -PDDOY.y,
    -HDDOY.y
  )     # drop the .y column


dfharvest <- deinesharmonicminmaxdf %>%
  left_join(phenology_df, by = "Field_Year")
dfharvest <- dfharvest %>%
  left_join(meteo_summary_df_harvest, by = "Field_Year")
dfharvest <- dfharvest %>%
  rename(DOY_max_fit = DOY_max_fit.x) %>%  # remove .x suffix
  select(-DOY_max_fit.y)                   # drop the .y column
dfharvest <- dfharvest %>%
  rename(
    PDDOY = PDDOY.x,
    HDDOY = HDDOY.x
  ) %>%
  select(
    -PDDOY.y,
    -HDDOY.y
  )    

#---------------------------------------------------------
#Problematic data
#---------------------------------------------------------
remove_list <- c(
  "F_20581_68_MF_2015",
  "Judys_2022",
  "Seed_Rice_2022",
  "Walls_06_07_2022",
  "Walls_09_2021",
  "Cattlet_02_2020",
  "East_Joe_T_2022",
  "F_8252_7_HF_2015",
  "F_8319_65_5_2017",
  "F_8320_66_6_2017",
  "F_8320_66_6_2020"
)
df <- df[!df$Field_Year %in% remove_list, ]
dfharvest <- dfharvest[!dfharvest$Field_Year %in% remove_list, ]

df$lagtrsupdate <- df$SOS_trs.sos - df$UD.UD
df$lagtrsgreenup <- df$SOS_trs.sos - df$Greenup.Greenup
df$lagsosderpddoy<-df$SOS_deriv.sos  - df$PDDOY
df$lagsostrspddoy<-df$SOS_trs.sos - df$PDDOY
df$lagudpddoy<-df$UD.UD - df$PDDOY
df$gsl<-df$HDDOY - df$PDDOY

dfharvest <- dfharvest[!dfharvest$Field_Year %in% remove_list, ]
dfharvest$lagtrsupdate <- dfharvest$SOS_trs.sos - dfharvest$UD.UD
dfharvest$lagtrsgreenup <- dfharvest$SOS_trs.sos - dfharvest$Greenup.Greenup

#------------------------------------------------------------------------------
#RANDOM FOREST
#------------------------------------------------------------------------------
library(randomForest)
library(Metrics)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Keep only top20_rfe + response variable
df_planting <- df %>%
  dplyr::select(
    # --- Top 20 RFE variables ---
    a1, 
    #UD.UD, 
    DOY_min_fit, 
    #Greenup.Greenup, 
    DOY_min_fit,
    SOS_trs.sos, 
    a2, 
    #Dormancy.Dormancy, 
    #RD.RD, 
    EOS_trs.eos, 
    #DOY_max_before_min_fit, 
    Value_max_obs,
    #DD.DD, 
    b1, 
    b2,
    #mx.mx, 
    #EOS_deriv.eos, 
    #SOS_deriv.sos, 
    #lagtrsgreenup, 
    #Senescence.Senescence, 
    #rsp.rsp, 
    #lagtrsupdate, 
    #a3.a3,
    
    # --- Response variable ---
    PDDOY, 
    avgsoilclay, avgsoilorg, 
    # --- Commented out variables ---
    # lagobserved,
    # POS.pos,
    # cumGDVI,
    # Laglocalmaxglomax, Laglocalminglomax, Laglocalmaxlocalmin,
    #cum_gdd, 
    cum_meansrad, 
    #cum_vpd, 
    cum_tmin, 
    cum_tmax, cum_soiltemp, cum_RH,
 mean_ExG  ,mean_MCARI1    ,mean_MTVI1 ,  mean_OSAVI  ,
   mean_TGI ,    mean_AWEInsh 
  ) %>%
  dplyr::filter(!is.na(PDDOY)) %>%
  drop_na()

# 2. Create train-test split (80:20)
  set.seed(11)
n <- nrow(df_planting)
train_idx <- sample(1:n, size = 0.8 * n)
train <- df_planting[train_idx, ]
test <- df_planting[-train_idx, ]

# 3. Train random forest model
rf_model <- randomForest(PDDOY ~ ., data = train,
                         ntree = 100, importance = TRUE, do.trace = 10)

# 4. Predict on train and test
train$pred_PDDOY <- predict(rf_model, newdata = train)
test$pred_PDDOY <- predict(rf_model, newdata = test)

# 5. Evaluate performance
train_r2 <- summary(lm(PDDOY ~ pred_PDDOY, data = train))$r.squared
train_mae <- mae(train$PDDOY, train$pred_PDDOY)

test_r2 <- summary(lm(PDDOY ~ pred_PDDOY, data = test))$r.squared
test_mae <- mae(test$PDDOY, test$pred_PDDOY)

# 6. Plot predictions (Train)
ggplot(train, aes(x = PDDOY, y = pred_PDDOY)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Training Set Performance",
       x = "Observed PDDOY", y = "Predicted PDDOY") +
  annotate("text", x = min(train$PDDOY), y = max(train$pred_PDDOY),
           label = paste0("R² = ", round(train_r2, 2), 
                          "\nMAE = ", round(train_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")


# 7. Plot predictions (Test)
ggplot(test, aes(x = PDDOY, y = pred_PDDOY)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Testing Set Performance",
       x = "Observed PDDOY", y = "Predicted PDDOY") +
  annotate("text", x = min(test$PDDOY), y = max(test$pred_PDDOY),
           label = paste0("R² = ", round(test_r2, 2), 
                          "\nMAE = ", round(test_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")
# 8. Variable importance
varImpPlot(rf_model, main = "Random Forest Variable Importance")

# Remove PDDOY (response is lagtrs)
df_predictors <- df_planting %>% dplyr::select(-PDDOY)
response <- df_planting$PDDOY

# Set seed for reproducibility
set.seed(42)
# ------------------------------
# 5. Recursive Feature Elimination (RFE)
# ------------------------------
df_predictors <- df_planting %>% dplyr::select(-PDDOY)
response <- df_planting$PDDOY
control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

rfe_results <- rfe(
  x = df_predictors,
  y = response,
  sizes = c(5, 10, 15, 20, 25, 30),
  rfeControl = control
)

# Print RFE summary
print(rfe_results)

# ------------------------------
# 6. Extract top 10 variables from RFE
# ------------------------------
top_rfe_vars <- predictors(rfe_results)
top30_rfe <- top_rfe_vars[1:30]
print("Top 30 variables by RFE:")
print(top30_rfe)


# Filter the list to only include columns that are actually in the 'test' dataframe.
predictor_cols <- intersect(all_possible_predictors, colnames(test))
# Create an empty list to store the plots
residual_plots <- list()

for (col in predictor_cols) {
  # We use a tryCatch block to skip any columns that might not exist in the 'test' dataframe
  # for example if a column was entirely removed by the drop_na() call.
  tryCatch({
    # Create a plot for the current predictor
    p <- ggplot(test, aes_string(x = col, y = "residual")) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = paste("Residuals vs.", col),
        x = col,
        y = "Residual (Actual PDDOY - Predicted PDDOY)"
      ) +
      theme_minimal()
    
    # Add the plot to our list
    residual_plots[[col]] <- p
  }, error = function(e) {
    message(paste("Skipping column '", col, "' due to error: ", e$message))
  })
}

# 4. Save the plots to the specified directory.
# This loop will save each plot generated above as a PNG file.
save_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/residualtestingPD"

# Create the directory if it doesn't exist
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}
for (col in predictor_cols) {
  if (!is.null(residual_plots[[col]])) {
    file_path <- file.path(save_dir, paste0("residual_plot_", col, ".png"))
    ggsave(file_path, plot = residual_plots[[col]], width = 8, height = 6)
  }
}

##----------------------------------------------------
#Check for Null Values
##----------------------------------------------------
sum(sapply(phenology_list, is.null))  # How many NULL entries?
head(phenology_list)
null_entries <- names(phenology_list)[sapply(phenology_list, is.null)]
print(null_entries)
(vi_list_gt20$Baker_30_2024$kNDVI)
vi_list_gt20$Baker_20_2023$kNDVI
vi_list_gt20$Baker_50_2024$kNDVI


##----------------------------------------------------
#harvest
##----------------------------------------------------

# 1. Filter valid rows (no NA in response or predictors)
df_harvest <- dfharvest %>%
  dplyr::select(
    a1, 
    #UD.UD, 
    #DOY_min_fit, 
    Greenup.Greenup, 
    DOY_min_fit,
    SOS_trs.sos, 
    a2, 
    #Dormancy.Dormancy, 
    RD.RD, 
    EOS_trs.eos, 
    DOY_max_before_min_fit, 
    DD.DD, 
    b1, 
    mx.mx, 
    EOS_deriv.eos, 
    #SOS_deriv.sos, 
    #lagtrsgreenup, 
    #Senescence.Senescence, 
    rsp.rsp, 
    #lagtrsupdate, 
    #a3.a3,
    
    # --- Response variable ---
    HDDOY, 
    avgsoilclay, avgsoilorg, 
    # --- Commented out variables ---
    #lagobserved,
     POS.pos,
     #cumGDVI,
    #Laglocalmaxglomax, Laglocalminglomax, Laglocalmaxlocalmin,
    #cum_gdd, 
    cum_meansrad, 
    cum_vpd, 
    cum_tmin, 
    DOY_maxROC_IAVI, DOY_maxROC_MSI, DOY_maxROC_TGI,DOY_maxROC_ExGR,
    
    DOY_maxROC_DSI, DOY_maxROC_EVI,  DOY_maxROC_IAVI,  DOY_maxROC_MSI,
    #DOY_maxROC_ExGR,DOY_maxROC_MRBVI,
    DOY_maxROC_NMDI
    #cum_tmax, cum_soiltemp, cum_RH
  ) %>%
  dplyr::filter(!is.na(HDDOY)) %>%  # or !is.na(lagobserved) if preferred
  drop_na()

# 2. Create train-test split (80:20)
set.seed(32)
n <- nrow(df_harvest)
train_idx <- sample(1:n, size = 0.8 * n)
train <- df_harvest[train_idx, ]
test <- df_harvest[-train_idx, ]

# 3. Train random forest model
rf_model <- randomForest(HDDOY ~ ., data = train,
                         ntree = 200, importance = TRUE, do.trace = 10)

# 4. Predict on train and test
train$pred_HDDOY <- predict(rf_model, newdata = train)
test$pred_HDDOY <- predict(rf_model, newdata = test)

# 5. Evaluate performance
train_r2 <- summary(lm(HDDOY ~ pred_HDDOY, data = train))$r.squared
train_mae <- mae(train$HDDOY, train$pred_HDDOY)

test_r2 <- summary(lm(HDDOY ~ pred_HDDOY, data = test))$r.squared
test_mae <- mae(test$HDDOY, test$pred_HDDOY)

# 6. Plot predictions (Train)
ggplot(train, aes(x = HDDOY, y = pred_HDDOY)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Training Set Performance",
       x = "Observed HDDOY", y = "Predicted HDDOY") +
  annotate("text", x = min(train$HDDOY), y = max(train$pred_HDDOY),
           label = paste0("R² = ", round(train_r2, 2), 
                          "\nMAE = ", round(train_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")


# 7. Plot predictions (Test)
ggplot(test, aes(x = HDDOY, y = pred_HDDOY)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Testing Set Performance",
       x = "Observed HDDOY", y = "Predicted HDDOY") +
  annotate("text", x = min(test$HDDOY), y = max(test$pred_HDDOY),
           label = paste0("R² = ", round(test_r2, 2), 
                          "\nMAE = ", round(test_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")
# 8. Variable importance
varImpPlot(rf_model, main = "Random Forest Variable Importance")


library(ggplot2)

ggplot(df_planting, aes(x = DOY_min_fit, y = PDDOY, color = meansrad_M5)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  scale_color_viridis_c(option = "plasma") +  # nice continuous color scale
  labs(
    title = "DOY_min_fit vs PDDOY colored by SOS_trs.sos",
    x = "DOY_min_fit",
    y = "PDDOY",
    color = "SOS_trs.sos"
  ) +
  theme_minimal()


df_planting$meansrad_M5

#######################################################
#Phenology data check
# 10 smallest values ignoring NA
head(sort(phenology_df$Dormancy.Dormancy, na.last = NA), 20)
# Get indices of 10 smallest UD.UD values ignoring NA
idx <- order(phenology_df$Greenup.Greenup, na.last = NA)[1:20]

# Extract corresponding Field_Year values
phenology_df$Field_Year[idx]

#problems F_8252_7_HF_2015, F_8320_66_6_2020, F_20578_65_Wg_N_2021

#------------------------------------------------------------------------------------------
#### Create another meteo for Residual plots of SOS
#------------------------------------------------------------------------------------------

merged_list_sos <- merged_list[names(merged_list) != "Pond_South_2023"]
phenology_df$SOS_trs.sos
merged_list_sos <- lapply(merged_list_sos, function(df) {  # Make sure Field_Year exists in both
  if("Field_Year" %in% colnames(df)) {
    df <- df %>%   # Join phenology_df columns UD.UD and DD.DD by Field_Year
      left_join(
        phenology_df %>% select(Field_Year, SOS_trs.sos, DD.DD),
        by = "Field_Year"
      )
  }
  return(df)
})
# Check if SOS_trs.sos and DD.DD exist in each dataframe inside merged_list_sos
check_cols <- lapply(names(merged_list_sos), function(field_id) {
  df <- merged_list_sos[[field_id]]
  cols <- colnames(df)
  
  list(
    Field_ID = field_id,
    has_SOS_trs.sos = "SOS_trs.sos" %in% cols,
    has_DD = "DD.DD" %in% cols
  )
})

# Convert to data frame for easier viewing
check_cols_df <- do.call(rbind, lapply(check_cols, as.data.frame))
print(check_cols_df)
# list of soil indices
soil_indices <- c("AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "BCC", "BNDVI", "BWDRVI", "CIG", "CVI",
                  "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI",
                  "ENDVI", "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR","FCVI", 
                  "GARI", "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", "GOSAVI", "GRNDVI", "GRVI", "GSAVI", 
                  "GVMI","IAVI", "IKAW", "IPVI",
                  "MCARI1", "MCARI2", "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2",
                  "NDDI",  "NDII", "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv", 
                  "NLI", "NMDI", "NRFIg", "NRFIr", "NormG", "NormNIR", "NormR", 
                  "OCVI", "OSAVI", "RCC", "RDVI", "RGBVI", "RGRI", "RI",
                  "SARVI", "SAVI", "SAVI2", "SEVI", "SI",  "SLAVI", "SR", "SR2", 
                  "TDVI", "TGI", "TSAVI", "TVI", "TriVI", "VARI", "VIG", "WDRVI", "WDVI",
                  "bNIRv", "sNIRvLSWI", "sNIRvNDPI", "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR",
                  "ANDWI", "AWEInsh", "AWEIsh",  "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", "MuWIR", 
                  "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI",
                  "RNDVI", "SWM", "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS",
                  "BaI", "DBSI", "EMBI", "MBI", "NDSoI", "NSDS", "NSDSI1", "NSDSI2", "NSDSI3",
                  "RI4XS", "kIPVI", "kNDVI", "kRVI", "nir")
# Remove "Dewitt_2_2023" from merged_list_sos
merged_list_sos[["Dewitt_2_2023"]] <- NULL
meteo_summary_list_sos <- lapply(names(merged_list_sos), function(field_id) {
  df <- merged_list_sos[[field_id]]
  df$Date <- as.Date(df$Date)
  df$DOY <- yday(df$Date)
  # Get DOY_max_fit from the same dataframe
  doy_max <- unique(df$SOS_trs.sos)
  if(length(doy_max) != 1 || is.na(doy_max)) {
    doy_max <- max(df$DOY, na.rm = TRUE) # fallback if missing
  }
  # Define 45-day window
  start_doy <- doy_max - 45
  end_doy <- doy_max
  df_window <- df %>%
    dplyr::filter(DOY >= start_doy & DOY <= end_doy)
  
  # ---- compute soil index means within the window ----
  soil_means <- df_window %>%
    dplyr::summarise(across(all_of(soil_indices), ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"))
  
  # ---- compute DOY of maximum rate of change within the window ----
  soil_doys <- lapply(soil_indices, function(idx) {
    vals <- df_window[[idx]]
    if (all(is.na(vals))) return(NA)  # skip if missing
    rate <- diff(vals) / diff(df_window$DOY)  # approximate derivative
    max_doy <- df_window$DOY[-1][which.max(abs(rate))]  # DOY where max change occurs
    return(max_doy)
  }) %>%
    setNames(paste0("DOY_maxROC_", soil_indices)) %>%
    as_tibble()
  
  # ---- weather cumulative + soil averages ----
  df_summary <- df_window %>%
    dplyr::summarise(
      cum_tmean     = sum(tmean, na.rm = TRUE),
      cum_gdd       = sum(gdd, na.rm = TRUE),
      cum_meansrad  = sum(srad, na.rm = TRUE),
      cumkNDVI       = sum(kNDVI, na.rm = TRUE),
      cumRNDVI      = sum(Lai, na.rm = TRUE),
      cum_vpd       = sum(vpd, na.rm = TRUE),
      cum_tmin      = sum(tmin, na.rm = TRUE),
      cum_tmax      = sum(tmax, na.rm = TRUE),
      cum_RH        = sum(avgRH, na.rm = TRUE),
      cum_soiltemp  = sum(SoilTMP0_10cm_inst, na.rm = TRUE),
      avgsoilorg    = mean(SOC_avg_0_30cm, na.rm = TRUE),
      avgsoilclay   = mean(Clay_avg_0_30cm, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      Field_ID = field_id,
      DOY_max_fit = doy_max
    ) %>%
    dplyr::select(Field_ID, DOY_max_fit, everything()) %>%
    bind_cols(soil_means, soil_doys) %>%  # attach soil stats
    as_tibble()
  
  return(df_summary)
})

meteo_summary_df_sos <- bind_rows(meteo_summary_list_sos)
meteo_summary_df_sos <- meteo_summary_df_sos %>%
  mutate(Field_ID = gsub("_VI", "", Field_ID)) %>%
  rename(Field_Year = Field_ID)

meteo_summary_df_sos

