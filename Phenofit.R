#Link: https://github.com/eco-hydro/phenofit/blob/master/R/examples/ex-PhenoTrs.R
detach("package:tibble", unload = TRUE)
library(phenofit)
library(phenofit)
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
dates <- as.Date(vi_list_gt20[[1]]$Date)
ndvi  <- vi_list_gt20[[1]]$kNDVI

# Convert dates to DOY (day-of-year)
t <- as.numeric(format(dates, "%j"))

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
library(phenofit)
library(furrr)
library(future)
library(progressr)
library(dplyr)

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
    tout <- 1:365
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
View(phenology_df)

library(ggplot2)
library(gridExtra)  # For arranging multiple plots

# Rename columns for ease of use
df <- phenology_df

# Individual scatter plots
p1 <- ggplot(df, aes(x = PDDOY, y = SOS_trs.sos)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "SOS_trs vs PDDOY", x = "PDDOY", y = "SOS_trs")

p2 <- ggplot(df, aes(x = PDDOY, y = SOS_deriv.sos)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(title = "SOS_deriv vs PDDOY", x = "PDDOY", y = "SOS_deriv")

p3 <- ggplot(df, aes(x = PDDOY, y = UD.UD)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  labs(title = "UD (Gu's Method) vs PDDOY", x = "PDDOY", y = "UD")

p4 <- ggplot(df, aes(x = PDDOY, y = Greenup.Greenup)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(title = "Greenup (Kl) vs PDDOY", x = "PDDOY", y = "Greenup")

# Combine all plots into a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol = 2)

library(ggplot2)
library(gridExtra)


meteo_summary_df <- meteo_summary_df %>%
  mutate(Field_ID = gsub("_VI", "", Field_ID)) %>%
  rename(Field_Year = Field_ID)

df <- phenology_df %>%
  left_join(meteo_summary_df, by = "Field_Year")




library(randomForest)
library(Metrics)

# 1. Filter valid rows (no NA in response or predictors)
df <- df %>%
  dplyr::select(
    SOS_trs.sos, #EOS_trs.eos, 
    #SOS_deriv.sos, POS.pos, 
    #EOS_deriv.eos,
    UD.UD, SD.SD, 
    #DD.DD, RD.RD,
    Greenup.Greenup, Maturity.Maturity, 
    #Senescence.Senescence, Dormancy.Dormancy,
    t0.t0, mn.mn, mx.mx, rsp.rsp, a3.a3, a5.a5,
    # rau.rau,
    PDDOY,
    #mean_Es_M4, mean_Es_M5, mean_Es_M6,
    mean_gdd_M4, mean_gdd_M5, mean_gdd_M6, 
    meanMLSWI26_M2, meanMLSWI26_M3, meanMLSWI26_M4,
    #mean_gdd_M7,
    meansrad_M2,meansrad_M3, meansrad_M4, 
    #meansrad_M5, 
    #meansrad_M6, meansrad_M7,
    #meanRH_M6,meanRH_M7,
    mean_tmean_M2, mean_tmean_M3,mean_tmean_M4, 
    #mean_tmean_M5, mean_tmean_M6, 
    #mean_tmean_M7,
    mean_tmin_M2, mean_tmin_M3, mean_tmin_M4, 
    #mean_tmin_M5, mean_tmin_M6,
    #mean_tmin_M7,
    mean_tmax_M3,mean_tmax_M4,
    #mean_tmax_M5, mean_tmax_M6,
  ) %>%
  dplyr::filter(!is.na(PDDOY)) %>%  # or !is.na(lagobserved) if preferred
  drop_na()

# 2. Create train-test split (80:20)
set.seed(42)
n <- nrow(df)
train_idx <- sample(1:n, size = 0.8 * n)
train <- df[train_idx, ]
test <- df[-train_idx, ]

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
