# --- 2. PHENOLOGY EXTRACTION FUNCTION (FIXED) ---
# Helper to safely extract metrics, returning NA vector on failure
safe_extract_metrics <- function(f, x, names) {
  tryCatch({
    result <- f(x)
    if (is.null(result)) { stop("Pheno metric result is NULL.") }
    return(result[names])
  }, error = function(e) {
    # Return a named vector of NAs if extraction fails
    setNames(rep(NA_real_, length(names)), names)
  })
}
extract_phenology <- function(vi) {
  # Define a default row of NAs to return in case of failure
  na_row <- data.frame(
    Model = "AG", SOS_trs = NA, EOS_trs = NA, SOS_deriv = NA, POS = NA, 
    EOS_deriv = NA, UD = NA, SD = NA, DD = NA, RD = NA, Greenup = NA, 
    Maturity = NA, Senescence = NA, Dormancy = NA, t0 = NA, mn = NA, 
    mx = NA, rsp = NA, a3 = NA, rau = NA, a5 = NA, 
    PDDOY = if ("PDDOY" %in% names(vi)) vi$PDDOY[1] else NA,
    HDDOY = if ("HDDOY" %in% names(vi)) vi$HDDOY[1] else NA,
    Field_Year = if ("Field_Year" %in% names(vi)) vi$Field_Year[1] else NA,
    stringsAsFactors = FALSE
  )
  # 1. Basic Data Preparation & Check
  dates <- as.Date(vi$Date)
  ndvi  <- vi$kNDVI
  
  # Use tout <- 1:365 for full annual cycle (safer than 1:300)
  tout <- 1:365 
  
  if (all(is.na(ndvi)) || length(ndvi) < 10) { 
    message(paste("Skipping site due to insufficient data:", vi$Field_Year[1]))
    return(na_row) # Return a row of NAs immediately
  }
  
  t <- as.numeric(format(dates, "%j"))
  methods <- c("AG", "Beck", "Elmore", "Gu", "Zhang")
  
  # 2. Curve Fitting (The most common point of failure)
  fit <- tryCatch({
    curvefit(ndvi, t, tout, methods)
  }, error = function(e) {
    # Log the error and return the NA row
    warning(paste("curvefit failed for site:", vi$Field_Year[1], "Error:", conditionMessage(e)))
    return(na_row)
  })
  
  # Extract the AG model
  x <- fit$model$AG
  
  if (is.null(x) || is.data.frame(x)) {
    # If the model didn't converge or failed, x might be NULL or an error structure
    return(na_row) 
  }
  
  # 3. Phenology Metrics Extraction
  
  # Define names for clean extraction
  trs_names <- c("sos", "eos")
  der_names <- c("sos", "pos", "eos")
  gu_names <- c("UD", "SD", "DD", "RD")
  kl_names <- c("Greenup", "Maturity", "Senescence", "Dormancy")
  
  trs <- safe_extract_metrics(PhenoTrs, x, trs_names)
  der <- safe_extract_metrics(PhenoDeriv, x, der_names)
  gu  <- safe_extract_metrics(PhenoGu, x, gu_names)
  kl  <- safe_extract_metrics(PhenoKl, x, kl_names)
  
  # 4. Extract Model Parameters
  params <- tryCatch(x$par["nlminb", ], error = function(e) setNames(rep(NA_real_, 7), c("t0", "mn", "mx", "rsp", "a3", "rau", "a5")))
  
  
  # 5. Combine results into a single data frame row
  phenology_row <- data.frame(
    Model = "AG",
    SOS_trs = trs["sos"],
    EOS_trs = trs["eos"],
    SOS_deriv = der["sos"],
    POS = der["pos"],
    EOS_deriv = der["eos"],
    UD = gu["UD"],
    SD = gu["SD"],
    DD = gu["DD"],
    RD = gu["RD"],
    Greenup = kl["Greenup"],
    Maturity = kl["Maturity"],
    Senescence = kl["Senescence"],
    Dormancy = kl["Dormancy"],
    t0 = params["t0"],
    mn = params["mn"],
    mx = params["mx"],
    rsp = params["rsp"],
    a3 = params["a3"],
    rau = params["rau"],
    a5 = params["a5"],
    PDDOY = na_row$PDDOY,
    HDDOY = na_row$HDDOY,
    Field_Year = na_row$Field_Year,
    stringsAsFactors = FALSE
  )
  
  # Ensure all columns (except Model) are numeric
  phenology_row[ , -1] <- lapply(phenology_row[ , -1], as.numeric)
  
  return(phenology_row)
}


# --- 3. PARALLEL EXECUTION ---

# Set up parallel execution using all but one core
plan(multisession, workers = parallel::detectCores() - 1)
message(paste("Starting parallel run using", future::nbrOfWorkers(), "cores."))

start_time <- Sys.time()

progressr::with_progress({
  p <- progressor(steps = length(vi_list_gt20))
  
  # Custom wrapper to call p() and then the function (ensures p() is called)
  safe_map_wrapper <- function(vi) {
    on.exit(p(), add = TRUE) # Ensures p() is called when the function exits (success or failure)
    extract_phenology(vi)
  }
  
  # Parallel apply across all sites
  phenology_list <- future_map(vi_list_gt20, safe_map_wrapper, .options = furrr_options(seed = TRUE))
})

# Combine results into single dataframe (bind_rows naturally handles NULLs/NA rows)
phenology_df <- bind_rows(phenology_list)

# Clear the future plan after completion
plan(sequential) 

end_time <- Sys.time()
print(paste("Time taken:", round(end_time - start_time, 2), attributes(end_time - start_time)$units))


# View result
cat("\n--- FINAL PHENOLOGY RESULT DATAFRAME ---\n")
print(phenology_df)
