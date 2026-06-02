# --- 2. PHENOLOGY EXTRACTION FUNCTION (FIXED) ---
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
