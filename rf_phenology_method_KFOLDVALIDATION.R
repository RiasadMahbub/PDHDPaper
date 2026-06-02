library(randomForest)
library(Metrics)
library(hydroGOF) # for NSE
library(dplyr)
library(ggplot2)
library(tidyr)
library(fastshap)
library(stringr)

# --- Define SHAP Wrapper Function ---
p_fun <- function(object, newdata) {
  predict(object, newdata = newdata)
}

# --- Data Preparation & Merging Source ---
# Fixing the case_when error by being explicit about the data object
df <- df %>%
  mutate(
    FIELD_NAME = str_remove(Field_ID, "_\\d{4}$"),
    year = as.numeric(str_extract(Field_ID, "\\d{4}$")) # Extract year from Field_ID
  ) %>%
  left_join(fieldnamesmergedshpdf %>% select(FIELD_NAME, source), by = "FIELD_NAME") %>%
  mutate(source = case_when(
    FIELD_NAME %in% c("Carr_North", "East_Harvey", "Flat") ~ "Isbell",
    TRUE ~ .data$source # Use .data$ to avoid conflict with source() function
  ))

df$lagtrs <- df$SOS_trs.sos - df$PDDOY
df$lagtrsupdate <- df$SOS_trs.sos - df$UD.UD
df$lagtrsgreenup <- df$SOS_trs.sos - df$Greenup.Greenup
df$lagtrsminfit <- df$SOS_trs.sos - df$DOY_min_fit

# Fix dormancy for Harvest
na_count <- sum(is.na(dfharvest$Dormancy.Dormancy))
mean_diff <- mean(dfharvest$SOS_deriv.sos - dfharvest$Dormancy.Dormancy, na.rm = TRUE)
dfharvest$Dormancy.Dormancy[is.na(dfharvest$Dormancy.Dormancy)] <- mean_diff

# ========================================================
# PLANTING MODEL (100 RUNS WITH SOURCE & YEAR TRACKING)
# ========================================================

# Include 'source' and 'year' in the select but we will exclude them from the formula
df_planting_pheno <- df %>%
  dplyr::select(cum_tmin, cum_soiltemp, SOS_trs.sos, cum_gdd, SOS_deriv.sos, 
                cum_RH, cum_vpd, avgsoilorg, source, year, PDDOY) %>%
  dplyr::filter(!is.na(PDDOY)) %>%
  drop_na()

set.seed(123)
n_planting <- nrow(df_planting_pheno)
test_idx_planting <- sample(1:n_planting, size = 0.2 * n_planting)
test_set_planting <- df_planting_pheno[test_idx_planting, ]
remaining_df_planting <- df_planting_pheno[-test_idx_planting, ]

results_planting <- data.frame()
planting_source_stats <- list() # To store error and counts per source
planting_year_stats <- list()   # To store error and counts per year

for (i in 1:100) {
  set.seed(100 + i)
  n_remain_planting <- nrow(remaining_df_planting)
  train_idx_planting <- sample(1:n_remain_planting, size = 0.75 * n_remain_planting)
  train_set_planting <- remaining_df_planting[train_idx_planting, ]
  val_set_planting <- remaining_df_planting[-train_idx_planting, ]
  
  # Train model EXCLUDING 'source' and 'year'
  predictor_names <- setdiff(colnames(df_planting_pheno), c("PDDOY", "source", "year"))
  rf_model_planting <- randomForest::randomForest(
    x = train_set_planting[, predictor_names],
    y = train_set_planting$PDDOY,
    ntree = 100, 
    importance = TRUE
  )
  
  # Predictions
  val_pred_planting <- predict(rf_model_planting, newdata = val_set_planting[, predictor_names])
  val_set_planting$pred <- val_pred_planting
  
  # --- Track Error and Diversity per Source ---
  run_source_summary <- val_set_planting %>%
    group_by(source) %>%
    summarise(
      Val_RMSE = Metrics::rmse(PDDOY, pred),
      Val_MAE = Metrics::mae(PDDOY, pred),
      Val_Count = n(),
      .groups = 'drop'
    ) %>%
    mutate(run = i)
  
  planting_source_stats[[i]] <- run_source_summary
  
  # --- Track Error and Diversity per Year ---
  run_year_summary <- val_set_planting %>%
    group_by(year) %>%
    summarise(
      Val_RMSE = Metrics::rmse(PDDOY, pred),
      Val_MAE = Metrics::mae(PDDOY, pred),
      Val_Count = n(),
      .groups = 'drop'
    ) %>%
    mutate(run = i)
  
  planting_year_stats[[i]] <- run_year_summary
  
  # Standard metrics storage
  results_planting <- rbind(results_planting, data.frame(
    run = i,
    Val_RMSE = Metrics::rmse(val_set_planting$PDDOY, val_pred_planting),
    Val_MAE = Metrics::mae(val_set_planting$PDDOY, val_pred_planting),
    Val_R2 = summary(lm(PDDOY ~ pred, data = val_set_planting))$r.squared
  ))
}

# Combine Source Stats with Mean and SD
planting_source_analysis <- bind_rows(planting_source_stats) %>%
  group_by(source) %>%
  summarise(
    Avg_RMSE = mean(Val_RMSE, na.rm = TRUE),
    SD_RMSE = sd(Val_RMSE, na.rm = TRUE),
    Avg_MAE = mean(Val_MAE, na.rm = TRUE),
    SD_MAE = sd(Val_MAE, na.rm = TRUE),
    Avg_Count_Val = mean(Val_Count, na.rm = TRUE),
    Freq_In_Val = n()
  )

# Combine Year Stats with Mean and SD
planting_year_analysis <- bind_rows(planting_year_stats) %>%
  group_by(year) %>%
  summarise(
    Avg_RMSE = mean(Val_RMSE, na.rm = TRUE),
    SD_RMSE = sd(Val_RMSE, na.rm = TRUE),
    Avg_MAE = mean(Val_MAE, na.rm = TRUE),
    SD_MAE = sd(Val_MAE, na.rm = TRUE),
    Avg_Count_Val = mean(Val_Count, na.rm = TRUE),
    Freq_In_Val = n()
  )
planting_year_analysis
# Final Test Set Diversity
test_source_diversity <- test_set_planting %>% group_by(source) %>% summarise(Count = n())
test_year_diversity <- test_set_planting %>% group_by(year) %>% summarise(Count = n())
test_set_planting
test_set_planting
# ========================================================
# PERFORMANCE PLOTS & SHAP
# ========================================================

# Final Model Planting
predictor_names <- setdiff(colnames(remaining_df_planting), c("PDDOY", "source", "year"))
final_model_planting <- randomForest(
  x = remaining_df_planting[, predictor_names],
  y = remaining_df_planting$PDDOY,
  ntree = 100
)

# Extract just the numeric matrix for SHAP
X_test_p <- as.matrix(test_set_planting[, predictor_names])

# Calculate SHAP values
final_shap_p <- fastshap::explain(
  final_model_planting, 
  X = X_test_p, 
  pred_wrapper = p_fun, 
  nsim = 50
)

plot_shap_summary <- function(shap_df, X_df, title) {
  shap_long <- as.data.frame(shap_df) %>%
    mutate(ID = row_number()) %>%
    tidyr::pivot_longer(-ID, names_to = "Variable", values_to = "SHAP_Value")
  feat_long <- as.data.frame(X_df) %>%
    mutate(across(everything(), ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)))) %>%
    mutate(ID = row_number()) %>%
    tidyr::pivot_longer(-ID, names_to = "Variable", values_to = "Feature_Value")
  plot_data <- left_join(shap_long, feat_long, by = c("ID", "Variable"))
  var_order <- plot_data %>% group_by(Variable) %>% summarise(m = mean(abs(SHAP_Value))) %>% arrange(m) %>% pull(Variable)
  plot_data$Variable <- factor(plot_data$Variable, levels = var_order)
  
  ggplot(plot_data, aes(x = SHAP_Value, y = Variable, color = Feature_Value)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = "dashed") +
    geom_jitter(alpha = 0.6, size = 2, height = 0.2) +
    scale_color_gradient(low = "#4DBBD5FF", high = "#E64B35FF", name = "Feature Value") +
    labs(title = title, x = "SHAP Value (Impact in days)", y = NULL) +
    theme_minimal()
}

p_planting_shap <- plot_shap_summary(final_shap_p, X_test_p, "Planting Date: SHAP Distribution")

# --- Outputs ---
print("--- Planting Source-Wise Error Analysis (Validation) ---")
print(planting_source_analysis)

print("--- Planting Year-Wise Error Analysis (Validation) ---")
print(planting_year_analysis)

print("--- Test Set Diversity ---")
print("Source counts:")
print(test_source_diversity)
print("Year counts:")
print(test_year_diversity)

print(p_planting_shap)
final_shap_p
colMeans(abs(final_shap_p))
#--------------------------------------------
#Bar chart with sampling--------------------
#--------------------------------------------
# Step 1: Create new labels based on lat/lon
source_locations <- tibble::tribble(
  ~source, ~lat, ~lon, ~n_fields,
  "DWMRU_rice_data", 35.7, -90.2, 14,
  "Isbell", 34.6, -91.7, 72,
  "arva_rice_data", 34.4, -91.7, 48,
  "isbellcl", 34.6, -91.8, 1,
  "matt_morris_data", 34.7, -91.8, 14,
  "ryan_moore_sullivan_data", 35.7, -90.1, 65,
  "unilever_data", 34.8, -91.8, 44
)
# Step 2: Merge lat/lon info and create new simplified labels
planting_source_analysis <- planting_source_analysis %>%
  left_join(source_locations %>% select(source, lat, lon), by = "source") %>%
  mutate(source_label = paste0("lat", round(lat,1), "_lon", abs(round(lon,1))))

# Step 3: Plot bar chart with error bars for SD_MAE
ggplot(planting_source_analysis, aes(x = source_label, y = Avg_MAE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Avg_MAE - SD_MAE, ymax = Avg_MAE + SD_MAE), width = 0.1) +
  labs(
    x = "Location",
    y = "Average Mean Absolute Error (day)",
    title = "Avg MAE per Source with SD Error Bars"
  ) +
  theme_classic(base_size = 16)

library(ggplot2)

# Assuming your tibble is planting_year_analysis
ggplot(planting_year_analysis, aes(x = factor(year), y = Avg_MAE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Avg_MAE - SD_MAE, ymax = Avg_MAE + SD_MAE), width = 0.3) +
  labs(x = "Year", y = "Average Mean Absolute Error (day)") +
  theme_classic(base_size = 16)


# Select only required columns and round to 2 decimal places
mae_table_space <- planting_source_analysis %>%
  select(lat, lon, Avg_MAE, SD_MAE) %>%
  mutate(
    lat = round(lat, 2),
    lon = round(lon, 2),
    Avg_MAE = round(Avg_MAE, 2),
    SD_MAE = round(SD_MAE, 2)
  )

# Save as CSV
write.csv(mae_table_space, "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Table/planting_source_mae.csv", row.names = FALSE)
#Select required columns and round
yearly_mae_table <- planting_year_analysis %>%
  select(year, Avg_MAE, SD_MAE) %>%
  mutate(
    Avg_MAE = round(Avg_MAE, 2),
    SD_MAE = round(SD_MAE, 2)
  )

# Save as CSV
write.csv(yearly_mae_table, "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Table/yearly_avg_mae.csv", row.names = FALSE)
library(dplyr)

# Count rows per source in df
df_counts <- df %>%
  group_by(source) %>%
  summarise(count = n())

# Merge with source_locations
source_summary <- source_locations %>%
  left_join(df_counts, by = "source") %>%
  # Optional: replace NAs with 0 if some sources are missing
  mutate(count = ifelse(is.na(count), 0, count))

source_summary
