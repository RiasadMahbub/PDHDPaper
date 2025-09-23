library(ggplot2)
library(randomForest)
library(Metrics)
library(dplyr)
library(ggplot2)
library(caret)

deinesharmonicminmaxdf
# Rename columns for ease of use
df <- phenology_df

meteo_summary_list <- lapply(seq_along(merged_list), function(i) {
  df <- merged_list[[i]]
  field_id <- gsub("\\.csv$", "", basename(vi_csv_files_gt20[i]))
  
  df$Date <- as.Date(df$Date)
  df$Month <- month(df$Date)
  df$DOY <- yday(df$Date) # Add DOY column
  
  # Find the DOY when ATSAVI crosses the 0.50 threshold
  atsavi_threshold_doy <- df %>%
    dplyr::filter(ATSAVI >= 0.50) %>%
    dplyr::summarise(ATSAVI_0.50_DOY = min(DOY, na.rm = TRUE)) %>%
    pull(ATSAVI_0.50_DOY)
  
  # If no DOY crosses the threshold, set to NA
  if (is.infinite(atsavi_threshold_doy)) {
    atsavi_threshold_doy <- NA
  }
  
  df_aprjun <- df %>% dplyr::filter(Month %in% 2:9)
  
  # Group by Month and calculate monthly means
  df_monthly <- df_aprjun %>%
    group_by(Month) %>%
    dplyr::summarise(
      mean_tmean = mean(tmean, na.rm = TRUE),
      sum_ppt    = sum(ppt, na.rm = TRUE),
      mean_tmin  = mean(tmin, na.rm = TRUE),
      mean_tmax  = mean(tmax, na.rm = TRUE),
      mean_vpd   = mean(vpd, na.rm = TRUE),
      #mean_Es    = mean(Es, na.rm = TRUE),
      mean_gdd = mean(gdd, na.rm = TRUE),
      #mean_soiltemp = mean (SoilTMP0_10cm_inst, na.rm = TRUE),
      meansrad    = mean(srad, na.rm = TRUE),
      #meandayl    = sum(dayl, na.rm = TRUE),
      meanRH = mean(avgRH, na.rm = TRUE),
      meanNDWI = mean(NDWI, na.rm = TRUE),
      meanLai = mean(Lai, na.rm = TRUE),
      meanIAVI = mean(IAVI, na.rm = TRUE),
      meanGDVI = mean(GDVI, na.rm = TRUE),
      meanVARI = mean(VARI, na.rm = TRUE),
      meanRNDVI = mean(RNDVI, na.rm = TRUE),
      meanMLSWI26 = mean(MLSWI26, na.rm = TRUE),
      # Removed meanATSAVI as requested
      .groups = "drop"
    )
  # Pivot wider to create column per metric per month
  df_summary <- df_monthly %>%
    pivot_wider(
      names_from = Month,
      values_from = c(mean_tmean, sum_ppt, mean_vpd, mean_gdd, meanRH, mean_tmin,
                      mean_tmax, meanNDWI, meanLai, meanIAVI, meanGDVI, meanVARI,
                      meanRNDVI, meanMLSWI26, meansrad#, meandayl
      ), # Removed meanATSAVI
      names_glue = "{.value}_M{Month}"
    ) %>%
    dplyr::mutate(
      Field_ID = field_id,
      ATSAVI_0.50_DOY = atsavi_threshold_doy # Add the new metric here
    ) %>%
    dplyr::select(Field_ID, ATSAVI_0.50_DOY, everything()) %>% # Reorder to place new metric near Field_ID
    as_tibble()
  
  return(df_summary)
})


meteo_summary_df <- bind_rows(meteo_summary_list)
meteo_summary_df <- meteo_summary_df %>%
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
  )            # drop the .y column

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

# Calculate lags and GSL
df$lagtrs <- df$SOS_trs.sos - df$PDDOY
df$lagder <- df$SOS_deriv.sos - df$PDDOY
df$GSL <- df$HDDOY - df$PDDOY
df <- df[df$Field_Year != "F_8252_7_HF_2015", ]
# Calculate R (correlation) values
r_trs <- cor(df$lagtrs, df$GSL, use = "complete.obs")
r_der <- cor(df$lagder, df$GSL, use = "complete.obs")
sort(df$lagtrs, na.last = NA)[1:10]
sort(df$lagder, na.last = NA)[1:10]
# Row(s) with minimum lagtrs value (excluding NA)
min_lagtrs_rows <- df[df$lagtrs == min(df$lagtrs, na.rm = TRUE), ]
print("Rows with minimum lagtrs:")
print(min_lagtrs_rows$Field_Year)

# Row(s) with minimum lagder value (excluding NA)
min_lagder_rows <- df[df$lagder == min(df$lagder, na.rm = TRUE), ]
print("Rows with minimum lagder:")
print(min_lagder_rows$Field_Year)

df$Field_Year[df$Greenup.Greenup < 70]
# Get unique Field_Year names where Greenup.Greenup < 80 (remove NAs)
fields_to_plot <- na.omit(unique(df$Field_Year[df$Greenup.Greenup < 80]))

# Loop through each field and plot DOY vs kNDVI
for (field in fields_to_plot) {
  if (!is.null(vi_list_gt20[[field]])) {  # Check that field exists in list
    plot(vi_list_gt20[[field]]$DOY, 
         vi_list_gt20[[field]]$kNDVI,
         type = "l", col = "blue", lwd = 2,
         main = field,
         xlab = "DOY", ylab = "kNDVI")
  }
}



# Plot 1: GSL vs lagtrs
p1 <- ggplot(df, aes(x = lagtrs, y = GSL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.8, size = 4,
           label = sprintf("R = %.2f", r_trs)) +
  labs(
    title = "GSL vs lag (trs method)",
    x = "lag (trs)",
    y = "GSL"
  )

# Plot 2: GSL vs lagder
p2 <- ggplot(df, aes(x = lagder, y = GSL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.8, size = 4,
           label = sprintf("R = %.2f", r_der)) +
  labs(
    title = "GSL vs lag (deriv method)",
    x = "lag (deriv)",
    y = "GSL"
  )

p1
ggplot(df, aes(df$Laglocalminglomax, y = GSL)) +
  geom_point()


# Compute lag
df$lagtrs <- df$SOS_trs.sos - df$PDDOY
df$lagtrsupdate <- df$SOS_trs.sos - df$UD.UD
df$lagtrsgreenup <- df$SOS_trs.sos - df$Greenup.Greenup
df$lagtrsminfit <- df$SOS_trs.sos - df$DOY_min_fit
# Step 2: Select features and response
# Your top variables list
# Your top variables list
top30_vars <- c(
  "DOY_max_fit", "RD.RD", "a1", "SOS_deriv.sos", "avgsoilclay", "lagtrsgreenup", "POS.pos",
  "avgsoilorg", "DOY_min_fit", "EOS_trs.eos", "EOS_deriv.eos", "a3.a3",
  "lagtrsupdate", "b1", "SOS_trs.sos", "a2",  "DD.DD",
  "UD.UD", "rsp.rsp", "DOY_max_before_min_fit",
  "Greenup.Greenup", "mx.mx", "cum_tmax", "cum_tmin", "cum_gdd", "cum_soiltemp",
"cum_meansrad","mean_ExG" ,"mean_MCARI1"  ,"mean_MCARI2"  ,"mean_MTVI1" ,  "mean_OSAVI " ,
  "mean_TGI" ,    "mean_AWEInsh", 
  "Laglocalmaxlocalmin", 'Value_max_obs' 
)


# Function to build dataset with top N vars + lagtrs
make_model_df <- function(df, top_vars, N) {
  df %>%
    dplyr::select(all_of(top_vars[1:N]), lagtrs, PDDOY) %>%
    drop_na()
}

# Create the four datasets
df_rf_top5  <- make_model_df(df, top30_vars, 5)   # 5 + lagtrs = 6 predictors
df_rf_top10 <- make_model_df(df, top30_vars, 10)  # 10 + lagtrs = 11 predictors
df_rf_top15 <- make_model_df(df, top30_vars, 15)  # 15 + lagtrs = 16 predictors
df_rf_top20 <- make_model_df(df, top30_vars, 20)  # 20 + lagtrs = 21 predictors
df_rf_top25 <- make_model_df(df, top30_vars, 25)  # 20 + lagtrs = 21 predictors
nrow(df)
df_rf_top30 <- make_model_df(df, top30_vars, 29)  # 20 + lagtrs = 21 predictors
df_vif_rf <- df %>%
  select(
    a1, Greenup.Greenup, DOY_min_fit, SOS_trs.sos, SOS_deriv.sos, a2, 
    RD.RD, EOS_trs.eos, DOY_max_before_min_fit, DD.DD, b1, mx.mx, EOS_deriv.eos,
     rsp.rsp, avgsoilclay, avgsoilorg, POS.pos, cum_tmin, lagtrs, PDDOY,
    cum_soiltemp,  cum_meansrad, #cumGDVI, 
    a3.a3, Value_max_obs,  mean_ExG ,mean_MCARI1  ,mean_MCARI2  ,mean_MTVI1 ,  mean_OSAVI  ,
    mean_TGI ,     mean_AWEInsh 
  ) %>%
  dplyr::filter(!is.na(lagtrs)) %>%
  drop_na()
nrow(df_rf_top30)
# Suppose df_list contains your data frames
sum(is.na(df$lagtrs))

# Step 3: Train-test split
set.seed(25)
n <- nrow(df_rf_top30)
n
train_idx <- sample(1:n, size = 0.8 * n)
train <- df_rf_top30[train_idx, ]
test <- df_rf_top30[-train_idx, ]

# Step 4: Train random forest on lagtrs
rf_model <- randomForest(lagtrs ~ . -PDDOY, data = train,
                         ntree = 100, importance = TRUE,  do.trace = 10)

# 4. Predict
train$pred_lag <- predict(rf_model, newdata = train)
test$pred_lag <- predict(rf_model, newdata = test)

# 5. Evaluate
train_r2 <- summary(lm(lagtrs ~ pred_lag, data = train))$r.squared
train_mae <- mae(train$lagtrs, train$pred_lag)
test_r2 <- summary(lm(lagtrs ~ pred_lag, data = test))$r.squared
test_mae <- mae(test$lagtrs, test$pred_lag)

# 6. Plot: Train
ggplot(train, aes(x = lagtrs, y = pred_lag)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Training: Predicting lagtrs",
       x = "Observed lag (trs)", y = "Predicted lag (trs)") +
  annotate("text", x = min(train$lagtrs), y = max(train$pred_lag),
           label = paste0("R² = ", round(train_r2, 2), 
                          "\nMAE = ", round(train_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

# 7. Plot: Test
ggplot(test, aes(x = lagtrs, y = pred_lag)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Testing: Predicting lagtrs",
       x = "Observed lag (trs)", y = "Predicted lag (trs)") +
  annotate("text", x = min(test$lagtrs), y = max(test$pred_lag),
           label = paste0("R² = ", round(test_r2, 2), 
                          "\nMAE = ", round(test_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

# 8. Variable importance
varImpPlot(rf_model, main = "Variable Importance for lagtrs")


# ------------------------------
# 5. Recursive Feature Elimination (RFE)
# ------------------------------
library(randomForest)
library(caret)

# Train random forest
set.seed(42)
rf_model <- randomForest(lagtrs ~ ., data = df_predictors, importance = TRUE)

# Get variable importance
var_imp <- importance(rf_model)

# Convert to data frame and sort by importance
var_imp_df <- data.frame(
  Variable = rownames(var_imp),
  Importance = var_imp[, 1]  # %IncMSE
) %>%
  arrange(desc(Importance))

# Get top 30 variables
top30_vars <- head(var_imp_df$Variable, 30)
print(top30_vars)



# Step 6: Reconstruct predicted PDDOY using SOS_trs.sos - predicted lag
train$pred_PDDOY <- train$SOS_trs.sos - train$pred_lag
test$pred_PDDOY <- test$SOS_trs.sos - test$pred_lag

# Step 7: Metrics
train_r2 <- summary(lm(PDDOY ~ pred_PDDOY, data = train))$r.squared
train_mae <- mae(train$PDDOY, train$pred_PDDOY)

test_r2 <- summary(lm(PDDOY ~ pred_PDDOY, data = test))$r.squared
test_mae <- mae(test$PDDOY, test$pred_PDDOY)

# Step 8: Plot for train set
ggplot(train, aes(x = PDDOY, y = pred_PDDOY)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Train Set: Predicted vs Observed PDDOY",
       x = "Observed PDDOY", y = "Predicted PDDOY") +
  annotate("text", x = min(train$PDDOY), y = max(train$pred_PDDOY),
           label = paste0("R² = ", round(train_r2, 2),
                          "\nMAE = ", round(train_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

# Step 9: Plot for test set
ggplot(test, aes(x = PDDOY, y = pred_PDDOY)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Test Set: Predicted vs Observed PDDOY",
       x = "Observed PDDOY", y = "Predicted PDDOY") +
  annotate("text", x = min(test$PDDOY), y = max(test$pred_PDDOY),
           label = paste0("R² = ", round(test_r2, 2),
                          "\nMAE = ", round(test_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")



# Remove PDDOY (response is lagtrs)
df_predictors <- df_rf %>% dplyr::select(-PDDOY, -lagtrs)
response <- df_rf$lagtrs

# Set seed for reproducibility
set.seed(42)

# Define control for rfe using random forest and 5-fold CV
control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

# Run recursive feature elimination
rfe_results <- rfe(
  x = df_predictors,
  y = response,
  sizes = c(5, 10, 15, 20, 25),  # adjust as you like
  rfeControl = control
)

# Show results
print(rfe_results)











#---------------------------------------------------
# Compute lagder
#---------------------------------------------------
df$lagder <- df$SOS_deriv.sos - df$PDDOY
df$lagupdate <- df$SOS_deriv.sos - df$UD.UD
df$laggreenup <- df$SOS_deriv.sos - df$Greenup.Greenup

# Step 2: Select features and response
df_rf_der <- df %>%
  dplyr::select(
    # --- Top 20 RFE variables ---
    a1, 
    #UD.UD, 
    DOY_min_fit, 
    Greenup.Greenup, 
    SOS_trs.sos, 
    a2, 
    Dormancy.Dormancy, 
    #RD.RD, 
    #EOS_trs.eos, 
    DOY_max_before_min_fit, 
    #DD.DD, 
    #b1, 
    #mx.mx, 
    #EOS_deriv.eos, 
    #SOS_deriv.sos, 
    lagtrsgreenup, 
    Senescence.Senescence, 
    #rsp.rsp, 
    lagtrsupdate, 
    #a3.a3,
    PDDOY,
    lagder,lagtrs,
    cum_gdd, cum_meansrad, cum_vpd, cum_tmin, cum_tmax, cum_RH,
    
    # --- Commented out variables ---
    # lagobserved,
    POS.pos,
    #cumGDVI,
    #Laglocalmaxglomax, Laglocalminglomax, Laglocalmaxlocalmin,
  ) %>%
  drop_na()

# Step 3: Train-test split
set.seed(21)
n <- nrow(df_rf_der)
train_idx <- sample(1:n, size = 0.8 * n)
train <- df_rf_der[train_idx, ]
test <- df_rf_der[-train_idx, ]

# Step 4: Train random forest on lagder
rf_model <- randomForest(lagder ~ . - PDDOY - lagtrs, 
                         data = train,
                         ntree = 100, 
                         importance = TRUE)


# Step 5: Predict lagder
train$pred_lag <- predict(rf_model, newdata = train)
test$pred_lag <- predict(rf_model, newdata = test)

# Step 6: Evaluate lag prediction
train_r2 <- summary(lm(lagder ~ pred_lag, data = train))$r.squared
train_mae <- mae(train$lagder, train$pred_lag)
test_r2 <- summary(lm(lagder ~ pred_lag, data = test))$r.squared
test_mae <- mae(test$lagder, test$pred_lag)

# Step 7: Plot lag prediction (train)
ggplot(train, aes(x = lagder, y = pred_lag)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Training: Predicting lagder",
       x = "Observed lag (der)", y = "Predicted lag (der)") +
  annotate("text", x = min(train$lagder), y = max(train$pred_lag),
           label = paste0("R² = ", round(train_r2, 2),
                          "\nMAE = ", round(train_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

# Step 8: Plot lag prediction (test)
ggplot(test, aes(x = lagder, y = pred_lag)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Testing: Predicting lagder",
       x = "Observed lag (der)", y = "Predicted lag (der)") +
  annotate("text", x = min(test$lagder), y = max(test$pred_lag),
           label = paste0("R² = ", round(test_r2, 2),
                          "\nMAE = ", round(test_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

# Step 9: Variable importance
varImpPlot(rf_model, main = "Variable Importance for lagder")

# Step 10: Reconstruct predicted PDDOY
train$pred_PDDOY <- train$SOS_deriv.sos - train$pred_lag
test$pred_PDDOY <- test$SOS_deriv.sos - test$pred_lag

# Step 11: Evaluate predicted PDDOY
train_r2 <- summary(lm(PDDOY ~ pred_PDDOY, data = train))$r.squared
train_mae <- mae(train$PDDOY, train$pred_PDDOY)

test_r2 <- summary(lm(PDDOY ~ pred_PDDOY, data = test))$r.squared
test_mae <- mae(test$PDDOY, test$pred_PDDOY)

# Step 12: Plot predicted vs observed PDDOY (train)
ggplot(train, aes(x = PDDOY, y = pred_PDDOY)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Train Set: Predicted vs Observed PDDOY (using SOS_deriv)",
       x = "Observed PDDOY", y = "Predicted PDDOY") +
  annotate("text", x = min(train$PDDOY), y = max(train$pred_PDDOY),
           label = paste0("R² = ", round(train_r2, 2),
                          "\nMAE = ", round(train_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

# Step 13: Plot predicted vs observed PDDOY (test)
ggplot(test, aes(x = PDDOY, y = pred_PDDOY)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Test Set: Predicted vs Observed PDDOY (using SOS_deriv)",
       x = "Observed PDDOY", y = "Predicted PDDOY") +
  annotate("text", x = min(test$PDDOY), y = max(test$pred_PDDOY),
           label = paste0("R² = ", round(test_r2, 2),
                          "\nMAE = ", round(test_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

plot(df$cum_RH, df$lagtrs)

merged_list$F_20581_68_MF_2015
merged_list$Judys_2022
merged_list$Seed_Rice_2022
merged_list$Walls_06_07_2022
merged_list$Walls_09_2021
# Access elements using $
merged_list$Cattlet_02_2020
merged_list$East_Joe_T_2022
merged_list$F_8252_7_HF_2015
merged_list$F_8319_65_5_2017
merged_list$F_8320_66_6_2017
merged_list$F_8320_66_6_2020

plot(merged_list$F_20578_65_Wg_N_2023$kNDVI)


# Assuming df is your dataframe and top30_vars contains column names
for (var in top30_vars) {
  if (var %in% names(df)) {  # Check if the column exists in df
    plot(df$lagtrs, df[[var]],
         xlab = "lagtrs",
         ylab = var,
         main = paste(var, "vs lagtrs"),
         pch = 19, col = "blue")
  } else {
    message(paste("Column", var, "not found in df"))
  }
}

