combined_df_cleaned <- combined_df %>%
  mutate(Field_ID = str_remove(Field_ID, "_VI"))

# Method 2: Create a new column for joining (if you want to keep the original Field_ID)
# combined_df_for_join <- combined_df %>%
#  mutate(Join_ID = str_remove(Field_ID, "_VI"))

# Now, perform the join using the cleaned Field_ID column.
# The 'by' argument specifies the columns to join on.
# We join 'sos_eos_with_lag' with the newly cleaned 'combined_df_cleaned'.
# An 'inner_join' will keep only the rows that have a match in both data frames.

joined_data <- sos_eos_with_lag %>%
  inner_join(combined_df_cleaned, by = c("Field_Year" = "Field_ID"))


# 1. Filter valid rows (no NA in response or predictors)
df <- joined_data %>%
  dplyr::select(
    a1, b1, a2, b2, DOY_max_fit, DOY_max_obs, Value_max_obs, Value_max_fit, 
    mean_PDDOY,
    #a, b, c,d,e,f,
    #Lag_Days, 
    SOS,lagobserved,
    meandayl_M2, meandayl_M3, meandayl_M4, meandayl_M5,
    #meandayl_M6,
    #mean_Es_M4, mean_Es_M5, mean_Es_M6,
    mean_gdd_M3, mean_gdd_M4, mean_gdd_M5, #mean_gdd_M6, 
    #mean_gdd_M7,
    meansrad_M2,meansrad_M3, meansrad_M4, 
    #meansrad_M5, meansrad_M6, meansrad_M7,
    #meanRH_M6,meanRH_M7,
    mean_tmean_M2, mean_tmean_M3,mean_tmean_M4, 
    #mean_tmean_M5, mean_tmean_M6, 
    #mean_tmean_M7,
    mean_tmin_M2, mean_tmin_M3, mean_tmin_M4, mean_tmin_M5, mean_tmin_M6,
    #mean_tmin_M7,
    mean_tmax_M3, mean_tmax_M4, mean_tmax_M5, mean_tmax_M6,
    #mean_tmax_M7, 
    meanNDWI_M2, meanNDWI_M3, meanNDWI_M4, meanNDWI_M5, 
    #meanNDWI_M6,meanNDWI_M7,
    #meanMLSWI26_M3, meanMLSWI26_M4, meanMLSWI26_M5, meanMLSWI26_M6,meanMLSWI26_M7,
    #meanRNDVI_M2, meanRNDVI_M3, meanRNDVI_M4, 
    #meanRNDVI_M5, 
    #meanATSAVI_M2, meanATSAVI_M3, meanATSAVI_M3, meanATSAVI_M4, meanATSAVI_M5, 
    #meanRNDVI_M6, meanRNDVI_M7, 
    # meanLai_M2, meanLai_M3, meanLai_M4,
    #meanLai_M5, meanLai_M6,meanLai_M7,
    #meanIAVI_M3,meanIAVI_M4, meanIAVI_M5, meanIAVI_M6,
    #meanGDVI_M3, meanGDVI_M4, meanGDVI_M5, meanGDVI_M6, meanGDVI_M7
    #meanVARI_M3, meanVARI_M4, meanVARI_M5, meanVARI_M6, meanVARI_M7
  ) %>%  # drop Field_ID for modeling
  dplyr::filter(!is.na(lagobserved))%>%
  drop_na()  # This removes rows with NA in any column 

# 2. Create train-test split (80:20)
set.seed(42)
n <- nrow(df)
train_idx <- sample(1:n, size = 0.8 * n)
train <- df[train_idx, ]
test <- df[-train_idx, ]

# 3. Train random forest model
rf_model <- randomForest(lagobserved ~ ., data = train,
                         ntree = 100, importance = TRUE, do.trace = 10)
rf_model <- randomForest(lagobserved ~ . - mean_PDDOY, 
                         data = train,
                         ntree = 100, importance = TRUE, do.trace = 10)
# 4. Predict on train and test
train$pred_PDDOY <- predict(rf_model, newdata = train)
test$pred_PDDOY <- predict(rf_model, newdata = test)

# 5. Evaluate performance
train_r2 <- summary(lm(lagobserved ~ pred_PDDOY, data = train))$r.squared
train_mae <- mae(train$lagobserved, train$pred_PDDOY)

test_r2 <- summary(lm(lagobserved ~ pred_PDDOY, data = test))$r.squared
test_mae <- mae(test$lagobserved, test$pred_PDDOY)

# 6. Plot predictions (Train)
ggplot(train, aes(x = lagobserved, y = pred_PDDOY)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Training Set Performance",
       x = "Observed PDDOY", y = "Predicted PDDOY") +
  annotate("text", x = min(train$mean_PDDOY), y = max(train$pred_PDDOY),
           label = paste0("R² = ", round(train_r2, 2), 
                          "\nMAE = ", round(train_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")


# 7. Plot predictions (Test)
ggplot(test, aes(x = lagobserved, y = pred_PDDOY)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Testing Set Performance",
       x = "Observed PDDOY", y = "Predicted PDDOY") +
  annotate("text", x = min(test$mean_PDDOY), y = max(test$pred_PDDOY),
           label = paste0("R² = ", round(test_r2, 2), 
                          "\nMAE = ", round(test_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")
test$predPDOYsos<-test$SOS- test$pred_PDDOY
test_r2 <- summary(lm(mean_PDDOY ~ predPDOYsos, data = test))$r.squared
test_mae <- mae(test$mean_PDDOY, test$predPDOYsos)
ggplot(test, aes(x = mean_PDDOY, y = predPDOYsos)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Testing Set Performance",
       x = "Observed PDDOY", y = "Predicted PDDOY") +
  annotate("text", x = min(test$mean_PDDOY), y = max(test$pred_PDDOY),
           label = paste0("R² = ", round(test_r2, 2), 
                          "\nMAE = ", round(test_mae, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")
# 8. Variable importance
varImpPlot(rf_model, main = "Random Forest Variable Importance")
