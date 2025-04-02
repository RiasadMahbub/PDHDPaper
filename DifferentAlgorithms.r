# Load required libraries
library(ggplot2)
library(dplyr)
library(xgboost)

# Define logistic functions for spring (S) and fall (F) green-up
spring_logistic <- function(t, a1, a2, a3) {
  return(a1 / (1 + exp(-a2 * (t - a3))))
}

fall_logistic <- function(t, a5, a6, a7) {
  return(a5 / (1 + exp(-a6 * (t - a7))))
}

# Define Gaussian function for potential cover crop growth
cover_crop_growth <- function(t, a9, a10, a11) {
  return(a9 * exp(-((t - a10)^2) / (2 * a11^2)))
}

# Generate a sequence of days in a year
t_days <- 1:365

# Example parameters (these should be derived from data)
a1 <- 0.6  # Spring min green
a2 <- 0.1  # Spring green-up rate
a3 <- 100  # Spring inflection date
a5 <- 0.4  # Fall min green
a6 <- 0.1  # Fall decline rate
a7 <- 280  # Fall inflection date
a9 <- 0.8  # Peak cover crop greenness
a10 <- 150 # Peak greenness day
a11 <- 30  # Standard deviation

# Compute NDVI values using logistic and Gaussian functions
ndvi_spring <- spring_logistic(t_days, a1, a2, a3)
ndvi_fall <- fall_logistic(t_days, a5, a6, a7)
ndvi_cover_crop <- cover_crop_growth(t_days, a9, a10, a11)

# Final NDVI model
ndvi_final <- ifelse(t_days <= a3, ndvi_spring + ndvi_cover_crop, ndvi_fall)

# Plot the NDVI curve
ggplot(data.frame(Day = t_days, NDVI = ndvi_final), aes(x = Day, y = NDVI)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "NDVI Seasonal Curve with Cover Crop Growth",
       x = "Day of Year", y = "NDVI") +
  theme_minimal()

# Compute RGDD: Accumulate Growing Degree Days (GDD)
compute_gdd <- function(temp_max, temp_min, base_temp = 10) {
  gdd <- (temp_max + temp_min) / 2 - base_temp
  return(ifelse(gdd > 0, gdd, 0))  # Ensure no negative GDD
}

# Example temperature data (from PRISM)
set.seed(42)
temp_max <- runif(365, min = 10, max = 30)
temp_min <- runif(365, min = 5, max = 20)
gdd_values <- compute_gdd(temp_max, temp_min)

# Compute cumulative GDD until spring inflection date
rgdd <- sum(gdd_values[1:a3])

# Print RGDD result
print(paste("RGDD until spring inflection date:", round(rgdd, 2)))

# Simulate dataset for XGBoost model
set.seed(42)
data_train <- data.frame(
  NDVI_inflection = runif(100, 0.2, 0.6),
  Precipitation = runif(100, 50, 200),
  VPD = runif(100, 0.5, 3.0),
  Soil_Clay = runif(100, 10, 40),
  Soil_Silt = runif(100, 10, 40),
  Soil_SOC = runif(100, 1, 5),
  RGDD = runif(100, 500, 1500)  # Target variable
)

# Split into training and testing (80% train, 20% test)
train_indices <- sample(1:nrow(data_train), 0.8 * nrow(data_train))
train_data <- data_train[train_indices, ]
test_data <- data_train[-train_indices, ]

# Prepare XGBoost matrices
dtrain <- xgb.DMatrix(data = as.matrix(train_data[, -7]), label = train_data$RGDD)
dtest <- xgb.DMatrix(data = as.matrix(test_data[, -7]), label = test_data$RGDD)

# Train XGBoost model
params <- list(objective = "reg:squarederror", eta = 0.1, max_depth = 6)
xgb_model <- xgb.train(params, dtrain, nrounds = 100)

# Predict RGDD on test data
preds <- predict(xgb_model, dtest)

# Compute RMSE
rmse <- sqrt(mean((test_data$RGDD - preds)^2))
print(paste("XGBoost model RMSE:", round(rmse, 2)))
