
######################################
###########DOP#######################
#######################################
### Applyting XGBoost on EVI DOP dataset
EVIallRicefieldssummarised<-read.csv("EVIallRicefieldssummarised.csv")
data = EVIallRicefieldssummarised[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meanDOP","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]

# Split the dataset into training and testing sets
library(caTools)
#set.seed(123)
trainIndex <- sample.split(data, SplitRatio = 0.8)
train <- data[trainIndex, ]
test <- data[!trainIndex, ]


#Here, max_depth: tree complexity, eta: learning rate, nthread: number of processors
model <- lm(meanDOP ~ doyb + doya + meana + meanb + maxevi + meang1 + meang2 + meanDSWR + meanTemp + meanLSWi + meanTmax + meanEVI_uncor, data=train)
summary(model)
###########################################
##Training
############################################

predictions <- predict(model)

train <- cbind(train, predictions)
library(ggplot2)
# Calculate the R2 and MAE
R2 <- round(summary(lm(meanDOP ~ predictions, data = train))$r.squared,2)
MAE <- round(mean(abs(train$meanDOP - train$predictions)),2)

# Plot the observed and predicted values
ggplot(train, aes(x = meanDOP, y = predictions)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "MTM (MLR)Training set",
    x = "Observed Day of Planting (DOY)",
    y = "Predicted Day of Planting (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )




###########################################
##Testing
############################################

predictions <- predict(model, test)

test <- cbind(test, predictions)
library(ggplot2)
# Calculate the R2 and MAE
R2 <- round(summary(lm(meanDOP ~ predictions, data = test))$r.squared, 2)
MAE <- round(mean(abs(test$meanDOP - test$predictions)), 2)

# Plot the observed and predicted values
ggplot(test, aes(x = meanDOP, y = predictions)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "MTM (MLR)Testing set",
    x = "Observed Day of Planting (DOY)",
    y = "Predicted Day of Planting (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )


######################################
###########DOH#######################
#######################################
### Applyting XGBoost on EVI DOH dataset
EVIallRicefieldssummarised<-read.csv("EVIallRicefieldssummarised.csv")
data = EVIallRicefieldssummarised[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meanDOH","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]

# Split the dataset into training and testing sets
library(caTools)
#set.seed(123)
trainIndex <- sample.split(data, SplitRatio = 0.8)
train <- data[trainIndex, ]
test <- data[!trainIndex, ]


#Here, max_depth: tree complexity, eta: learning rate, nthread: number of processors
model <- lm(meanDOH ~ doyb + doya + meana + meanb + maxevi + meang1 + meang2 + meanDSWR + meanTemp + meanLSWi + meanTmax + meanEVI_uncor, data=train)
summary(model)
###########################################
##Training
############################################

predictions <- predict(model)

train <- cbind(train, predictions)
library(ggplot2)
# Calculate the R2 and MAE
R2 <- round(summary(lm(meanDOH ~ predictions, data = train))$r.squared,2)
MAE <- round(mean(abs(train$meanDOH - train$predictions)),2)

# Plot the observed and predicted values
ggplot(train, aes(x = meanDOH, y = predictions)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "MTM (MLR)Training set",
    x = "Observed Day of Harvest (DOY)",
    y = "Predicted Day of Harvest (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )




###########################################
##Testing
############################################

predictions <- predict(model, test)

test <- cbind(test, predictions)
library(ggplot2)
# Calculate the R2 and MAE
R2 <- round(summary(lm(meanDOH ~ predictions, data = test))$r.squared, 2)
MAE <- round(mean(abs(test$meanDOH - test$predictions)), 2)

# Plot the observed and predicted values
ggplot(test, aes(x = meanDOH, y = predictions)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "MTM (MLR)Testing set",
    x = "Observed Day of Harvesting (DOY)",
    y = "Predicted Day of Harvesting (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )

