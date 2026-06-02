#### random forest


# RF
library(randomForest) # this is the Random Forests package we need


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
fit = randomForest(meanDOP~., data, ntree=1000, do.trace=100, importance=TRUE)
plot(1:1000,fit$mse)
varImpPlot(fit, title("Random Forest Feature Importance"))
###########################################
##Training
############################################

p1<-predict(fit, newdata = train)

train <- cbind(train, p1)
library(ggplot2)
# Calculate the R2 and MAE
R2 <- round(summary(lm(meanDOP ~ p1, data = train))$r.squared,2)
MAE <- round(mean(abs(train$meanDOP - train$p1)),2)

# Plot the observed and predicted values
ggplot(train, aes(x = meanDOP, y = p1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Training set Random Forest",
    x = "Observed Day of Planting (DOY)",
    y = "Predicted Day of Planting (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )




###########################################
##Testing
############################################

p1<-predict(fit, test)

test<- cbind(test, p1)
library(ggplot2)
# Calculate the R2 and MAE
R2 <- round(summary(lm(meanDOP ~ p1, data = test))$r.squared, 2)
MAE <- round(mean(abs(test$meanDOP - test$p1)), 2)

# Plot the observed and predicted values
ggplot(test, aes(x = meanDOP, y = p1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Testing set Random Forest",
    x = "Observed Day of Planting (DOY)",
    y = "Predicted Day of Planting (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )




######################################
###########DOH#######################
#######################################
### Applyting RANDOM Forest on EVI DOH dataset
library(randomForest) # this is the Random Forests package we need


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
fit = randomForest(meanDOH~., data, ntree=1000, do.trace=100, importance=TRUE)
plot(1:1000,fit$mse)
varImpPlot(fit)
###########################################
##Training
############################################

p1<-predict(fit, newdata = train)

train <- cbind(train, p1)
library(ggplot2)
# Calculate the R2 and MAE
R2 <- round(summary(lm(meanDOH ~ p1, data = train))$r.squared,2)
MAE <- round(mean(abs(train$meanDOH - train$p1)),2)

# Plot the observed and predicted values
ggplot(train, aes(x = meanDOH, y = p1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Training set Random Forest",
    x = "Observed Day of Harvest (DOY)",
    y = "Predicted Day of Harvest (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )




###########################################
##Testing
############################################

p1<-predict(fit, test)

test<- cbind(test, p1)
library(ggplot2)
# Calculate the R2 and MAE
R2 <- round(summary(lm(meanDOH ~ p1, data = test))$r.squared, 2)
MAE <- round(mean(abs(test$meanDOH - test$p1)), 2)

# Plot the observed and predicted values
ggplot(test, aes(x = meanDOH, y = p1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Testing set Random Forest",
    x = "Observed Day of Harvest (DOY)",
    y = "Predicted Day of Harvest (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )

