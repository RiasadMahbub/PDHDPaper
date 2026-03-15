### These code are copied from Xiao Liu's class and lectures

## date; 4-23-2023

library(ISLR)
data("Hitters")
head(Hitters)
# For decision tree model
library(rpart)
# For data visualization
library(rpart.plot)
# Contains the data
library(ISLR)


##A regression tree for predicting the log-salary of a baseball players based on years and hits

## Exercise question
##Load the data using
load("data3.RData")


##Select the columns that we would like to use
data = data3[,c("o3","T","w_x","w_y","L1","L2","L3","L4","L5")]
load("data3.RData")# 
data = data3[,c("o3","T","w_x","w_y","L1","L2","L3","L4","L5")]

library(rpart)

cart.tree = rpart(o3~., 
                  data=data, 
                  method="anova",cp=0.0005) # contruct a decision tree
plot(cart.tree, margin=0.04, uniform = TRUE)
text(cart.tree)

plotcp(cart.tree) 

### Applyting CART on EVI DOP dataset

######################################
###########DOP#######################
#######################################
### Applyting CART on EVI DOP dataset
EVIallRicefieldssummarised<-read.csv("EVIallRicefieldssummarised.csv")
data = EVIallRicefieldssummarised[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meanDOP","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]

# Split the dataset into training and testing sets
library(caTools)
set.seed(123)
trainIndex <- sample.split(data, SplitRatio = 0.8)
train <- data[trainIndex, ]
test <- data[!trainIndex, ]

cart.tree = rpart(meanDOP~., 
                  data=train, 
                  method="anova",cp=0.005) # contruct a decision tree

plot(cart.tree, margin=0.092, uniform = TRUE)
text(cart.tree)
plotcp(cart.tree) 
rpart.plot(cart.tree)

###########################################
##Training
############################################
p1<-predict(cart.tree, train)

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
    title = "Training set CART",
    x = "Observed Day of Planting (DOY)",
    y = "Predicted Day of Planting (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )


feature.importances <- cart.tree$variable.importance
ggplot(data.frame(feature = names(feature.importances), importance = feature.importances), aes(x = feature, y = importance)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Feature Importances",
    x = "Feature",
    y = "Importance"
  )

###########################################
##Testing
############################################
p1<-predict(cart.tree, test)

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
    title = "Testing set CART",
    x = "Observed Day of Planting (DOY)",
    y = "Predicted Day of Planting (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )




######################################
###########DOH#######################
#######################################
### Applyting CART on EVI DOH dataset
EVIallRicefieldssummarised<-read.csv("EVIallRicefieldssummarised.csv")
data = EVIallRicefieldssummarised[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meanDOH","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]

# Split the dataset into training and testing sets
library(caTools)
set.seed(123)
trainIndex <- sample.split(data, SplitRatio = 0.8)
train <- data[trainIndex, ]
test <- data[!trainIndex, ]

cart.tree = rpart(meanDOH~., 
                  data=train, 
                  method="anova",cp=0.005) # contruct a decision tree

plot(cart.tree, margin=0.092, uniform = TRUE)
text(cart.tree)
plotcp(cart.tree) 
rpart.plot(cart.tree)

###########################################
##Training
############################################
p1<-predict(cart.tree, train)

train <- cbind(train, p1)
library(ggplot2)
# Calculate the R2 and MAE
R2 <- round(summary(lm(meanDOH ~ p1, data = train))$r.squared, 2)
MAE <- round(mean(abs(train$meanDOH - train$p1)),2)

# Plot the observed and predicted values
ggplot(train, aes(x = meanDOH, y = p1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Training set CART",
    x = "Observed Day of Harvest (DOY)",
    y = "Predicted Day of Harvest (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )


feature.importances <- cart.tree$variable.importance
ggplot(data.frame(feature = names(feature.importances), importance = feature.importances), aes(x = feature, y = importance)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Feature Importances CART",
    x = "Feature",
    y = "Importance"
  )

###########################################
##Testing
############################################
p1<-predict(cart.tree, test)

test<- cbind(test, p1)
library(ggplot2)
# Calculate the R2 and MAE
R2 <- round(summary(lm(meanDOH ~ p1, data = test))$r.squared,2 )
MAE <- round(mean(abs(test$meanDOH - test$p1)),2)

# Plot the observed and predicted values
ggplot(test, aes(x = meanDOH, y = p1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Testing set CART",
    x = "Observed Day of Harvest (DOY)",
    y = "Predicted Day of Harvest (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )


