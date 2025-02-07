### Running the code in xgboost



######################################
###########DOP#######################
#######################################
### Applyting XGBoost on EVI DOP dataset
EVIallRicefieldssummarised<-read.csv("EVIallRicefieldssummarised.csv")
data = EVIallRicefieldssummarised[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meanDOP","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]

# Split the dataset into training and testing sets
library(xgboost)
set.seed(123)
trainIndex <- sample.split(data, SplitRatio = 0.8)
train <- data[trainIndex, ]
test <- data[!trainIndex, ]

setting <- list(max_depth = 3, eta = 0.1, nthread = 8) 
#Here, max_depth: tree complexity, eta: learning rate, nthread: number of processors
fit.boost.1 = xgboost(data = as.matrix(train[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]), 
                      label = train$meanDOP,
                      nrounds = 100,
                      params = setting)  # nrounds: number of trees



###########################################
##Training
############################################
dtrain<-train[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]
# Convert the dataframe to an XGBoost DMatrix object
dtrainmatrix <- data.matrix(dtrain)
p1<-predict(fit.boost.1, newdata = dtrainmatrix)

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
    title = "Training set XGBoost",
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


# Calculate the feature importances
# Load the model
model <- readRDS("fit.boost.1")
importances <- xgb.importance(fit.boost.1)
# Compute feature importance matrix
importance_matrix = xgb.importance(colnames(dtrainmatrix), model = fit.boost.1)
importance_matrix
# Print the feature importances
print(importances)

plot(importance_matrix$Gain, importance_matrix$Frequency)

###########################################
##Testing
############################################
dtest<-test[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]
# Convert the dataframe to an XGBoost DMatrix object
dtestmatrix <- data.matrix(dtest)
p1<-predict(fit.boost.1, dtestmatrix)

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
    title = "Testing set XGBoost",
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
library(xgboost)
set.seed(123)
trainIndex <- sample.split(data, SplitRatio = 0.8)
train <- data[trainIndex, ]
test <- data[!trainIndex, ]

setting <- list(max_depth = 3, eta = 0.1, nthread = 8) 
#Here, max_depth: tree complexity, eta: learning rate, nthread: number of processors
fit.boost.1 = xgboost(data = as.matrix(train[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]), 
                      label = train$meanDOH,
                      nrounds = 100,
                      params = setting)  # nrounds: number of trees



###########################################
##Training
############################################
dtrain<-train[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]
# Convert the dataframe to an XGBoost DMatrix object
dtrainmatrix <- data.matrix(dtrain)
p1<-predict(fit.boost.1, newdata = dtrainmatrix)

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
    title = "Training set XGBoost",
    x = "Observed Day of Harvest (DOY)",
    y = "Predicted Day of Harvest (DOY)",
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


# Calculate the feature importances
# Load the model
model <- readRDS("fit.boost.1")
importances <- xgb.importance(fit.boost.1)
# Compute feature importance matrix
importance_matrix = xgb.importance(colnames(dtrainmatrix), model = fit.boost.1)
importance_matrix
ggplot(data.frame(importance_matrix), aes(x = Feature, y = Gain )) +
  geom_bar(stat = "identity") +
  labs(
    title = "Feature Importances XGBoost",
    x = "Feature",
    y = "Importance"
  )

# Print the feature importances
print(importances)

plot(importance_matrix$Gain, importance_matrix$Frequency)

###########################################
##Testing
############################################
dtest<-test[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]
# Convert the dataframe to an XGBoost DMatrix object
dtestmatrix <- data.matrix(dtest)
p1<-predict(fit.boost.1, dtestmatrix)

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
    title = "Testing set XGBoost",
    x = "Observed Day of Harvest (DOY)",
    y = "Predicted Day of Harvest (DOY)",
    caption = paste0("R2: ", R2, ", MAE: ", MAE)
  )



### Hyperparameter tuning
# Space-Filling Design
d = 6
B.range = seq(5,300,20)
lambda.range = seq(0.001,0.1,0.002)

# generate a space-filling design with 30 points
library(MaxPro)
Design = MaxProLHD(n = 40, p = 2)$Design # n: number of points, p: number of factors
Design2 = MaxPro(Design)$Design
plot(Design2[,c(1,2)], xlab="B", ylab="l")

B.points = (max(B.range)-min(B.range))*Design2[,1]+min(B.range)
B.points = round(B.points)
lambda.points = (max(lambda.range)-min(lambda.range))*Design2[,2]+min(lambda.range)
lambda.points = round(lambda.points, 3)

# take a look at the selected points:
# plot the mesh grid, if you'd like to see how it looks:
plot( rep(B.range, each=length(lambda.range)),  
      rep(lambda.range, length(B.range)),
      xlab = "B",ylab="lambda",pch=20)
points(B.points, lambda.points, col="red",cex=2)


set.seed(10)
case.select = sample(1:456,350)
data.train = dtrain
data.test = dtest
obs = test$meanDOH # get the actual observed o3 concentration in the testing data set

# evaluate the rmse
rmse = array(0/0, dim=c(length(B.points), 1)) # an array that contains the model performance
for (i in 1:length(B.points)){
  B = B.points[i]
  lambda = lambda.points[i]	
  
  setting <- list(max_depth = d, eta = lambda, nthread = 8) 
  #Here, max_depth: tree complexity, eta: learning rate, nthread: number of processors
  fit.boost = xgboost(data = as.matrix(train[,c("doyb","doya","meana","meanb","maxevi","meang1","meang2","meandoy", "meanDSWR", "meanTemp","meanLSWi", "meanTmax", "meanEVI_uncor")]), 
                      label = train$meanDOH,
                      nrounds = B,
                      params = setting, verbose=0)  # nrounds: number of trees
  
  pred <- predict(fit.boost, as.matrix(data.train))
  output = sqrt( mean((pred-obs)^2, na.rm=TRUE) )
  rmse[i]= output
  print(i)	
}


table = cbind(B.points, lambda.points, rmse)
table = data.frame(table)
colnames(table) = c("B","lambda","rmse")
print(table)

# you could ask R to rank the rows based on rmse (from the smallest to the largest)
table = table[order(table$rmse),]
# now, take a look at the first 10 choices:
head(table, 10)

# since the design space is two-dimensional mesh, you may see the entire response surface
library(rgl)
plot3d(as.matrix(table), col="red", type="s", size=1, axes=T)




loess.fit <- loess(rmse ~ B + lambda, table, span=0.5) #loess: Scatter-diagram smoothing
summary(loess.fit )
grid.mar <- list(B.range=B.range, lambda.range=lambda.range)
grid.mesh = expand.grid(grid.mar)
colnames(grid.mesh) = c("B","lambda")
# get the fitted (interpolated) values
rmse.interp <- predict(loess.fit , grid.mesh)
rmse.interp <- matrix(rmse.interp, length(B.range), length(lambda.range))
# plot the interpolated values as shaded rectangles and contours
library(latticeExtra)
library(RColorBrewer)
nclr <- 8
plotclr <- brewer.pal(nclr, "PuOr")
plotclr <- plotclr[nclr:1] # reorder colors
#plot(orotl.shp)
image(B.range,lambda.range, rmse.interp, col=plotclr)
contour(B.range,lambda.range,  rmse.interp, add=TRUE, nlevels = 50, col="white")
points(B.points, lambda.points, col="yellow")



