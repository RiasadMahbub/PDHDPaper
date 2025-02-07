data1 = read.csv("Test_Data.csv", stringsAsFactors=FALSE, header=TRUE)
library(ISLR)
data("Hitters")
head(Hitters)
# For decision tree model
library(rpart)
# For data visualization
library(rpart.plot)
# Contains the data
library(ISLR)
ncol(data1)
##Select the columns that we would like to use
data = data1[,c("o3","T","w_x","w_y","L1","L2","L3","L4","L5")]
load("data3.RData")# 
data = data3[,c("o3","T","w_x","w_y","L1","L2","L3","L4","L5")]

library(rpart)

cart.tree = rpart(MEDV~., 
                  data=data1, 
                  method="anova",cp=0.047)


plot(cart.tree, margin=0.04, uniform = TRUE)
text(cart.tree)
plotcp(cart.tree) 



data1 = read.csv("Test_Data.csv", stringsAsFactors=FALSE, header=TRUE)

cart.tree <- rpart(MEDV ~ ., data = data1, method = "anova", cp = 0.047)

library(sail)
set.seed(123)

library(caret)
folds <- createFolds(data1$MEDV, k = 3)
folds
mse <- c()
for (i in 1:3) {
  train <- data1[folds != i, ]
  test <- data1[folds == i, ]
  
  pruned.tree <- prune(cart.tree, cp = cart.tree$cptable[which.min(cart.tree$cptable[, "xerror"]), "CP"])

  
  predictions <- predict(pruned.tree, newdata = test)

  
  mse[i] <- mean((predictions - test$MEDV)^2)
}


print(mse)



library(rpart)

# Set the seed for reproducibility
set.seed(123)

# Split the data into 3 parts
folds <- sample(1:3, size = nrow(data1), replace = TRUE)
folds
# Initialize a vector to store the MSEs
mse <- rep(0, 3)

# Perform the cross-validation
for (i in 1:3) {
  # Split the data into training and testing sets
  test_indices <- which(folds == i)
  test_data <- data1[test_indices, ]
  train_data <- data1[-test_indices, ]
  
  # Fit the tree on the training data
  tree_fit <- rpart(MEDV ~ ., data = train_data, method = "anova", cp = 0.047)
  
  # Predict the target variable on the testing data
  predictions <- predict(tree_fit, test_data)
  
  # Calculate the MSE on the testing data
  mse[i] <- mean((test_data$MEDV - predictions)^2)
}

# Calculate the average MSE across all folds
avg_mse <- mean(mse)
mse




# Set the seed for reproducibility
set.seed(123)

# Name the folds as A, B, and C
fold_names <- c("A", "B", "C")
folds <- rep(fold_names, length.out = nrow(data1))

# Initialize a vector to store the MSEs
mse <- rep(0, 3)

# Perform the cross-validation
for (i in 1:3) {
  # Split the data into training and testing sets
  test_indices <- which(folds == fold_names[i])
  test_data <- data1[test_indices, ]
  train_data <- data1[-test_indices, ]
  
  # Fit the tree on the training data
  tree_fit <- rpart(MEDV ~ ., data = train_data, method = "anova", cp = 0.047)
  
  # Predict the target variable on the testing data
  predictions <- predict(tree_fit, test_data)
  
  # Calculate the MSE on the testing data
  mse[i] <- mean((test_data$MEDV - predictions)^2)
  
  # Print the MSE of the current fold
  cat("MSE for fold", fold_names[i], ":", round(mse[i], 4), "\n")
}

# Calculate the average MSE across all folds
avg_mse <- mean(mse)

# Print the average MSE across all folds
cat("Average MSE across all folds:", round(avg_mse, 4), "\n")
mse






# Set the seed for reproducibility
set.seed(123)

# Name the folds as A, B, and C
fold_names <- c("A", "B", "C")
folds <- rep(fold_names, length.out = nrow(data1))

# Initialize a vector to store the MSEs
mse <- rep(0, 3)

# Initialize a list to store the training and testing indices for each fold
fold_indices <- list()

# Perform the cross-validation
for (i in 1:3) {
  # Split the data into training and testing sets
  test_indices <- which(folds == fold_names[i])
  test_data <- data1[test_indices, ]
  train_data <- data1[-test_indices, ]
  
  # Rename the training and testing data sets
  train_name <- paste0("Train_", fold_names[i])
  test_name <- paste0("Test_", fold_names[i])
  assign(train_name, train_data)
  assign(test_name, test_data)
  
  # Fit the tree on the training data
  tree_fit <- rpart(MEDV ~ ., data = train_data, method = "anova", cp = 0.047)
  
  # Predict the target variable on the testing data
  predictions <- predict(tree_fit, test_data)
  
  # Calculate the MSE on the testing data
  mse[i] <- mean((test_data$MEDV - predictions)^2)
  
  # Store the training and testing indices for this fold
  fold_indices[[i]] <- list(train = which(folds != fold_names[i]), test = test_indices)
}

# Print out the fold indices for each fold
for (i in 1:3) {
  cat("Fold", fold_names[i], "was used for testing\n")
  cat("The model was trained on indices", fold_indices[[i]]$train, "\n")
  cat("The model was tested on indices", fold_indices[[i]]$test, "\n\n")
}
fold_indices
# Calculate the average MSE across all folds
avg_mse <- mean(mse)
cat("The average MSE across all folds was", round(avg_mse, 2))



#Here, max_depth: tree complexity, eta: learning rate, nthread: number of processors
fit = randomForest(MEDV~., data1, ntree=1000, do.trace=100, importance=TRUE)
plot(1:1000,fit$mse)
varImpPlot(fit)

fit

