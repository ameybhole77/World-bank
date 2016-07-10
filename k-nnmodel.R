# Load packages
library(dplyr)
library(data.table)
library(ggplot2)

# Assign the training set
world_bank_train = read.csv("/media/amey/1E02DDE102DDBDC9/Amey/Work/Data A/Assignments/World Bank/world_bank_train.csv", sep="")

# Assign the test set
world_bank_test = read.csv("/media/amey/1E02DDE102DDBDC9/Amey/Work/Data A/Assignments/World Bank/world_bank_test.csv", sep="")

# k-nn model
my_knn <- function(x_pred, x, y, k){
  m <- length(x_pred)
  predict_knn <- rep(0, m)
  for (i in 1:m) {
    
    # Calculate the absolute distance between x_pred[i] and x
    dist <- abs(x_pred[i] - x)
    
    # Apply order() to dist, sort_index will contain 
    # the indices of elements in the dist vector, in 
    # ascending order. This means sort_index[1:k] will
    # return the indices of the k-nearest neighbors.
    sort_index <- order(dist)    
    
    # Apply mean() to the responses of the k-nearest neighbors
    predict_knn[i] <- mean(y[sort_index[1:k]])    
    
  }
  return(predict_knn)
}
###

# Applied alogrithm on the test set
test_output = my_knn(world_bank_test$cgdp ,world_bank_train$cgdp , world_bank_train$urb_pop , 30)

# Plot of the output
plot(world_bank_train, 
     xlab = "GDP per Capita", 
     ylab = "Percentage Urban Population")
points(world_bank_test$cgdp, test_output, col = "green")

# Define ranks to order the predictor variables in the test set
ranks <- order(world_bank_test$cgdp)

# Define ranks to order the predictor variables in the test set
ranks <- order(world_bank_test$cgdp)

# Scatter plot of test set
plot(world_bank_test, 
     xlab = "GDP per Capita", ylab = "Percentage Urban Population")

# Predict with k-NN and add line
test_output_knn <- my_knn(world_bank_test$cgdp, world_bank_train$cgdp, world_bank_train$urb_pop, 30)
lines(world_bank_test$cgdp[ranks], test_output_knn[ranks], lwd = 2, col = "green")

# Calculate RMSE for k-NN technique
sqrt(mean( (test_output_knn - world_bank_test$urb_pop) ^ 2))
     