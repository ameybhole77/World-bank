# Load packages
library(dplyr)
library(data.table)
library(ggplot2)

# Assign the training set
world_bank_train = read.csv("/media/amey/1E02DDE102DDBDC9/Amey/Work/Data A/Assignments/World Bank/world_bank_train.csv", sep="")

# Assign the test set
world_bank_test = read.csv("/media/amey/1E02DDE102DDBDC9/Amey/Work/Data A/Assignments/World Bank/world_bank_test.csv", sep="")

# Plot urb_pop as function of cgdp
plot(world_bank_train$urb_pop , world_bank_train$cgdp , xlab = "Urban population" , ylab = "CGDP")

# Set up a linear model between the two variables
lm_wb = lm(urb_pop ~ cgdp , data= world_bank_train)

# Add a red regression line to your scatter plot
abline(lm_wb$coefficient , col = "red")

# Summarize lm_wb and select R-squared
summary(lm_wb)$r.squared

# Define ranks to order the predictor variables in the test set
ranks <- order(world_bank_test$cgdp)

# Scatter plot of test set
plot(world_bank_test, 
     xlab = "GDP per Capita", ylab = "Percentage Urban Population")

# Predict with simple linear model and add line
test_output_lm <- predict(lm_wb, data.frame(cgdp = world_bank_test$cgdp))
lines(world_bank_test$cgdp[ranks], test_output_lm[ranks], lwd = 2, col = "blue")

# Calculate RMSE for simple linear model
sqrt(mean( (test_output_lm - world_bank_test$urb_pop) ^ 2))