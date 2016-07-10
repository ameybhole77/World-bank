# Load packages
library(dplyr)
library(data.table)
library(ggplot2)

# Assign the training set
world_bank_train = read.csv("/media/amey/1E02DDE102DDBDC9/Amey/Work/Data A/Assignments/World Bank/world_bank_train.csv", sep="")

# Assign the test set
world_bank_test = read.csv("/media/amey/1E02DDE102DDBDC9/Amey/Work/Data A/Assignments/World Bank/world_bank_test.csv", sep="")

# Build the log-linear model
lm_wb_log <- lm(urb_pop ~ log(cgdp), data = world_bank_train)

# Calculate rmse_train
rmse_train <- sqrt(mean(lm_wb_log$residuals ^ 2))

# Summarize lm_wb and select R-squared
summary(lm_wb_log)$r.squared

# The real percentage of urban population in the test set
world_bank_test_truth <- world_bank_test$urb_pop

# The predictions of the percentage of urban population in the test set
world_bank_test_input <- data.frame(cgdp = world_bank_test$cgdp)
world_bank_test_output <- predict(lm_wb_log, world_bank_test_input)

# The residuals: the difference between the ground truth and the predictions
res_test <- world_bank_test_output - world_bank_test_truth


# Use res_test to calculate rmse_test
rmse_test = sqrt(sum((res_test)^2)/nrow(world_bank_test))

# Print the ratio of the test RMSE over the training RMSE
rmse_test/rmse_train

# Define ranks to order the predictor variables in the test set
ranks <- order(world_bank_test$cgdp)

# Scatter plot of test set
plot(world_bank_test, 
     xlab = "GDP per Capita", ylab = "Percentage Urban Population")

# Predict with log-linear model and add line
test_output_lm_log <- predict(lm_wb_log, data.frame(cgdp = world_bank_test$cgdp))
lines(world_bank_test$cgdp[ranks], test_output_lm_log[ranks], lwd = 2, col = "red")

# Calculate RMSE for log-linear model
sqrt(mean( (test_output_lm_log - world_bank_test$urb_pop) ^ 2))
