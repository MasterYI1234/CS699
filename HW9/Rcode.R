#Problem2
data <- read.csv("hw9_p2.csv")

#Question1: Plot the time series
plot(data$Year, data$Exports, type='o', col='blue', main='Time Series Plot', xlab='Year', ylab='Exports')

#Question2: Split the dataset
train_size <- length(data$Year) - 10
train <- data[1:train_size,]
validation <- data[(train_size+1):length(data$Year),]

#Question3: Linear Trend Model
linear_model <- lm(Exports ~ Year, data=train)
linear_predictions <- predict(linear_model, newdata=validation)
plot(validation$Year, validation$Exports, type='o', col='blue', main='Linear Model: Actual vs Predicted', xlab='Year', ylab='Exports')
points(validation$Year, linear_predictions, type='o', col='red')
linear_rmse <- sqrt(mean((validation$Exports - linear_predictions)^2))
print(paste("Linear Model RMSE:", linear_rmse))

#Question4: Exponential Trend Model
exp_model <- lm(log(Exports) ~ Year, data=train)
exp_predictions <- exp(predict(exp_model, newdata=validation))
plot(validation$Year, validation$Exports, type='o', col='blue', main='Exponential Model: Actual vs Predicted', xlab='Year', ylab='Exports')
points(validation$Year, exp_predictions, type='o', col='red')
exp_rmse <- sqrt(mean((validation$Exports - exp_predictions)^2))
print(paste("Exponential Model RMSE:", exp_rmse))

#Question5: Quadratic Trend Model
quad_model <- lm(Exports ~ Year + I(Year^2), data=train)
quad_predictions <- predict(quad_model, newdata=validation)
plot(validation$Year, validation$Exports, type='o', col='blue', main='Quadratic Model: Actual vs Predicted', xlab='Year', ylab='Exports')
points(validation$Year, quad_predictions, type='o', col='red')
quad_rmse <- sqrt(mean((validation$Exports - quad_predictions)^2))
print(paste("Quadratic Model RMSE:", quad_rmse))

#Question6: Compare the models
min_rmse <- min(linear_rmse, exp_rmse, quad_rmse)
if(min_rmse == linear_rmse){
  print("Best Model: Linear")
} else if(min_rmse == exp_rmse){
  print("The best Model is Exponential")
} else {
  print("The best Model is Quadratic")
}








#Problem3
library(forecast)
library(zoo)

data <- read.csv("hw9_p3.csv", header = TRUE)
data$Quarter <- as.yearqtr(data$Quarter, format = "%Y Q%q")

#Convert quarters to a numeric time index for regression analysis
data$TimeIndex <- as.numeric(format(data$Quarter, "%Y")) + (as.numeric(format(data$Quarter, "%q")) - 1) / 4

#Question1: Plot the time series
plot(data$Quarter, data$Trips, type = "l", xlab = "Quarter", ylab = "Number of Trips", main = "Time Series Plot")

#Question2: Decompose the time series
ts_data <- ts(data$Trips, frequency = 4, start = c(1998, 1))
decomposed <- stl(ts_data, s.window = "periodic")
plot(decomposed)

#Prepare the regression model data with seasonal dummy variables
data$Trend <- seq_along(data$Trips)
data$TrendSquared <- data$Trend^2

#Create dummy variables for each season
for(i in 1:4) {
  data[paste("Season", i, sep = "")] <- ifelse(cycle(ts_data) == i, 1, 0)
}

#Question3: Split into training and validation sets
train_set <- head(ts_data, -12)
validation_set <- tail(ts_data, 12)
train_time_index <- seq_along(train_set)
validation_time_index <- (length(train_set) + 1):length(data$Trips)

#Create newdata for training and validation sets
train_newdata <- data[train_time_index, c("Trend", "TrendSquared", "Season1", "Season2", "Season3", "Season4")]
validation_newdata <- data[validation_time_index, c("Trend", "TrendSquared", "Season1", "Season2", "Season3", "Season4")]

#Question4: Build a regression model with seasonal and quadratic trend
lm_model <- lm(train_set ~ Trend + TrendSquared + Season1 + Season2 + Season3 + Season4, data = train_newdata)

#Predict for the validation set
predicted <- predict(lm_model, newdata = validation_newdata)

#Plot the actual vs predicted values
plot(data$Quarter[validation_time_index], validation_set, type = "l", col = "blue", main = "Actual vs Predicted Plot")
lines(data$Quarter[validation_time_index], predicted, col = "red")
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1, cex = 0.8)

#Calculate RMSE
rmse_quad <- sqrt(mean((validation_set - predicted)^2))
print(paste("RMSE for the regression model with seasonal and quadratic trend:", rmse_quad))

#Question5: Build a Holt-Winters model
hw_model <- HoltWinters(train_set)
hw_forecast <- forecast(hw_model, h=12)

#Plot the Holt-Winters model's forecast results
plot(hw_forecast)
lines(validation_set, col="red")
legend("topright", legend = c("HW Forecast", "Actual"), col = c("blue", "red"), lty = 1, cex = 0.8)

#Calculate RMSE for the Holt-Winters model
hw_rmse <- sqrt(mean((validation_set - hw_forecast$mean)^2))
print(paste("RMSE for the Holt-Winters model:", hw_rmse))

#Question6: Run auto.arima to find the best parameters
best_arima <- auto.arima(train_set)
print(best_arima)

#Question7: Build an ARIMA model with the best parameters
arima_model <- Arima(train_set, model=best_arima)

#Make predictions
arima_forecast <- forecast(arima_model, h=12)

#Plot the actual vs predicted values from the ARIMA model
plot(arima_forecast)
lines(data$Quarter[length(train_set) + (1:12)], validation_set, col="red", type="o")
legend("topright", legend = c("ARIMA Forecast", "Actual"), col = c("blue", "red"), lty = c(1,1), pch = c(NA, 1), cex = 0.8)

#Calculate RMSE for the ARIMA model
arima_rmse <- sqrt(mean((validation_set - arima_forecast$mean)^2))
print(paste("RMSE for the ARIMA model:", arima_rmse))