# Load required libraries
library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(ggplot2)    # For data visualization
library(TTR)        # For technical indicators (e.g., SMA, RSI)
library(caret)      # For splitting data into training and testing sets
library(randomForest) # For training a Random Forest model

# Step 1: Load and preprocess the dataset
ibm_data <- read_csv("/Users/Bharathi/Downloads/intraday_5min_IBM.csv") %>%
  mutate(
    timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"),
    close_lag1 = lag(close, 1),
    close_lag2 = lag(close, 2),
    close_lag3 = lag(close, 3),
    volume_lag1 = lag(volume, 1),
    sma_10 = SMA(close, n = 10),
    rsi = RSI(close, n = 14),
    hour = as.numeric(format(timestamp, "%H")),
    minute = as.numeric(format(timestamp, "%M")),
    next_close = lead(close, 1)  # Target variable: next 5-minute close price
  ) %>%
  na.omit()  # Remove rows with missing values after feature engineering

# Step 2: Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(ibm_data$next_close, p = 0.8, list = FALSE)
trainData <- ibm_data[trainIndex, ]
testData <- ibm_data[-trainIndex, ]

# Step 3: Train the Random Forest model
predictors <- c("open", "high", "low", "close_lag1", "close_lag2", "close_lag3", 
                "volume_lag1", "sma_10", "rsi", "hour", "minute")
target <- "next_close"

model <- randomForest(as.formula(paste(target, "~", paste(predictors, collapse = "+"))),
                      data = trainData,
                      ntree = 100)

# Step 4: Define a prediction function
predict_next_close <- function(model) {
  # Collect user inputs interactively
  open <- as.numeric(readline(prompt = "Enter the opening price: "))
  high <- as.numeric(readline(prompt = "Enter the highest price: "))
  low <- as.numeric(readline(prompt = "Enter the lowest price: "))
  
  # Validate inputs
  if (any(is.na(c(open, high, low)))) {
    stop("One or more inputs are missing or invalid. Please ensure all inputs are numeric.")
  }
  
  # Use realistic default values for other features (based on the dataset)
  close_lag1 <- mean(trainData$close_lag1, na.rm = TRUE)
  close_lag2 <- mean(trainData$close_lag2, na.rm = TRUE)
  close_lag3 <- mean(trainData$close_lag3, na.rm = TRUE)
  volume_lag1 <- mean(trainData$volume_lag1, na.rm = TRUE)
  sma_10 <- mean(trainData$sma_10, na.rm = TRUE)
  rsi <- mean(trainData$rsi, na.rm = TRUE)
  hour <- as.numeric(format(Sys.time(), "%H"))
  minute <- as.numeric(format(Sys.time(), "%M"))
  
  # Create a data frame with the user inputs and default values
  user_input <- data.frame(
    open = open,
    high = high,
    low = low,
    close_lag1 = close_lag1,
    close_lag2 = close_lag2,
    close_lag3 = close_lag3,
    volume_lag1 = volume_lag1,
    sma_10 = sma_10,
    rsi = rsi,
    hour = hour,
    minute = minute
  )
  
  # Make a prediction using the trained model
  prediction <- predict(model, user_input)
  
  # Return the predicted next close price
  return(prediction)
}

# Step 5: Test the prediction function
cat("Welcome to the IBM Stock Price Predictor!\n")
predicted_price <- predict_next_close(model)
cat("Predicted Next Close Price:", round(predicted_price, 4), "\n")