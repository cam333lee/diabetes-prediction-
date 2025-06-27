library(readr)
library(caTools)
library(caret)
library(e1071)


clean_data <- function(data, split_ratio = 0.7, seed = 123) {
    colSums(is.na(data))
    
    # Split the data into features and target variable (last column as target)
    X <- data[, -ncol(data)]
    y <- data[, ncol(data)]
    
    # Scale the features
    scaled_X <- as.data.frame(scale(X))
    
    # Combine scaled features with target variable
    scaled_data <- cbind(scaled_X, y)
    
    # Split the data into features and target variable again
    X <- scaled_data[, -ncol(scaled_data)]
    y <- scaled_data[, ncol(scaled_data)]
    
    # Use the new split_train_test function
    split_train_test(X, y, split_ratio, seed)
}

split_train_test <- function(X, y, split_ratio = 0.7, seed = 123) {
    set.seed(seed)
    sample <- sample.split(y, SplitRatio = split_ratio)
    X_train <- X[sample == TRUE, ]
    y_train <- y[sample == TRUE]
    X_test <- X[sample == FALSE, ]
    y_test <- y[sample == FALSE]
    return(list(
        X_train = X_train,
        y_train = y_train,
        X_test = X_test,
        y_test = y_test
    ))
}
