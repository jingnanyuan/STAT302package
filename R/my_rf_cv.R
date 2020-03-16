#' Random Forest Cross-Validation
#'
#' This function predicts output from existing variables by using
#'  random forest cross-validation
#'
#' @param k The number of folds.
#'
#' @return A numeric with the cross-validation error.
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  n <- nrow(iris)
  fold <- sample(rep(1:k, length = n))
  mse <- rep(NA, k)
  #train the model through every fold
  for (i in 1:k) {
    data_train <- iris[fold != i, ]
    data_test <- iris[fold == i, ]
    model <- randomForest(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = data_train, ntree = 100)
    pred <- predict(model, data_test[, -1])
    #the average squared difference between
    #predicted Sepal.Length and true Sepal.Length
    mse[i] <- mean((pred - data_test$Sepal.Length)^2)
  }
  output <- mean(mse)
  return(output)
}
