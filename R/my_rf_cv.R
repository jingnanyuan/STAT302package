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
#' @import randomForest
#'
#' @export
my_rf_cv <- function(k) {
  n <- nrow(my_gapminder)
  fold <- sample(rep(1:k, length = n))
  mse <- rep(NA, k)
  #train the model through every fold
  for (i in 1:k) {
    data_train <- my_gapminder[fold != i, ]
    data_test <- my_gapminder[fold == i, ]
    model <- randomForest(lifeExp ~ gdpPercap , data = data_train, ntree = 100)
    pred <- predict(model, data_test[, 6])
    mse[i] <- mean((pred - data_test$lifeExp)^2)
  }
  output <- mean(mse)
  return(output)
}
