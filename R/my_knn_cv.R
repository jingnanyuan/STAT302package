#'k-Nearest Neighbors Cross-Validation
#'
#'This function predicts output from existing data by using k-nearest neighbors cross-validation
#'
#'@param train The input data frame.
#'@param cl The true class value of training data.
#'@param k_nn The integer representing the number of neighbors.
#'@param k_cv The integer representing the number of folds.
#'
#'@return List with objects:
#'\describe{
#'\item{class}{a vector of the predicted class $\hat{Y}_{i}$ for all observations}
#'\item{df}{a numeric with the cross-validation misclassification error}
#'}
#'
#'@examples
#'my_knn_cv(iris[, -5],iris$Species, 5, 5)
#'
#'@export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  n <- nrow(train)
  fold <- sample(rep(1:k_cv, length = n))
  mcr <- rep(NA, k_cv)
  #train the model through every fold
  for (i in 1:k_cv) {
    data_train <- train[fold != i, ] #X_i
    data_test <- train[fold == i, ] #X_i^*
    cl_train <- cl[fold != i] #Y_i
    cl_test <- cl[fold == i] #Y_i^*
    knn_output <- knn(train = data_train, cl = cl_train,
                      test = data_test, k = k_nn) #Y_i^*_hat
    #the misclassification rate
    mcr[i] <- sum(knn_output != cl_test)/length(cl_test)
  }
  class <- knn(train = train, test = train, cl = cl, k = k_nn)
  cv_err <- mean(mcr)
  output <- list("class" = class, "cv_err" = cv_err)
  return(output)
}