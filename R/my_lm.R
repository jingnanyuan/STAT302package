#'Linear model
#'
#'This function fits a linear model in statistic
#'
#'@param formula An object of class \code{formula},
#'  a symbolic description of the model to be fitted.
#'@param data A input data frame containing the variables in the model.
#'
#'@return a table with rows for each coefficient (including the \code{(Intercept)})
#'  and columns for the \code{Estimate}, \code{Std. Error}, \code{t value},
#'  and \code{Pr(>|t|)}.
#'
#'@examples
#'my_lm(mpg ~ hp + wt, mtcars)
#'
#'@export
my_lm <- function(formula, data) {
  #set the variables
  x <- model.matrix(formula, data = data)
  my_frame <- model.frame(formula, data = data)
  y <- model.response(my_frame)
  #calculate beta hat
  est <- solve(t(x) %*% x) %*% t(x) %*% y
  #calculate the degrees of freedom
  df <- nrow(x) - ncol(x)
  #calculate sigma square
  var <- sum((y - x %*% est) ^ 2 / df)
  #calculate the standard error
  se <- sqrt(diag(var * solve(t(x) %*% x)))
  #calculate the t value
  t_v <- est/se
  #calculate the p-value
  p_vals <- 2 * pt(abs(t_v), df, lower.tail = FALSE)
  #aggregate all results in a table
  output <- as.table(cbind(est, se, t_v, p_vals))
  #rename the column names
  colnames(output) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(output)
}
