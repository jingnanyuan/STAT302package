#' T-test function
#'
#' This function performs a one sample t-test
#'
#' @param x Numberic vector of data.
#' @param alternative Character string specifying the alternative hypothesis.
#'   Only accept \code{"two.sided"}, \code{"less"}, or \code{"greater"}.
#' @param mu Numberic input indicating the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return List with elements:
#' \describe{
#' \item{test_stat}{the numeric test statistic}
#' \item{df}{the degrees of freedom}
#' \item{alternative}{the value of the parameter alternative}
#' \item{p_val}{the numeric p-value}
#' }
#'
#' @examples
#' my_t_test(7, "two.sided", 12)
#'
#' @import stats
#'
#' @export
my_t_test <- function(x, alternative, mu) {
  mu_hat <- mean(x)
  se <- sd(x)/sqrt(length(x))
  test_stat <- (mu_hat - mu)/se
  df <- length(x)-1
  #calculate different p-value according different hypothesis
  if (!(alternative %in% c("two.sided", "less", "greater"))) {
    stop("Must input a valid althernative pyhothesis!")
  } else if (alternative == c("two.sided")){
    p_val <- 2*pt(test_stat, df, lower.tail = TRUE)
  } else if (alternative == c("less")) {
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else { #alternative == c("greater")
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  }
  my_list <- list("test_stat" = test_stat, "df" = df, "alternative" = alternative, "p_value" = p_val)
  return(my_list)
}
