library(STAT302package)
library(stats)

x <- c(9.741193, 11.410979, 9.114147, 11.450535, 8.742045, 10.780749, 10.108428, 9.927062)

less_test <- my_t_test(x, "less", 9)

two_sided_test <- my_t_test(x, "two.sided", 10)

greater_test <- my_t_test(x, "greater", 10)

test_that("my_t_test works", {
  expect_equal(less_test[[1]], c(3.290445), tolerance = .0001)
  expect_equal(less_test[[2]], c(7))
  expect_equal(less_test[[3]], "less")
  expect_equal(less_test[[4]], c(0.9933528), tolerance = .0001)
  expect_equal(two_sided_test[[1]], c(0.4523674), tolerance = .0001)
  expect_equal(two_sided_test[[2]], c(7))
  expect_equal(two_sided_test[[3]], "two.sided")
  expect_equal(two_sided_test[[4]], c(1.33531), tolerance = .0001)
  expect_equal(greater_test[[1]], c(0.4523674), tolerance = .0001)
  expect_equal(greater_test[[2]], c(7))
  expect_equal(greater_test[[3]], "greater")
  expect_equal(greater_test[[4]], c(0.3323451), tolerance = .0001)
})

test_that("wrong input throws error", {
  expect_error(my_t_test(3, "a string", 11))
})
