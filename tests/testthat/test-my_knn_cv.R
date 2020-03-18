library(STAT302package)

knn_test <- my_knn_cv(my_iris[, -5], my_iris$Species, 5, 5)

test_that("my_knn_cv works", {
  expect_equal(knn_test[[2]], c(0), tolerance = .05)
})

test_that("wrong input throws error", {
  expect_error(my_knn_cv(my_iris[, 8], my_iris$Species, 5, 5))
  expect_error(my_knn_cv(my_iris[, 8], my_iris$Species, 0, 5))
})
