set.seed(302)

knn_test <- my_knn_cv(my_iris[, -5], my_iris$Species, 5, 5)

test_that("my_knn_cv works", {
  expect_equal(knn_test[[2]], c(0), tolerance = .05)
})
