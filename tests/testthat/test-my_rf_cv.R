library(STAT302package)

test_that("my_rf_cv works", {
  expect_equal(my_rf_cv(5), 76, tolerance = .5)
})

test_that("wrong input throws error", {
  expect_error(my_rf_cv("a string"))
})
