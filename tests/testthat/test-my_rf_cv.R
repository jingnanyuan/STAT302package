test_that("my_rf_cv works", {
  expect_equal(my_rf_cv(5), 75.73032, tolerance = .0001)
})
