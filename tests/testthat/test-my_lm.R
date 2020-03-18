library(STAT302package)

lm_test <- my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)

test_that("my_lm works", {
  expect_equal(lm_test[1,1], c(47.88), tolerance = .01)
  expect_equal(lm_test[2,3], c(18), tolerance = .1)
  expect_equal(lm_test[2,4], c(0), tolerance = .0001)
  expect_equal(lm_test[3,2], c(0.6), tolerance = .001)
  expect_equal(lm_test[5,3], c(28.1), tolerance = .001)
  expect_equal(lm_test[5,4], c(0), tolerance = .0001)
})

test_that("wrong input throws error", {
  expect_error(my_lm(lifeExp ~ gdpPercap + continent, 7))
})
