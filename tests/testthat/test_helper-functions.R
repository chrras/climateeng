library(climateeng)
context("Helper functions")

test_that("Check my_round()", {
  expect_identical(my_round(c(24.9, 20.1, 25), 5, "ceiling"), c(25, 25, 25))
  expect_identical(my_round(24.9, 5, "floor"), 20)
})
