library(climateeng)
context("Barometric pressure")

test_that("barometric pressure equals table values in ASHRAE handbook", {
  expect_equal(bar_press(), 101.325)
  expect_equal(bar_press(alt = 10000), 26.436)
})
