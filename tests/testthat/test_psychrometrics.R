library(climateeng)
context("Psychrometrics")

test_that("Barometric pressure equals table values in ASHRAE handbook", {
  expect_equal(bar_press(), 101.325)
  expect_equal(round(bar_press(alt = 10000), 3), 26.436)
})

test_that("Dew-point is within tolerance", {
  expect_equal(dewpoint(hum.ratio = 0), NA)
  expect_lt(
    max(na.omit(abs(par_w_press(seq(0, 0.050, by = 0.001), 0) /
    sat_w_press(dewpoint(seq(0, 0.050, by = 0.001))) * 100 - 100))),
    0.25)
})

test_that("Relative humidity does not exceed 100 %", {
  hum.ratio <- 0.010
  temp.air <- 10
  alt <- 0

  expect_identical(rel_hum(temp.air, hum.ratio), 100)
  expect_gt(par_w_press(hum.ratio, alt) / sat_w_press(temp.air) * 100, 100)
})

test_that("Relative humidity fits ASHRAE Handbook psychrometric chart", {
  expect_identical(round(rel_hum(40, 0.00925, alt = 0), 0), 20)
  expect_identical(round(rel_hum(23, 0.016, alt = 0), 0), 90)
  expect_identical(round(rel_hum(0, 0.003, alt = 0), 0), 80)
  expect_identical(round(rel_hum(4, 0.0005, alt = 0), 0), 10)
})

test_that("Humidity ratio fits ASHRAE Handbook psychrometric chart", {
  expect_identical(round(hum_ratio(100, 24, alt = 0), 3), 0.019)
  expect_identical(round(hum_ratio(40, 27, alt = 0), 3), 0.009)
  expect_identical(round(hum_ratio(10, 4, alt = 0), 4), 0.0005)
})
