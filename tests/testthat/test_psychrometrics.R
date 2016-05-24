library(climateeng)
context("Psychrometrics")

test_that("Barometric pressure validation", {
  expect_equal(bar_press(), 101.325)
  expect_equal(round(bar_press(alt = 10000), 3), 26.436)
})

test_that("Dew-point tolerance", {
  expect_equal(dewpoint(hum.ratio = 0), NA)
  expect_lt(
    max(na.omit(abs(par_w_press(seq(0, 0.050, by = 0.001), 0) /
    sat_w_press(dewpoint(seq(0, 0.050, by = 0.001))) * 100 - 100))),
    0.25)
})

test_that("Relative humidity upper limit check", {
  hum.ratio <- 0.010
  temp.air <- 10
  alt <- 0

  expect_identical(rel_hum(temp.air, hum.ratio), 100)
  expect_gt(par_w_press(hum.ratio, alt) / sat_w_press(temp.air) * 100, 100)
})

test_that("Relative humidity validation", {
  expect_identical(round(rel_hum(40, 0.00925, alt = 0), 0), 20)
  expect_identical(round(rel_hum(23, 0.016, alt = 0), 0), 90)
  expect_identical(round(rel_hum(0, 0.003, alt = 0), 0), 80)
  expect_identical(round(rel_hum(4, 0.0005, alt = 0), 0), 10)
})

test_that("Humidity ratio validation", {
  expect_identical(round(hum_ratio(100, 24, alt = 0), 3), 0.019)
  expect_identical(round(hum_ratio(40, 27, alt = 0), 3), 0.009)
  expect_identical(round(hum_ratio(10, 4, alt = 0), 4), 0.0005)
})

test_that("Moist air density validation", {
  expect_identical(round((1 + 0.0195) / density_air(35, 0.0195), 2), 0.90)
  expect_identical(round((1 + 0.0075) / density_air(13, 0.0075), 2), 0.82)
  expect_identical(round((1 + 0.0090) / density_air(47, 0.0090), 2), 0.92)
})

test_that("Specific volume validation", {
  expect_identical(round(specific_vol(35, 0.0195, alt = 0), 2), 0.90)
  expect_identical(round(specific_vol(13, 0.0075, alt = 0), 2), 0.82)
  expect_identical(round(specific_vol(47, 0.0090, alt = 0), 2), 0.92)
})

test_that("Enthalpy intersect validation", {
  expect_equal(enthalpy_intersect(30, alt = 0), 10.3)
  expect_equal(enthalpy_intersect(20, alt = 0), 5.7)
  expect_equal(enthalpy_intersect(69, alt = 0), 23.2)
})

test_that("Enthalpy validation", {
  expect_equal(round(enthalpy(22, 0.011), 0), 50)
  expect_equal(round(enthalpy(46, 0.0285), 0), 120)
  expect_equal(round(enthalpy(5, 0.002), 0), 10)
})

test_that("Humidity ratio by enthalpy validation", {
  expect_equal(round(hum_ratio_enthalpy(50, 22, alt = 0), 3), 0.011)
  expect_equal(round(hum_ratio_enthalpy(120, 46, alt = 0), 4), 0.0285)
  expect_equal(round(hum_ratio_enthalpy(10, 5, alt = 0), 3), 0.002)
})
