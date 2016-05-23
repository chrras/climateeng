library(climateeng)
context("Comfort")

test_that("PMV and PPD validation (ASHRAE 55-2010)", {
  clo <- rep(c(1, 0.5), each = 3)
  temp.air <- c(19.6, 23.9, 21.2, 23.6, 27.9, 24.7)
  temp.rad <- temp.air
  rel.hum <- c(86, 66, 20, 67, 13, 16)
  met <- 1.1
  wme <- 0
  air.velo <- 0.1
  par.w.press <- 0

  pmv <- pmv(clo, temp.air, temp.rad, rel.hum, met, wme, air.velo, par.w.press)
  ppd <- ppd(clo, temp.air, temp.rad, rel.hum, met, wme, air.velo, par.w.press)

  expected_pmv <- c(-0.5, 0.5, -0.5, -0.5, 0.5, -0.5)
  expected_ppd <- rep(10, 6)

  expect_identical(round(pmv, 1), expected_pmv)
  expect_identical(round(ppd, 0), expected_ppd)
})
