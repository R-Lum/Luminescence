context("apply_EfficiencyCorrection")

test_that("check function", {
  testthat::skip_on_cran()

  ##load data
  data(ExampleData.XSYG, envir = environment())

  ##run tests
  expect_silent(apply_EfficiencyCorrection(TL.Spectrum,spectral.efficiency = as.data.frame(TL.Spectrum[,1:2])))

})

