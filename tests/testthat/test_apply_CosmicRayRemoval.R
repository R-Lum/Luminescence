context("apply_CosmicRayRemoval")

test_that("check function", {
  testthat::skip_on_cran()

  ##load data
  data(ExampleData.XSYG, envir = environment())

  ##run tests
  expect_silent(apply_CosmicRayRemoval(TL.Spectrum, method = "Pych"))
  expect_silent(apply_CosmicRayRemoval(TL.Spectrum, method = "smooth"))
  expect_silent(apply_CosmicRayRemoval(TL.Spectrum, method = "smooth", MARGIN = 1))
  expect_output(apply_CosmicRayRemoval(TL.Spectrum, method = "Pych", MARGIN = 2, verbose = TRUE, plot = TRUE))
  expect_silent(apply_CosmicRayRemoval(TL.Spectrum, method = "Pych", method.Pych.smoothing = 2, method.Pych.threshold_factor = 2))

})

