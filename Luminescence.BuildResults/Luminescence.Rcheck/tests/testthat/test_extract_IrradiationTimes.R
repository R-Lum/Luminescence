context("extract_IrradiationTimes")

test_that("Test the extraction of irradiation times", {
  testthat::skip_on_cran()

  ##general test
  file <- system.file("extdata/XSYG_file.xsyg", package="Luminescence")

  ##general test
  expect_is(extract_IrradiationTimes(object = file, txtProgressBar = FALSE), "RLum.Results")
})


