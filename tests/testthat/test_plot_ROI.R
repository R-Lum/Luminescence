context("Test plot_ROI")

test_that("Complete test", {
  testthat::skip_on_cran()

  ##create suitable dataset
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
  temp <- read_RF2R(file)

  ##crash function
  expect_error(plot_ROI(object = "stop"),
               regexp = "At least one input element is not of type 'RLum.Analysis' and/or does")

  ##test standard cases
  expect_silent(plot_ROI(temp))

  ##test non-list case
  expect_silent(plot_ROI(temp[[1]]))

  ##output only case
  expect_s4_class(plot_ROI(temp, plot = FALSE), class = "RLum.Results")

})
