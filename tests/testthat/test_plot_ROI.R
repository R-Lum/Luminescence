test_that("Complete test", {
  testthat::skip_on_cran()
  local_edition(3)

  ##create suitable dataset
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
  temp <- read_RF2R(file)

  ##crash function
  expect_error(plot_ROI(object = "stop"),
               regexp = "At least one input element is not of type 'RLum.Analysis' and/or does")

  ##test standard cases
  expect_silent(plot_ROI(temp))
  expect_silent(plot_ROI(temp, grid = TRUE))
  expect_silent(plot_ROI(temp, dim.CCD = c(8192,8192)))
  expect_silent(plot_ROI(temp, dist_thre = 20))
  expect_silent(plot_ROI(temp, exclude_ROI = NULL))

  ##test non-list case
  expect_silent(plot_ROI(temp[[1]]))
  expect_silent(plot_ROI(temp[[1]], exclude_ROI = NULL))

  ##output only case
  expect_s4_class(plot_ROI(temp, plot = FALSE), class = "RLum.Results")

})
