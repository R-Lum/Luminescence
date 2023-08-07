test_that("Complete test", {
  testthat::skip_on_cran()
  local_edition(3)

  ##create suitable dataset
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
  temp <- read_RF2R(file)

  ##crash function
  expect_error(plot_ROI(object = "stop"),
               regexp = "\\[plot\\_ROI\\(\\)\\] Input for 'object' not supported, please check documentation!")

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

  ## test combination with extract_ROI()
  m <- matrix(runif(100,0,255), ncol = 10, nrow = 10)
  roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
  t <- extract_ROI(object = m, roi = roi)
  expect_s4_class(plot_ROI(t, bg_image = m, exclude_ROI = NULL), "RLum.Results")

  ## trigger warning
  expect_warning(plot_ROI(t, bg_image = "stop", exclude_ROI = NULL),
    "\\[plot\\_ROI\\(\\)] 'bg\\_image' is not of type RLum.Data.Image and cannot be converted into such; background image plot skipped!")

})
