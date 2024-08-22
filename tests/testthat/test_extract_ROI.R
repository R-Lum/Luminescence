test_that("extract_ROI", {
  testthat::skip_on_cran()

  ## generate random data
  m <- matrix(runif(100,0,255), ncol = 10, nrow = 10)
  set.seed(12245)
  a <- array(runif(300, 0,255), c(10,10,3))
  RLum <- set_RLum("RLum.Data.Image", data = a)
  RLum_list <- list(RLum, RLum)
  roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)


  ## crash the function
  expect_error(extract_ROI(object = "error", roi), "\\[extract_ROI\\(\\)\\] Input for argument 'object' not supported\\!")
  expect_error(extract_ROI(object = m, "error"), "\\[extract_ROI\\(\\)\\] Please check the format of roi, it looks wrong\\!")
  expect_error(extract_ROI(object = m, matrix()), "\\[extract_ROI\\(\\)\\] Please check the format of roi, it looks wrong\\!")
  expect_error(extract_ROI(object = m, matrix(ncol = 3, nrow = 0)), "\\[extract_ROI\\(\\)\\] Please check the format of roi, it looks wrong\\!")

  ## run function for all supported input objects
  ## matrix
  expect_s4_class(extract_ROI(object = m, roi), "RLum.Results")

  ## array
  expect_s4_class(extract_ROI(object = a, roi), "RLum.Results")

  ## RLum.Data.Image
  expect_s4_class(extract_ROI(object = RLum, roi), "RLum.Results")

  ## list
  results <- expect_s4_class(extract_ROI(object = RLum_list, roi), "RLum.Results")

  ## regression test if it fails, we have to amend the documentation
  expect_length(results@data$roi_signals, 6)
  expect_length(results@data, 3)

  ## with plot output
  expect_silent(extract_ROI(object = RLum, roi, plot = TRUE))
  expect_silent(extract_ROI(object = RLum_list, roi, plot = TRUE))

  ## test with package example dataset
  data(ExampleData.RLum.Data.Image, envir = environment())
  roi <- matrix(c(200,400,200,40,60,80,10,10,10), ncol = 3)
  expect_s4_class(extract_ROI(object = ExampleData.RLum.Data.Image, roi), "RLum.Results")

  ## test ROI summary options
  roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
  t_mean <- expect_type(extract_ROI(object = RLum, roi, roi_summary = "mean")@data$roi_summary, "double")
  expect_equal(sum(t_mean),1124, tolerance = 0.001)

  t_median <- expect_type(extract_ROI(object = RLum, roi, roi_summary = "median")@data$roi_summary, "double")
  expect_equal(sum(t_median),1104, tolerance = 0.001)

  t_sd <- expect_type(extract_ROI(object = RLum, roi, roi_summary = "sd")@data$roi_summary, "double")
  expect_equal(sum(t_sd),730, tolerance = 0.001)

  t_sum <- expect_type(extract_ROI(object = RLum, roi, roi_summary = "sum")@data$roi_summary, "double")
  expect_equal(sum(t_sum), 8117, tolerance = 0.001)

  ## crash
  expect_error(extract_ROI(object = RLum, roi, roi_summary = "error"),
               "\\[extract\\_ROI\\(\\)\\] roi\\_summary method not supported, check manual!")

})
