data(ExampleData.CW_OSL_Curve, envir = environment())
temp <- calc_FastRatio(ExampleData.CW_OSL_Curve, plot = FALSE, verbose = FALSE)


test_that("input validation", {
  testthat::skip_on_cran()

  obj <- ExampleData.CW_OSL_Curve
  expect_error(calc_FastRatio(obj, Ch_L1 = NULL),
               "'Ch_L1' must be a positive integer scalar")
  expect_error(calc_FastRatio(obj, Ch_L1 = 0),
               "'Ch_L1' must be a positive integer scalar")
  expect_error(calc_FastRatio(obj, Ch_L1 = c(1, 2)),
               "'Ch_L1' must be a positive integer scalar")

  expect_error(calc_FastRatio(obj, Ch_L2 = 0),
               "'Ch_L2' must be a positive integer scalar")
  expect_error(calc_FastRatio(obj, Ch_L2 = c(1, 2)),
               "'Ch_L2' must be a positive integer scalar")

  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve,
                              Ch_L3 = 50),
               "Input for 'Ch_L3' must be a vector of length 2")
  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve,
                              Ch_L3 = c(40, 50, 60)),
               "Input for 'Ch_L3' must be a vector of length 2")
  expect_error(calc_FastRatio(obj, Ch_L3 = list(4, 5)),
               "Input for 'Ch_L3' must be a vector of length 2")
  expect_error(calc_FastRatio(obj, Ch_L3 = c(0, 2)),
               "'Ch_L3[1]' must be a positive integer scalar",
               fixed = TRUE)
  expect_error(calc_FastRatio(obj, Ch_L3 = c(5, 2)),
               "Ch_L3[2] must be greater than or equal to Ch_L3[1]",
               fixed = TRUE)
  expect_error(calc_FastRatio(obj, Ch_L3 = c(5, 1001)),
               "Value in Ch_L3 (5, 1001) exceeds number of available channels",
               fixed = TRUE)

  expect_warning(expect_null(calc_FastRatio(ExampleData.CW_OSL_Curve,
                                            Ch_L2 = 1)),
                 "Calculated time/channel for L2 is too small (0, 1)",
                 fixed = TRUE)
  expect_warning(expect_null(calc_FastRatio(ExampleData.CW_OSL_Curve,
                                            Ch_L2 = 2000)),
                 "The calculated channel for L2 (2000) exceeds the number",
                 fixed = TRUE)
  SW({
  expect_warning(calc_FastRatio(ExampleData.CW_OSL_Curve,
                                Ch_L3 = c(1000, 1000)),
                 "The calculated channels for L3 (1000, 1000) exceed",
                 fixed = TRUE)
  })
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 5)

  ## fitCW.sigma and fitCW.curve
  SW({
  calc_FastRatio(ExampleData.CW_OSL_Curve, plot = FALSE,
                 fitCW.sigma = TRUE, fitCW.curve = TRUE)

  ## RLum.Analysis object
  data(ExampleData.XSYG, envir = environment())
  calc_FastRatio(get_RLum(OSL.SARMeasurement$Sequence.Object)[[1]])

  expect_warning(calc_FastRatio(get_RLum(TL.Spectrum)),
                 "L3 contains more counts (566) than L2 (562)",
                 fixed = TRUE)
  })
})

test_that("check values from output", {
  testthat::skip_on_cran()

  results <- get_RLum(temp)

  expect_equal(round(results$fast.ratio, digits = 3), 405.122)
  expect_equal(round(results$fast.ratio.se, digits = 4), 119.7442)
  expect_equal(round(results$fast.ratio.rse, digits = 5), 29.55756)
  expect_equal(results$channels, 1000)
  expect_equal(round(results$channel.width, digits = 2), 0.04)
  expect_equal(results$dead.channels.start, 0)
  expect_equal(results$dead.channels.end, 0)
  expect_equal(results$sigmaF, 2.6e-17)
  expect_equal(results$sigmaM, 4.28e-18)
  expect_equal(results$stimulation.power, 30.6)
  expect_equal(results$wavelength, 470)
  expect_equal(results$t_L1, 0)
  expect_equal(round(results$t_L2, digits = 6), 2.446413)
  expect_equal(round(results$t_L3_start, digits = 5), 14.86139)
  expect_equal(round(results$t_L3_end, digits = 5), 22.29208)
  expect_equal(results$Ch_L1, 1)
  expect_equal(results$Ch_L2, 62)
  expect_equal(results$Ch_L3_start, 373)
  expect_equal(results$Ch_L3_end, 558)
  expect_equal(results$Cts_L1, 11111)
  expect_equal(results$Cts_L2, 65)
  expect_equal(round(results$Cts_L3, digits = 5), 37.66667)

})
