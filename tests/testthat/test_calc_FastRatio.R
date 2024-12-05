## load data
data(ExampleData.CW_OSL_Curve, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  obj <- ExampleData.CW_OSL_Curve
  expect_error(calc_FastRatio("error"),
               "'object' should be of class 'RLum.Analysis', 'RLum.Results'")
  expect_error(calc_FastRatio(iris[0, ]),
               "'object' cannot be an empty data.frame")
  expect_error(calc_FastRatio(matrix(nrow = 1, ncol = 0)),
               "'object' cannot be an empty matrix")
  expect_error(calc_FastRatio(matrix()),
               "'object' should have at least two columns")

  expect_error(calc_FastRatio(obj, Ch_L1 = NULL),
               "'Ch_L1' should be a positive integer scalar")
  expect_error(calc_FastRatio(obj, Ch_L1 = 0),
               "'Ch_L1' should be a positive integer scalar")
  expect_error(calc_FastRatio(obj, Ch_L1 = c(1, 2)),
               "'Ch_L1' should be a positive integer scalar")

  expect_error(calc_FastRatio(obj, Ch_L2 = 0),
               "'Ch_L2' should be a positive integer scalar")
  expect_error(calc_FastRatio(obj, Ch_L2 = c(1, 2)),
               "'Ch_L2' should be a positive integer scalar")

  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve,
                              Ch_L3 = 50),
               "'Ch_L3' should have length 2")
  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve,
                              Ch_L3 = c(40, 50, 60)),
               "'Ch_L3' should have length 2")
  expect_error(calc_FastRatio(obj, Ch_L3 = list(4, 5)),
               "'Ch_L3' should be of class 'integer' or 'numeric'")
  expect_error(calc_FastRatio(obj, Ch_L3 = c(0, 2)),
               "'Ch_L3[1]' should be a positive integer scalar",
               fixed = TRUE)
  expect_error(calc_FastRatio(obj, Ch_L3 = c(5, 2)),
               "'Ch_L3[2]' must be greater than or equal to 'Ch_L3[1]'",
               fixed = TRUE)
  expect_error(calc_FastRatio(obj, Ch_L3 = c(5, 1001)),
               "Value in 'Ch_L3' (5, 1001) exceeds number of available channels",
               fixed = TRUE)

  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve, wavelength=0),
               "'wavelength' should be a positive scalar")
  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve, sigmaF= 0),
               "'sigmaF' should be a positive scalar")
  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve, sigmaM= 0),
               "'sigmaM' should be a positive scalar")
  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve, x = -12),
               "'x' should be a positive scalar")
  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve, x2 = -12),
               "'x2' should be a positive scalar")
  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve, dead.channels = TRUE),
               "'dead.channels' should be of class 'integer' or 'numeric'")
  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve, dead.channels = 1),
               "'dead.channels' should have length 2")
  expect_error(calc_FastRatio(ExampleData.CW_OSL_Curve, dead.channels = c(-1, 1)),
               "All elements of 'dead.channels' should be non-negative")

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

test_that("check functionality", {
  testthat::skip_on_cran()

  temp <- calc_FastRatio(ExampleData.CW_OSL_Curve, plot = FALSE,
                         verbose = FALSE)
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

  ## RLum.Data.Curve object
  curve <- set_RLum("RLum.Data.Curve", data = as.matrix(ExampleData.CW_OSL_Curve))
  expect_s4_class(calc_FastRatio(curve, plot = FALSE, verbose = FALSE),
                  "RLum.Results")

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

test_that("regression tests", {

  ## issue 471 --------------------------------------------------------------

  expect_s4_class(suppressWarnings(
      calc_FastRatio(ExampleData.CW_OSL_Curve[1:3, ], verbose = FALSE)),
      "RLum.Results")

  expect_s4_class(suppressWarnings(
      calc_FastRatio(ExampleData.CW_OSL_Curve[1:5, ], verbose = FALSE)),
      "RLum.Results")

  SW({
  expect_message(expect_s4_class(
      calc_FastRatio(ExampleData.CW_OSL_Curve, fitCW.sigma=TRUE,
                     n.components.max = 0),
      "RLum.Results"),
      "Error: Fitting failed, please call 'fit_CWCurve()' manually",
      fixed = TRUE)

  set.seed(1)
  expect_message(expect_s4_class(
      calc_FastRatio(ExampleData.CW_OSL_Curve[sample(50), ],
                     fitCW.sigma = TRUE),
      "RLum.Results"),
      "Error: Fitting failed, please call 'fit_CWCurve()' manually",
      fixed = TRUE)
  })
})
