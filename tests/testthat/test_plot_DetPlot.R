data(ExampleData.BINfileData, envir = environment())
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_DetPlot("error"),
               "Input must be an 'RLum.Analysis' object")
  expect_error(plot_DetPlot(object, signal.integral.min = "error"),
               "'signal.integral.min' must be a positive integer scalar")
  expect_error(plot_DetPlot(object, signal.integral.min = 1,
                            signal.integral.max = 1),
               "'signal.integral.max' must be greater than 'signal.integral.min'")
})

test_that("plot_DetPlot", {
  testthat::skip_on_cran()

  ## simple run with default
  SW({
  results <- expect_s4_class(plot_DetPlot(
    object,
    method = "shift",
    signal.integral.min = 1,
    signal.integral.max = 3,
    background.integral.min = 900,
    background.integral.max = 1000,
    analyse_function.control = list(
      fit.method = "LIN"),
    n.channels = 2),
    "RLum.Results")
  })

  ## simple run with default but no plot
  results <- expect_s4_class(plot_DetPlot(
    object,
    method = "shift",
    signal.integral.min = 1,
    signal.integral.max = 3,
    background.integral.min = 900,
    background.integral.max = 1000,
    analyse_function.control = list(
      fit.method = "LIN"),
    n.channels = 2,
    verbose = FALSE,
    plot = FALSE),
    "RLum.Results")

  ## test with trim channels
  results <- expect_s4_class(plot_DetPlot(
    object,
    method = "shift",
    signal.integral.min = 1,
    signal.integral.max = 3,
    background.integral.min = 900,
    background.integral.max = 1000,
    analyse_function.control = list(
      fit.method = "LIN",
      trim_channels = TRUE
      ),
    n.channels = 2,
    verbose = FALSE,
    plot = FALSE),
    "RLum.Results")

  ## test self call with multi core
  SW({
  results <- expect_s4_class(plot_DetPlot(
    object = list(x = object, y = object),
    method = "shift",
    signal.integral.min = 1,
    signal.integral.max = 3,
    background.integral.min = 900,
    background.integral.max = 1000,
    analyse_function.control = list(
      fit.method = "LIN",
      trim_channels = TRUE
    ),
    multicore = 1,
    n.channels = 2,
    verbose = TRUE,
    plot = FALSE),
    "RLum.Results")
  })

  ## simple run with default
  results <- expect_s4_class(plot_DetPlot(
    object,
    method = "expansion",
    signal.integral.min = 1,
    signal.integral.max = 3,
    background.integral.min = 900,
    background.integral.max = 1000,
    analyse_function.control = list(
      fit.method = "LIN"),
    verbose = FALSE,
    n.channels = 2),
    "RLum.Results")

  ## try with NA values
  object@records[[2]][,2] <- 1
  object@records[[4]][,2] <- 1
  object@records[[6]][,2] <- 1
  object@records[[8]][,2] <- 1
  results <- expect_s4_class(suppressWarnings(plot_DetPlot(
    object,
    method = "expansion",
    signal.integral.min = 1,
    signal.integral.max = 3,
    background.integral.min = 900,
    background.integral.max = 1000,
    analyse_function.control = list(
      fit.method = "EXP"),
    verbose = FALSE,
    n.channels = 1)),
    "RLum.Results")

  SW({
  suppressWarnings( # ignore additional warnings from plot_GrowthCurve()
  expect_error(
      expect_warning(plot_DetPlot(
          object,
          signal.integral.min = 1,
          signal.integral.max = 2,
          background.integral.min = 900,
          background.integral.max = 1000,
          analyse_function = "analyse_pIRIRSequence",
          analyse_function.control = list(
              fit.method = "LIN"),
          verbose = FALSE,
          n.channels = 1),
          "An error occurred, analysis skipped"),
      "No valid results produced")
  )
  })
})
