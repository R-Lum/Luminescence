test_that("plot_DetPlot", {
  testthat::skip_on_cran()
  local_edition(3)

  ##ExampleData.BINfileData contains two BINfileData objects
  ##CWOSL.SAR.Data and TL.SAR.Data
  data(ExampleData.BINfileData, envir = environment())

  ##transform the values from the first position in a RLum.Analysis object
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

  ## trigger stop
  expect_error(
    plot_DetPlot(object = "error"),
    regexp = "\\[plot_DetPlot\\(\\)\\] input must be an RLum\\.Analysis object\\!")

  ## simple run with default
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
    n.channels = 1)),
    "RLum.Results")

})
