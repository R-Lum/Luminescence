## load data
data(ExampleData.BINfileData, envir = environment())
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_DetPlot("error"),
               "'object' should be of class 'RLum.Analysis'")
  expect_error(plot_DetPlot(set_RLum("RLum.Analysis")),
               "'object' cannot be an empty RLum.Analysis")
  expect_warning(
  expect_error(plot_DetPlot(object, signal.integral.min = "error"),
               "'signal.integral.min' should be of class 'integer' or 'numeric'"),
               "were deprecated in v1.2.0, use 'signal_integral'")

  expect_error(plot_DetPlot(object,
                            signal_integral = 1:2,
                            background_integral = 900:1000,
                            method = "error"),
               "'method' should be one of 'shift' or 'expansion'")
  expect_error(plot_DetPlot(object,
                            signal_integral = 1:2,
                            background_integral = 900:1000,
                            analyse_function = "error",
                            verbose = FALSE),
               "'analyse_function' should be one of 'analyse_SAR.CWOSL'")
})

test_that("plot_DetPlot", {
  testthat::skip_on_cran()

  ## simple run with default but no plot
  results <- expect_s4_class(plot_DetPlot(
    object,
    method = "shift",
    signal_integral = 1:3,
    background_integral = 900:1000,
    analyse_function.control = list(
      fit.method = "LIN"),
    n.channels = 2,
    verbose = FALSE,
    plot = FALSE),
    "RLum.Results")

  ## test self call with multi core
  SW({
  expect_message(expect_s4_class(plot_DetPlot(
    object = list(x = object, y = object),
    method = "shift",
    signal_integral = 1:3,
    background_integral = 900:1000,
    analyse_function.control = list(
      fit.method = "LIN",
      trim_channels = TRUE
    ),
    multicore = 2,
    n.channels = 2,
    verbose = TRUE,
    plot = FALSE),
    "RLum.Results"),
    "Running multicore session using 2 cores")

  expect_s4_class(plot_DetPlot(
    object = list(x = object, y = object),
    method = "shift",
    signal_integral = 1:3,
    background_integral = 900:1000,
    analyse_function.control = list(
      fit.method = "LIN",
      trim_channels = TRUE
    ),
    n.channels = 2,
    verbose = TRUE,
    plot = FALSE),
    "RLum.Results")

  ## serial
  expect_s4_class(plot_DetPlot(
    object = list(x = object, y = object),
    method = "shift",
    signal_integral = 1:3,
    background_integral = 900:1000,
    analyse_function.control = list(
      fit.method = "LIN",
      trim_channels = TRUE
    ),
    multicore = FALSE,
    n.channels = 2,
    verbose = TRUE,
    plot = FALSE),
    "RLum.Results")
  })

  ## try with NA values
  object@records[[2]][,2] <- 1
  object@records[[4]][,2] <- 1
  object@records[[6]][,2] <- 1
  object@records[[8]][,2] <- 1
  results <- expect_s4_class(suppressWarnings(plot_DetPlot(
    object,
    method = "expansion",
    signal_integral = 1:3,
    background_integral = 900:1000,
    analyse_function.control = list(
      fit.method = "EXP"),
    verbose = FALSE,
    n.channels = 1)),
    "RLum.Results")

  SW({
  ## n.channels not set
  expect_message(plot_DetPlot(object,
                              method = "shift",
                              signal_integral = 5:6,
                              background_integral = 10:50,
                              analyse_function.control = list(
                                  fit.method = "LIN"),
                              verbose = TRUE),
                 "'n.channels' not specified, set to 3")

  ## analyse_pIRIRSequence
  tmp <- subset(object, recordType != "IRSL (PMT)" & ID != 1)
  plot_DetPlot(
      tmp,
      method = "expansion",
      signal_integral = 1:2,
      background_integral = 900:1000,
      analyse_function = "analyse_pIRIRSequence",
      analyse_function.control = list(
          sequence.structure = c("TL", "IR50"),
          fit.method = "LIN"),
      respect_RC.Status = TRUE,
      n.channels = 2)

  expect_warning(plot_DetPlot(
      tmp,
      method = "expansion",
      signal_integral = 1:2,
      background_integral = 900:1000,
      plot.single = TRUE,
      n.channels = 2),
      "'plot.single' was deprecated in v1.0.0, use 'plot_singlePanels' instead")

  ## analyse_pIRIRSequence on an inconsistent object
  suppressWarnings( # ignore additional warnings from fit_DoseResponseCurve()
  expect_error(
      expect_warning(plot_DetPlot(
          object,
          signal_integral = 1:2,
          background_integral = 900:1000,
          analyse_function = "analyse_pIRIRSequence",
          analyse_function.control = list(
              fit.method = "LIN"),
          verbose = FALSE,
          n.channels = 1),
          "An error occurred, analysis skipped"),
      "No valid results produced")
  )

  ## integral_input
  set.seed(1)
  res1 <- plot_DetPlot(object, method = "shift",
                       signal_integral = c(0.04, 0.12),
                       background_integral = 34:40,
                       integral_input = "measurement",
                       analyse_function.control = list(
                           fit.method = "LIN",
                           trim_channels = TRUE),
                       n.channels = 3,
                       plot = FALSE)
  set.seed(1)
  res2 <- plot_DetPlot(object, method = "shift",
                       signal_integral = 1:3,
                       background_integral = 850:1000,
                       integral_input = "channel",
                       analyse_function.control = list(
                           fit.method = "LIN",
                           trim_channels = TRUE),
                       n.channels = 3,
                       plot = FALSE)
  res1@info <- res2@info <- list() # remove $call
  expect_equal(res1, res2)

  ## deprecated argument
  expect_warning(plot_DetPlot(
      object,
      method = "shift",
      signal.integral.min = 1,
      signal.integral.max = 3,
      background.integral.min = 900,
      background.integral.max = 1000,
      n.channels = 1),
      "were deprecated in v1.2.0, use 'signal_integral' and 'background_integral'")
  expect_warning(expect_error(plot_DetPlot(
      object,
      method = "shift",
      signal.integral.min = 1,
      signal.integral.max = 3,
      background.integral.min = 900,
      background.integral.max = 1000,
      integral_input = "measurement",
      n.channels = 1),
      "'integral_input' is not supported with old argument names"),
      "were deprecated in v1.2.0, use 'signal_integral' and 'background_integral'")
  })
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  set.seed(1)

  SW({
  vdiffr::expect_doppelganger("shift trim",
                              plot_DetPlot(object,
                                           method = "shift",
                                           signal_integral = 1:3,
                                           background_integral = 900:1000,
                                           analyse_function.control = list(
                                               fit.method = "LIN",
                                               trim_channels = TRUE
                                           ),
                                           n.channels = 3))
  vdiffr::expect_doppelganger("expansion",
                              plot_DetPlot(object,
                                           method = "expansion",
                                           signal_integral = 1:3,
                                           background_integral = 900:1000,
                                           analyse_function.control = list(
                                               fit.method = "LIN"),
                                           n.channels = 2))
  })
})
