## load data
data(ExampleData.LxTxData, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(
      plot_GrowthCurve("error"),
      "[fit_DoseResponseCurve()] 'object' should be of class 'data.frame'",
      fixed = TRUE)
  expect_error(
      plot_GrowthCurve(LxTxData, mode = "error"),
      "[fit_DoseResponseCurve()] 'mode' should be one of 'interpolation'",
      fixed = TRUE)
  expect_error(
      plot_GrowthCurve(LxTxData, fit.method = "error"),
      "'fit.method' should be one of 'LIN', 'QDR', 'EXP', 'EXP OR LIN'")
  expect_error(
      plot_GrowthCurve(LxTxData, output.plotExtended = "error"),
      "'output.plotExtended' should be a single logical value")
  expect_error(
      plot_GrowthCurve(LxTxData, plot_singlePanels = "error"),
      "'plot_singlePanels' should be a single logical value")
  expect_error(
      plot_GrowthCurve(LxTxData, verbose = "error"),
      "'verbose' should be a single logical value")

  ## Weird LxTx values
  LxTx <- structure(list(
    Dose = c(0, 250, 500, 750, 1000, 1500, 0, 500, 500),
    LxTx = c(1, Inf, 0, -Inf, Inf, 0, Inf, -0.25, 2),
    LxTx.Error = c(1.5813365, Inf, 0, Inf, Inf, 0, Inf, 1.4114626, 3.1626729)),
    class = "data.frame", row.names = c(NA, -9L))
  SW({
  expect_warning(
      plot_GrowthCurve(LxTx),
      "Inf values found, replaced by NA")
  })

  ## same dose for all points
  tmp_LxTx <- LxTxData
  tmp_LxTx$Dose <- 10
  SW({
  expect_message(expect_null(
      plot_GrowthCurve(tmp_LxTx)),
      "All points have the same dose, NULL returned")
  })

  ## only two columns
  expect_warning(
      plot_GrowthCurve(LxTxData[, 1:2], verbose = FALSE),
      "Error column invalid or 0, 'fit.weights' ignored")

  ## test case with all NA
  tmp_LxTx <- LxTxData
  tmp_LxTx$LxTx <- NA
  expect_message(expect_warning(expect_null(
      plot_GrowthCurve(tmp_LxTx, verbose = FALSE)),
      "7 NA values removed"),
      "Error: After NA removal, nothing is left from the data set")

  ## test case without TnTx column
  tmp_LxTx <- LxTxData
  tmp_LxTx$TnTx <- NULL
  expect_s4_class(
    plot_GrowthCurve(tmp_LxTx, verbose = FALSE),
    "RLum.Results")

  ## do not include reg point
  expect_s4_class(
    plot_GrowthCurve(
      sample = LxTxData,
      verbose = FALSE,
      fit.includingRepeatedRegPoints = FALSE),
    class = "RLum.Results")

  ## deprecated option
  expect_warning(
      plot_GrowthCurve(LxTxData, verbose = FALSE,
                       output.plotExtended.single = TRUE),
      "'output.plotExtended.single' is deprecated, use 'plot_singlePanels'")
})

test_that("main tests", {
  testthat::skip_on_cran()

  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "EXP",
                                 n.MC = 10))
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "LIN",
                                 n.MC = 10))
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "EXP+LIN",
                                 n.MC = 10))
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "EXP+EXP",
                                 n.MC = 10))
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "QDR",
                                 n.MC = 10))
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "GOK",
                                 n.MC = 10))
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "OTOR",
                                 n.MC = 10))

  ## force through the origin
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "EXP+LIN",
                                 fit.bounds = FALSE,
                                 fit.force_through_origin = TRUE,
                                 n.MC = 10))
  temp_LxTx <- LxTxData
  temp_LxTx$LxTx[[7]] <- 1
  expect_output(plot_GrowthCurve(temp_LxTx,
                                 fit.method = "GOK",
                                 fit.force_through_origin = TRUE,
                                 n.MC = 10))
})

test_that("additional tests", {
  testthat::skip_on_cran()

  LxTxData[1, 2:3] <- c(0.5, 0.001)

  ## Check extrapolation ----------------------------------------------------
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "LIN",
                                 mode = "extrapolation",
                                 n.MC = 10))
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "EXP",
                                 mode = "extrapolation",
                                 n.MC = 10))
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "EXP+LIN",
                                 mode = "extrapolation",
                                 n.MC = 10))
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "GOK",
                                 mode = "extrapolation",
                                 n.MC = 10))
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "OTOR",
                                 mode = "extrapolation",
                                 n.MC = 10))

  ## force through the origin
  expect_output(plot_GrowthCurve(LxTxData,
                                 fit.method = "QDR",
                                 mode = "extrapolation",
                                 fit.force_through_origin = TRUE,
                                 n.MC = 10))

  ## Check alternate --------------------------------------------------------
  expect_silent(plot_GrowthCurve(LxTxData,
                                 fit.method = "QDR",
                                 mode = "alternate",
                                 verbose = FALSE,
                                 n.MC = 10))
  expect_silent(plot_GrowthCurve(LxTxData,
                                 fit.method = "LIN",
                                 mode = "alternate",
                                 n.MC = 10))
  expect_silent(plot_GrowthCurve(LxTxData,
                                 fit.method = "EXP",
                                 mode = "alternate",
                                 n.MC = 10))
  expect_silent(plot_GrowthCurve(LxTxData,
                                 fit.method = "EXP+LIN",
                                 mode = "alternate",
                                 verbose = FALSE,
                                 n.MC = 10))
  expect_silent(plot_GrowthCurve(LxTxData,
                                 fit.method = "GOK",
                                 mode = "alternate",
                                 n.MC = 10))
  expect_silent(plot_GrowthCurve(LxTxData,
                                 fit.method = "OTOR",
                                 mode = "alternate",
                                 n.MC = 10))

  ## only two valid points provided
  SW({
  warnings <- capture_warnings(expect_message(plot_GrowthCurve(
    data.frame(
        dose = c(0, 1388.88888888889, NA),
        LxTx = c(1.54252220145258, 4.43951568403849, NA),
        LxTx_X = c(0.130074482379272, 2.59694106608, NA)),
    verbose = TRUE, NumberIterations.MC = 10),
    "'fit.method' changed to 'LIN'"))
  })
  expect_match(warnings, "1 NA values removed",
               all = FALSE, fixed = TRUE)
  expect_match(warnings, "Fitting a non-linear least-squares model requires",
               all = FALSE, fixed = TRUE)
  expect_match(warnings, "'NumberIterations.MC' is deprecated, use 'n.MC' instead",
               all = FALSE, fixed = TRUE)
})
