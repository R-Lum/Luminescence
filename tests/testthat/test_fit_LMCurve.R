## load data
data(ExampleData.FittingLM, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fit_LMCurve("error"),
               "'values' should be of class 'data.frame' or 'RLum.Data.Curve'")
  expect_error(fit_LMCurve(data.frame()),
               "'values' cannot be an empty data.frame")
  expect_error(fit_LMCurve(set_RLum("RLum.Data.Curve")),
               "recordType should be 'RBR' or 'LM-OSL'")
  expect_error(fit_LMCurve(set_RLum("RLum.Data.Curve", recordType = "OSL")),
               "recordType should be 'RBR' or 'LM-OSL'")
  expect_error(fit_LMCurve(values.curve, values.bg = "error"),
               "'values.bg' should be of class 'data.frame' or 'RLum.Data.Curve'")
  expect_error(fit_LMCurve(set_RLum("RLum.Data.Curve", recordType = "LM-OSL"),
                           values.bg = values.curveBG),
               "Lengths of 'values' and 'values.bg' differ")
  expect_error(fit_LMCurve(values.curve, values.bg = values.curveBG,
                           bg.subtraction = "error"),
               "'bg.subtraction' should be one of 'polynomial', 'linear' or")
  expect_error(fit_LMCurve(values.curve, n.components = "error"),
               "'n.components' should be a positive integer scalar")
  expect_error(fit_LMCurve(values.curve, input.dataType = "error"),
               "'input.dataType' should be one of 'LM' or 'pLM'")
  expect_error(fit_LMCurve(values.curve, fit.method = "error"),
               "'fit.method' should be one of 'port' or 'LM'")
  expect_error(fit_LMCurve(
    values = values.curve,
    values.bg = set_RLum("RLum.Data.Curve", data = as.matrix(values.curve), recordType = "OSL"),
    verbose = FALSE),
               "'recordType' for values.bg should be 'RBR'!")

  ## warning for failed confint ...skip on windows because with R >= 4.2 is does not fail anymore
  SW({
  if (!grepl(pattern = "mingw", sessionInfo()$platform) && !grepl(pattern = "linux", sessionInfo()$platform))
    expect_warning(fit_LMCurve(values = values.curve, fit.calcError = TRUE))
  })
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-4

  SW({
  set.seed(1)
  fit <- fit_LMCurve(values.curve, values.bg = values.curveBG,
                     n.components = 3, log = "x",
                     method_control = list(
                         export.comp.contrib.matrix = TRUE),
                     start_values = data.frame(Im = c(170,25,400),
                                               xm = c(56,200,1500)))
  })
  expect_snapshot_RLum(fit, tolerance = snapshot.tolerance)

  SW({
  fit2 <- fit_LMCurve(values = values.curve,
                      values.bg = values.curveBG,
                      n.components = 3,
                      log = "x",
                      fit.method = "LM",
                      method_control = list(
                          export.comp.contrib.matrix = TRUE),
                      plot = FALSE)
  })
  expect_snapshot_RLum(fit2, tolerance = snapshot.tolerance)

  SW({
  expect_message(fit <- fit_LMCurve(values.curve, values.bg = values.curveBG,
                                    start_values = data.frame(Im = c(70,25,400),
                                                              xm = c(56,200,10))),
                 "Error: Fitting failed, plot without fit produced")
  expect_equal(fit@data$component_matrix, NA)

  set.seed(1)
  expect_snapshot_RLum(fit_LMCurve(values.curve, values.bg = values.curveBG,
                                   method_control = list(
                                       export.comp.contrib.matrix = TRUE),
                                   plot.BG = TRUE, bg.subtraction = "linear"),
                       tolerance = snapshot.tolerance)

  suppressWarnings(
      expect_warning(fit_LMCurve(values.curve, values.bg = values.curveBG,
                                 fit.advanced = TRUE, fit.calcError = TRUE),
                 "The computation of the parameter confidence intervals failed")
  )

  ## more coverage
  curveBG <- set_RLum("RLum.Data.Curve", data = as.matrix(values.curveBG), recordType = "RBR")
  expect_warning(fit_LMCurve(values.curve, curveBG, xlab = "s", ylab = "a.u.",
                             xlim = c(0, 4000), log = "xy"),
                 "'xlim' changed to avoid 0 values for log-scale")
  })
  expect_message(fit_LMCurve(values.curve[1:15, ], main = ""),
                 "Fitting failed, plot without fit produced")
  pdf(tempfile(), width = 1, height = 1)
  expect_message(fit_LMCurve(values.curve, verbose = FALSE),
                 "Figure margins too large or plot area too small")

  SW({
  skip_on_os("mac")
  expect_snapshot_RLum(fit_LMCurve(values.curve, values.bg = values.curveBG,
                                   xlim = c(0, 4000), ylim = c(0, 600), cex = 0.9,
                                   method_control = list(
                                       export.comp.contrib.matrix = TRUE),
                                   fit.calcError = TRUE),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(fit_LMCurve(values.curve, values.bg = values.curveBG,
                                   plot.BG = TRUE, input.dataType = "pLM",
                                   method_control = list(
                                       export.comp.contrib.matrix = TRUE),
                                   bg.subtraction = "channel"),
                       tolerance = snapshot.tolerance)
  skip_on_os("windows")
  set.seed(1)
  expect_snapshot_RLum(fit_LMCurve(values.curve, values.bg = values.curveBG,
                                   plot.BG = TRUE, fit.advanced = TRUE),
                       tolerance = snapshot.tolerance)
  })
})
