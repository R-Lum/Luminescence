## load data
data(ExampleData.LxTxData, envir = environment())
set.seed(1)
fit <- fit_DoseResponseCurve(LxTxData, verbose = FALSE)
fit.extra.gok <- fit_DoseResponseCurve(LxTxData, mode = "extrapolation",
                                       fit.method = "GOK", verbose = FALSE)
fit.alt <- fit_DoseResponseCurve(LxTxData, mode = "alternate",
                                 verbose = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(
      plot_DoseResponseCurve("error"),
      "[plot_DoseResponseCurve()] 'object' should be of class 'RLum.Results'",
      fixed = TRUE)
  expect_error(
      plot_DoseResponseCurve(set_RLum("RLum.Results", originator = "error")),
      "'object' has an unsupported originator")
  expect_error(
      plot_DoseResponseCurve(fit, plot_extended = "error"),
      "'plot_extended' should be a single logical value")
  expect_error(
      plot_DoseResponseCurve(fit, plot_singlePanels = "error"),
      "'plot_singlePanels' should be a single logical value")
  expect_error(
      plot_DoseResponseCurve(fit, verbose = "error"),
      "'verbose' should be a single logical value")
  expect_message(
      plot_DoseResponseCurve(fit, reg_points_pch = 1),
      "'reg_points_pch' should have length 3")

  ## there's nothing we can do for this, apart adding a useless 'plot' argument
  expect_error(plot_DoseResponseCurve(fit, plot = NA),
               "argument 2 matches multiple formal arguments")
  })

test_that("plot output", {
  testthat::skip_on_cran()

  ## standard return
  expect_s4_class(plot_DoseResponseCurve(fit), "RLum.Results")

  ## check plot settings
  expect_s4_class(plot_DoseResponseCurve(fit, legend = FALSE,
                                         reg_points_pch = c(19, 1, 2),
                                         density_polygon = FALSE,
                                         box = FALSE), "RLum.Results")
  expect_message(plot_DoseResponseCurve(fit.extra.gok, log = "x"),
                 "Logarithmic transformation not allowed on an object fitted with")
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  vdiffr::expect_doppelganger("default",
                              plot_DoseResponseCurve(fit))
  vdiffr::expect_doppelganger("log-xy",
                              plot_DoseResponseCurve(fit, log = "xy"))
  vdiffr::expect_doppelganger("extrapolation-gok",
                              plot_DoseResponseCurve(fit.extra.gok))
  vdiffr::expect_doppelganger("alternate",
                              plot_DoseResponseCurve(fit.alt))
  vdiffr::expect_doppelganger("cex.global",
                              plot_DoseResponseCurve(fit, legend = FALSE,
                                                     density_polygon_col = "azure",
                                                     cex = 2))
  vdiffr::expect_doppelganger("drc",
                              plot_DoseResponseCurve(fit, legend = FALSE,
                                                     lwd_drc = 3,
                                                     col_drc = "green",
                                                     lty_drc = 3,
                                                     cex = 2))
  vdiffr::expect_doppelganger("rlum.results",
                              plot_RLum.Results(fit, main = "plot_RLum.Results"))

  ## De is NA
  df <- data.frame(DOSE = c(0, 5, 10, 20, 30),
                   LxTx = c(10, 5, -20, -30, -40),
                   LxTx_X = c(1, 1, 1, 1, 1))
  vdiffr::expect_doppelganger("De.NA",
                              plot_DoseResponseCurve(fit_DoseResponseCurve(df)))
  })

  ## graphical snapshots that also check numerical correctness
  data(ExampleData.LxTxData, envir = environment())
  LxTxData$LxTx.Error[[5]] <- 0.8
  LxTxData$LxTx.Error[[4]] <- 0.4

  SW({
  set.seed(1234)
  for (var in c("NULL", "inverse_var", "inverse_std", "norm_inverse_std")) {
    vdiffr::expect_doppelganger(var,
                                fit_DoseResponseCurve(
                                    object = LxTxData,
                                    fit.method = "EXP",
                                    fit.weights = if (var == "NULL") NULL else var) |>
                                plot_DoseResponseCurve(plot_extended = FALSE,
                                                       main = var,
                                                       plot_singlePanels = TRUE))
  }
  })

  ## from analyse_SAR.CWOSL
  data(ExampleData.BINfileData, envir = environment())
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 4)
  SW({
  vdiffr::expect_doppelganger("analyse_SAR.CWOSL",
                              analyse_SAR.CWOSL(object, signal_integral = 1:4,
                                                background_integral = 100:200,
                                                plot = FALSE) |>
                              plot_DoseResponseCurve())
  })
})
