## load data
data(ExampleData.BINfileData, envir = environment())
data(ExampleData.XSYG, envir = environment())

## convert values for position 1
temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)

## only one curve
c1 <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1,
                                      run = 1, set = 6)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_RLum.Analysis("error"),
               "'object' should be of class 'RLum.Analysis'")

  expect_error(plot_RLum.Analysis(temp, nrows = -1),
               "'nrows' should be a positive scalar")
  expect_error(plot_RLum.Analysis(temp, ncols = -1),
               "'ncols' should be a positive scalar")
  expect_error(plot_RLum.Analysis(temp, combine = -1),
               "'combine' should be a single logical value")

  expect_error(plot_RLum.Analysis(
      set_RLum("RLum.Analysis", records = list(c1@records[[1]],
                                               set_RLum("RLum.Data.Image"))),
      combine = TRUE),
      "'combine' is valid only for 'RLum.Data.Curve' objects")

  ## this generates multiple warnings
  warnings <- capture_warnings(plot_RLum.Analysis(c1, col = 2,
                                                  xlim = c(-1, 50),
                                                  ylim = c(-1, 3000)))
  expect_match(warnings, all = FALSE, fixed = TRUE,
               "min('xlim') < x-value range for curve #1, reset to minimum")
  expect_match(warnings, all = FALSE, fixed = TRUE,
               "max('xlim') > x-value range for curve #1, reset to maximum")
  expect_match(warnings, all = FALSE, fixed = TRUE,
               "min('ylim') < y-value range for curve #1, reset to minimum")
  expect_match(warnings, all = FALSE, fixed = TRUE,
               "max('ylim') > y-value range for curve #1, reset to maximum")

  expect_error(plot_RLum.Analysis(c1, curve.transformation = "error"),
               "'curve.transformation' should be one of 'CW2pLM', 'CW2pLMi'")
  expect_warning(.warningCatcher(
      plot_RLum.Analysis(temp, subset = list(recordType = "TL"),
                         norm = TRUE, log = "y")),
      "12 y values <= 0 omitted from logarithmic plot")

  expect_warning(plot_RLum.Analysis(c1, combine = TRUE, main = "Curve"),
                 "'combine' can't be used with fewer than two curves")
  expect_warning(plot_RLum.Analysis(c1, plot.single = TRUE),
                  "'plot.single' is deprecated, use 'plot_singlePanels'")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## trigger warning
  expect_silent(expect_warning(plot_RLum.Analysis(
    set_RLum("RLum.Analysis", records = list(
      c1@records[[1]],
      set_RLum("RLum.Data.Curve", recordType = "OSL")
      )), norm = TRUE, combine = TRUE),
    "[plot_RLum.Analysis()] Curve normalisation produced Inf/NaN values",
    fixed = TRUE))

  ##Basic plot
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = TRUE,
    norm = TRUE,
    abline = list(v = c(110))
  ))

  ## Basic plot with spectrum
  expect_silent(
    plot_RLum.Analysis(
      set_RLum(class = "RLum.Analysis", records = list(TL.Spectrum, temp[[1]])),
      plot.type = "persp"))

  ## test norm = "max"
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = TRUE,
    norm = "max",
    abline = list(v = c(110))
  ))

  ## test norm = "min"
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "OSL"),
    combine = TRUE,
    norm = "last",
    abline = list(v = c(110))
  ))

  ## test norm = "autoscale"
  expect_silent(plot_RLum.Analysis(
    temp[1:4],
    subset = list(recordType = "OSL"),
    combine = TRUE,
    auto_scale = TRUE,
    xlim = c(10, 20),
    norm = "last",
    abline = list(v = c(110))
  ))

  ## test norm = "huot
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "OSL"),
    combine = TRUE,
    norm = "huot",
    abline = list(v = c(110))
  ))

  ## test records_max
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = TRUE,
    norm = TRUE,
    sub_title = "(5 K/s)",
    records_max = 5,
    smooth = TRUE,
    type = "p",
    abline = list(v = c(110)),
    ## more coverage
    main = "TL curves combined",
    log = "xy",
    col = get("col", pos = .LuminescenceEnv)[1:4],
    xlab = "Temperature recorded [log \u00B0C]", ylab = "log TL [a.u.]",
    xlim = c(0, 200), ylim = c(0, 1), lty = c(1, 2),
    legend.text = paste("Curve", 1:4),
    legend.col = get("col", pos = .LuminescenceEnv)[1:4],
    legend.pos = "outside",
  ))

  ##test arguments
  ##ylim
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = FALSE,
    norm = TRUE,
    ylim = c(0, 1),
    xlim = c(1,100),
    abline = list(v = c(110))
  ))

  ## curve transformation
  plot_RLum.Analysis(temp,
                     subset = list(recordType = "IRSL"),
                     curve.transformation = "CW2pLMi")

  plot_RLum.Analysis(temp,
                     subset = list(recordType = "OSL"),
                     combine = TRUE,
                     curve.transformation = "CW2pHMi")

  plot_RLum.Analysis(temp,
                     subset = list(recordType = "TL"),
                     curve.transformation = "CW2pPMi")

  ## empty object
  expect_silent(plot_RLum.Analysis(set_RLum("RLum.Analysis")))
  expect_warning(plot_RLum.Analysis(set_RLum("RLum.Analysis"), combine = TRUE),
                 "'combine' can't be used with fewer than two curves")
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("plot_RLum.Analysis",
                              plot_RLum.Analysis(
                                  temp,
                                  subset = list(recordType = "TL"),
                                  combine = TRUE,
                                  norm = TRUE,
                                  abline = list(v = 110)))
  vdiffr::expect_doppelganger("plot_RLum.Analysis persp",
                              plot_RLum.Analysis(
                                  set_RLum(class = "RLum.Analysis",
                                           records = list(TL.Spectrum, temp[[1]])),
                                  plot.type = "persp"))
  })
})
