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
               "Input object is not of type 'RLum.Analysis'")

  expect_error(plot_RLum.Analysis(temp, nrows = -1),
               "'nrows' must be a positive scalar")
  expect_error(plot_RLum.Analysis(temp, ncols = -1),
               "'ncols' must be a positive scalar")

  expect_error(plot_RLum.Analysis(
      set_RLum("RLum.Analysis", records = list(c1@records[[1]],
                                               set_RLum("RLum.Data.Image"))),
      combine = TRUE),
      "Only 'RLum.Data.Curve' objects are allowed")

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

  expect_warning(plot_RLum.Analysis(c1, curve.transformation = "error"),
                 "Function for 'curve.transformation' is unknown")
  expect_warning(.warningCatcher(
      plot_RLum.Analysis(temp, subset = list(recordType = "TL"),
                         norm = TRUE, log = "y")),
      "12 y values <= 0 omitted from logarithmic plot")

  expect_warning(plot_RLum.Analysis(c1, combine = TRUE, main = "Curve"),
                  "Nothing to combine, object contains a single curve")
})

test_that("Test the basic plot functionality", {
  testthat::skip_on_cran()

  ## trigger warning
  expect_silent(expect_warning(plot_RLum.Analysis(
    set_RLum("RLum.Analysis", records = list(
      c1@records[[1]],
      set_RLum("RLum.Data.Curve", recordType = "OSL")
      )), norm = TRUE, combine = TRUE),
    "[plot_RLum.Analysis()] Normalisation led to Inf or NaN values, values replaced by 0",
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
    abline = list(v = c(110))
  ))

  ##test arguments
  ##ylim
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = FALSE,
    norm = TRUE,
    ylim = c(1,100),
    xlim = c(1,100),
    abline = list(v = c(110))
  ))
})
