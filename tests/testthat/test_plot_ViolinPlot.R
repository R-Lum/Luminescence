## load data
data(ExampleData.DeValues, envir = environment())
df <- ExampleData.DeValues$CA1

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_ViolinPlot(),
               "'data' should be of class 'RLum.Results', 'data.frame' or 'matrix'")
  expect_error(plot_ViolinPlot("error"),
               "'data' should be of class 'RLum.Results', 'data.frame' or 'matrix'")
  expect_error(plot_ViolinPlot(df, summary = 5),
               "'summary' should be of class 'character'")
  expect_error(plot_ViolinPlot(df, summary.pos = list()),
               "'summary.pos' should be of class 'numeric' or 'character'")
  expect_error(plot_ViolinPlot(df, summary.pos = 5),
               "'summary.pos' should have length 2")
  expect_error(plot_ViolinPlot(df, summary.pos = "error"),
               "'summary.pos' should be one of 'sub', 'left', 'center', 'right'")

  expect_error(plot_ViolinPlot(data.frame()),
               "'data' cannot be an empty data.frame")
  expect_error(plot_ViolinPlot(df[0, ]),
               "'data' cannot be an empty data.frame")
  expect_warning(plot_ViolinPlot(df[1, ]),
                 "Single data point found, no density calculated")
  expect_warning(plot_ViolinPlot(df, summary = "error"),
                 "Only keywords for weighted statistical measures are supported")

  ## missing values
  df.na <- df
  df.na[, 1] <- NA
  expect_warning(expect_warning(
      plot_ViolinPlot(df.na),
      "62 NA values removed"),
      "After NA removal, nothing is left from the data set")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## data.frame
  expect_silent(plot_ViolinPlot(df, summary.pos = "topleft"))

  ## matrix
  expect_silent(plot_ViolinPlot(as.matrix(df)))

  ## RLum.Results object
  expect_silent(plot_ViolinPlot(calc_CommonDose(df, plot = FALSE,
                                                verbose = FALSE)))
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("ViolinPlot defaults",
                              plot_ViolinPlot(df))
  vdiffr::expect_doppelganger("ViolinPlot summary sub",
                              plot_ViolinPlot(df, summary.pos = "sub",
                                              summary = c("n", "serel", "kurtosis")))
  vdiffr::expect_doppelganger("ViolinPlot summary left",
                              plot_ViolinPlot(df, summary.pos = "left",
                                              summary = c("mean", "skewness")))
  })
})
