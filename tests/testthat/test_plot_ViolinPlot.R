## load data
data(ExampleData.DeValues, envir = environment())
df <- ExampleData.DeValues$CA1

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_ViolinPlot(),
               "'data' should be of class 'RLum.Results', 'data.frame' or 'matrix'")
  expect_error(plot_ViolinPlot("error"),
               "'data' should be of class 'RLum.Results', 'data.frame' or 'matrix'")
  expect_error(plot_ViolinPlot(df, summary.pos = 5),
               "'summary.pos' should be of class 'character'")

  expect_error(plot_ViolinPlot(data.frame()),
               "'data' cannot be an empty data.frame")
  expect_error(plot_ViolinPlot(df[0, ]),
               "'data' cannot be an empty data.frame")
  expect_warning(plot_ViolinPlot(df[1, ]),
                 "Single data point found, no density calculated")
  expect_warning(plot_ViolinPlot(df, summary = "error"),
                 "Only keywords for weighted statistical measures are supported")
  expect_warning(plot_ViolinPlot(df, summary.pos = "error"),
                 "Value provided for 'summary.pos' is not a valid keyword")

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
