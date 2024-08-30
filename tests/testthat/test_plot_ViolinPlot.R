data(ExampleData.DeValues, envir = environment())
df <- ExampleData.DeValues$CA1

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_ViolinPlot(),
               "data input needed")
  expect_error(plot_ViolinPlot(df, summary.pos = 5),
               "'summary.pos' needs to be of type character")

  expect_warning(plot_ViolinPlot(df[0, ]),
                 "it is rather hard to plot 0 values, returning")
  expect_warning(plot_ViolinPlot(df[1, ]),
                 "Single data point found, no density calculated")
  expect_warning(plot_ViolinPlot(df, summary = "error"),
                 "Only keywords for weighted statistical measures are supported")
  expect_warning(plot_ViolinPlot(df, summary.pos = "error"),
                 "Value provided for 'summary.pos' is not a valid keyword")
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

  ## missing values
  df.na <- df
  df.na[10:11, 1] <- NA
  expect_warning(plot_ViolinPlot(df.na),
                 "2 NA values removed")
})
