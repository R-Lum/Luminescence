## load data
set.seed(1)
data(ExampleData.DeValues, envir = environment())
df <- ExampleData.DeValues$BT998[7:11,]
df.list <- list(df, df * c(runif(5, 0.9, 1.1), 1))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_DRTResults("error"),
               "'values' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(plot_DRTResults(list("error")),
               "'values' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(plot_DRTResults(df, preheat = c(200, 240, 240)),
               "'preheat' should have length equal to the number of De values")
  expect_error(plot_DRTResults(df, given.dose = "error"),
               "'given.dose' should be of class 'numeric'")
  expect_error(plot_DRTResults(df, given.dose = numeric(0)),
               "'given.dose' cannot be an empty numeric")
  expect_error(plot_DRTResults(df, given.dose = c(2800, 3000)),
               "'given.dose' should have length equal to the number of input")
  expect_warning(plot_DRTResults(df, boxplot = TRUE),
                 "Option 'boxplot' requires a value in 'preheat'")
  expect_error(plot_DRTResults(df, summary = 5),
               "'summary' should be of class 'character'")
  expect_error(plot_DRTResults(df, summary.pos = list()),
               "'summary.pos' should be of class 'numeric' or 'character'")
  expect_error(plot_DRTResults(df, summary.pos = 5),
               "'summary.pos' should have length 2")
  expect_error(plot_DRTResults(df, summary.pos = "error"),
               "'summary.pos' should be one of 'sub', 'left', 'center', 'right'")

  empty <- set_RLum("RLum.Results")
  expect_error(plot_DRTResults(empty),
               "'values' cannot be an empty RLum.Results")
  expect_error(plot_DRTResults(list()),
               "'values' cannot be an empty list")
  expect_error(plot_DRTResults(data.frame()),
               "'values' cannot be an empty data.frame")
  expect_error(plot_DRTResults(list(empty, empty)),
                     "No valid records in 'values'")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_silent(plot_DRTResults(df))
  expect_silent(plot_DRTResults(df, preheat = c(200, 200, 200, 240, 240)))
  expect_silent(plot_DRTResults(df, preheat = c(200, 200, 200, 240, 240),
                                boxplot = FALSE, given.dose = 2800))
  expect_silent(plot_DRTResults(df, preheat = c(200, 200, 200, 240, 240),
                                boxplot = TRUE, given.dose = 2800,
                                summary = "mean", summary.pos = "sub"))
  expect_silent(plot_DRTResults(df.list, given.dose = c(2800, 2900)))
  expect_silent(plot_DRTResults(df.list, preheat = c(200, 200, 200, 240, 240),
                                boxplot = TRUE, summary.pos = "bottom"))

  ## more coverage
  expect_silent(plot_DRTResults(df[1, ], preheat = 200, boxplot = TRUE))
  expect_silent(plot_DRTResults(df[1, ], preheat = 200, boxplot = TRUE,
                                given.dose = 0,
                                summary.pos = "sub", col = 3))
  expect_silent(plot_DRTResults(df[1, ], boxplot = FALSE,
                                summary.pos = "sub", pch = 12, col = 1))
  expect_silent(plot_DRTResults(df, given.dose = 2800,
                                main = "Title", mtext = "Example data",
                                xlim = c(0, 6), ylim = c(0.8, 1.2),
                                xlab = "x", ylab = "y",
                                summary = "mean", summary.pos = c(0, 1.2),
                                legend = "legend", legend.pos = c(5, 1.2),
                                col = 2, cex = 1, pch = 2))
  expect_silent(plot_DRTResults(df, summary = "n", summary.pos = "sub"))
  expect_silent(plot_DRTResults(df, summary.pos = "bottomright",
                                legend.pos = "top"))
  expect_silent(plot_DRTResults(df, preheat = 1:5, na.rm = TRUE))

  ## RLum.Results object
  expect_silent(plot_DRTResults(calc_CommonDose(df, plot = FALSE,
                                                verbose = FALSE)))

  ## missing values
  df.na <- df
  df.na[2, 1] <- NA
  expect_silent(plot_DRTResults(df, preheat = c(200, 200, 200, 240, 240)))
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("defaults",
                              plot_DRTResults(df))
  vdiffr::expect_doppelganger("summary sub",
                              plot_DRTResults(df, summary.pos = "sub",
                                              summary = c("n", "se.rel")))
  vdiffr::expect_doppelganger("summary left",
                              plot_DRTResults(df, summary.pos = "left",
                                              summary = c("mean", "sd.abs")))
  })
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 769
  expect_silent(plot_DRTResults(list(df, df * 2), preheat = rep(200, 5),
                                boxplot = TRUE))
})
