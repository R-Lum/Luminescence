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
               "Number of preheat temperatures != De values")
  expect_error(plot_DRTResults(df, given.dose = c(2800, 3000)),
               "'given.dose' > number of input data sets")
  expect_warning(plot_DRTResults(df, boxplot = TRUE),
                 "Option 'boxplot' requires a value in 'preheat'")

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
                                boxplot = TRUE, given.dose = 2800))
  expect_silent(plot_DRTResults(df, preheat = c(200, 200, 200, 240, 240),
                                boxplot = TRUE, given.dose = 2800,
                                summary = "mean", summary.pos = "sub"))
  expect_silent(plot_DRTResults(df.list, given.dose = c(2800, 2900)))
  expect_silent(plot_DRTResults(df.list, preheat = c(200, 200, 200, 240, 240),
                                boxplot = TRUE))

  ## more coverage
  expect_silent(plot_DRTResults(df, given.dose = 2800,
                                main = "Title", mtext = "Example data",
                                xlim = c(0, 6), ylim = c(0.8, 1.2),
                                xlab = "x", ylab = "y",
                                summary = "mean", summary.pos = c(0, 1.2),
                                legend = "legend", legend.pos = c(5, 1.2),
                                col = 2, cex = 1, pch = 2))
  expect_silent(plot_DRTResults(df, summary = "n", summary.pos = "sub"))
  expect_silent(plot_DRTResults(df, summary.pos = "top",
                                legend.pos = "bottom"))
  expect_silent(plot_DRTResults(df, summary.pos = "topright",
                                legend.pos = "topleft"))
  expect_silent(plot_DRTResults(df, summary.pos = "left",
                                legend.pos = "right"))
  expect_silent(plot_DRTResults(df, summary.pos = "center",
                                legend.pos = "center"))
  expect_silent(plot_DRTResults(df, summary.pos = "right",
                                legend.pos = "left"))
  expect_silent(plot_DRTResults(df, summary.pos = "bottomleft",
                                legend.pos = "bottomright"))
  expect_silent(plot_DRTResults(df, summary.pos = "bottom",
                                legend.pos = "top"))
  expect_silent(plot_DRTResults(df, summary.pos = "bottomright",
                                legend.pos = "bottomleft"))
  expect_silent(plot_DRTResults(df, preheat = 1:5, na.rm = TRUE))

  ## plot_DRTResults(df.list, preheat = c(200, 200, 200, 240, 240),
  ##                 given.dose = 2800, boxplot = TRUE)

  ## RLum.Results object
  expect_silent(plot_DRTResults(calc_CommonDose(df, plot = FALSE,
                                                verbose = FALSE)))

  ## missing values
  df.na <- df
  df.na[2, 1] <- NA
  expect_silent(plot_DRTResults(df, preheat = c(200, 200, 200, 240, 240)))
})
