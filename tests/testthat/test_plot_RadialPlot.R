## load data
data(ExampleData.DeValues, envir = environment())

set.seed(12310)
x <- rnorm(30, 5, 0.5)
y <- x * runif(30, 0.05, 0.10)
df <- data.frame(x, y)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_RadialPlot("error"),
               "All elements of 'data' should be of class 'data.frame' or")
  expect_error(plot_RadialPlot(list()),
               "'data' cannot be an empty list")
  expect_error(plot_RadialPlot(df[, 1]),
               "All elements of 'data' should be of class 'data.frame' or")
  expect_error(plot_RadialPlot(df[0, ]),
               "'data' cannot be an empty data.frame")
  expect_error(plot_RadialPlot(list(df[0, ])),
               "Input 'data[[1]]' cannot be an empty data.frame",
               fixed = TRUE)
  expect_error(plot_RadialPlot(data.frame(NA, 1:5)),
               "After NA removal, nothing is left from data set 1")
  expect_error(plot_RadialPlot(data.frame(1, 3)),
               "At least two data points are required")
  expect_error(plot_RadialPlot(df, xlab = "x"),
               "'xlab' should have length 2")
  expect_error(plot_RadialPlot(df, centrality = list("error")),
               "'centrality' should be of class 'character' or 'numeric'")
  expect_error(plot_RadialPlot(df, centrality = "error"),
               "'centrality' should be one of 'mean', 'mean.weighted', 'median'")
  expect_error(plot_RadialPlot(df, summary = 5),
               "'summary' should be of class 'character'")
  expect_error(plot_RadialPlot(df, summary.pos = 5),
               "'summary.pos' should have length 2")
  expect_error(plot_RadialPlot(df, summary.pos = list()),
               "'summary.pos' should be one of 'sub', 'left', 'center', 'right'")
  expect_error(plot_RadialPlot(df, summary.pos = "error"),
               "'summary.pos' should be one of 'sub', 'left', 'center', 'right'")
  expect_error(plot_RadialPlot(df, line = c(NA, NA)),
               "'line' should be of class 'numeric', 'integer' or NULL")
  expect_error(plot_RadialPlot(df, zlim = 1),
               "'zlim' should have length 2")
  expect_error(plot_RadialPlot(list(df, df), lty = 1),
               "'lty' should have length 2")

  expect_warning(plot_RadialPlot(df, xlim = c(-1, 100), show = FALSE),
                 "Lower x-axis limit not set to zero, corrected")
  expect_warning(plot_RadialPlot(ExampleData.DeValues, log.z = FALSE,
                                 xlim = c(0, 5), zlim = c(100, 200),
                                 show = FALSE),
                 "'log.z' is set to 'FALSE' altough more than one data set")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## standard data
  expect_silent(
  plot_RadialPlot(
    data = df,
    centrality = 5))

  ## standard data with two datasets
  expect_silent(
    plot_RadialPlot(
      data = list(df, df),
      centrality = c(5,5)))

  ## simple test - unlogged
  expect_silent(
    plot_RadialPlot(
      data = df,
      centrality = 5,
      log.z = FALSE))

  ## simple test - unlogged with statistics
  expect_silent(
    plot_RadialPlot(
      data = df,
      summary = c(
        "n", "mean", "median", "mean.weighted", "median.weighted", "kdemax", "sdabs",
        "sdrel", "seabs", "serel", "skewness", "kurtosis", "in.2s", "sdabs.weighted",
        "sdrel.weighted", "seabs.weighted", "serel.weighted"),
      log.z = FALSE))

  ## simple test - unlogged
  expect_silent(
    plot_RadialPlot(
      data = df,
      centrality = -1,
      log.z = FALSE))

  ## single-column data frame
  expect_message(plot_RadialPlot(data.frame(x = c(-0.1, -1.2, 10))),
                 "Attention, small standardised estimate scatter")

  ## data frame with more than 2 columns
  expect_silent(plot_RadialPlot(cbind(df, df)))

  ## data frame with negative values
  df.neg <- df
  df.neg[, 1] <- df.neg[, 1] - 5
  plot_RadialPlot(df.neg)

  ## data frame with zeros
  df.zeros <- data.frame(ED = c(rep(0, 4), 10),
                         ED_Error = rnorm(5) + 3)
  expect_silent(plot_RadialPlot(df.zeros))
  expect_silent(plot_RadialPlot(df.zeros, zlim = c(5, 10),
                                centrality = "median.weighted"))

  ## more coverage
  expect_type(plot_RadialPlot(df, main = "Title", sub = "Subtitle", rug = TRUE,
                              centrality = "mean", log.z = TRUE,
                              stats = c("min", "max", "median"),
                              summary = "mean", summary.pos = c(0, 40),
                              legend = "Data", legend.pos = c(4, 10),
                              xlab = c("x1", "x2"), xlim = c(0, 20),
                              ylab = "y", ylim = c(-10, 10),
                              zlab = "z", zlim = c(3, 7),
                              line = c(3.5, 5.5), y.ticks = FALSE,
                              cex = 0.8, lty = 2, lwd = 2, pch = 2, col = 2,
                              tck = 1, tcl = 2),
              "list")

  plot_RadialPlot(df, show = FALSE, centrality = c(1, 2, 3))
  plot_RadialPlot(df, show = FALSE, centrality = "median",
                  summary.pos = "topleft", legend.pos = "topright",
                  log.z = FALSE, rug = TRUE)

  ## RLum.Results object
  expect_silent(plot_RadialPlot(calc_CommonDose(ExampleData.DeValues$BT998,
                                                verbose = FALSE)))

  # Messages,  Warnings, and Errors -----------------------------------------
  ## trigger message
  expect_message(
    plot_RadialPlot(
      data = data.frame(x = df$x, y = rep(0.0001, nrow(df))),
      centrality = -1,
      log.z = FALSE),
    regexp = "Attention.*")

  expect_message(plot_RadialPlot(df, line = -1),
                 "Lines with negative value skipped due to 'log.z = TRUE'")

  ## trigger warning
  expect_warning(plot_RadialPlot(
      data = df,
      #centrality = ,
      central.value = -1,
      log.z = FALSE),
      "z-scale touches 2s-polygon, decrease plot ratio"
    )
  expect_silent(plot_RadialPlot(df, central.value = -1, log.z = FALSE,
                                bar.col = "none"))
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  vdiffr::expect_doppelganger("RadialPlot defaults",
                              plot_RadialPlot(df, centrality = 6))
  vdiffr::expect_doppelganger("RadialPlot summary sub",
                              plot_RadialPlot(df, summary.pos = "sub",
                                              stat = c("min", "max"),
                                              summary = c("n", "se.rel", "kurtosis")))
  vdiffr::expect_doppelganger("RadialPlot summary left",
                              plot_RadialPlot(df, summary.pos = "left",
                                              summary = c("mean", "in.2s", "skewness")))
  vdiffr::expect_doppelganger("central value xlim zlim pch",
                              plot_RadialPlot(ExampleData.DeValues$CA1,
                                              central.value = 69.9,
                                              xlim = c(0, 16),
                                              zlim = c(15, 143),
                                              pch = 1,
                                              summary = ""))
  vdiffr::expect_doppelganger("regression 1044",
                              plot_RadialPlot(ExampleData.DeValues$CA1,
                                              xlim = c(0, 21.2),
                                              zlim = c(23, 135.1),
                                              summary = ""))
  vdiffr::expect_doppelganger("regression 1060",
                              plot_RadialPlot(ExampleData.DeValues$CA1,
                                              xlim = c(0, 30),
                                              zlim = c(5, 1000),
                                              plot.ratio = 0))
  df2 <- data.frame(x = df$x - 1, y = df$y * 0.75)
  vdiffr::expect_doppelganger("RadialPlot list",
                              plot_RadialPlot(list(df, df2),
                                              centrality = "median.weighted",
                                              summary = c("n", "in.2s", "median.weighted"),
                                              rug = TRUE, col = c(2, 3)))
  })
})

test_that("regression tests", {
    testthat::skip_on_cran()

    ## issue 1140
    expect_silent(plot_RadialPlot(df, zlim = c(0, 100)))
})
