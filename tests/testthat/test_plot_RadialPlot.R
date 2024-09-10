data(ExampleData.DeValues, envir = environment())

set.seed(12310)
x <- rnorm(30, 5, 0.5)
y <- x * runif(30, 0.05, 0.10)
df <- data.frame(x, y)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_RadialPlot("error"),
               "Input data must be 'data.frame' or 'RLum.Results'")
  expect_error(plot_RadialPlot(list()),
               "'data' is an empty list")
  expect_error(plot_RadialPlot(df[, 1]),
               "Input data must be 'data.frame' or 'RLum.Results'")
  expect_error(plot_RadialPlot(df[0, ]),
               "Input data 1 has 0 rows")
  expect_error(plot_RadialPlot(df, xlab = "x"),
               "'xlab' must have length 2")
  expect_error(plot_RadialPlot(df, centrality = "error"),
               "Measure of centrality not supported")

  expect_warning(plot_RadialPlot(df, xlim = c(-1, 100), show = FALSE),
                 "Lower x-axis limit not set to zero, corrected")
  expect_warning(plot_RadialPlot(ExampleData.DeValues, log.z = FALSE,
                                 xlim = c(0, 5), zlim = c(100, 200),
                                 show = FALSE),
                 "Option 'log.z' is not set to 'TRUE' altough more than one")
})

test_that("dedicated test for the radialplot", {
  testthat::skip_on_cran()

  ##distribution plots

  ## standard data
  ## simple test
  expect_silent(
  plot_RadialPlot(
    data = df,
    centrality = 5))

  ## standard data with two datasets
  ## simple test
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
  expect_message(plot_RadialPlot(df[, 1, drop = FALSE]),
                 "Attention, small standardised estimate scatter")

  ## data frame with more than 2 columns
  expect_silent(plot_RadialPlot(cbind(df, df)))

  ## data frame with negative values
  df.neg <- df
  df.neg[, 1] <- df.neg[, 1] - 5
  plot_RadialPlot(df.neg)

  ## more coverage
  expect_type(plot_RadialPlot(df, main = "Title", sub = "Subtitle", rug = TRUE,
                              centrality = "mean", log.z = TRUE,
                              stats = c("min", "max", "median"),
                              summary = "mean", summary.pos = c(0, 40),
                              legend = TRUE, legend.pos = c(4, 40),
                              xlab = c("x1", "x2"), xlim = c(0, 20),
                              ylab = "y", ylim = c(-10, 10),
                              zlab = "z", zlim = c(3, 7),
                              line = c(3.5, 5.5), y.ticks = FALSE,
                              cex = 0.8, lty = 2, lwd = 2, pch = 2, col = 2,
                              tck = 1, tcl = 2, output = TRUE),
              "list")

  plot_RadialPlot(df, show = FALSE, centrality = c(1, 2, 3))
  plot_RadialPlot(df, show = FALSE, centrality = "median",
                  summary.pos = "topleft", legend.pos = "topright",
                  log.z = FALSE, rug = TRUE)
  plot_RadialPlot(df, show = FALSE, centrality = "median.weighted",
                  summary.pos = "top", legend.pos = "bottom")
  plot_RadialPlot(df, show = FALSE,
                  summary.pos = "topright", legend.pos = "topleft")
  plot_RadialPlot(df, show = FALSE,
                  summary.pos = "left", legend.pos = "right")
  plot_RadialPlot(df, show = FALSE,
                  summary.pos = "center", legend.pos = "center")
  plot_RadialPlot(df, show = FALSE,
                  summary.pos = "right", legend.pos = "left")
  plot_RadialPlot(df, show = FALSE,
                  summary.pos = "bottomleft", legend.pos = "bottomright")
  plot_RadialPlot(df, show = FALSE,
                  summary.pos = "bottom", legend.pos = "top")
  plot_RadialPlot(df, show = FALSE,
                  summary.pos = "bottomright", legend.pos = "bottomleft")

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

  ## trigger warning
  expect_warning(plot_RadialPlot(
      data = df,
      #centrality = ,
      central.value = -1,
      log.z = FALSE),
      "\\[plot\\_RadialPlot\\(\\)\\] z-scale touches.*"
    )
})
