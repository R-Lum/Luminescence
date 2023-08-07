test_that("dedicated test for the radialplot", {
  testthat::skip_on_cran()
  local_edition(3)

  ##distribution plots
  set.seed(12310)
  x <- rnorm(30,5,0.5)
  y <- x * runif(30, 0.05, 0.10)
  df <- data.frame(x,y)

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

  ## trigger stop
  expect_error(
    plot_RadialPlot(
      data = df,
      centrality = "error"),
    "\\[plot\\_RadialPlot\\(\\)\\] Measure of centrality not supported\\!")

})
