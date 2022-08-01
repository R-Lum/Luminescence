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

  ## simple test - unlogged
  expect_silent(
    plot_RadialPlot(
      data = df,
      centrality = 5,
      log.z = FALSE))

  ## simple test - unlogged
  expect_silent(
    plot_RadialPlot(
      data = df,
      centrality = -1,
      log.z = FALSE))

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
