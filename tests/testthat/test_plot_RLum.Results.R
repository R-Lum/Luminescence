data(ExampleData.DeValues, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_RLum.Results("error"),
               "Input object is not of type 'RLum.Results'")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## calc_MinDose
  d1 <- calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                     bootstrap = TRUE, bs.M = 10, bs.N = 10,
                     plot = FALSE, verbose = FALSE)
  expect_silent(plot_RLum.Results(d1, main = "Title"))
  expect_silent(plot_RLum.Results(d1, single = FALSE, log = "", lty = 1,
                                  type = "l", col = 2))

  ## calc_CentralDose
  d2 <-calc_CentralDose(ExampleData.DeValues$CA1,
                        plot = FALSE, verbose = FALSE)
  expect_silent(plot_RLum.Results(d2))

  ## calc_FuchsLang2001
  d3 <- calc_FuchsLang2001(ExampleData.DeValues$BT998, cvThreshold = 5,
                           plot = FALSE, verbose = FALSE)
  expect_silent(plot_RLum.Results(d3))
  expect_silent(plot_RLum.Results(d3, main = "Title", sub = "Subtitle",
                                  xlab = "x", xlim = c(2500, 4000),
                                  ylab = "y", ylim = c(0, 25), mtext = "",
                                  cex = 1, lwd = 1, pch = 1))

  ## calc_FiniteMixture
  d4 <- calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.2,
                           n.components = c(2:4),
                           dose.scale = c(0, 100),
                           plot = FALSE, verbose = FALSE)
  expect_silent(plot_RLum.Results(d4, pdf.colors = "colors"))
  expect_silent(plot_RLum.Results(d4, main = "Title", plot.proportions = FALSE,
                                  pdf.weight = FALSE, pdf.sigma = "sigmab"))

  ## calc_AliquotSize
  d5 <- calc_AliquotSize(grain.size = c(100, 150), sample.diameter = 1,
                         MC.iter = 100, plot = FALSE, verbose = FALSE)
  expect_silent(plot_RLum.Results(d5))

  ## calc_SourceDoseRate
  d6 <- calc_SourceDoseRate(measurement = "2012-01-27", calib = "2014-12-19",
                            calib.dose.rate = 0.0438, calib.error = 0.0019)
  expect_silent(plot_RLum.Results(d6))

  ## calc_FastRatio
  data(ExampleData.CW_OSL_Curve, envir = environment())
  d7 <- calc_FastRatio(ExampleData.CW_OSL_Curve, plot = FALSE, verbose = FALSE)
  expect_silent(plot_RLum.Results(d7))
})
