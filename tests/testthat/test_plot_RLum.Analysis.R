context("plot_RLum.Analysis")

test_that("Test the basic plot functionality", {
  testthat::skip_on_cran()

  ##create dataset
  ##load data
  data(ExampleData.BINfileData, envir = environment())

  ##convert values for position 1
  temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

  ##Basic plot
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = TRUE,
    norm = TRUE,
    abline = list(v = c(110))
  ))

  ##test arguments
  ##ylim
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = FALSE,
    norm = TRUE,
    ylim = c(1,100),
    xlim = c(1,100),
    abline = list(v = c(110))
  ))

  ##test arguments
  ##ylim - warning
  expect_warning(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = FALSE,
    norm = TRUE,
    ylim = c(1,200),
    xlim = c(1,100),
    abline = list(v = c(110))
  ))

  ##test arguments
  ##ylim - warning
  expect_warning(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = FALSE,
    norm = TRUE,
    log = "y"
  ))




})
