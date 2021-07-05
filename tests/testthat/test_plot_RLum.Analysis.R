test_that("Test the basic plot functionality", {
  testthat::skip_on_cran()
  local_edition(3)

  ##create dataset
  ##load data
  data(ExampleData.BINfileData, envir = environment())

  ##convert values for position 1
  temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

  ## trigger warning
  expect_silent(suppressWarnings(plot_RLum.Analysis(
    set_RLum("RLum.Analysis", records = list(
      set_RLum("RLum.Data.Curve", recordType = "OSL"),
      set_RLum("RLum.Data.Curve", recordType = "OSL")
      )), norm = TRUE, combine = TRUE)))

  ##Basic plot
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = TRUE,
    norm = TRUE,
    abline = list(v = c(110))
  ))

  ## test norm = "max"
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = TRUE,
    norm = "max",
    abline = list(v = c(110))
  ))

  ## test norm = "min"
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "OSL"),
    combine = TRUE,
    norm = "last",
    abline = list(v = c(110))
  ))

  ## test norm = "huot
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "OSL"),
    combine = TRUE,
    norm = "huot",
    abline = list(v = c(110))
  ))

  ## test records_max
  expect_silent(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = TRUE,
    norm = TRUE,
    sub_title = "(5 K/s)",
    records_max = 5,
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
  #TODO
  expect_warning(Luminescence:::.warningCatcher(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = FALSE,
    #norm = TRUE,
    ylim = c(1,200),
    xlim = c(1,100),
    abline = list(v = c(110))
  )))

  ##test arguments
  #ylim - warning
  expect_warning(Luminescence:::.warningCatcher(plot_RLum.Analysis(
    temp,
    subset = list(recordType = "TL"),
    combine = FALSE,
    norm = TRUE,
    log = "y"
  )))

})
