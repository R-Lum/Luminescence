## load data
data(ExampleData.BINfileData, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_Risoe.BINfileData("error"),
               "'data' should be of class 'Risoe.BINfileData'")
  expect_error(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1,
                                      curve.transformation = "error"),
               "'curve.transformation' should be one of 'CW2pLM', 'CW2pLMi'")
})

test_that("general test", {
  testthat::skip_on_cran()

  expect_silent(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1))
  expect_silent(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1,
                                       sorter = "RUN"))
  expect_silent(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1,
                                       sorter = "SET"))

  expect_silent(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1,
                                       curve.transformation = "CW2pLM"))
  expect_silent(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1,
                                       curve.transformation = "CW2pLMi"))

  expect_warning(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1,
                                        curve.transformation = "CW2pHMi"),
                 "132 invalid values have been found and replaced by the mean")
  SW({
  expect_warning(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1,
                                        curve.transformation = "CW2pPMi"),
                 "t' is beyond the time resolution")
  })

  expect_silent(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1,
                                       dose_rate = 3))
  ## simulate v4
  CWOSL.SAR.Data@METADATA$VERSION <- 04
  expect_silent(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1))
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("TL",
                              plot_Risoe.BINfileData(CWOSL.SAR.Data,
                                                     pos = 1, run = 2, set = 5))
  vdiffr::expect_doppelganger("OSL",
                              plot_Risoe.BINfileData(CWOSL.SAR.Data,
                                                     pos = 2, run = 7, set = 3))
  vdiffr::expect_doppelganger("IRSL",
                              plot_Risoe.BINfileData(CWOSL.SAR.Data,
                                                     pos = 2, run = 8, set = 3))
  vdiffr::expect_doppelganger("dose rate",
                              plot_Risoe.BINfileData(CWOSL.SAR.Data,
                                                     pos = 2, run = 5, set = 3,
                                                     dose_rate = 0.123))
  })
})
