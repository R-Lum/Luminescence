test_that("input validation", {
  testthat::skip_on_cran()

  data(ExampleData.BINfileData, envir = environment())
  expect_error(plot_Risoe.BINfileData("error"),
               "'object' is expected to be of type 'Risoe.BINfileData'")
})

test_that("general test", {
  testthat::skip_on_cran()

  data(ExampleData.BINfileData, envir = environment())
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
                 "132 values have been found and replaced by the mean")
  SW({
  expect_warning(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1,
                                        curve.transformation = "CW2pPMi"),
                 "t' is beyond the time resolution")
  expect_warning(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1,
                                        curve.transformation = "error"),
                 "Unknown 'curve.transformation', no transformation performed")
  })

  expect_silent(plot_Risoe.BINfileData(CWOSL.SAR.Data, position = 1,
                                       dose_rate = 3))
})
