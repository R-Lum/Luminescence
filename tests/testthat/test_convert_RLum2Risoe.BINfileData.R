context("convert_RLum2Risoe.BINfileData")

test_that("test for errors", {
  testthat::skip_on_cran()

  expect_error(convert_RLum2Risoe.BINfileData(object = NA))

})


test_that("functionality", {
  testthat::skip_on_cran()

  ##load example data
  data(ExampleData.RLum.Analysis, envir = environment())

  ##provide RLum.Analysis
  expect_is(convert_RLum2Risoe.BINfileData(IRSAR.RF.Data), "Risoe.BINfileData")

  ##provide RLum.Data.Curve
  expect_is(convert_RLum2Risoe.BINfileData(IRSAR.RF.Data@records[[1]]), "Risoe.BINfileData")

  ##provide list
  expect_is(convert_RLum2Risoe.BINfileData(list(IRSAR.RF.Data,IRSAR.RF.Data)), "Risoe.BINfileData")

})

