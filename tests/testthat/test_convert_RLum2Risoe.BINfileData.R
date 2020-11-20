test_that("test for errors", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_error(convert_RLum2Risoe.BINfileData(object = NA))

})


test_that("functionality", {
  testthat::skip_on_cran()
  local_edition(3)

  ##load example data
  data(ExampleData.RLum.Analysis, envir = environment())

  ##provide RLum.Analysis
  expect_s4_class(convert_RLum2Risoe.BINfileData(IRSAR.RF.Data), "Risoe.BINfileData")

  ##provide RLum.Data.Curve
  expect_s4_class(convert_RLum2Risoe.BINfileData(IRSAR.RF.Data@records[[1]]), "Risoe.BINfileData")

  ##provide list
  expect_s4_class(convert_RLum2Risoe.BINfileData(list(IRSAR.RF.Data,IRSAR.RF.Data)), "Risoe.BINfileData")

})

