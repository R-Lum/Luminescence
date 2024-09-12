test_that("test for errors", {
  testthat::skip_on_cran()

  expect_error(convert_RLum2Risoe.BINfileData(object = NA))

})


test_that("functionality", {
  testthat::skip_on_cran()

  ##load example data
  data(ExampleData.RLum.Analysis, envir = environment())

  ##provide RLum.Analysis
  expect_s4_class(convert_RLum2Risoe.BINfileData(IRSAR.RF.Data), "Risoe.BINfileData")

  ##provide RLum.Data.Curve
  expect_s4_class(convert_RLum2Risoe.BINfileData(IRSAR.RF.Data@records[[1]]), "Risoe.BINfileData")

  ##provide list
  expect_s4_class(convert_RLum2Risoe.BINfileData(list(IRSAR.RF.Data,IRSAR.RF.Data)), "Risoe.BINfileData")
  expect_s4_class(convert_RLum2Risoe.BINfileData(list(IRSAR.RF.Data)),
                  "Risoe.BINfileData")

  ## additional metadata
  obj <- IRSAR.RF.Data@records[[1]]
  obj@info <- list(version="12", name="test",
                   startDate="20210101150845")
  res <- convert_RLum2Risoe.BINfileData(obj)
  expect_equal(res@METADATA$VERSION, "12")
  expect_equal(res@METADATA$SAMPLE, "test")
  expect_equal(res@METADATA$DATE, "20210101")
  expect_equal(res@METADATA$TIME, "150845")
})
