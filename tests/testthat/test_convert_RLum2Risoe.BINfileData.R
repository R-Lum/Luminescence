test_that("test for errors", {
  testthat::skip_on_cran()

  expect_error(convert_RLum2Risoe.BINfileData(object = NA),
               "'object' should be of class 'RLum.Analysis', 'RLum.Data.Curve' or")
  expect_error(convert_RLum2Risoe.BINfileData(set_RLum("RLum.Analysis")),
               "'object' cannot be an empty RLum.Analysis")
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

  ## regression check for "raw" conversion error from double conversion
  data(ExampleData.BINfileData, envir = environment())
  expect_s4_class(
    convert_RLum2Risoe.BINfileData(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data)),
    class = "Risoe.BINfileData")

  ## regression for combination of data where we mixed datasets that should not be mixed
  tmp_object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data)
  tmp_curves <- tmp_object[[3]]@records[[1]]
  tmp_curves@recordType <- "NONE"
  tmp_object[[3]]@records <- c(tmp_object[[3]]@records, rep(tmp_curves,2))
  expect_s4_class(
    convert_RLum2Risoe.BINfileData(tmp_object),
    class = "Risoe.BINfileData")

})
