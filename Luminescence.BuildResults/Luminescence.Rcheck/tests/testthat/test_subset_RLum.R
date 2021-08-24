# RLum.Analysis -----------------------------------------------------------
test_that("subset RLum.Analysis", {
  testthat::skip_on_cran()
  local_edition(3)

  data("ExampleData.RLum.Analysis")
  temp <- IRSAR.RF.Data


  ## subset.RLum.Analysis() - S3 method
  ### empty call
  expect_s4_class(subset(temp), "RLum.Analysis")
  expect_length(subset(temp), length(temp))
  expect_identical(subset(temp)[[1]], temp[[1]])

  ### errors
  expect_error(subset(temp, LTYPE == "RF"), regexp = "Valid terms are")
  expect_null(subset(temp, recordType == "xx"))

  ### valid
  expect_s4_class(subset(temp, recordType == "RF"), class = "RLum.Analysis")
  expect_s4_class(subset(temp, recordType == "RF")[[1]], class = "RLum.Data.Curve")
  expect_length(subset(temp, recordType == "RF"), n = length(temp))

  ## get_RLum(<obj>, subset = (<condition>))
  expect_s4_class(get_RLum(temp, subset = recordType == "RF"), class = "RLum.Analysis")
  expect_s4_class(get_RLum(temp, subset = recordType == "RF")[[1]], class = "RLum.Data.Curve")
  expect_length(get_RLum(temp, subset = recordType == "RF"), n = length(temp))

})
