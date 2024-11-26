test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(PSL2Risoe.BINfileData("error"),
               "'object' should be of class 'RLum.Analysis'")
  expect_error(PSL2Risoe.BINfileData(set_RLum("RLum.Analysis")),
               "'object' cannot be an empty RLum.Analysis")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  data("ExampleData.portableOSL", envir = environment())
  merged <- merge_RLum(ExampleData.portableOSL)
  bin <- PSL2Risoe.BINfileData(merged)

  ##checks
  expect_s4_class(bin, "Risoe.BINfileData")
  expect_equal(length(bin), 70)

  ## manipulate the object to trigger other errors
  fake <- merged
  fake@records[20] <- "unexpected-subclass"
  expect_error(PSL2Risoe.BINfileData(fake),
               "All elements of 'object' should be of class 'RLum.Data.Curve'")

  fake <- merged
  fake@records[10][[1]]@originator <- "unexpected-originator"
  expect_error(PSL2Risoe.BINfileData(fake),
               "Only objects originating from 'read_PSL2R()' are allowed",
               fixed=TRUE)
})
