test_that("simple test", {
  testthat::skip_on_cran()

  data("ExampleData.portableOSL", envir = environment())
  merged <- merge_RLum(ExampleData.portableOSL)
  bin <- PSL2Risoe.BINfileData(merged)

  ##checks
  expect_s4_class(bin, "Risoe.BINfileData")
  expect_equal(length(bin), 70)

  ## input validation
  expect_error(PSL2Risoe.BINfileData("wrong-class"),
               "Only objects of class 'RLum.Analysis' are allowed")

  ## manipulate the object to trigger other errors
  fake <- merged
  fake@records[20] <- "unexpected-subclass"
  expect_error(PSL2Risoe.BINfileData(fake),
               "must only contain objects of class 'RLum.Data.Curve'")

  fake <- merged
  fake@records[10][[1]]@originator <- "unexpected-originator"
  expect_error(PSL2Risoe.BINfileData(fake),
               "Only objects originating from 'read_PSL2R()' are allowed",
               fixed=TRUE)
})
