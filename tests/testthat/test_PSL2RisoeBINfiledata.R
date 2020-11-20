test_that("simple test", {
  testthat::skip_on_cran()
  local_edition(3)
  data("ExampleData.portableOSL", envir = environment())
  merged <- merge_RLum(ExampleData.portableOSL)
  bin <- PSL2Risoe.BINfileData(merged)

  ##checks
  expect_s4_class(bin, "Risoe.BINfileData")
  expect_equal(length(bin), 70)

})
