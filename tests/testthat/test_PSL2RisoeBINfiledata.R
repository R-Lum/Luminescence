context("Test PSL2Risoe.BINfileData")


test_that("simple test", {

  data("ExampleData.portableOSL", envir = environment())
  merged <- merge_RLum(ExampleData.portableOSL)
  bin <- PSL2Risoe.BINfileData(merged)

  ##checks
  expect_is(bin, "Risoe.BINfileData")
  expect_equal(length(bin), 70)

})
