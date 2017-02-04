context("Test verify_SingleGrainData")

test_that("Various function test", {

  data(ExampleData.XSYG, envir = environment())
  output <- verify_SingleGrainData(OSL.SARMeasurement$Sequence.Object)

  ##return value
  expect_is(output, "RLum.Results")
  expect_is(output$selection_full, "data.frame")

})

