test_that("check class and length of output", {
  testthat::skip_on_cran()

  ## set dataframe
  data_activity <- data.frame(
    NUCLIDES = c("U-238", "Th-232", "K-40"),
    VALUE = c(40,80,100),
    VALUE_ERROR = c(4,8,10),
    stringsAsFactors = FALSE)

  ## set dataframe
  data_abundance <- data.frame(
    NUCLIDES = c("U-238", "Th-232", "K-40"),
    VALUE = c(4,8,1),
    VALUE_ERROR = c(0.1,0.1,0.1),
    stringsAsFactors = FALSE)

  ## crash function
  expect_error(
    object = convert_Activity2Concentration(),
    regexp = "\\[convert\\_Activity2Concentration\\(\\)\\] I'm still waiting for input data ...")

  expect_error(
    object = convert_Activity2Concentration(data = data_activity[,1:2]),
    regexp = "\\[convert\\_Activity2Concentration\\(\\)\\] Input data.frame should have at least three columns.")

  expect_error(
    object = convert_Activity2Concentration(data = data_activity, input_unit = "stop"),
    regexp = "\\[convert\\_Activity2Concentrations\\(\\)\\] Input for parameter 'input_unit' invalid.")

  ## check for standard input
  SW({
  results <- expect_s4_class(convert_Activity2Concentration(data_activity),
                             c("RLum.Results"))
  })
  expect_s4_class(convert_Activity2Concentration(data_activity, verbose = FALSE), c("RLum.Results"))
  expect_equal(length(results), 1)

  ## this test should flag if constants were changed, so that this is
  ## not forgotten in the NEWS
  expect_equal(round(sum(results$data$`ABUND. (mug/g or mass. %)`),5),  23.20909)
  expect_equal(round(sum(results$data$`ABUND. ERROR (mug/g or mass. %)`),5),  2.32091)

  ## check for concentration input
  SW({
  results_abundance <- expect_s4_class(
    object = convert_Activity2Concentration(data_abundance, input_unit = "abundance"),
    class = "RLum.Results")
  })

  expect_equal(round(sum(results_abundance$data$`ABUND. (mug/g or mass. %)`),5),  13)
  expect_equal(round(sum(results_abundance$data$`ABUND. ERROR (mug/g or mass. %)`),5),  0.3)

  ## additional checks for input
  ## capitalized input units
  SW({
    expect_s4_class(convert_Activity2Concentration(data_activity, input_unit = "ACTIVITY"), c("RLum.Results"))

    ## check backwards compatibility
    expect_s4_class(convert_Activity2Concentration(data_activity, input_unit = "Bq/kg"), c("RLum.Results"))
  })
})
