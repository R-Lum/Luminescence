## load data
data_activity <- data.frame(
    NUCLIDES = c("U-238", "Th-232", "K-40"),
    VALUE = c(40,80,100),
    VALUE_ERROR = c(4,8,10),
    stringsAsFactors = FALSE)

data_abundance <- data.frame(
    NUCLIDES = c("U-238", "Th-232", "K-40"),
    VALUE = c(4,8,1),
    VALUE_ERROR = c(0.1,0.1,0.1),
    stringsAsFactors = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(
    object = convert_Activity2Concentration(),
    "[convert_Activity2Concentration()] 'data' should be of class 'data.frame'",
    fixed = TRUE)

  expect_error(
    object = convert_Activity2Concentration(data = data_activity[,1:2]),
    "[convert_Activity2Concentration()] 'data' should have at least 3 columns",
    fixed = TRUE)

  expect_error(
    object = convert_Activity2Concentration(data = data_activity, input_unit = "stop"),
    "'input_unit' should be one of 'activity' or 'abundance'")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  SW({
  ## check for standard input
  expect_snapshot_RLum(convert_Activity2Concentration(data_activity),
                       tolerance = snapshot.tolerance)

  ## check for concentration input
  expect_snapshot_RLum(convert_Activity2Concentration(data_abundance,
                                                      input_unit = "abundance"),
                       tolerance = snapshot.tolerance)
  })
})

test_that("more checks", {
  testthat::skip_on_cran()

  ## additional checks for input
  ## capitalized input units
  SW({
    expect_s4_class(convert_Activity2Concentration(data_activity, input_unit = "ACTIVITY"), c("RLum.Results"))

    ## check backwards compatibility
    expect_s4_class(convert_Activity2Concentration(data_activity, input_unit = "Bq/kg"), c("RLum.Results"))
  })
})
