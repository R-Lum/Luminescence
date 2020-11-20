data <- data.frame(
    NUCLIDES = c("U-238", "Th-232", "K-40"),
    VALUE = c(40,80,100),
    VALUE_ERROR = c(4,8,10),
    stringsAsFactors = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(is(convert_Activity2Concentration(data)), c("RLum.Results", "RLum"))
  expect_equal(is(convert_Activity2Concentration(data, verbose = FALSE)), c("RLum.Results", "RLum"))
  expect_equal(length(convert_Activity2Concentration(data)), 1)
  expect_error(convert_Activity2Concentration())
  expect_error(convert_Activity2Concentration(data = data.frame(a = 1, b = 2)))

})

test_that("check values from output example", {
  testthat::skip_on_cran()
  local_edition(3)

  results <- convert_Activity2Concentration(data)
  expect_equal(round(sum(results$data$`CONC. ERROR (ppm/%)`),5),  2.32815)


})
