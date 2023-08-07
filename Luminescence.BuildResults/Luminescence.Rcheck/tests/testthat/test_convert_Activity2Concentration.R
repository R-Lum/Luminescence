test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  data <- data.frame(
    NUCLIDES = c("U-238", "Th-232", "K-40"),
    VALUE = c(40,80,100),
    VALUE_ERROR = c(4,8,10),
    stringsAsFactors = FALSE)


  results <- expect_s4_class(convert_Activity2Concentration(data), c("RLum.Results"))
  expect_s4_class(convert_Activity2Concentration(data, verbose = FALSE), c("RLum.Results"))
  expect_equal(length(convert_Activity2Concentration(data)), 1)
  expect_error(convert_Activity2Concentration())
  expect_error(convert_Activity2Concentration(data = data.frame(a = 1, b = 2)))

  ## this test should flag if constants were changed, so that this is
  ## not forgotten in the NEWS
  expect_equal(round(sum(results$data$`CONC. (ppm/%)`),5),  23.20909)
  expect_equal(round(sum(results$data$`CONC. ERROR (ppm/%)`),5),  2.32091)

})
