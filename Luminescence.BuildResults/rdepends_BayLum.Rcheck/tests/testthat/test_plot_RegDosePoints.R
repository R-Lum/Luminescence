test_that("Simple test", {
  testthat::skip_on_cran()
  local_edition(3)

  ## load example data
  data(DATA3,envir = environment())
  testthat::expect_null(plot_RegDosePoints(DATA3, nrow = 1, ncol = 1))

  ## remove attribute
  DATA3 <- c(DATA3)
  testthat::expect_error(
    plot_RegDosePoints(DATA3),
    regexp = "\\[plot\\_RegDosePoints\\(\\)\\] Unsupported input.")

})


