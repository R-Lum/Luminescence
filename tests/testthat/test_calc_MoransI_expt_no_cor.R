## load data
obj <- set_RLum(class = "RLum.Results",
                data = list(vn_values = rep(x = c(1,2), each = 50)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_MoransI_expt_no_cor("error"),
               "'object' should be of class 'RLum.Results', 'numeric' or 'integer'")
  expect_error(calc_MoransI_expt_no_cor(obj, n = "error"),
               "'n' should be a positive integer scalar")
  expect_error(calc_MoransI_expt_no_cor(obj, n = 1),
               "n > 1 is not TRUE")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_equal(calc_MoransI_expt_no_cor(obj),
               -0.010101010)
  expect_equal(calc_MoransI_expt_no_cor(1:100),
               -0.010101010)
  expect_equal(calc_MoransI_expt_no_cor(n = 20),
               -0.052631579)
})
