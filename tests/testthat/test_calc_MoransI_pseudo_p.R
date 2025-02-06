## load data
obj <- set_RLum(class = "RLum.Results",
                data = list(vn_values = rep(x = c(1,2), each = 50)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_MoransI_pseudo_p("error"),
               "'object' should be of class 'RLum.Results', 'numeric' or 'integer'")
  expect_error(calc_MoransI_pseudo_p(obj, n_perm = "error"),
               "'n_perm' should be a positive integer scalar")
  expect_error(calc_MoransI_pseudo_p(obj, df_neighbour = "error"),
               "'df_neighbour' should be of class 'data.frame'")

  expect_warning(res <- calc_MoransI_pseudo_p(obj, df_neighbour = data.frame()),
                 "There seems to be no bordering grain locations, returning NaN")
  expect_equal(res, NaN)
})

test_that("check functionality", {
  testthat::skip_on_cran()

  set.seed(1)
  expect_equal(calc_MoransI_pseudo_p(obj),
               0)
  expect_equal(calc_MoransI_pseudo_p(1:100, n_moransI = 0.125),
               0.0350)
})
