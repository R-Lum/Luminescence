## load data
obj <- set_RLum(class = "RLum.Results",
                data = list(vn_values = rep(x = c(1,2), each = 50)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(apply_Crosstalk("error"),
               "'object' should be of class 'RLum.Results', 'numeric' or 'integer'")
  expect_error(apply_Crosstalk(obj, n_crosstalk = "error"),
               "'n_crosstalk' should be of class 'numeric'")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_snapshot_plain(apply_Crosstalk(obj))
  expect_snapshot_plain(apply_Crosstalk(1:100))
})
