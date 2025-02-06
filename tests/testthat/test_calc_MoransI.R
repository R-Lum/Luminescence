## load data
obj <- set_RLum(class = "RLum.Results",
                data = list(vn_values = rep(x = c(1,2), each = 50)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_MoransI("error"),
               "'object' should be of class 'RLum.Results', 'numeric' or 'integer'")
  expect_error(calc_MoransI(obj, df_neighbour = "error"),
               "'df_neighbour' should be of class 'data.frame'")
  expect_error(calc_MoransI(obj, bo_return_inbetween_numbers = "error"),
               "'bo_return_inbetween_numbers' should be of class 'logical'")

  expect_warning(res <- calc_MoransI(obj, df_neighbour = data.frame()),
                 "There seems to be no bordering grain locations, returning NaN")
  expect_equal(res, NaN)
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_snapshot_plain(calc_MoransI(obj))
  expect_snapshot_plain(calc_MoransI(1:100))
  expect_snapshot_plain(calc_MoransI(obj, bo_return_inbetween_numbers = TRUE))
})
