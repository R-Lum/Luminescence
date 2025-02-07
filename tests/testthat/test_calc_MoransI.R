## load data
obj <- set_RLum(class = "RLum.Results",
                data = list(vn_values = rep(x = c(1,2), each = 50)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_MoransI("error"),
               "'object' should be of class 'RLum.Results', 'numeric' or 'integer'")
  expect_error(calc_MoransI(obj, df_neighbours = "error"),
               "'df_neighbours' should be of class 'data.frame'")
  expect_error(calc_MoransI(obj, return_intermediate_values = "error"),
               "'return_intermediate_values' should be of class 'logical'")

  expect_warning(res <- calc_MoransI(obj, df_neighbours = data.frame()),
                 "No bordering grain locations given in 'df_neighbours'")
  expect_equal(res, NaN)
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_snapshot_plain(calc_MoransI(obj))
  expect_snapshot_plain(calc_MoransI(1:100))
  expect_snapshot_plain(calc_MoransI(obj, return_intermediate_values = TRUE))
})

test_that("check .get_Neighbours", {
  testthat::skip_on_cran()

  expect_snapshot_plain(.get_Neighbours(obj))
  expect_snapshot_plain(.get_Neighbours(obj, restrict_to_8x8 = TRUE))
  expect_snapshot_plain(.get_Neighbours(NULL))
  expect_snapshot_plain(.get_Neighbours(c(1:99, NA)))
})
