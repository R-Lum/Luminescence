## load data
obj <- set_RLum(class = "RLum.Results",
                data = list(vn_values = rep(x = c(1,2), each = 50)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(get_Neighbour("error"),
               "'object' should be of class 'RLum.Results', 'numeric' or 'integer'")
  expect_error(get_Neighbour(obj, restrict_to_8x8 = "error"),
               "'restrict_to_8x8' should be of class 'logical'")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_snapshot_plain(get_Neighbour(obj))
  expect_snapshot_plain(get_Neighbour(obj, restrict_to_8x8 = TRUE))
  expect_snapshot_plain(get_Neighbour(NULL))
  expect_snapshot_plain(get_Neighbour(c(1:99, NA)))
})
