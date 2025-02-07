## load data
obj <- set_RLum(class = "RLum.Results",
                data = list(vn_values = rep(x = c(1,2), each = 50)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_MoransI("error"),
               "'object' should be of class 'RLum.Results', 'numeric' or 'integer'")
  expect_error(calc_MoransI(obj, df_neighbours = "error"),
               "'df_neighbours' should be of class 'data.frame'")
  expect_error(calc_MoransI(obj, spatial_autocorrelation = "error"),
               "'spatial_autocorrelation' should be of class 'logical'")
  expect_error(calc_MoransI(obj, compute_pseudo_p = "error"),
               "'compute_pseudo_p' should be of class 'logical'")
  expect_error(calc_MoransI(obj, n_permutations = "error"),
               "'n_permutations' should be a positive integer scalar")
  expect_error(calc_MoransI(obj, return_intermediate_values = "error"),
               "'return_intermediate_values' should be of class 'logical'")

  expect_error(calc_MoransI(c(1, rep(NA, 99)),
                            spatial_autocorrelation = FALSE),
               "There should be at least 2 non-missing values")
  expect_warning(calc_MoransI(c(1, rep(NA, 99)),
                              spatial_autocorrelation = TRUE),
                 "No bordering grain locations given in 'df_neighbours'") # FIXME(mcol)
  expect_warning(res <- calc_MoransI(obj, df_neighbours = data.frame()),
                 "No bordering grain locations given in 'df_neighbours'")
  expect_equal(res, NaN)
})

test_that("check functionality", {
  testthat::skip_on_cran()

  set.seed(1)

  expect_snapshot_plain(calc_MoransI(obj))
  expect_snapshot_plain(calc_MoransI(1:100))
  expect_snapshot_plain(calc_MoransI(obj, return_intermediate_values = TRUE))
  expect_snapshot_plain(calc_MoransI(obj,
                                     compute_pseudo_p = TRUE,
                                     tested_moransI = 0.125,
                                     return_intermediate_values = TRUE))

  expect_equal(calc_MoransI(obj, spatial_autocorrelation = FALSE),
               -0.010101010)
  expect_equal(calc_MoransI(1:100, spatial_autocorrelation = FALSE),
               -0.010101010)
  expect_equal(calc_MoransI(1:20, spatial_autocorrelation = FALSE),
               -0.052631579)

  expect_warning(res <- calc_MoransI(obj, compute_pseudo_p = TRUE),
                 "Pseudo-p might be overestimated: the real p-value is closer")
  expect_equal(res, 0.0010)
  expect_equal(calc_MoransI(1:100, compute_pseudo_p = TRUE,
                            tested_moransI = 0.05),
               0.188)
})

test_that("check .get_Neighbours", {
  testthat::skip_on_cran()

  expect_snapshot_plain(.get_Neighbours(obj))
  expect_snapshot_plain(.get_Neighbours(obj, restrict_to_8x8 = TRUE))
  expect_snapshot_plain(.get_Neighbours(NULL))
  expect_snapshot_plain(.get_Neighbours(c(1:99, NA)))
})
