## load data
obj <- set_RLum(class = "RLum.Results",
                data = list(vn_values = rep(x = c(1,2), each = 50)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_MoransI("error"),
               "'object' should be of class 'RLum.Results', 'numeric' or 'integer'")
  expect_error(calc_MoransI(1:50),
               "'object' should have length 100")
  expect_error(calc_MoransI(obj, df_neighbours = "error"),
               "'df_neighbours' should be of class 'data.frame'")
  expect_error(calc_MoransI(obj, df_neighbours = iris),
               "'df_neighbours' should be a data frame with 3 columns")
  expect_error(calc_MoransI(obj, spatial_autocorrelation = "error"),
               "'spatial_autocorrelation' should be a single logical value")
  expect_error(calc_MoransI(obj, compute_pseudo_p = "error"),
               "'compute_pseudo_p' should be a single logical value")
  expect_error(calc_MoransI(obj, n_permutations = "error"),
               "'n_permutations' should be a positive integer scalar")
  expect_error(calc_MoransI(obj, ignore_borders = "error"),
               "'ignore_borders' should be a single logical value")
  expect_error(calc_MoransI(obj, return_intermediate_values = "error"),
               "'return_intermediate_values' should be a single logical value")

  expect_error(calc_MoransI(c(1, rep(NA, 99)),
                            spatial_autocorrelation = FALSE),
               "There should be at least 2 non-missing values")
  expect_warning(calc_MoransI(c(1, rep(NA, 99)),
                              spatial_autocorrelation = TRUE),
                 "No bordering grain locations given in 'df_neighbours'") # FIXME(mcol)
  expect_warning(res <- calc_MoransI(obj, df_neighbours = iris[0, 1:3]),
                 "No bordering grain locations given in 'df_neighbours'")
  expect_equal(res, NaN)
})

test_that("check functionality", {
  testthat::skip_on_cran()

  set.seed(1)

  expect_snapshot_plain(calc_MoransI(obj))
  expect_snapshot_plain(calc_MoransI(1:100, ignore_borders = TRUE))
  expect_snapshot_plain(calc_MoransI(obj, return_intermediate_values = TRUE))
  expect_snapshot_plain(calc_MoransI(obj,
                                     compute_pseudo_p = TRUE,
                                     tested_moransI = 0.125,
                                     return_intermediate_values = TRUE))

  expect_equal(calc_MoransI(obj, spatial_autocorrelation = FALSE),
               -0.010101010)
  expect_equal(calc_MoransI(1:100, spatial_autocorrelation = FALSE),
               -0.010101010)
  expect_equal(calc_MoransI(c(1:20, rep(NA, 80)), spatial_autocorrelation = FALSE),
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

  expect_error(.get_Neighbours(NULL),
               "'object' should be of class 'RLum.Results', 'numeric' or")
  expect_snapshot_plain(.get_Neighbours(obj))
  expect_snapshot_plain(.get_Neighbours(obj, ignore_borders = TRUE))
  expect_snapshot_plain(.get_Neighbours(c(1:99, NA)))
})
