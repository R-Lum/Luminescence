## load data
data(ExampleData.XSYG, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(apply_CosmicRayRemoval("error"),
               "'object' should be of class 'RLum.Data.Spectrum'")
  expect_error(apply_CosmicRayRemoval(TL.Spectrum, method = "error"),
               "'method' should be one of 'smooth', 'smooth.spline', 'smooth_RLum' or 'Pych'",
               fixed = TRUE)
  expect_error(apply_CosmicRayRemoval(set_RLum("RLum.Data.Spectrum"),
                                      "'object' contains no data"))

  expect_error(apply_CosmicRayRemoval(TL.Spectrum,
                                      method.Pych.smoothing = iris),
               "'method.Pych.smoothing' should be a positive integer scalar")
  expect_error(apply_CosmicRayRemoval(TL.Spectrum,
                                      method.Pych.threshold_factor = -2),
               "'method.Pych.threshold_factor' should be a positive scalar")
  expect_error(apply_CosmicRayRemoval(TL.Spectrum, MARGIN = 3),
               "'MARGIN' should be one of '1' or '2'")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "Pych"),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth"),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth_RLum"),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth_RLum",
                                              MARGIN = 1, k = 0),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth_RLum",
                                              MARGIN = 1, k = 4),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth_RLum",
                                              MARGIN = 1, k = 10000),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth_RLum",
                                              MARGIN = 2, k = 0),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth_RLum",
                                              MARGIN = 2, k = 4),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth_RLum",
                                              MARGIN = 2, k = 4,
                                              method_smooth_RLum = "mean"),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth_RLum",
                                              MARGIN = 2, k = 10000),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth_RLum",
                                              k = 10),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth",
                                              MARGIN = 1),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum, method = "Pych",
                                              method.Pych.smoothing = 2,
                                              method.Pych.threshold_factor = 2),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(apply_CosmicRayRemoval(TL.Spectrum,
                                              method = "smooth.spline",
                                              MARGIN = 1,
                                              kind = "3RS3R",
                                              twiceit = TRUE,
                                              spar = NULL),
                       tolerance = snapshot.tolerance)
})

test_that("check functionality", {
  testthat::skip_on_cran()

  SW({
  expect_message(expect_message(
      apply_CosmicRayRemoval(TL.Spectrum, method = "Pych", MARGIN = 2,
                             verbose = TRUE, plot = TRUE),
      "1024 channels corrected in frame 4"),
      "0 channels corrected in frame 24")
  })

  ##construct objects for different tests
  RLum_list <- list(TL.Spectrum)
  RLum.Analysis <- set_RLum("RLum.Analysis", records = RLum_list)
  RLum.Analysis_list <- list(RLum.Analysis)
  RLum_list_mixed <- list(TL.Spectrum, set_RLum("RLum.Data.Curve"))
  RLum.Analysis_mixed <-  set_RLum("RLum.Analysis", records = RLum_list_mixed )
  RLum.Analysis_mixed_list <- list(RLum.Analysis_mixed)

  ##run tests
  expect_type(apply_CosmicRayRemoval(RLum_list),"list")
  expect_s4_class(apply_CosmicRayRemoval(RLum.Analysis), class = "RLum.Analysis")
  expect_type(apply_CosmicRayRemoval(RLum.Analysis_list), "list")
  expect_error(apply_CosmicRayRemoval(RLum_list_mixed))
  expect_s4_class(apply_CosmicRayRemoval(RLum.Analysis_mixed), class = "RLum.Analysis")
  expect_type(apply_CosmicRayRemoval(RLum.Analysis_mixed_list), "list")
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 985
  expect_silent(apply_CosmicRayRemoval(TL.Spectrum, method = "smooth_RLum",
                                       method_smooth_RLum = "Carter_etal_2018"))

  ## issue 987
  expect_silent(apply_CosmicRayRemoval(TL.Spectrum, method = "Pych",
                                       MARGIN = 1))
})
