## load data
set.seed(1)
data(ExampleData.XSYG, envir = environment())
eff_data <- data.frame(WAVELENGTH = 1:1000, runif(1000))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(apply_EfficiencyCorrection(object = "error"),
               "'object' should be of class 'RLum.Data.Spectrum'")
  expect_error(apply_EfficiencyCorrection(TL.Spectrum,
                                          spectral.efficiency = "error"),
               "'spectral.efficiency' should be of class 'data.frame'")
  expect_error(apply_EfficiencyCorrection(TL.Spectrum,
                                          spectral.efficiency = data.frame()),
               "'spectral.efficiency' cannot be an empty data.frame")
  expect_error(apply_EfficiencyCorrection(TL.Spectrum,
                                          spectral.efficiency = iris[, 1, drop = FALSE]),
               "'spectral.efficiency' should have 2 columns")
  expect_error(apply_EfficiencyCorrection(TL.Spectrum,
                                          spectral.efficiency = data.frame(1:10, NA)),
               "No valid data remains in 'spectral.efficiency' after removing")
  expect_error(apply_EfficiencyCorrection(TL.Spectrum,
                                          spectral.efficiency = data.frame(1:10,
                                                                           runif(10))),
               "Interpolation failed: this happens when the x-values in")
  eff_data[1, 2] <- 2
  expect_error(apply_EfficiencyCorrection(TL.Spectrum,
                                          spectral.efficiency = eff_data),
               "Relative quantum efficiency values > 1 are not allowed")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ##run list test
  expect_warning(
    apply_EfficiencyCorrection(list(a = "test", TL.Spectrum), spectral.efficiency = eff_data),
    "Skipping 'character' object in input list")

  ##run test with RLum.Analysis objects
  expect_s4_class(
    apply_EfficiencyCorrection(set_RLum("RLum.Analysis",
                                        records = list(TL.Spectrum)), spectral.efficiency = eff_data),
    "RLum.Analysis")
  expect_warning(
      apply_EfficiencyCorrection(set_RLum("RLum.Analysis",
                                          records = list(TL.Spectrum, "test")),
                                 spectral.efficiency = eff_data),
      "Skipping 'character' object in input list")

  ##run test with everything combined
  input <- list(a = "test", TL.Spectrum,set_RLum("RLum.Analysis", records = list(TL.Spectrum)))
  expect_warning(apply_EfficiencyCorrection(input, eff_data),
                 "Skipping 'character' object in input list")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-5

  expect_snapshot_RLum(apply_EfficiencyCorrection(TL.Spectrum,
                                                  spectral.efficiency = eff_data),
                       tolerance = snapshot.tolerance)
})
