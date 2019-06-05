context("apply_EfficiencyCorrection")

test_that("check function", {
  testthat::skip_on_cran()

  ##load data
  data(ExampleData.XSYG, envir = environment())

  ##create efficiency data
  eff_data <- data.frame(WAVELENGTH = 1:1000, runif(1000))

  ##break function
  expect_error(apply_EfficiencyCorrection(object = "ERROR"),
               regexp = "Input object is not of type RLum.Data.Spectrum")

  expect_error(apply_EfficiencyCorrection(object = TL.Spectrum, spectral.efficiency = "ERROR"),
               regexp = "'spectral.efficiency' is not of type data.frame")

  eff_data_false <- eff_data
  eff_data_false[1,2] <- 2
  expect_error(apply_EfficiencyCorrection(
    object = TL.Spectrum,
    spectral.efficiency = eff_data_false),
               regexp = "Relative quantum efficiency values > 1 are not allowed.")


  ##run tests
  expect_s4_class(apply_EfficiencyCorrection(TL.Spectrum,spectral.efficiency = eff_data), "RLum.Data.Spectrum")

  ##run list test
  expect_warning(
    apply_EfficiencyCorrection(list(a = "test", TL.Spectrum), spectral.efficiency = eff_data),
    regexp = "Skipping character object in input list.")

  ##run test with RLum.Analysis objects
  expect_s4_class(
    apply_EfficiencyCorrection(set_RLum("RLum.Analysis", records = list(TL.Spectrum)), spectral.efficiency = eff_data),
    "RLum.Analysis")

  ##run test with everything combined
  input <- list(a = "test", TL.Spectrum,set_RLum("RLum.Analysis", records = list(TL.Spectrum)))
  apply_EfficiencyCorrection(input, eff_data)

})

