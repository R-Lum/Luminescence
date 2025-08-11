test_that("Test internals", {
  testthat::skip_on_cran()

## let if fail fast
expect_error(
  object = correct_PMTLinearity("error", PMT_pulse_pair_resolution = 10),
  regexp = "'object' should be of class 'RLum.Analysis'")

## try RLum.Data.Curve
o <- set_RLum("RLum.Data.Curve")
expect_s4_class(
  object = correct_PMTLinearity(o, PMT_pulse_pair_resolution = 10),
  class = "RLum.Data.Curve")

## run with only one row
o <- set_RLum(
  class = "RLum.Analysis",
  records = list(
    set_RLum("RLum.Data.Curve")))
t <- expect_s4_class(
  object = correct_PMTLinearity(o, PMT_pulse_pair_resolution = 10),
  class = "RLum.Analysis")

## create dataset
tmp_data <- matrix(data = c(seq(0,1,0.1), seq(10000,1000,length.out = 11)), ncol = 2)

## run normally
o <- set_RLum(
  class = "RLum.Analysis",
  records = list(
    set_RLum("RLum.Data.Curve", data = tmp_data)))
expect_s4_class(
  object = correct_PMTLinearity(o, PMT_pulse_pair_resolution = 10),
  class = "RLum.Analysis")

## intermix different records; the non-RLum.Data.Curve() should be skipped
o <- set_RLum(
  class = "RLum.Analysis",
  records = list(
    set_RLum("RLum.Data.Curve", data = tmp_data),
    set_RLum("RLum.Data.Image")
  ))
t <- expect_s4_class(
  object = correct_PMTLinearity(o, PMT_pulse_pair_resolution = 10),
  class = "RLum.Analysis")
expect_length(t, 2)
expect_equal(t@records[[1]][1,2], 10010, tolerance = 0.001)

})
