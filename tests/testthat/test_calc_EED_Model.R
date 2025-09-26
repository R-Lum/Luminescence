## load data
data(ExampleData.MortarData, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  ## data
  expect_error(calc_EED_Model(),
               "'data' should be of class 'data.frame'")
  expect_error(calc_EED_Model(1L),
               "'data' should be of class 'data.frame'")

  ## expected dose
  expect_error(calc_EED_Model(MortarData),
               "'expected_dose' should be of class 'numeric'")
  expect_error(calc_EED_Model(MortarData, expected_dose = "error"),
               "'expected_dose' should be of class 'numeric'")

  ## IndivDose
  expect_error(calc_EED_Model(MortarData, expected_dose = 0.17,
                              MinIndivDose = "error"),
               "'MinIndivDose' should be of class 'numeric")
  expect_error(calc_EED_Model(MortarData, expected_dose = 0.17,
                              MaxIndivDose = "error"),
               "'MaxIndivDose' should be of class 'numeric")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  set.seed(1)
  SW({
  expect_snapshot_RLum(calc_EED_Model(
      MortarData,
      expected_dose = 11.7),
      tolerance = snapshot.tolerance)
  })
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  vdiffr::expect_doppelganger("fast",
                              calc_EED_Model(MortarData,
                                             kappa = 14,
                                             sigma_distr = 0.37,
                                             verbose = FALSE,
                                             expected_dose = 11.7))
  })
})

test_that("more coverage", {
  testthat::skip_on_cran()

  SW({
  expect_s4_class(calc_EED_Model(
    data = MortarData,
    sigma_distr = 0.37,
    verbose = TRUE,
    method_control = list(trace = TRUE, trace_plot = TRUE),
    n.simul = 5,
    expected_dose = 11.7), "RLum.Results")

  set.seed(1)
  expect_error(calc_EED_Model(
    data = MortarData,
    kappa = 1,
    expected_dose = 95,
    n.simul = 300),
    "Surface interpolation failed: scales of x and y are too dissimilar")
  })
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 1051
  expect_silent(calc_EED_Model(
    data = MortarData,
    expected_dose = 11.7,
    n.simul = 10,
    verbose = FALSE,
    plot = TRUE))
})
