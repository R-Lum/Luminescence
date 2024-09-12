test_that("check different models",{
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Bailey2002")
  n <- parms$n$n
  test_simulate_irradiation <- expect_s4_class(RLumModel:::.simulate_irradiation(
    temp = 20,
    dose = 1,
    dose_rate = 1,
    n = n,
    parms = parms), "RLum.Results")

  parms <- RLumModel:::.set_pars("Bailey2004")
  n <- parms$n$n
  test_simulate_irradiation <- expect_s4_class(RLumModel:::.simulate_irradiation(
    temp = 20,
    dose = 1,
    dose_rate = 1,
    n = n,
    parms = parms), "RLum.Results")

  parms <- RLumModel:::.set_pars("Bailey2001")
  n <- parms$n$n
  test_simulate_irradiation <- expect_s4_class(RLumModel:::.simulate_irradiation(
    temp = 20,
    dose = 0,
    dose_rate = 1,
    n = n,
    parms = parms), "RLum.Results")

  parms <- RLumModel:::.set_pars("Friedrich2018")
  n <- parms$n$n
  test_simulate_irradiation <- RLumModel:::.simulate_irradiation(
    temp = 20,
    dose = 0,
    dose_rate = 1,
    n = n,
    parms = parms)

})


test_that("check output",{
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Bailey2001")
  n <- parms$n$n
  test_simulate_irradiation <- RLumModel:::.simulate_irradiation(
    temp = 20,
    dose = 1,
    dose_rate = 1,
    n = n,
    parms = parms)


  expect_equal(class(test_simulate_irradiation)[1], "RLum.Results")

  expect_equal(length(test_simulate_irradiation$n), length(parms$N) + 2)

  expect_equal(test_simulate_irradiation$temp, 20)

})

test_that("test controlled crash conditions", {
  skip_on_cran()
  local_edition(3)

  expect_error(
    RLumModel:::.simulate_irradiation(
      temp = -274,
      dose = 1,
      dose_rate = 1,
      n = n,
      parms = parms),
    regexp = "[.simulate_irradiation()] Argument 'temp' has to be > 0 K!",
    fixed = TRUE)

  expect_error(
    RLumModel:::.simulate_irradiation(
      temp = 20,
      dose = 1,
      dose_rate = -1,
      n = n,
      parms = parms),
    regexp = "[.simulate_irradiation()] Argument 'dose_rate' has to be a positive number!",
    fixed = TRUE)

  expect_error(
    RLumModel:::.simulate_irradiation(
      temp = 20,
      dose = -1,
      dose_rate = 1,
      n = n,
      parms = parms),
    regexp = "[.simulate_irradiation()] Argument 'dose' has to be a positive number!",
    fixed = TRUE)


  })
