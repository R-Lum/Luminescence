test_that("check output",{
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Bailey2001")
  n <- parms$n$n
  test_simulate_heating <- RLumModel:::.simulate_heating(
    temp_begin = 20,
    temp_end = 50,
    heating_rate = 5,
    n = n,
    parms = parms)

  expect_equal(class(test_simulate_heating)[1], "RLum.Results")

  expect_equal(length(test_simulate_heating$n), length(parms$N) + 2)

  expect_equal(test_simulate_heating$temp, 50)

})

test_that("test controlled crash conditions", {
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Bailey2001")
  n <- parms$n$n

  expect_error(
    RLumModel:::.simulate_heating(
      temp_begin = 10,
      temp_end = 20,
      heating_rate = -1,
      n = n,
      parms = parms),
    regexp = ".simulate_heating()] Heatingrate has the wrong algebraic sign!",
    fixed = TRUE)

  expect_error(
    RLumModel:::.simulate_heating(
      temp_begin = -274,
      temp_end = 20,
      heating_rate = 10,
      n = n,
      parms = parms),
    regexp = "[.simulate_heating()] Argument 'temp' has to be > 0 K!",
    fixed = TRUE)

})

