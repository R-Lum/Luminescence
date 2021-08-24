test_that("check output",{
  skip_on_cran()
  local_edition(3)

  parms <- RLumModel:::.set_pars("Bailey2001")
  n <- parms$n$n
  test_simulate_pause <- RLumModel:::.simulate_pause(
    temp = 20,
    duration = 1,
    detection = 1,
    n = n,
    parms = parms)

  expect_equal(class(test_simulate_pause)[1], "RLum.Results")

  expect_equal(length(test_simulate_pause$n), length(parms$N) + 2)

  expect_equal(test_simulate_pause$temp, 20)

})

test_that("test controlled crash conditions", {
  skip_on_cran()
  local_edition(3)

  expect_error(
    RLumModel:::.simulate_pause(
      temp = -274,
      duration = 1,
      n = n,
      parms = parms),
    regexp = "[.simulate_pause()] Argument 'temp' has to be > 0 K!",
    fixed = TRUE)

  expect_error(
    RLumModel:::.simulate_pause(
      temp = 20,
      duration = -1,
      n = n,
      parms = parms),
    regexp = "[.simulate_pause()] Argument 'duration' has to be an positive number!",
    fixed = TRUE)


})

