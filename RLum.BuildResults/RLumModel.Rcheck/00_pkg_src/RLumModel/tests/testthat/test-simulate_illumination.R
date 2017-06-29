context("simulate_illumination")


parms <- .set_pars("Bailey2001")
n <- parms$n$n
test_simulate_illumination <- .simulate_illumination(
  temp = 125, 
  duration = 100, 
  optical_power = 90, 
  n = n, 
  parms = parms)


test_that("check output",{
  expect_equal(class(test_simulate_illumination)[1], "RLum.Results")
  
  expect_equal(length(test_simulate_illumination$n), length(parms$N) + 2)
  
  expect_equal(test_simulate_illumination$temp, 125)
  
  parms <- .set_pars("Bailey2002")
  n <- parms$n$n
  test_simulate_illumination <- .simulate_illumination(
    temp = 125, 
    duration = 100, 
    optical_power = 90, 
    n = n, 
    parms = parms)

})

test_that("test controlled crash conditions", {
  
  parms <- .set_pars("Bailey2001")
  n <- parms$n$n
  
  expect_error(
    .simulate_illumination(
      temp = -274, 
      duration = 1, 
      optical_power = 100, 
      n = n, 
      parms = parms),
    regexp = "[.simulate_illumination()] Argument 'temp' has to be > 0 K!",
    fixed = TRUE)
  
  expect_error(
    .simulate_illumination(
      temp = 20, 
      duration = -1, 
      optical_power = 100, 
      n = n, 
      parms = parms),
    regexp = "[.simulate_illumination()] Argument 'duration' has to be a positive number!",
    fixed = TRUE)
  
  expect_error(
    .simulate_illumination(
      temp = 20, 
      duration = 1, 
      optical_power = -1, 
      n = n, 
      parms = parms),
    regexp = "[.simulate_illumination()] Argument 'optical_power' has to be a positive number!",
    fixed = TRUE)
  
  
})