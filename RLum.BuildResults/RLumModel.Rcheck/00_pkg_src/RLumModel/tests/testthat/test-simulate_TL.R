context("simulate_TL")


parms <- .set_pars("Bailey2001")
n <- parms$n$n
test_simulate_TL <- .simulate_TL(
  temp_begin = 20, 
  temp_end = 50, 
  heating_rate = 5, 
  n = n, 
  parms = parms)


test_that("check output",{
  expect_equal(class(test_simulate_TL)[1], "RLum.Results")
  
  expect_equal(length(test_simulate_TL$n), length(parms$N) + 2)
  
  expect_equal(test_simulate_TL$temp, 50)
  
  ##check concentrations
  expect_equal(length(test_simulate_TL$concentrations), length(parms$N) + 2)
  
  expect_equal(class(test_simulate_TL$concentrations), "list")
  
  expect_equal(class(test_simulate_TL$concentrations[[1]])[1], "RLum.Data.Curve")
  
})

test_that("test controlled crash conditions", {
  
  parms <- .set_pars("Bailey2001")
  n <- parms$n$n
  
  expect_error(
    .simulate_TL(
      temp_begin = 10, 
      temp_end = 20,
      heating_rate = -1,
      n = n, 
      parms = parms),
    regexp = ".simulate_TL()] Heatingrate has the wrong algebraic sign!",
    fixed = TRUE)
  
  expect_error(
    .simulate_TL(
      temp_begin = -274, 
      temp_end = 20,
      heating_rate = 10,
      n = n, 
      parms = parms),
    regexp = "[.simulate_TL()] Argument 'temp' has to be > 0 K!",
    fixed = TRUE)
  
  expect_error(
    .simulate_TL(
      temp_begin = 10, 
      temp_end = -20,
      heating_rate = -2,
      n = n, 
      parms = parms),
    regexp = "[.simulate_TL()] Argument 'heating_rate' has to be a positive number!",
    fixed = TRUE)
  
})

