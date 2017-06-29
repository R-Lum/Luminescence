context("simulate_LM_OSL")


parms <- .set_pars("Bailey2001")
n <- parms$n$n
test_simulate_LM_OSL <- .simulate_LM_OSL(
  temp = 125, 
  duration = 100, 
  n = n, 
  parms = parms)


test_that("check output",{
  expect_equal(class(test_simulate_LM_OSL)[1], "RLum.Results")
  
  expect_equal(length(test_simulate_LM_OSL$n), length(parms$N) + 2)
  
  expect_equal(test_simulate_LM_OSL$temp, 125)
  
  ##check concentrations
  expect_equal(length(test_simulate_LM_OSL$concentrations), length(parms$N) + 2)
  
  expect_equal(class(test_simulate_LM_OSL$concentrations), "list")
  
  expect_equal(class(test_simulate_LM_OSL$concentrations[[1]])[1], "RLum.Data.Curve")
  
})

parms <- .set_pars("Bailey2002")
n <- parms$n$n
test_simulate_LM_OSL <- .simulate_LM_OSL(
  temp = 125, 
  duration = 100, 
  n = n, 
  parms = parms)

test_that("check output of Bailey 2002/2004",{
  expect_equal(class(test_simulate_LM_OSL)[1], "RLum.Results")
  
  expect_equal(length(test_simulate_LM_OSL$n), length(parms$N) + 2)
  
  expect_equal(test_simulate_LM_OSL$temp, 125)
  
  ##check concentrations
  expect_equal(length(test_simulate_LM_OSL$concentrations), length(parms$N) + 2)
  
  expect_equal(class(test_simulate_LM_OSL$concentrations), "list")
  
  expect_equal(class(test_simulate_LM_OSL$concentrations[[1]])[1], "RLum.Data.Curve")
  
})

test_that("test controlled crash conditions", {
  expect_error(
    .simulate_LM_OSL(
      temp = -274, 
      duration = 100, 
      n = n, 
      parms = parms),
    regexp = "\n [.simulate_LM_OSL()] Argument 'temp' has to be > 0 K!", fixed = TRUE)
  
  expect_error(
    .simulate_LM_OSL(
      temp = 125, 
      duration = -10, 
      n = n, 
      parms = parms),
    regexp = "\n [.simulate_LM_OSL()] Argument 'duration' has to be a positive number!", fixed = TRUE)
  
  
  expect_error(
    .simulate_LM_OSL(
      temp = 125, 
      duration = 100,
      start_power = -10, 
      n = n, 
      parms = parms),
    regexp = "\n [.simulate_LM_OSL()] Argument 'start_power' has to be a positive number!", fixed = TRUE)
  
  expect_error(
    .simulate_LM_OSL(
      temp = 125, 
      duration = 100,
      start_power = 30,
      end_power =  10,
      n = n, 
      parms = parms),
    regexp = "\n [.simulate_LM_OSL()] Argument 'start_power' has to be smaller than 'end_power'!", fixed = TRUE)
  
  
  
  
})
