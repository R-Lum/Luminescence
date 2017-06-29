context("create_SAR.sequence")


test_that("check class of output",{
  expect_equal(class(sequence <- .create_SAR.sequence(
    RegDose = c(0,8),
    TestDose = 5,
    PH = 240,
    CH = 200,
    OSL_temp = 125
  ))
  , "list")
})

test_that("check error for missing object",{
  expect_error(
    sequence <- .create_SAR.sequence(
      error.test), 
    "object 'error.test' not found")
  
  expect_error(
    sequence <- .create_SAR.sequence(
      RegDose = c(0,8),
      TestDose = 5,
      PH = 240,
      CH = 200), 
    "argument \"OSL_temp\" is missing")
  
  expect_error(
    sequence <- .create_SAR.sequence(
      RegDose = c(0,8),
      TestDose = 5,
      PH = 240,
      OSL_temp = 125), 
    "argument \"CH\" is missing")
  
  expect_error(
    sequence <- .create_SAR.sequence(
      RegDose = c(0,8),
      TestDose = 5,
      OSL_temp = 125,
      CH = 200), 
    "argument \"PH\" is missing")
  
  expect_error(
    sequence <- .create_SAR.sequence(
      RegDose = c(0,8),
      PH = 240,
      CH = 200,
      OSL_temp = 125), 
    "argument \"TestDose\" is missing")
  
  expect_error(
    sequence <- .create_SAR.sequence(
      TestDose = 5,
      PH = 240,
      CH = 200,
      OSL_temp = 125), 
    "argument \"RegDose\" is missing")
  
  
})