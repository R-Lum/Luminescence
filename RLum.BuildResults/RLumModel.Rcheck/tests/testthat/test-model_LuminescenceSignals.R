context("model_LuminescenceSignals")

test_that("check class and output", {
  
  output <- model_LuminescenceSignals(
    model = "Bailey2001", 
    sequence = list(
      OSL = c(temp = 20, duration = 1, optical_power = 100)),
    plot = FALSE, 
    verbose = FALSE)
  
  expect_equal(length(output), 12)
  expect_equal(is(output), c("RLum.Analysis", "RLum"))
  
  expect_silent(model_LuminescenceSignals(
    model = "Bailey2001", 
    sequence = list(
      OSL = c(20, 1, 100)), 
    plot = TRUE, 
    verbose = FALSE))
  
})


test_that("check SAR sequence", {
  
   sequence <- list(
      RegDose = c(0,10,20),
      TestDose = 5,
      PH = 240,
      CH = 200,
      OSL_temp = 125,
      OSL_duration = 70)
    
    temp <- model_LuminescenceSignals(
      sequence = sequence,
      model = "Pagonis2007",
      plot = FALSE,
      verbose = FALSE
    )

})

test_that("check DRT sequence", {
  
  sequence <- list(
    Irr_2recover = c(20),
    RegDose = c(0,10,20),
    TestDose = 5,
    PH = 240,
    CH = 200,
    OSL_temp = 125,
    OSL_duration = 70)
  
  temp <- model_LuminescenceSignals(
    sequence = sequence,
    model = "Pagonis2007",
    plot = FALSE,
    verbose = FALSE
  )
  
})

test_that("check Risoe SEQ", {
  
  path <- system.file("extdata", "example_SAR_cycle.SEQ", package="RLumModel")
  
  sequence <- read_SEQ2R(file = path,
                         txtProgressBar = FALSE)
  
  temp <- model_LuminescenceSignals(
    sequence = sequence,
    model = "Bailey2001",
    plot = FALSE,
    verbose = FALSE
  )
  
  temp <- model_LuminescenceSignals(
    sequence = path,
    model = "Bailey2001",
    plot = FALSE,
    verbose = FALSE
  )
  
})

test_that("check custom models", {
  
  own_parameters <- list(
    N = c(2e15, 2e15, 1e17, 2.4e16),
    E = c(0, 0, 0, 0),
    s = c(0, 0, 0, 0),
    A = c(2e-8, 2e-9, 4e-9, 1e-8),
    B = c(0, 0, 5e-11, 4e-8),
    Th = c(0, 0),
    E_th = c(0, 0),
    k_B = 8.617e-5,
    W = 0.64,
    K = 2.8e7,
    model = "customized",
    R = 1.7e15
  )
  
  own_state_parameters <- c(0, 0, 0, 9.4e15)
  
  own_start_temperature <- 10

  sequence <- list(
      RF = c(20, 1, 1))
  
  temp <- model_LuminescenceSignals(
    model = "customized", 
    sequence = sequence, 
    own_parameters = own_parameters,
    own_state_parameters = own_state_parameters,
    own_start_temperature = own_start_temperature,
    plot = FALSE,
    verbose = FALSE)
  
  temp <- model_LuminescenceSignals(
    model = "customized", 
    sequence = sequence, 
    own_parameters = own_parameters,
    own_start_temperature = own_start_temperature,
    plot = FALSE,
    verbose = FALSE)
  
})

test_that("test controlled crash conditions", {
  
  expect_error(
    model_LuminescenceSignals(
      model = "Bailey2001", 
      sequence = list(
        OSL = c(20, 1, 100)), 
      plot = FALSE, 
      verbose = FALSE,
      lab.dose_rate =  -1), 
    regexp = "[model_LuminescenceSignals()] lab.dose_rate has to be a positive number!",
    fixed = TRUE)
  
  expect_error(
    model_LuminescenceSignals(
      sequence = list(
        OSL = c(20, 1, 100))),
    "argument \"model\" is missing")
  
  expect_error(
    model_LuminescenceSignals(
      model = "Bailey2001"),
    "argument \"sequence\" is missing")
  
  expect_error(
    model_LuminescenceSignals(
      model = "Bailey2000",
      sequence = list(
        OSL = c(20,1,100))),
    regexp = "[model_LuminescenceSignals()] Model not supported. Supported models are: Bailey2001, Bailey2004, Pagonis2008, Pagonis2007, Bailey2002, Friedrich2017, customized",
    fixed = TRUE)
  
  expect_error(
    model_LuminescenceSignals(
      model = "Bailey2001",
      sequence = list(
        OSL = c("20",1,100))),
    regexp = "[model_LuminescenceSignals()] Sequence comprises non-numeric arguments!",
    fixed = TRUE)
  
  expect_error(
    model_LuminescenceSignals(
      model = "Bailey2001",
      sequence = matrix(NA)),
    regexp = "[model_LuminescenceSignals()] Sequence has to be of class list or a *.seq file",
    fixed = TRUE)
  
})