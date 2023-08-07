test_that("full example test", {
  testthat::skip_on_cran()
  local_edition(3)

  ## load and prepare test 
  data(sample_osl_aliquots, envir = environment())
  
  ## set sequence
  sequence <- list(
    RegDose = c(0, 1, 2, 5, 10, 0, 1),
    TestDose = 2,
    PH = 220,
    CH = 200,
    OSL_temp = 125,
    OSL_duration = 70)
  
  ## simple run
  sar_all <- expect_type(measure_SAR_OSL(
    aliquot = sample_osl_aliquots,
    sequence = sequence,
    dose_rate = 0.1), "list")
  
  ## do we really have a RLum objects?
  expect_s4_class(sar_all[[1]], "RLum.Analysis")

  ## crash because of wrong input
  test <- numeric(1)
  attr(test,"package") <- "sandbox"

  expect_error(measure_SAR_OSL(
    aliquot = test, 
    sequence = sequence), 
    "\\[measure_SAR_OSL\\(\\)] the input for aliquot is not of type data.frame!")
  
  expect_error(measure_SAR_OSL(
    aliquot = numeric(), 
    sequence = sequence), 
    "\\[measure_SAR_OSL\\(\\)\\] the input for aliquot is not an object created by 'sandbox'!")
  
})
  