test_that("full example test", {
  testthat::skip_on_cran()
  local_edition(3)

  ## load example dataset (this type of loading is 
  ## needed otherwise testthat produces an error)
  data(sample, envir = environment())
  input <- sample

  ## simple run
  aliquot <- expect_type(prepare_Aliquot(
    sample = input, 
    diameter = 0.1), "list")
  
  ### make sure the attributes are correct
  expect_true(attr(aliquot, "package") == "sandbox")
  
  ## crash function
  expect_error(prepare_Aliquot(
    sample = input, 
    diameter = 500), 
    "\\[prepare_Aliquot\\(\\)\\] chosen aliquot diameter too large; exceeding area sum of all grains!")
  
  ## crash because of wrong input
  test <- numeric(1)
  attr(test,"package") <- "sandbox"

  expect_error(prepare_Aliquot(
    sample = test, 
    diameter = 0.1), 
    "\\[prepare_Aliquot\\(\\)\\] the input for sample is not of type data.frame!")
  
  expect_error(prepare_Aliquot(
    sample = numeric(), 
    diameter = 0.1), 
    "\\[prepare_Aliquot\\(\\)\\] the input for sample is not an object created by 'sandbox'!")
  
})
  