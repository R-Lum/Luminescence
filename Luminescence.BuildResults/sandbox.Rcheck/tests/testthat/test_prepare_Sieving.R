test_that("full example test", {
  testthat::skip_on_cran()
  local_edition(3)

  ## load example dataset (this type of loading is 
  ## needed otherwise testthat produces an error)
  data(sample, envir = environment())
  input <- sample

  ## simple run
  sample <- expect_type(prepare_Sieving(
    sample = input, 
    interval = c(1,9)), "list")
  
  ### make sure the attributes are correct
  expect_true(attr(sample, "package") == "sandbox")
  
  ## trigger warning
  expect_warning(prepare_Sieving(
    sample = input, 
    interval = c(1,9,2)), "Parameter interval must be numeric of length two, full interval taken!")
  
  ## crash because of wrong input
  test <- numeric(1)
  attr(test,"package") <- "sandbox"

  expect_error(prepare_Sieving(
    sample = test, 
    interval = c(1,2)), 
    "\\[prepare_Sieving\\(\\)\\] the input for sample is not of type data.frame!")
  
  expect_error(prepare_Sieving(
    sample = numeric(), 
    interval = c(1,2)), 
    "\\[prepare_Sieving\\(\\)\\] the input for sample is not an object created by 'sandbox'!")
  
})
  