test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

# Prepare test ------------------------------------------------------------
  ##set path to YAML reference file file
  yaml_file <- system.file("extdata/example.yml", package = "BayLum")

# Simple runs -------------------------------------------------------------

  ## run without parameters, which should give a list
  expect_type(write_YAMLConfigFile(), "list")
  expect_output(write_YAMLConfigFile())

  ## run with addition parameters to be provided ... for sample
  t <- expect_type(write_YAMLConfigFile(sample = "test"), "list")
  expect_true(t[[1]]$sample == "test")

  ## run with sample of length > 1
  t <- expect_type(write_YAMLConfigFile(sample = c("test1", "test2")), "list")
  expect_length(t, 2)

  ## run with additional valid parameter
  t <- expect_type(write_YAMLConfigFile(settings.rules.endTest = 10), "list")
  expect_true(t[[1]]$settings$rules$endTest == 10)

  ## use a longer input value than expect (should not change anything)
  t <- expect_type(write_YAMLConfigFile(settings.rules.endTest = c(10, 10)), "list")
  expect_true(t[[1]]$settings$rules$endTest == 10)

  ## use a parameter that is not supported ... should trigger warning
  expect_warning(
    object = write_YAMLConfigFile(error = "test"),
    regexp = "\\[write\\_YAMLConfigFile\\(\\)\\] The following parameter.+)")

  ## run with file to be written and the output is a character
  expect_type(write_YAMLConfigFile(output_file = tempfile("test")), "character")

})

