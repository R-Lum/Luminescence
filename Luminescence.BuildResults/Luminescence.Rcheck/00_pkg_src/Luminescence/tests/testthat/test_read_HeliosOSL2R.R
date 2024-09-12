test_that("Test functionality", {
  testthat::skip_on_cran()

  ## crash function
  expect_error(
    object = read_HeliosOSL2R("error"),
    regexp = "\\[read\\_HeliosOSL2R\\(\\)\\] File extension")
  expect_error(
    object = read_HeliosOSL2R("error.err"),
    regexp = "\\[read\\_HeliosOSL2R\\(\\)\\] File extension")

  ## standard input
  file <- system.file("extdata/HeliosOSL_Example.osl", package = "Luminescence")
  SW({
  expect_s4_class(
    object = read_HeliosOSL2R(file),
    class = "RLum.Analysis")
  })

  ## no verbose
  expect_silent(
    object = read_HeliosOSL2R(file, verbose = FALSE))

  ## list input
  files <- list(file, file)
  SW({
  expect_type(
    object = read_HeliosOSL2R(files),
    type = "list")
  })
})
