test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_HeliosOSL2R(data.frame()),
               "'file' should be of class 'character' or 'list'")
  expect_error(read_HeliosOSL2R(character(0)),
               "'file' should have length 1")
  expect_error(read_HeliosOSL2R("error"),
               "[read_HeliosOSL2R()] File extension '' not supported",
               fixed = TRUE)
  expect_error(object = read_HeliosOSL2R("error.err"),
               "[read_HeliosOSL2R()] File extension 'err' not supported",
               fixed = TRUE)
})

test_that("Test functionality", {
  testthat::skip_on_cran()

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

  ## list input
  files <- list(file, file, "wrong.xs")
  SW({
    t <- expect_type(
      object = read_HeliosOSL2R(files, verbose = FALSE),
      type = "list")
  })
  expect_length(t,2)
})
