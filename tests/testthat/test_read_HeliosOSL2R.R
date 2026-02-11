test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_HeliosOSL2R(data.frame()),
               "'file' should be of class 'character' or 'list'")
  expect_error(read_HeliosOSL2R(character(0)),
               "'file' cannot be an empty character")
  expect_error(read_HeliosOSL2R(NA_character_),
               "File 'NA' does not exist")
  expect_error(read_HeliosOSL2R("error"),
               "File '.*error' does not exist") # windows CI needs the regexp
  expect_error(object = read_HeliosOSL2R(test_path("_data/BINfile_V3.bin")),
               "[read_HeliosOSL2R()] File extension 'bin' is not supported, only",
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

  ## all list elements skipped
  expect_message(expect_null(read_HeliosOSL2R(list("error"), verbose = FALSE)),
                 "record skipped")
})
