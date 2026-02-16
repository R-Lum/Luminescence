## input file
psl.file <- system.file("extdata/DorNie_0016.psl", package = "Luminescence")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_PSL2R(data.frame()),
               "'file' should be of class 'character'")
  expect_error(read_PSL2R(character(0)),
               "'file' cannot be an empty character")

  ## directory given (assumes that we have a .psl file under inst/extdata)
  SW({
  expect_message(
    read_PSL2R(file = system.file("extdata", package = "Luminescence"),
               verbose = TRUE),
    "Directory detected, looking for")
  })
  expect_silent(
    read_PSL2R(file = system.file("extdata", package = "Luminescence"),
               verbose = FALSE))
  expect_error(read_PSL2R(system.file("extdata", package = "Luminescence"),
                          pattern = NA),
               "'pattern' should be of class 'character' and have length 1")

  ## single file with no .psl extension
  expect_error(
    read_PSL2R(file = system.file("extdata/RF_file.rf",
                                  package = "Luminescence")),
    "File extension 'rf' is not supported, only 'psl' is valid")
  expect_message(expect_message(
      expect_null(read_PSL2R(system.file("extdata", package = "Luminescence"),
                             pattern = "error")),
      "Directory detected, looking for 'error' files"),
      "No files matching the given pattern found in directory")

  ## list of files with a non-existent file
  expect_error(
    read_PSL2R(file = c(psl.file, "non-existent")),
    "The following files do not exist"
  )
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  SW({
  expect_snapshot_RLum(read_PSL2R(psl.file))

  ## custom values (all inverted), multiple files given to test merge = TRUE
  expect_snapshot_RLum(read_PSL2R(c(psl.file, psl.file), drop_bg = TRUE,
                                  as_decay_curve = FALSE, smooth = TRUE,
                                  merge = TRUE, verbose = FALSE))
  })
})
