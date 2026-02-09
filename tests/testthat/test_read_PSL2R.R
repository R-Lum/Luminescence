## load file
psl.file <- system.file("extdata/DorNie_0016.psl", package = "Luminescence")

test_that("Test functionality", {
  testthat::skip_on_cran()

  ## default values
  SW({
  expect_s4_class(read_PSL2R(
    file = psl.file
  ), "RLum.Analysis")
  })

  ## custom values (all inverted), multiple files given to test merge=TRUE
  expect_s4_class(read_PSL2R(
    file = c(psl.file, psl.file),
    drop_bg = TRUE, as_decay_curve = FALSE, smooth = TRUE, merge = TRUE,
    verbose = FALSE
  ), "RLum.Analysis")
})

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_PSL2R(data.frame()),
               "'file' should be of class 'character'")
  expect_error(read_PSL2R(character(0)),
               "'file' cannot be an empty character")

  ## directory given (assumes that we have a .psl file under inst/extdata)
  expect_message(
    read_PSL2R(file = system.file("extdata", package = "Luminescence"),
               verbose = TRUE),
    "The following files were found and imported")
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
    "No .psl files found"
  )
  expect_error(read_PSL2R(system.file("extdata", package = "Luminescence"),
                          pattern = "error"),
               "No .psl files found")

  ## list of files with a non-existent file
  expect_error(
    read_PSL2R(file = c(psl.file, "non-existent")),
    "The following files do not exist"
  )
})
