## input file
psl.file <- system.file("extdata/DorNie_0016.psl", package = "Luminescence")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_PSL2R(data.frame()),
               "'file' should be of class 'character'")
  expect_error(read_PSL2R(character(0)),
               "'file' cannot be an empty character")

  ## directory given (assumes that we have a .psl file under inst/extdata)
  expect_message(
    read_PSL2R(file = system.file("extdata", package = "Luminescence"),
               verbose = FALSE),
    "The following files were found and imported"
  )

  ## single file with no .psl extension
  expect_error(
    read_PSL2R(file = system.file("extdata/RF_file.rf",
                                  package = "Luminescence")),
    "No .psl files found"
  )

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

  ## custom values (all inverted), multiple files given to test merge=TRUE
  expect_snapshot_RLum(read_PSL2R(c(psl.file, psl.file), drop_bg = TRUE,
                                  as_decay_curve = FALSE, smooth = TRUE,
                                  merge = TRUE, verbose = FALSE))
  })
})
