psl.file <- system.file("extdata/DorNie_0016.psl", package = "Luminescence")

test_that("Test functionality", {
  testthat::skip_on_cran()

  ## default values
  expect_s4_class(read_PSL2R(
    file = psl.file
  ), "RLum.Analysis")

  ## custom values (all inverted), multiple files given to test merge=TRUE
  expect_s4_class(read_PSL2R(
    file = c(psl.file, psl.file),
    drop_bg = TRUE, as_decay_curve = FALSE, smooth = TRUE, merge = TRUE
  ), "RLum.Analysis")
})

test_that("Input validation", {
  testthat::skip_on_cran()

  ## directory given (assumes that we have a .psl file under inst/extdata)
  expect_message(
    read_PSL2R(file = system.file("extdata", package = "Luminescence")),
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
