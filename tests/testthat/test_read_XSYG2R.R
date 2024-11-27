## path to the XSYG file on github
github.url <- file.path("https://raw.githubusercontent.com/R-Lum",
                        "rxylib/master/inst/extdata/TLSpectrum.xsyg")
xsyg.file <- .download_file(github.url, tempfile("test_read_XSYG2R"),
                            verbose = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_XSYG2R(data.frame()),
               "'file' should be of class 'character' or 'list'")
  expect_error(read_XSYG2R(character(0)),
               "'file' cannot be an empty character")
  expect_message(expect_null(read_XSYG2R("_error_file_")),
                "Error: File does not exist, nothing imported")
  expect_message(expect_null(read_XSYG2R("/Test", fastForward = TRUE)),
                 "Error: File does not exist, nothing imported")
  expect_message(expect_null(read_XSYG2R(test_path("_data/xsyg-tests/XSYG_broken.xsyg"), fastForward = TRUE)),
                 "Error: XML file not readable, nothing imported")

  SW({
  expect_message(expect_null(read_XSYG2R(test_path("_data/bin-tests/"))),
                 "No files matching the given pattern found in directory")
  })
})

test_that("test import of XSYG files", {
  testthat::skip_on_cran()

  ## download from github
  expect_type(read_XSYG2R(github.url, import = FALSE, verbose = FALSE),
              "list")

  ## local file
  expect_type(read_XSYG2R(xsyg.file, verbose = FALSE),
              "list")
  expect_s3_class(read_XSYG2R(xsyg.file, fastForward = TRUE,
                              import = FALSE, verbose = FALSE),
                  "data.frame")
  expect_silent(read_XSYG2R(xsyg.file, verbose = FALSE,
                            recalculate.TL.curves = FALSE))
  expect_silent(read_XSYG2R(xsyg.file, verbose = FALSE, pattern = "xsyg",
                            recalculate.TL.curves = FALSE))
  expect_type(read_XSYG2R(xsyg.file, fastForward = FALSE,
                          verbose = FALSE),
              "list")
  results <- expect_type(read_XSYG2R(xsyg.file, fastForward = TRUE,
                                     verbose = FALSE),
                         type = "list")
  expect_type(results[[1]]@info$file, type = "character")
  expect_output(print(results))

  ## check n_records argument
  expect_type(read_XSYG2R(xsyg.file, verbose = FALSE, n_records = 1),
              "list")
  expect_type(read_XSYG2R(xsyg.file, verbose = FALSE, n_records = 10),
              "list")
  expect_error(read_XSYG2R(xsyg.file, verbose = FALSE, n_records = "error"),
               regexp = "\\[read\\_XSYG2R\\(\\)\\] 'n\\_records' should be of class")

  ## list input
  expect_type(read_XSYG2R(list(xsyg.file), fastForward = TRUE,
                          verbose = FALSE),
              "list")
  expect_s3_class(read_XSYG2R(list(xsyg.file), fastForward = TRUE,
                              import = FALSE, verbose = FALSE),
                  "data.frame")
  expect_type(read_XSYG2R(list(xsyg.file), fastForward = FALSE,
                          verbose = FALSE),
              "list")
  expect_length(read_XSYG2R(list()), 0)

  ## check also internal files
  SW({
  expect_type(read_XSYG2R(system.file("extdata/XSYG_file.xsyg",
                                      package = "Luminescence"),
                          fastForward = TRUE, import = TRUE,
                          verbose = TRUE),
              "list")
  })

  ## more tests for different TL curve calculation cases
  ## CASE2
  expect_s4_class(
    read_XSYG2R(test_path("_data/xsyg-tests/XSYG_file_TL_CASE2.xsyg"), fastForward = TRUE, txtProgressBar = FALSE, verbose = FALSE)[[1]],
    "RLum.Analysis")
  ## CASE3
  expect_s4_class(
    read_XSYG2R(test_path("_data/xsyg-tests/XSYG_file_TL_CASE3.xsyg"), fastForward = TRUE, txtProgressBar = FALSE, verbose = FALSE)[[1]],
    "RLum.Analysis")

  ## check case for no record Type
  t <- expect_s4_class(
    read_XSYG2R(test_path("_data/xsyg-tests/XSYG_noRecordType.xsyg"), fastForward = TRUE, txtProgressBar = FALSE, verbose = FALSE)[[1]],
    "RLum.Analysis")
  expect_equal(t@records[[1]]@recordType, "not_set (UVVIS)")

  ## check case for OSL record but IRSL stimulator
  t <- expect_s4_class(
    read_XSYG2R(test_path("_data/xsyg-tests/XSYG_corr_IRSL.xsyg"), fastForward = TRUE, txtProgressBar = FALSE, verbose = FALSE)[[1]],
    "RLum.Analysis")
  expect_equal(t@records[[1]]@recordType, "IRSL (NA)")

  ## check case for spectrum
  t <- expect_s4_class(
    read_XSYG2R(test_path("_data/xsyg-tests/XSYG_spectra.xsyg"), fastForward = TRUE, txtProgressBar = FALSE, verbose = FALSE)[[1]],
    "RLum.Analysis")
  ## check case for spectrum ... equal heating element values
  t <- expect_s4_class(
    read_XSYG2R(test_path("_data/xsyg-tests/XSYG_spectra_equal_TL.xsyg"), fastForward = TRUE, txtProgressBar = FALSE, verbose = FALSE)[[1]],
    "RLum.Analysis")
  ## check case for spectrum ... duplicated values
  expect_warning(
    read_XSYG2R(test_path("_data/xsyg-tests/XSYG_spectra_duplicated_TL.xsyg"), fastForward = TRUE, txtProgressBar = FALSE, verbose = FALSE),
    regexp = "Temperature values are found to be duplicated and increased by 1 K.")

})
