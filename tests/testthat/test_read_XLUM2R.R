xlum.file <- system.file("extdata/XLUM_file.xlum", package = "Luminescence")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_XLUM2R(data.frame()),
               "'file' should be of class 'character' or 'list'")
  expect_error(read_XLUM2R(character(0)),
               "'file' cannot be an empty character")
  expect_error(read_XLUM2R(list()),
               "'file' cannot be an empty list")
  expect_message(expect_null(read_XLUM2R("_error_file_")),
                "File '.*_error_file_' does not exist") # windows CI needs the regexp
  expect_message(expect_null(read_XLUM2R("/Test", fastForward = TRUE)),
                 "File '.*/Test' does not exist") # windows CI needs the regexp
  expect_message(expect_null(do.call(read_XLUM2R, list("a test"))),
                 "File '.*a test' does not exist") # windows CI needs the regexp
  expect_message(expect_null(read_XLUM2R(test_path("_data/xsyg-tests/XSYG_broken.xsyg"))),
                 "Error: XML file cannot be parsed (StartTag: invalid element name)",
                 fixed = TRUE)
  expect_error(read_XLUM2R(test_path("_data/xsyg-tests/XSYG_noSequence.xsyg")),
               "<xlum> root node not found in")
  expect_error(read_XLUM2R(xlum.file, n_sequences = "error"),
               "'n_sequences' should be a single positive integer value or NULL")
  expect_error(read_XLUM2R(xlum.file, n_records = 0),
               "'n_records' should be a single positive integer value or NULL")
  expect_error(read_XLUM2R(xlum.file, n_sequences = 2, n_records = 2),
               "Only one of 'n_sequences' and 'n_records' can be specified")
  expect_error(read_XLUM2R(xlum.file, pattern = c("p1", "p2")),
               "'pattern' should be of class 'character' and have length 1")

  ## must be tested on vector of input filenames
  expect_error(read_XLUM2R(c("A", "B"), fastForward = NA),
               "'fastForward' should be a single logical value")
  expect_error(read_XLUM2R(c("A", "B"), fastForward = TRUE, import = NA),
               "'import' should be a single logical value")

  SW({
  expect_message(expect_null(read_XLUM2R(test_path("_data/bin-tests/"))),
                 "No files matching the given pattern found in directory")
  })

  ## pretend that `base64enc` is not installed
  with_mocked_bindings("requireNamespace" = function(x, ...) FALSE, {
    expect_warning(read_XLUM2R(xlum.file, verbose = FALSE),
                   "Reading base64-encoded XLUM files")
  }, .package = "base")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_silent(res <- read_XLUM2R(xlum.file, verbose = FALSE))
  expect_type(res,
              "list")
  expect_named(res[[1]],
               c("Sequence.Header", "Sequence.Object"))
  expect_s3_class(read_XLUM2R(xlum.file, import = FALSE, fastForward = TRUE,
                              verbose = FALSE),
                  "data.frame")
  expect_type(read_XLUM2R(xlum.file, fastForward = FALSE,
                          verbose = FALSE),
              "list")
  results <- expect_type(read_XLUM2R(xlum.file, fastForward = TRUE,
                                     verbose = FALSE),
                         type = "list")
  expect_length(results, 1)
  expect_type(results[[1]]@info$file, type = "character")
  expect_equal(results[[1]]@records[[1]]@info$sequenceName,
               "Example")
  expect_length(results[[1]]@records[[1]]@info, 14)

  ## pattern
  SW({
  expect_message(read_XLUM2R(test_path("_data/"), pattern = "XLUM"),
                 "looking for files matching the 'XLUM' pattern")
  })

  ## n_sequences
  expect_length(res <- read_XLUM2R(xlum.file, n_sequences = 1, fastForward = TRUE,
                                   verbose = FALSE),
                1)
  expect_length(res[[1]]@records, 3)

  ## n_records
  expect_length(res <- read_XLUM2R(xlum.file, n_records = 2, fastForward = TRUE,
                                   verbose = FALSE),
                1)
  expect_length(res[[1]]@records, 2)

  ## list input
  expect_type(read_XLUM2R(list(xlum.file), fastForward = TRUE,
                          verbose = FALSE),
              "list")
  expect_s3_class(read_XLUM2R(list(xlum.file), fastForward = TRUE,
                              import = FALSE, verbose = FALSE),
                  "data.frame")
  expect_type(read_XLUM2R(list(xlum.file), fastForward = FALSE,
                          verbose = FALSE),
              "list")

  SW({
  ## recursive traversal
  expect_length(read_XLUM2R(test_path("_data"),
                            fastForward = TRUE, verbose = FALSE),
                8)
  })

  ## check broken files
  SW({
  expect_equal(read_XLUM2R(test_path("_data/xlum-tests/broken_1.xlum")),
               list())
  expect_equal(read_XLUM2R(test_path("_data/xlum-tests/broken_2.xlum")),
               list())
  expect_equal(read_XLUM2R(test_path("_data/xlum-tests/broken_3.xlum")),
               list())
  expect_warning(expect_equal(read_XLUM2R(test_path("_data/xlum-tests/broken_4.xlum")),
                              list()),
                 "1 incomplete sequence(s) removed", fixed = TRUE)
  ## no recordType
  expect_equal(read_XLUM2R(test_path("_data/xlum-tests/broken_5.xlum"),
                           fastForward = TRUE)[[1]]@records[[1]]@recordType,
               "not_set (string)")
  ## all xValues, yValues, tValues set to "0"
  expect_warning(read_XLUM2R(test_path("_data/xlum-tests/broken_6.xlum"),
                             fastForward = TRUE),
                 "Invalid value specification, <curve> node skipped")
  })
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_plain(read_XLUM2R(xlum.file, import = FALSE,
                                    fastForward = FALSE, verbose = FALSE))
  expect_snapshot_plain(read_XLUM2R(xlum.file, import = FALSE,
                                    fastForward = TRUE, verbose = FALSE))
  expect_snapshot_RLum(read_XLUM2R(xlum.file,
                                   fastForward = FALSE, verbose = FALSE))
  expect_snapshot_RLum(read_XLUM2R(xlum.file,
                                   fastForward = TRUE, verbose = FALSE))
  expect_snapshot_RLum(read_XLUM2R(test_path("_data/xlum-tests/base64.xlum"),
                                   fastForward = TRUE, verbose = FALSE))
  expect_snapshot_RLum(read_XLUM2R(test_path("_data/xlum-tests/curve_1.xlum"),
                                   fastForward = TRUE, verbose = FALSE))
  expect_snapshot_RLum(read_XLUM2R(test_path("_data/xlum-tests/image_1.xlum"),
                                   fastForward = TRUE, verbose = FALSE))
  expect_snapshot_RLum(read_XLUM2R(test_path("_data/xlum-tests/image_2.xlum"),
                                   fastForward = TRUE, verbose = FALSE))
  expect_snapshot_RLum(read_XLUM2R(test_path("_data/xlum-tests/spectrum_1.xlum"),
                                   fastForward = TRUE, verbose = FALSE))
  expect_snapshot_RLum(read_XLUM2R(test_path("_data/xlum-tests/risoe_1.xlum"),
                                   fastForward = TRUE, verbose = FALSE))
})
