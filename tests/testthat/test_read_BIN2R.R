test_that("input validation", {
  testthat::skip_on_cran()

  bin.v3 <- test_path("_data/BINfile_V3.bin")

  expect_error(read_BIN2R(TRUE),
               "'file' should be of class 'character' or 'list'")
  expect_error(read_BIN2R(character(0)),
               "'file' cannot be an empty character")
  expect_error(read_BIN2R(file = "error"),
               "File '.*error' does not exist") # windows CI needs the regexp
  expect_message(expect_null(read_BIN2R(test_path("test_read_BIN2R.R"))),
                 "is not a file of type 'BIN' or 'BINX'")
  expect_error(read_BIN2R(test_path("_data/BINfile_V3.bin"), verbose = FALSE,
                          forced.VersionNumber = 1),
               "BIN/BINX format version (01) is not supported or file is broken",
               fixed = TRUE)
  SW({
  expect_warning(read_BIN2R(bin.v3, position = 99),
                 "At least one position number is not valid")
  expect_message(read_BIN2R(bin.v3, forced.VersionNumber = 3),
                 "'forced.VersionNumber' set to 03, but this version")
  })

  ## arguments
  expect_error(read_BIN2R(bin.v3, show.raw.values = "error"),
               "'show.raw.values' should be a single logical value")
  expect_error(read_BIN2R(bin.v3, zero_data.rm = list(FALSE)),
               "'zero_data.rm' should be a single logical value")
  expect_error(read_BIN2R(bin.v3, duplicated.rm = "error"),
               "'duplicated.rm' should be a single logical value")
  expect_error(read_BIN2R(bin.v3, fastForward = "error"),
               "'fastForward' should be a single logical value")
  expect_error(read_BIN2R(bin.v3, show.record.number = NULL),
               "'show.record.number' should be a single logical value")
  expect_error(read_BIN2R(bin.v3, ignore.RECTYPE = "error"),
               "'ignore.RECTYPE' should be of class 'logical' or 'numeric'")

  ## check for broken files
  zero <- tempfile(pattern = "zero", fileext = ".binx")
  file.create(zero)
  expect_message(expect_null(read_BIN2R(zero, verbose = FALSE)),
                 "is a zero-byte file, NULL returned")
  write(raw(), zero)
  expect_error(read_BIN2R(zero, verbose = FALSE),
               "BIN/BINX format version \\(..\\) is not supported or file is")
  SW({
  expect_warning(
      expect_message(expect_null(read_BIN2R(zero, verbose = TRUE,
                                            forced.VersionNumber = 8)),
                     "Record #1 skipped due to wrong record length"),
      "0 records read, NULL returned")
  expect_warning(
      expect_message(expect_null(read_BIN2R(zero, verbose = TRUE,
                                            forced.VersionNumber = 3)),
                     "Record #1 skipped due to wrong record length"),
      "0 records read, NULL returned")
  })

  fake <- tempfile(pattern = "fake", fileext = ".binx")
  writeBin(as.raw(c(  8, 0,       # version
                    227, 5, 0, 0, # length
                      0, 0, 0, 0, # previous
                    250, 0, 0, 0, # npoints
                     99)          # rectype set to 99 to trigger errors
                  ), fake)
  expect_error(read_BIN2R(fake, verbose = FALSE),
               "Byte RECTYPE = 99 is not supported in record #1")
  SW({
  expect_message(res <- read_BIN2R(fake, verbose = TRUE, ignore.RECTYPE = TRUE),
                 "Byte RECTYPE = 99 is not supported in record #1")
  })
  expect_length(res, 0)
})

test_that("test the import of various BIN-file versions", {
  testthat::skip_on_cran()

  ## this test needs an internet connection ... test for it
  github.url <- file.path("https://github.com/R-Lum/Luminescence",
                          "raw/dev_0.9.x/tests/testthat/_data")
  if (!httr::http_error(github.url)) {
    ## V3
    expect_snapshot_plain(read_BIN2R(file.path(github.url, "BINfile_V3.bin"),
                                     verbose = FALSE))
  }

  ## V4
  expect_snapshot_plain(read_BIN2R(test_path("_data/BINfile_V4.bin"),
                                   verbose = FALSE))

  ## V5
  expect_snapshot_plain(read_BIN2R(test_path("_data/BINfile_V5.binx"),
                                   verbose = FALSE))

  ## V6
  expect_snapshot_plain(read_BIN2R(test_path("_data/BINfile_V6.binx"),
                                   verbose = FALSE))

  ## V7
  expect_snapshot_plain(read_BIN2R(test_path("_data/BINfile_V7.binx"),
                                   verbose = FALSE))

  SW({
  ## V8 - as part of the package
  bin.v8 <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  expect_snapshot_plain(read_BIN2R(bin.v8, verbose = FALSE))

  expect_message(res <- read_BIN2R(bin.v8, txtProgressBar = FALSE,
                                   position = 2),
                 "Kept records matching 'position': 2")
  expect_equal(length(res@.RESERVED), nrow(res@METADATA))
  expect_message(res <- read_BIN2R(bin.v8, txtProgressBar = FALSE,
                                   position = 1, fastForward = TRUE),
                 "Kept records matching 'position': 1")
  expect_type(res, "list")

    ## test n.records argument
    t_n.records_1 <- expect_s4_class(
      read_BIN2R(
        file = bin.v8,
        txtProgressBar = FALSE,
        n.records = 1), class = "Risoe.BINfileData")
    expect_length(t_n.records_1, n = 1)

    t_n.records_0 <- expect_s4_class(
      read_BIN2R(
        file = bin.v8,
        txtProgressBar = FALSE,
        n.records = 0), class = "Risoe.BINfileData")
    expect_length(t_n.records_0, n = 0)

    t_n.records_1_2 <- expect_s4_class(
      read_BIN2R(
        file = bin.v8,
        txtProgressBar = FALSE,
        n.records = 1:2), class = "Risoe.BINfileData")
    expect_length(t_n.records_1_2, n = 2)

  ## directory
  res <- read_BIN2R(test_path("_data"), show_record_number = TRUE)
  expect_type(res, "list")
  expect_length(res, 5)

  res <- read_BIN2R(test_path("_data"), pattern = "_V[34]")
  expect_type(res, "list")
  expect_length(res, 2)

  ## test further options
  ## n.records and fastForward
  expect_type(read_BIN2R(test_path("_data/BINfile_V4.bin"),
                         txtProgressBar = FALSE, n.records = 1,
                         fastForward = TRUE, verbose = FALSE),
              "list")

  ## options to Risoe.BINfileData2RLum.Analysis
  expect_warning(read_BIN2R(bin.v8, fastForward = FALSE, protocol = "test"),
                 "Additional arguments specified: 'protocol'")

  ## check ignore RECTYPE settings
  expect_message(t <- expect_s4_class(read_BIN2R(bin.v8, verbose = TRUE,
                                                 ignore.RECTYPE = 1),
                                      class = "Risoe.BINfileData"),
                 "Record #1 skipped due to ignore.RECTYPE setting")
  })

    ## should be zero now
    expect_length(t@DATA, n = 0)

  SW({
  res <- read_BIN2R(list(bin.v8, bin.v8), verbose = TRUE,
             position = list(1, 2), n.records = list(1, 2),
             show.raw.values = list(TRUE), zero_data.rm = list(FALSE),
             duplicated.rm = list(TRUE), show.record.number = list(TRUE),
             forced.VersionNumber = list(8), fastForward = TRUE)
  expect_length(res[[2]], 1)

  res <- read_BIN2R(bin.v8, verbose = FALSE, n.records = 2, fastForward = TRUE)
  expect_length(res, 1)

  res <- read_BIN2R(test_path("_data/BINfile_V3.bin"), n.records = 2)
  expect_length(res, 1)
  })

  res <- expect_silent(read_BIN2R(list()))
  expect_type(res, "list")
  expect_length(res, 0)
})

test_that("test hand-crafted files", {
  testthat::skip_on_cran()

  ## corrupted
  corrupted.bin <- test_path("_data/bin-tests/corrupted.bin")
  expect_warning(res <- read_BIN2R(corrupted.bin, verbose = FALSE),
                 "BIN-file appears to be corrupt, import limited to the first")
  expect_equal(nrow(res@METADATA), 1)
  expect_warning(res <- read_BIN2R(corrupted.bin, verbose = FALSE,
                                   n.records = 2),
                 "BIN-file appears to be corrupt, 'n.records' reset to 1")
  expect_equal(nrow(res@METADATA), 1)

  ## two versions
  res <- read_BIN2R(test_path("_data/bin-tests/two-versions.binx"),
                    verbose = FALSE)
  expect_equal(nrow(res@METADATA), 4)

  ## rectype 128
  rectype.128.bin <- test_path("_data/bin-tests/rectype-128.binx")
  res <- read_BIN2R(rectype.128.bin, verbose = FALSE)
  expect_equal(nrow(res@METADATA), 3)

  expect_output(res <- read_BIN2R(rectype.128.bin, verbose = FALSE,
                                  fastForward = TRUE),
                "BIN/BINX-file non-conform. TL curve may be wrong")
  expect_type(res, "list")
  expect_s4_class(res[[1]]@records[[1]], "RLum.Data.Curve")

  ## duplicate
  duplicate.bin <- test_path("_data/bin-tests/duplicated-records.binx")
  expect_warning(res <- read_BIN2R(duplicate.bin, verbose = FALSE),
                 "Duplicated records detected: 2")
  expect_equal(nrow(res@METADATA), 3)

  SW({
  expect_message(res <- read_BIN2R(duplicate.bin,
                                   duplicated.rm = TRUE, verbose = TRUE),
                 "Duplicated records detected and removed: 2")
  expect_equal(nrow(res@METADATA), 2)
  expect_equal(length(res@.RESERVED), nrow(res@METADATA))

  ## zero-data
  zero.data.bin <- test_path("_data/bin-tests/zero-data-record.binx")
  expect_warning(res <- read_BIN2R(zero.data.bin, verbose = TRUE),
                 "Zero-data records detected and removed: 2")
  expect_equal(nrow(res@METADATA), 1)
  expect_equal(length(res@.RESERVED), nrow(res@METADATA))

  expect_silent(res <- read_BIN2R(list(zero.data.bin), verbose = FALSE,
                                  zero_data.rm = FALSE))
  expect_equal(nrow(res[[1]]@METADATA), 2)

  expect_silent(res <- read_BIN2R(zero.data.bin, verbose = FALSE,
                                  zero_data.rm = FALSE))

  zero.data.all <- test_path("_data/bin-tests/zero-data-all.binx")
  expect_message(expect_warning(read_BIN2R(zero.data.all, txtProgressBar = FALSE),
                                "Zero-data records detected and removed: 1, 2"),
                 "Empty object returned")
  })
})
