test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_BIN2R(file = "error"),
               "File does not exist")
  expect_message(expect_null(read_BIN2R(test_path("test_read_BIN2R.R"))),
                 "is not a file of type 'BIN' or 'BINX'")
  expect_error(read_BIN2R(test_path("_data/BINfile_V3.bin"), verbose = FALSE,
                          forced.VersionNumber = 1),
               "BIN/BINX format version (01) is not supported or file is broken",
               fixed = TRUE)
  SW({
  expect_warning(read_BIN2R(test_path("_data/BINfile_V3.bin"), position = 99),
                 "At least one position number is not valid")
  expect_message(read_BIN2R(test_path("_data/BINfile_V3.bin"),
                            forced.VersionNumber = 3),
                 "'forced.VersionNumber' set to 03, but this version")
  })

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
  expect_message(read_BIN2R(fake, verbose = TRUE, ignore.RECTYPE = TRUE),
                 "Byte RECTYPE = 99 is not supported in record #1")
  })
})

test_that("test the import of various BIN-file versions", {
  testthat::skip_on_cran()

  ## this test needs an internet connection ... test for it
  github.url <- file.path("https://github.com/R-Lum/Luminescence",
                          "raw/dev_0.9.x/tests/testthat/_data")
  if (!httr::http_error(github.url)) {
    ## V3
    expect_s4_class(read_BIN2R(file.path(github.url, "BINfile_V3.bin"),
                               verbose = FALSE),
                    class = "Risoe.BINfileData")
  }

  ## V4
  expect_s4_class(read_BIN2R(test_path("_data/BINfile_V4.bin"),
                             verbose = FALSE),
                  class = "Risoe.BINfileData")

  ## V5
  expect_s4_class(read_BIN2R(test_path("_data/BINfile_V5.binx"),
                             verbose = FALSE),
                  class = "Risoe.BINfileData")

  ## V6
  expect_s4_class(read_BIN2R(test_path("_data/BINfile_V6.binx"),
                             verbose = FALSE),
                  class = "Risoe.BINfileData")

  ## V7
  expect_s4_class(read_BIN2R(test_path("_data/BINfile_V7.binx"),
                             verbose = FALSE),
                  class = "Risoe.BINfileData")

  SW({
  ## V8 - as part of the package
  bin.v8 <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  expect_s4_class(read_BIN2R(bin.v8, txtProgressBar = FALSE),
                  class = "Risoe.BINfileData")

  ## V8 - as part of the package ... with arguments
  expect_type(read_BIN2R(bin.v8, txtProgressBar = FALSE,
                         position = 1, fastForward = TRUE),
              "list")

    ## test n.records argument
    t_n.records_1 <- expect_s4_class(
      read_BIN2R(
        file = bin.v8,
        txtProgressBar = FALSE,
        n.records = 1), class = "Risoe.BINfileData")
    t_n.records_0 <- expect_s4_class(
      read_BIN2R(
        file = bin.v8,
        txtProgressBar = FALSE,
        n.records = 0), class = "Risoe.BINfileData")

    t_n.records_1_2 <- expect_s4_class(
      read_BIN2R(
        file = bin.v8,
        txtProgressBar = FALSE,
        n.records = 1:2), class = "Risoe.BINfileData")

      ## test length
      expect_length(t_n.records_1, n = 1)
      expect_length(t_n.records_0, n = 1)
      expect_length(t_n.records_1_2, n = 2)

  ## V8 - as part of the package ... with arguments
  expect_type(read_BIN2R(bin.v8, txtProgressBar = FALSE, fastForward = TRUE),
              "list")

  ## directory
  res <- read_BIN2R(test_path("_data"), show_record_number = TRUE)
  expect_type(res, "list")
  expect_length(res, 5)

    ## test n.records

  ## test further options
  ## n.records and fastForward
  expect_type(read_BIN2R(test_path("_data/BINfile_V4.bin"),
                         txtProgressBar = FALSE, n.records = 1,
                         fastForward = TRUE, verbose = FALSE),
              "list")
  })

  SW({
  ## check ignore RECTYPE settings
  expect_message(t <- expect_s4_class(read_BIN2R(bin.v8, verbose = TRUE,
                                                 ignore.RECTYPE = 1),
                                      class = "Risoe.BINfileData"),
                 "Record #1 skipped due to ignore.RECTYPE setting")
  })

    ## should be zero now
    expect_length(t@DATA, n = 0)

  SW({
  ## this combination of position and n.records creates an empty object
  ## at position 2
  res <- read_BIN2R(list(bin.v8, bin.v8), verbose = TRUE,
             position = list(1, 2), n.records = list(1, 2),
             show.raw.values = list(TRUE), zero_data.rm = list(FALSE),
             duplicated.rm = list(TRUE), show.record.number = list(TRUE),
             forced.VersionNumber = list(8), fastForward = TRUE)
  })
  expect_length(res[[2]], 0)
})
