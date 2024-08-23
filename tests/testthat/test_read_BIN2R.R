test_that("test the import of various BIN-file versions", {
  testthat::skip_on_cran()

  ##test for various errors
  expect_error(read_BIN2R(file = ""), "[read_BIN2R()] File does not exist!", fixed = TRUE)

  ## this test needs an internet connection ... test for it
  github.url <- file.path("https://github.com/R-Lum/Luminescence",
                          "raw/dev_0.9.x/tests/testthat/_data")
  if (!httr::http_error(github.url)) {
    ## V3
    expect_s4_class(read_BIN2R(file.path(github.url, "BINfile_V3.bin"),
                               txtProgressBar = FALSE),
                    class = "Risoe.BINfileData")
  }

  ## V4
  expect_s4_class(read_BIN2R(test_path("_data/BINfile_V4.bin"),
                             txtProgressBar = FALSE),
                  class = "Risoe.BINfileData")

  ## V5
  expect_s4_class(read_BIN2R(test_path("_data/BINfile_V5.binx"),
                             txtProgressBar = FALSE),
                  class = "Risoe.BINfileData")

  ## V6
  expect_s4_class(read_BIN2R(test_path("_data/BINfile_V6.binx"),
                             txtProgressBar = FALSE),
                  class = "Risoe.BINfileData")

  ## V6 - show method
  expect_output(read_BIN2R(test_path("_data/BINfile_V6.binx"),
                           txtProgressBar = FALSE))

  ## V7
  expect_s4_class(read_BIN2R(test_path("_data/BINfile_V7.binx"),
                             txtProgressBar = FALSE),
                  class = "Risoe.BINfileData")

  ## V8 - as part of the package
  expect_s4_class(
      read_BIN2R(system.file("extdata/BINfile_V8.binx", package = "Luminescence"),
                                  txtProgressBar = FALSE), class = "Risoe.BINfileData")

    ##V8 - as part of the package ... with arguments
    expect_type(
      read_BIN2R(
        file = system.file("extdata/BINfile_V8.binx", package = "Luminescence"),
        txtProgressBar = FALSE,
        position = 1,
        fastForward = TRUE), "list")

    ## test n.records argument
    t_n.records_1 <- expect_s4_class(
      read_BIN2R(
        file = system.file("extdata/BINfile_V8.binx", package = "Luminescence"),
        txtProgressBar = FALSE,
        n.records = 1), class = "Risoe.BINfileData")
    t_n.records_0 <- expect_s4_class(
      read_BIN2R(
        file = system.file("extdata/BINfile_V8.binx", package = "Luminescence"),
        txtProgressBar = FALSE,
        n.records = 0), class = "Risoe.BINfileData")

    t_n.records_1_2 <- expect_s4_class(
      read_BIN2R(
        file = system.file("extdata/BINfile_V8.binx", package = "Luminescence"),
        txtProgressBar = FALSE,
        n.records = 1:2), class = "Risoe.BINfileData")

      ## test length
      expect_length(t_n.records_1, n = 1)
      expect_length(t_n.records_0, n = 1)
      expect_length(t_n.records_1_2, n = 2)

    ##V8 - as part of the package ... with arguments
    expect_type(
      read_BIN2R(system.file("extdata/BINfile_V8.binx", package = "Luminescence"),
                 txtProgressBar = FALSE, fastForward = TRUE), "list")


    ## test n.records

  ## test further options
  ## n.records and fastForward
  expect_type(read_BIN2R(test_path("_data/BINfile_V4.bin"),
                         txtProgressBar = FALSE, n.records = 1,
                         fastForward = TRUE, verbose = FALSE),
              "list")

   ## check for broken files
   t <- tempfile(pattern = "zero", fileext = ".binx")
   write(raw(), t)
   expect_error(read_BIN2R(t), regexp = "\\[read\\_BIN2R\\(\\)\\] Found BIN\\/BINX-format version \\(..\\) is not supported or the BIN/BINX-file is broken! Supported version numbers are: 03, 04, 05, 06, 07, 08.")
   file.remove(t)

   ## check ignore RECTYPE settings
   t <- expect_s4_class(
     read_BIN2R(system.file("extdata/BINfile_V8.binx", package = "Luminescence"),
                txtProgressBar = FALSE, ignore.RECTYPE = 1), class = "Risoe.BINfileData")

    ## should be zero now
    expect_length(t@DATA, n = 0)
})
