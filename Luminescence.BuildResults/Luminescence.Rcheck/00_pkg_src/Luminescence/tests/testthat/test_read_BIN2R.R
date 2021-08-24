test_that("test the import of various BIN-file versions", {
  testthat::skip_on_cran()
  local_edition(3)

  ##test for various errors
  expect_error(read_BIN2R(file = ""), "[read_BIN2R()] File does not exist!", fixed = TRUE)

  ##this test need an internet connect ... test for it
  if(!httr::http_error("https://github.com/R-Lum/Luminescence/tree/master/tests/testdata")){
    ##try to import every format by using the files on GitHub
    ##V3
    expect_s4_class(
      suppressWarnings(read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V3.bin",
                 txtProgressBar = FALSE)), class = "Risoe.BINfileData")

    ##V4
    expect_s4_class(
      suppressWarnings(read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V4.bin",
                 txtProgressBar = FALSE)), class = "Risoe.BINfileData")

    ##V5
    expect_s4_class(
      suppressWarnings(read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V5.binx",
                 txtProgressBar = FALSE)), class = "Risoe.BINfileData")

    ##V6
    expect_s4_class(
      suppressWarnings(read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V6.binx",
                 txtProgressBar = FALSE)), class = "Risoe.BINfileData")

    ##V6 - show method
    expect_output(
      suppressWarnings(read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V6.binx",
                 txtProgressBar = FALSE))
    )

    ##V7
    expect_s4_class(
      suppressWarnings(read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V7.binx",
                 txtProgressBar = FALSE)), class = "Risoe.BINfileData")

    ##V8
    expect_s4_class(
      suppressWarnings(read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V8.binx",
                 txtProgressBar = FALSE)), class = "Risoe.BINfileData")

    ##test further options
    ##n.records and fastForward
    expect_type(
      suppressWarnings(read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V4.bin",
                 txtProgressBar = FALSE, n.records = 1, fastForward = TRUE, verbose = FALSE)), "list")

   }



})
