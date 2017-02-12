context("read_BIN2R")

test_that("test the import of various BIN-file versions", {
  testthat::skip_on_cran()

  ##test for various erros
  expect_error(read_BIN2R(file = ""), "[read_BIN2R()] File does not exist!", fixed = TRUE)


  ##this test need an internet connect ... test for it
  if(!httr::http_error("https://github.com/R-Lum/Luminescence/tree/master/tests/testdata")){

    ##tryp to import every format by using the files on GitHub

    ##V3
    expect_is(
      read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V3.bin%20",
                 txtProgressBar = FALSE), class = "Risoe.BINfileData")

    ##V4
    expect_is(
      read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V4.bin%20",
                 txtProgressBar = FALSE), class = "Risoe.BINfileData")

    ##V6
    expect_is(
      read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V6.bin%20",
                 txtProgressBar = FALSE), class = "Risoe.BINfileData")

    ##V7
    expect_is(
      read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V7.bin%20",
                 txtProgressBar = FALSE), class = "Risoe.BINfileData")

    ##V8
    expect_is(
      read_BIN2R(file = "https://github.com/R-Lum/Luminescence/raw/master/tests/testdata/BINfile_V8.bin%20",
                 txtProgressBar = FALSE), class = "Risoe.BINfileData")



  }

})
