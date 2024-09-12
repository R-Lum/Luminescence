## path to the XSYG file on github
github.url <- file.path("https://raw.githubusercontent.com/R-Lum",
                        "rxylib/master/inst/extdata/TLSpectrum.xsyg")
SW({
xsyg.file <- .download_file(github.url, tempfile("test_read_XSYG2R"))
})

test_that("test import of XSYG files", {
  testthat::skip_on_cran()

  ##force error
  expect_message(expect_null(read_XSYG2R("_error_file_")),
                "XML file not readable, nothing imported")
  expect_message(expect_null(read_XSYG2R("/Test", fastForward = TRUE)),
                 "XML file not readable, nothing imported")
  SW({
  expect_message(expect_null(read_XSYG2R(test_path("_data"))),
                 "No files matching the given pattern found in directory")
  })

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

  ## check also internal files
  SW({
  expect_type(read_XSYG2R(system.file("extdata/XSYG_file.xsyg",
                                      package = "Luminescence"),
                          fastForward = TRUE, import = TRUE,
                          verbose = TRUE),
              "list")
  })
})
