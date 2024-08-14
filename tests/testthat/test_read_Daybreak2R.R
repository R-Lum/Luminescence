test_that("Test functionality", {
  testthat::skip_on_cran()
  local_edition(3)

  ##TXT
  ##basic import options
  expect_type(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
  ), "list")

  ## directory
  expect_error(
    expect_output(read_Daybreak2R(
      file = system.file("extdata", package = "Luminescence")),
      "Directory detected, trying to extract"),
  "file name does not seem to exist")

  ##verbose off
  expect_type(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence"),
    verbose = FALSE
  ), "list")

  ##txtProgressbar off
  expect_type(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence"),
    txtProgressBar = FALSE
  ), "list")

  ##DAT
  ##basic import options
  expect_type(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.DAT", package = "Luminescence")
  ), "list")

  expect_s3_class(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.DAT", package = "Luminescence"), raw = TRUE
  ), "data.table")

  ##test silence
  expect_silent(read_Daybreak2R(
    file = system.file("extdata/Daybreak_TestFile.DAT", package = "Luminescence"), verbose = FALSE))

  ## test presence of non-ascii characters
  expect_error(read_Daybreak2R(
    file = system.file("extdata/BINfile_V8.binx", package = "Luminescence"),
    verbose = FALSE),
    "The provided file is no ASCII-file and cannot be imported")

  file.nonascii <- tempfile()
  writeLines(gsub("ScriptFile", "ScriptFile ö",
                  readLines(system.file("extdata/Daybreak_TestFile.txt",
                                        package = "Luminescence"))),
             file.nonascii)
  expect_error(read_Daybreak2R(file = file.nonascii, verbose = FALSE),
    "The provided file is no ASCII-file and cannot be imported")
})
