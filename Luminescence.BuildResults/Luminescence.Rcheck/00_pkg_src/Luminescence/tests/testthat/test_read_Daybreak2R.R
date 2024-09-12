test_that("Test functionality", {
  testthat::skip_on_cran()

  txt.file <- system.file("extdata/Daybreak_TestFile.txt",
                          package = "Luminescence")
  dat.file <- system.file("extdata/Daybreak_TestFile.DAT",
                          package = "Luminescence")

  ## TXT
  SW({
  expect_type(read_Daybreak2R(txt.file), "list")
  expect_type(read_Daybreak2R(txt.file, txtProgressBar = FALSE),
              "list")
  })
  expect_silent(read_Daybreak2R(txt.file, verbose = FALSE))

  ## DAT
  SW({
  expect_type(read_Daybreak2R(dat.file), "list")
  expect_s3_class(read_Daybreak2R(dat.file, raw = TRUE),
                  "data.table")
  })
  expect_silent(read_Daybreak2R(dat.file, verbose = FALSE))

  ## list
  SW({
  expect_type(read_Daybreak2R(list(dat.file)), "list")
  })

  ## directory
  expect_error(
    expect_output(read_Daybreak2R(
      file = system.file("extdata", package = "Luminescence")),
      "Directory detected, trying to extract"),
  "file name does not seem to exist")

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
