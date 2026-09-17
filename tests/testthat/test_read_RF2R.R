## load data
file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
macro <- readLines(file)

## create file
file.rlumimage <- "RF_rlumimage.Rf"
rlumimage <- gsub("macro_version", "rlumimage_version",
                  gsub("17-10-2018", "0.0.1", macro))
writeLines(rlumimage, file.rlumimage)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(read_RF2R("error"),
               "File '.*error' does not exist") # windows CI needs the regexp
  expect_error(read_RF2R(2),
               "'file' should be of class 'character' or 'list'")
  expect_error(read_RF2R(character(0)),
               "'file' cannot be an empty character")
  expect_error(read_RF2R(list(data.frame())),
                 "All elements of 'file' should be of class 'character'")
  expect_message(expect_null(read_RF2R(list("error"))),
                 "Import for file 'error' failed, NULL returned")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ##simple import
  SW({
  expect_type(read_RF2R(file), type = "list")
  })
  expect_type(read_RF2R(file.rlumimage, verbose = FALSE),
              "list")

  ##import list
  expect_type(expect_message(read_RF2R(list(file, "test"), verbose = FALSE),
                             "Error: Import for file 'test' failed"),
              type = "list")

  ## import multiple files is allowed
  expect_type(read_RF2R(c(file, file), verbose = FALSE),
              type = "list")

  ## create a file with unsupported version
  file.wrong <- "RF_wrong_version.Rf"
  writeLines(gsub("macro_version", "unknown_version", macro),
             file.wrong)
  expect_error(read_RF2R(file.wrong),
               "Could not find a supported file format in the header line")

  writeLines(gsub("17-10-2018", "99-99-99", macro),
             file.wrong)
  expect_error(read_RF2R(file.wrong),
               "Format version 99-99-99 not supported for type 'macro'")

  writeLines(gsub("0.0.1", "99-99-99", rlumimage),
             file.wrong)
  expect_error(read_RF2R(file.wrong),
               "Format version 99-99-99 not supported for type 'rlumimage'")
  file.remove(file.wrong)
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  ## to keep snapshots small, we only test the info object

  expect_snapshot_plain(read_RF2R(file, verbose = FALSE)[[1]]@info)
  expect_snapshot_plain(read_RF2R(file.rlumimage, verbose = FALSE)[[1]]@info)

  ## create a file with malformed header
  file.wrong <- "RF_wrong_header.Rf"
  writeLines(gsub("grain_d=20", "grain_d=", readLines(file)),
             file.wrong)
  expect_snapshot_plain(read_RF2R(file.wrong, verbose = FALSE)[[1]]@info)
  file.remove(file.wrong)
})
