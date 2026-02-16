xsyg <- system.file("extdata/XSYG_file.xsyg", package="Luminescence")
binx <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(extract_IrradiationTimes("fail"),
               "File '.*fail' does not exist") # windows CI needs the regexp
  expect_error(extract_IrradiationTimes(character(0)),
               "'object' cannot be an empty character")
  expect_error(extract_IrradiationTimes(letters),
               "'object' should have length 1")
  expect_error(extract_IrradiationTimes(tempdir()),
               "File '.*' does not exist") # windows CI needs the regexp
  expect_error(extract_IrradiationTimes(test_path("_data/BINfile_V3.bin")),
               "File extension 'bin' is not supported, only 'xsyg' is valid")
  expect_error(extract_IrradiationTimes(FALSE),
               "'object' should be of class 'character', 'RLum.Analysis' or a")
  expect_error(extract_IrradiationTimes(xsyg, return_same_as_input = "error"),
               "return_same_as_input' should be a single logical value")
  SW({
  expect_error(extract_IrradiationTimes(xsyg, file.BINX = "fail"),
               "File '.*fail' does not exist") # windows CI needs the regexp
  expect_error(extract_IrradiationTimes(xsyg, file.BINX = tempdir()),
               "File '.*' does not exist") # windows CI needs the regexp
  })
  expect_message(extract_IrradiationTimes(xsyg, file.BINX = binx,
                                          txtProgressBar = FALSE),
                 "XSYG-file and BINX-file do not contain similar entries")
  expect_warning(extract_IrradiationTimes(list(xsyg), file.BINX = binx,
                                          txtProgressBar = FALSE),
                 "'file.BINX' is not supported in self-call mode")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  res_a <- extract_IrradiationTimes(list(xsyg), txtProgressBar = FALSE,
                                    return_same_as_input = TRUE)
  expect_type(res_a, "list")
  expect_s4_class(res_a[[1]], "RLum.Analysis")

  ## apply the function to something previously imported via read_BIN2R
  SW({
  temp <- read_BIN2R(binx, fastForward = TRUE)
  temp <- expect_s4_class(extract_IrradiationTimes(temp)[[1]], "RLum.Results")
  })
  expect_type(temp$irr.times$START, "double")

  ## write results to BINX-file
  tmp_BINX <- tempfile(fileext = ".binx")
  file.copy(
    from = system.file("extdata/BINfile_V8.binx", package = "Luminescence"),
    to = tmp_BINX, overwrite = TRUE)
  expect_message(
    extract_IrradiationTimes(xsyg, file.BINX = tmp_BINX, txtProgressBar = FALSE),
    "XSYG-file and BINX-file do not contain similar entries, BINX-file update skipped")

  tmp_XSYG <- test_path("_data/xsyg-tests/XSYG_file_TL_CASE3.xsyg")
  tmp_BINX <- tempfile(fileext = ".binx")
  write_R2BIN(read_BIN2R(binx, position = 2, verbose = FALSE),
              tmp_BINX, verbose = FALSE)
  expect_message(extract_IrradiationTimes(tmp_XSYG, file.BINX = tmp_BINX,
                                          txtProgressBar = FALSE),
                 "'Time Since Irradiation' was redefined in the exported BINX-file")

  ## empty list
  expect_null(extract_IrradiationTimes(list()))

  ## special case extraction with startDate missing
  tmp <- read_XSYG2R(xsyg, verbose = FALSE, fastForward = TRUE)
    ## remove startDate
    tmp[[1]]@records[[9]]@info$startDate <- NULL
    ## test
    expect_s4_class(
      object = extract_IrradiationTimes(tmp[[1]]), class = "RLum.Results")

   ## use TL curves with NA
   tmp <- read_XSYG2R(xsyg, verbose = FALSE, fastForward = TRUE)
   expect_s4_class(extract_IrradiationTimes(tmp[[1]]), class = "RLum.Results")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(extract_IrradiationTimes(xsyg, txtProgressBar = FALSE),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(extract_IrradiationTimes(xsyg, txtProgressBar = FALSE,
                                                return_same_as_input = TRUE),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(extract_IrradiationTimes(list(xsyg), txtProgressBar = FALSE,
                                                recordType = list("OSL (UVVIS)")),
                       tolerance = snapshot.tolerance)
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 1273
  expect_error(extract_IrradiationTimes(iris, file.BINX = NA),
               "'object' should be of class 'character', 'RLum.Analysis' or")
})
