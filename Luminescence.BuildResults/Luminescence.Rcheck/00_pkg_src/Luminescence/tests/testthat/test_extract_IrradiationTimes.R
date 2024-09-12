xsyg <- system.file("extdata/XSYG_file.xsyg", package="Luminescence")
binx <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(extract_IrradiationTimes("fail"),
               "Wrong XSYG file name or file does not exist!")
  expect_error(extract_IrradiationTimes(tempdir()),
               "File is expected to have 'xsyg' or 'XSYG' extension")
  expect_error(extract_IrradiationTimes(FALSE),
               "neither of type 'character' nor of type 'RLum.Analysis")
  expect_error(extract_IrradiationTimes(xsyg, file.BINX = "fail"),
               "Wrong BINX file name or file does not exist!")
  expect_error(extract_IrradiationTimes(xsyg, file.BINX = tempdir()),
               "File is expected to have 'binx' or 'BINX' extension")

  expect_message(extract_IrradiationTimes(xsyg, file.BINX = binx,
                                          txtProgressBar = FALSE),
                 "XSYG-file and BINX-file do not contain similar entries")
  expect_warning(extract_IrradiationTimes(list(xsyg), file.BINX = binx,
                                          txtProgressBar = FALSE),
                 "'file.BINX' is not supported in self-call mode")
})

test_that("Test the extraction of irradiation times", {
  testthat::skip_on_cran()

  ##general test
  SW({
  res <- expect_s4_class(extract_IrradiationTimes(xsyg, txtProgressBar = FALSE),
                         "RLum.Results")
  })

  ##check whether it makes sense
  expect_equal(sum(res$irr.times$IRR_TIME), 80)

  ## recordType
  res <- extract_IrradiationTimes(list(xsyg), recordType = list("OSL (UVVIS)"),
                                  txtProgressBar = FALSE)
  expect_true(all(res[[1]]@data$irr.times$STEP == "OSL (UVVIS)"))

  ## apply the function to something previously imported via read_BIN2R
  SW({
  temp <- read_BIN2R(binx, fastForward = TRUE)
  temp <- expect_s4_class(extract_IrradiationTimes(temp)[[1]], "RLum.Results")
  })
  expect_type(temp$irr.times$START, "double")
})
