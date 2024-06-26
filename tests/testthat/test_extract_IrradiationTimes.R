test_that("Test the extraction of irradiation times", {
  testthat::skip_on_cran()
  local_edition(3)

  ##set file
  file <- system.file("extdata/XSYG_file.xsyg", package="Luminescence")

  ## break function
  expect_error(extract_IrradiationTimes("fail"), "\\[extract_IrradiationTimes\\(\\)\\] Wrong XSYG file name or file does not exsits!")

  ##general test
  results <- expect_s4_class(extract_IrradiationTimes(object = file, txtProgressBar = FALSE), "RLum.Results")

  ##check whether it makes sense
  expect_equal(sum(results$irr.times$IRR_TIME), 80)

  ## apply the function to something previously imported via read_BIN2R
  file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  temp <- read_BIN2R(file, fastForward = TRUE)
  temp <- expect_s4_class(extract_IrradiationTimes(temp)[[1]], "RLum.Results")
  expect_type(temp$irr.times$START, "double")

})


