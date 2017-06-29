context("read_SEQ2R")

path <- system.file("extdata", "example_SAR_cycle.SEQ", package = "RLumModel")

test_that("file is *.SEQ file",{
  expect_error(read_SEQ2R(file = 2), regexp = "[read_SEQ2R()] class of file has to be a character.", fixed = TRUE)
  expect_error(read_SEQ2R(), "argument \"file\" is missing")
  expect_error(read_SEQ2R(""), regexp = "[read_SEQ2R()] file name doesn't seem to exist.", fixed = TRUE)
})

test_that("output",{
  expect_equal(class(read_SEQ2R(file = path, txtProgressBar = FALSE)), "list")
  expect_output(read_SEQ2R(file = path), " [read_SEQ2R()] 
	 Parse *.seq file to sequence for RLumModel
  |=========================================================| 100%")
})

test_that("lab.dose_rate > 0",{
  expect_error(read_SEQ2R(file = path, txtProgressBar = FALSE, lab.dose_rate =  -1), "Argument 'lab.dose_rate' has to be positiv")
})