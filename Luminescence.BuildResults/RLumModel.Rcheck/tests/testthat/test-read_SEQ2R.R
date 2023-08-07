test_that("file is *.SEQ file",{
  skip_on_cran()
  local_edition(3)

  path <- system.file("extdata", "example_SAR_cycle.SEQ", package = "RLumModel")

  expect_error(read_SEQ2R(file = 2), regexp = "[read_SEQ2R()] class of file has to be a character.", fixed = TRUE)
  expect_error(read_SEQ2R(), "argument \"file\" is missing")
  expect_error(read_SEQ2R(""), regexp = "[read_SEQ2R()] file name doesn't seem to exist.", fixed = TRUE)
})

test_that("output",{
  skip_on_cran()
  local_edition(3)

  path <- system.file("extdata", "example_SAR_cycle.SEQ", package = "RLumModel")

  expect_equal(class(read_SEQ2R(file = path, txtProgressBar = FALSE)), "list")
  expect_output(read_SEQ2R(file = path), " [read_SEQ2R()]
	 Parse *.seq file to sequence for RLumModel
  |=========================================================| 100%")
})

test_that("lab.dose_rate > 0",{
  skip_on_cran()
  local_edition(3)

  path <- system.file("extdata", "example_SAR_cycle.SEQ", package = "RLumModel")

  expect_error(
    read_SEQ2R(file = path, txtProgressBar = FALSE, lab.dose_rate =  -1),
    "Argument 'lab.dose_rate' has to be positiv")
})
