## load data
data(ExampleData.BINfileData, envir = environment())
test_file_MG <- test_file_SG <- CWOSL.SAR.Data
test_file_SG@METADATA$GRAIN <- 1

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(convert_SG2MG(data.frame()),
               "'object' should be of class 'character' or 'Risoe.BINfileData'")
  expect_error(convert_SG2MG(character(0)),
               "'object' cannot be an empty character")
  expect_error(convert_SG2MG("error"),
               "File '.*error' does not exist") # windows CI needs the regexp
})

test_that("test conversion from single grain data to multiple grain data", {
  testthat::skip_on_cran()

  ## test pass through for pure multiple grain data
  expect_s4_class(convert_SG2MG(test_file_MG), "Risoe.BINfileData")

  ## test with pseudo single grain data
  expect_s4_class(convert_SG2MG(test_file_SG), "Risoe.BINfileData")

  ## test write option
  ## create environment
  dir <- tempdir()
  tmp <- file.path(dir, "test.bin")
  SW({
  write_file_test <- write_R2BIN(
      read_BIN2R(file = test_path("_data/BINfile_V4.bin")),
      file = tmp)

  expect_s4_class(convert_SG2MG(tmp, write_file = TRUE, txtProgressBar = FALSE),
                  "Risoe.BINfileData")
  })

  ##clear temp folder otherwise we have a problem with the CRAN check
  file.remove(list.files(dir,pattern = ".bin", full.names = TRUE))
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 1415
  small <- subset(test_file_SG, POSITION == 1)
  expect_silent(convert_SG2MG(small))
})
