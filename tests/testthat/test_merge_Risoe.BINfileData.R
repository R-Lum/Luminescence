## load data
data(ExampleData.BINfileData, envir = environment())
o1 <- CWOSL.SAR.Data
o2 <- CWOSL.SAR.Data

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(merge_Risoe.BINfileData(input.objects = c("data", "data2")),
               "File 'data' does not exist")
  expect_error(merge_Risoe.BINfileData(input.objects = list("data", "data2")),
               "All elements of 'input.objects' should be of class 'Risoe.BINfileData'")
  expect_error(merge_Risoe.BINfileData(input.objects = c(FALSE, FALSE)),
               "'input.objects' should be of class 'character' or 'list'")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## nothing done
  input <- "data"
  expect_message(res <- merge_Risoe.BINfileData(input.objects = input),
                 "At least two input objects are needed, nothing done")
  expect_equal(res, input)

  ## write to an output file and read it back in
  binx <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  output.file <- tempfile(fileext = ".binx")
  SW({
  merge_Risoe.BINfileData(c(binx, binx), output.file)
  })
  expect_true(file.exists(output.file))
  expect_s4_class(new <- read_BIN2R(output.file, verbose = FALSE),
                  "Risoe.BINfileData")
  expect_length(new, 4)
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot(merge_Risoe.BINfileData(c(o1, o2)))
})
