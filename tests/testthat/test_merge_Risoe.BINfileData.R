test_that("Test merging", {
  testthat::skip_on_cran()

  ##expect error
  expect_error(merge_Risoe.BINfileData(input.objects = c("data", "data2")),
               "File 'data' does not exist")
  expect_error(merge_Risoe.BINfileData(input.objects = list("data", "data2")),
               "All elements of 'input.objects' should be of class 'Risoe.BINfileData'")
  expect_error(merge_Risoe.BINfileData(input.objects = c(FALSE, FALSE)),
               "'input.objects' should be of class 'character' or 'list'")

  ## nothing done
  input <- "data"
  expect_message(res <- merge_Risoe.BINfileData(input.objects = input),
                 "Nothing done: at least two input objects are needed")
  expect_equal(res, input)

  ## expect success
  data(ExampleData.BINfileData, envir = environment())
  object1 <- CWOSL.SAR.Data
  object2 <- CWOSL.SAR.Data
  expect_s4_class(merge_Risoe.BINfileData(c(object1, object2)), "Risoe.BINfileData")

  binx <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  output.file <- tempfile()
  SW({
  merge_Risoe.BINfileData(c(binx, binx), output.file)
  })
  expect_true(file.exists(output.file))
})
