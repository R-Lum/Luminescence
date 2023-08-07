##Full check
test_that("Test merging", {
  testthat::skip_on_cran()
  local_edition(3)

  ##expect error
  expect_message(merge_Risoe.BINfileData(input.objects = "data"), regexp = "\\[merge\\_Risoe\\.BINfileData\\(\\)\\] Nothing done.+")
  expect_error(merge_Risoe.BINfileData(input.objects = c("data", "data2")))
  expect_error(merge_Risoe.BINfileData(input.objects = list("data", "data2")), regexp = "[merge_Risoe.BINfileData()] Input list does not contain Risoe.BINfileData objects!", fixed = TRUE)

  ##expect success
  data(ExampleData.BINfileData, envir = environment())
  object1 <- CWOSL.SAR.Data
  object2 <- CWOSL.SAR.Data
  expect_s4_class(merge_Risoe.BINfileData(c(object1, object2)), "Risoe.BINfileData")

})

