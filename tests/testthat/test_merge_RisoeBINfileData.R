context("merge_RisoeBINfileData")

##Full check
test_that("Test merging", {
  skip_on_cran()

  ##expect error
  expect_error(merge_Risoe.BINfileData(input.objects = "data"))
  expect_error(merge_Risoe.BINfileData(input.objects = c("data", "data2")))
  expect_error(merge_Risoe.BINfileData(input.objects = list("data", "data2")), regexp = "[merge_Risoe.BINfileData()] Input list does not contain Risoe.BINfileData objects!", fixed = TRUE)

  ##expect success
  data(ExampleData.BINfileData, envir = environment())
  object1 <- CWOSL.SAR.Data
  object2 <- CWOSL.SAR.Data
  expect_is(merge_Risoe.BINfileData(c(object1, object2)), "Risoe.BINfileData")

})

