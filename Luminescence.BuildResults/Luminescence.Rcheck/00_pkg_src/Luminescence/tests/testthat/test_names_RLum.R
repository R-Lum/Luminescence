context("names_RLum")

test_that("Test whether function works", {
  testthat::skip_on_cran()

  data(ExampleData.RLum.Analysis, envir = environment())
  expect_silent(names_RLum(IRSAR.RF.Data))
  expect_is(names_RLum(IRSAR.RF.Data), "character")

})
