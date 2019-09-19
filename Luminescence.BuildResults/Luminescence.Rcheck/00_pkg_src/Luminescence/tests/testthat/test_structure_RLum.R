context("structure_RLum")

test_that("Test whether the function works", {
  testthat::skip_on_cran()

  data(ExampleData.RLum.Analysis, envir = environment())
  expect_silent(structure_RLum(IRSAR.RF.Data))
  expect_is(structure_RLum(IRSAR.RF.Data), "data.frame")

  ##test a list of such elements
  expect_is(structure_RLum(list(IRSAR.RF.Data,IRSAR.RF.Data, "a")), "list")

})
