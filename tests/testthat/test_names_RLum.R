test_that("check functionality", {
  testthat::skip_on_cran()

  data(ExampleData.RLum.Analysis, envir = environment())
  expect_silent(names_RLum(IRSAR.RF.Data))
  expect_type(names_RLum(IRSAR.RF.Data), "character")

  ##test a list of such elements
  expect_type(names_RLum(list(IRSAR.RF.Data,IRSAR.RF.Data, "a")), "list")

  ## empty object
  res <- names_RLum(set_RLum("RLum.Analysis"))
  expect_type(res, "list")
  expect_length(res, 0)
})
