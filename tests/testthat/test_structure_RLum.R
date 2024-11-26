test_that("check functionality", {
  testthat::skip_on_cran()

  data(ExampleData.RLum.Analysis, envir = environment())
  expect_silent(structure_RLum(IRSAR.RF.Data))
  expect_s3_class(structure_RLum(IRSAR.RF.Data), "data.frame")

  ##test a list of such elements
  expect_type(structure_RLum(list(IRSAR.RF.Data,IRSAR.RF.Data, "a")), "list")

  ## empty object
  res <- structure_RLum(set_RLum("RLum.Analysis"))
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})
