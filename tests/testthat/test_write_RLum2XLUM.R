data(ExampleData.RLum.Analysis, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(write_RLum2XLUM("any", tempfile()),
               "unable to find an inherited method for function 'write_RLum2XLUM'")
  expect_error(write_RLum2XLUM(iris, tempfile()),
               "unable to find an inherited method for function 'write_RLum2XLUM'")
  expect_error(write_RLum2XLUM(IRSAR.RF.Data, tempfile()),
               "A method for RLum.Analysis objects is provided by the 'xlum' package")
  expect_error(write_RLum2XLUM(list(IRSAR.RF.Data), tempfile()),
               "A method for list objects is provided by the 'xlum' package")
})
