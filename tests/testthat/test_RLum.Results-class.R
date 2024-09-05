data(ExampleData.DeValues, envir = environment())
obj <- calc_FuchsLang2001(ExampleData.DeValues$BT998, cvThreshold = 5,
                          plot = FALSE, verbose = FALSE)
empty <- set_RLum("RLum.Results")

test_that("check class", {
  testthat::skip_on_cran()

  ## set_RLum()
  expect_s4_class(obj, "RLum.Results")

  ## as()
  expect_type(as(obj, "list"),
              "list")
  expect_s4_class(as(list(), "RLum.Results"),
                  "RLum.Results")

  ## show()
  expect_output(show(obj))
  expect_output(show(empty))

  ## names()
  expect_equal(names_RLum(obj),
               c("summary", "data", "args", "usedDeValues"))
})

test_that("get_RLum", {
  testthat::skip_on_cran()

  ## input validation
  expect_error(get_RLum(obj, "error"),
               "unknown 'data.object', valid names are:")
  expect_error(get_RLum(obj, FALSE),
               "'data.object' has to be of type character or numeric")
  expect_error(get_RLum(obj, 100),
               "'data.object' index out of bounds")
  expect_warning(expect_null(get_RLum(obj, info.object = "error")),
                 "[get_RLum()] Invalid 'info.object' name, valid names are:",
                 fixed = TRUE)
  expect_warning(expect_null(get_RLum(empty, info.object = "error")),
                 "[get_RLum()] This 'RLum.Results' object has no info objects",
                 fixed = TRUE)

  ## full functionality
  expect_s3_class(get_RLum(obj),
                  "data.frame")
  expect_s3_class(get_RLum(obj, data.object = 2),
                  "data.frame")
  expect_s3_class(get_RLum(obj, data.object = c(1, 2)),
                  "data.frame")
  expect_s3_class(get_RLum(obj, data.object = "summary"),
                  "data.frame")
  expect_s3_class(get_RLum(obj, data.object = c("summary", "data")),
                  "data.frame")
  expect_s4_class(get_RLum(obj, data.object = c("summary", "data"), drop = FALSE),
                  "RLum.Results")
  expect_type(get_RLum(obj, info.object = "call"),
              "list")
})
