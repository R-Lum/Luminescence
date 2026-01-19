## load data
data(ExampleData.RLum.Analysis, envir = environment())
temp <- IRSAR.RF.Data

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(subset(temp, LTYPE == "RF"),
               "[get_RLum()] Invalid subset expression, valid terms are",
               fixed = TRUE)
  SW({
  expect_message(expect_null(subset(temp, recordType == "xx")),
                 "'subset' expression produced an empty selection, NULL returned")
  })
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ### empty call
  expect_s4_class(subset(temp), "RLum.Analysis")
  expect_length(subset(temp), length(temp))
  expect_identical(subset(temp)[[1]], temp[[1]])

  ### valid
  expect_s4_class(subset(temp, recordType == "RF"), class = "RLum.Analysis")
  expect_s4_class(subset(temp, recordType == "RF")[[1]], class = "RLum.Data.Curve")
  expect_length(subset(temp, recordType == "RF"), n = length(temp))

  ## get_RLum(<obj>, subset = (<condition>))
  expect_s4_class(get_RLum(temp, subset = recordType == "RF"), class = "RLum.Analysis")
  expect_s4_class(get_RLum(temp, subset = recordType == "RF")[[1]], class = "RLum.Data.Curve")
  expect_length(get_RLum(temp, subset = recordType == "RF"), n = length(temp))

})
