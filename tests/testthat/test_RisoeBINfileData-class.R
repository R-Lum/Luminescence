context("RisoeBINfileData Class Tests")

test_that("Check the example and the numerical values", {
  testthat::skip_on_cran()
  ##construct empty object
  temp <-
    set_Risoe.BINfileData(METADATA = data.frame(), DATA = list(), .RESERVED = list())

  ##get function and check whether we get NULL
  expect_null(get_Risoe.BINfileData(temp))


  ##check object
  expect_is(temp, class = "Risoe.BINfileData")
  expect_output(show(temp))

  ##show method
  data(ExampleData.BINfileData, envir = environment())
  expect_output(show(CWOSL.SAR.Data))

})
