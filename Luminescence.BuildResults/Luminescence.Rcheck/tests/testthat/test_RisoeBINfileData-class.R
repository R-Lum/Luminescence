test_that("Check the example and the numerical values", {
  testthat::skip_on_cran()
  local_edition(3)

  ##construct empty object
  temp <-
    set_Risoe.BINfileData(METADATA = data.frame(), DATA = list(), .RESERVED = list())

  ##get function and check whether we get NULL
  expect_null(get_Risoe.BINfileData(temp))

  ##check object
  expect_s4_class(temp, class = "Risoe.BINfileData")
  expect_output(show(temp))

  ##show method
  data(ExampleData.BINfileData, envir = environment())
  expect_output(show(CWOSL.SAR.Data))

  ##as.data.frame
  expect_s3_class(as.data.frame(CWOSL.SAR.Data), "data.frame")

})
