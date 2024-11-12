test_that("Check the example and the numerical values", {
  testthat::skip_on_cran()

  ##construct empty object
  temp <-
    set_Risoe.BINfileData(METADATA = data.frame(), DATA = list(), .RESERVED = list())

  ##get function and check whether we get NULL
  expect_output(expect_null(
    get_Risoe.BINfileData(temp)),
    "No direct access is provided for this object type.")

  ##check object
  expect_s4_class(temp, class = "Risoe.BINfileData")
  expect_output(show(temp),
                "This object is empty!")

  ##show method
  data(ExampleData.BINfileData, envir = environment())
  expect_output(show(CWOSL.SAR.Data))

  ##as.data.frame
  expect_s3_class(as.data.frame(CWOSL.SAR.Data), "data.frame")

  ## metadata provided
  meta <- data.frame(ID = 1, VERSION = 7, RECTYPE = 128)
  temp <- set_Risoe.BINfileData(METADATA = meta, DATA = list(),
                                .RESERVED = list())
  expect_equal(temp@METADATA, meta)
  expect_output(show(temp))

  meta$RECTYPE <- NULL
  temp <- set_Risoe.BINfileData(METADATA = meta, DATA = list(),
                                .RESERVED = list())
  expect_equal(temp@METADATA, meta)
  expect_output(show(temp))
})
