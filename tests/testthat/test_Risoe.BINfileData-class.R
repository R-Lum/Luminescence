## load data
data(ExampleData.BINfileData, envir = environment())
obj <- subset(CWOSL.SAR.Data, ID <= 50)

test_that("Check the example and the numerical values", {
  testthat::skip_on_cran()

  expect_error(set_Risoe.BINfileData(METADATA = NA),
               "'METADATA' should be of class 'data.frame'")

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

  ## also empty object
  temp <- set_Risoe.BINfileData(METADATA = NULL)
  expect_s4_class(temp, class = "Risoe.BINfileData")
  expect_output(show(temp),
                "This object is empty!")

  ##show method
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

test_that("sort_RLum", {
  testthat::skip_on_cran()

  ## input validation
  expect_error(sort_RLum(obj),
               "'info_element' should be of class 'character'")
  expect_error(sort_RLum(obj, info_element = "error"),
               "Invalid 'info_element' name, valid names are:")
  expect_error(sort_RLum(obj, info_element = "ID", decreasing = "error"),
               "'decreasing' should be a single logical value")

  ## check functionality
  expect_snapshot_Risoe(sort_RLum(obj, info_element = "POSITION"))
})
