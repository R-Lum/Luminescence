context("RisoeBINfileData Class Tests")

test_that("Check the example and the numerical values", {
  ##construct empty object
  temp <-
    set_Risoe.BINfileData(METADATA = data.frame(), DATA = list(), .RESERVED = list())

  ##check object
  expect_is(temp, class = "Risoe.BINfileData")

})
