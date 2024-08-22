data(ExampleData.LxTxOSLData, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_OSLLxTxDecomposed(),
               "is missing, with no default")
  expect_error(calc_OSLLxTxDecomposed("test"),
               "No valid component data.frame for Lx value")
  expect_error(calc_OSLLxTxDecomposed(data.frame(col = integer(0))),
               "No valid component data.frame for Lx value")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, "test"),
               "No valid component data.frame for Tx value")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, data.frame(col = integer(0))),
               "No valid component data.frame for Tx value")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, Tx.data,
                                      OSL.component = NA),
               "Type error! No valid data type for OSL.component")
})
