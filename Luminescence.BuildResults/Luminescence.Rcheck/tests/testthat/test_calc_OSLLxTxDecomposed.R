data(ExampleData.LxTxOSLData, envir = environment())
colnames(Lx.data) <- colnames(Tx.data) <- c("n", "n.error")

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
               "Invalid data type for OSL component")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, Tx.data,
                                      OSL.component = "error"),
               "Invalid OSL component name, valid names are:")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, Tx.data,
                                      OSL.component = 1000),
               "Invalid OSL component index, component table has 100 rows")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, digits = NA),
               "'digits' must be a positive integer scalar")
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  res <- calc_OSLLxTxDecomposed(Lx.data, Tx.data, digits = 2)
  expect_equal(is(res), c("RLum.Results", "RLum"))
  expect_equal(length(res), 1)
})
