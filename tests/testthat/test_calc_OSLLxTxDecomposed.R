## load data
data(ExampleData.LxTxOSLData, envir = environment())
colnames(Lx.data) <- colnames(Tx.data) <- c("n", "n.error")

## object from OSLdecomposed
Lx.decomp <- data.frame(name = c("Component 1", "Component 2"),
                        lambda = c(2.03, 0.02),
                        lambda.error = c(0.009, 0.002),
                        bleaching.grade = c(1, 0.3816),
                        t.start = c(0, 1.7),
                        t.end = c(1.7, 20),
                        ch.start = c(1, 18),
                        ch.end = c(17, 200),
                        n = c(4899.3, 7507.4),
                        bin = c(5046, 2701),
                        bin.RSS = c(5166.9, 5004.9),
                        bin.error = c(74.09, 70.94),
                        n.error = c(77.28, 208.61),
                        n.residual = c(0, 4643),
                        initial.signal = c(0.9804, 0.0196))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_OSLLxTxDecomposed(),
               "'Lx.data' should be of class 'data.frame")
  expect_error(calc_OSLLxTxDecomposed("test"),
               "'Lx.data' should be of class 'data.frame")
  expect_error(calc_OSLLxTxDecomposed(data.frame(col = integer(0))),
               "No valid component data.frame for Lx value")
  expect_error(calc_OSLLxTxDecomposed(iris),
               "'Lx.data' should contain the following columns:")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, "test"),
               "'Tx.data' should be of class 'data.frame")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, data.frame(col = integer(0))),
               "No valid component data.frame for Tx value")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, iris),
               "'Tx.data' should contain the following columns:")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, Tx.data,
                                      OSL.component = NA),
               "'OSL.component' should be of class 'integer', 'numeric' or")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, Tx.data,
                                      OSL.component = "error"),
               "Invalid OSL component name, valid names are:")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, Tx.data,
                                      OSL.component = 1000),
               "Invalid OSL component index, component table has 100 rows")
  expect_error(calc_OSLLxTxDecomposed(Lx.data, digits = NA),
               "'digits' should be a single positive integer value")
  expect_error(calc_OSLLxTxDecomposed(Lx.decomp, OSL.component = "1"),
               "Invalid OSL component name, valid names are: 'Component 1', 'Component 2'")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_RLum(calc_OSLLxTxDecomposed(Lx.data, Tx.data, digits = 2))
  expect_snapshot_RLum(calc_OSLLxTxDecomposed(Lx.data, Tx.data, digits = 2,
                                              OSL.component = 2))
  expect_snapshot_RLum(calc_OSLLxTxDecomposed(Lx.data, Tx.data, digits = 2,
                                              OSL.component = 3, sig0 = 1000))
  expect_snapshot_RLum(calc_OSLLxTxDecomposed(Lx.decomp,
                                              OSL.component = "Component 1"))
})
