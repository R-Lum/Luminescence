context("plot_GrowthCurve")

set.seed(1)
data(ExampleData.LxTxData, envir = environment())
temp_EXP <-
  plot_GrowthCurve(
    LxTxData,
    fit.method = "EXP",
    output.plot = FALSE,
    verbose = FALSE,
    NumberIterations.MC = 10
  )
temp_LIN <-
  plot_GrowthCurve(
    LxTxData,
    fit.method = "LIN",
    output.plot = FALSE,
    verbose = FALSE,
    NumberIterations.MC = 10
  )
temp_EXPLIN <-
  plot_GrowthCurve(
    LxTxData,
    fit.method = "EXP+LIN",
    output.plot = FALSE,
    verbose = FALSE,
    NumberIterations.MC = 10
  )
temp_EXPEXP <-
  plot_GrowthCurve(
    LxTxData,
    fit.method = "EXP+EXP",
    output.plot = FALSE,
    verbose = FALSE,
    NumberIterations.MC = 10
  )
temp_QDR <-
  plot_GrowthCurve(
    LxTxData,
    fit.method = "QDR",
    output.plot = FALSE,
    verbose = FALSE,
    NumberIterations.MC = 10
  )

test_that("check class and length of output", {
  expect_is(temp_EXP, class = "RLum.Results", info = NULL, label = NULL)
    expect_is(temp_EXP$Fit, class = "nls")

  expect_is(temp_LIN, class = "RLum.Results", info = NULL, label = NULL)
    expect_is(temp_LIN$Fit, class = "lm")

  expect_is(temp_EXPLIN, class = "RLum.Results", info = NULL, label = NULL)
   expect_is(temp_EXPLIN$Fit, class = "nls")

  expect_is(temp_EXPEXP, class = "RLum.Results", info = NULL, label = NULL)
    expect_is(temp_EXPEXP$Fit, class = "nls")

  expect_is(temp_QDR, class = "RLum.Results", info = NULL, label = NULL)
    expect_is(temp_QDR$Fit, class = "lm")

})

test_that("check values from output example", {
 expect_equivalent(round(temp_EXP$De[[1]], digits = 2), 1737.88)
  expect_equal(round(sum(temp_EXP$De.MC, na.rm = TRUE), digits = 2), 17440.55)

 expect_equivalent(round(temp_LIN$De[[1]], digits = 2), 1811.33)
  expect_equal(round(sum(temp_LIN$De.MC, na.rm = TRUE), digits = 2),18238.02)

 expect_equivalent(round(temp_EXPLIN$De[[1]], digits = 2), 1791.53)
   expect_equal(round(sum(temp_EXPLIN$De.MC, na.rm = TRUE), digits = 2),17474.29)

 expect_equivalent(round(temp_EXPEXP$De[[1]], digits = 2), 1787.15)
  expect_equal(round(sum(temp_EXPEXP$De.MC, na.rm = TRUE), digits = 0), 7316)

 expect_equivalent(round(temp_QDR$De[[1]], digits = 2), 1666.2)
  expect_equal(round(sum(temp_QDR$De.MC, na.rm = TRUE), digits = 2), 14936.76)


})
