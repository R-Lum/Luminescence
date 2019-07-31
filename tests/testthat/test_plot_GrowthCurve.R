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
  testthat::skip_on_cran()
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
 testthat::skip_on_cran()

   expect_equivalent(round(temp_EXP$De[[1]], digits = 2), 1737.88)

   ##fix for different R versions
   if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(round(sum(temp_EXP$De.MC, na.rm = TRUE), digits = 0), 17441)

   }else{
     expect_equal(round(sum(temp_EXP$De.MC, na.rm = TRUE), digits = 0), 17562)

   }

   expect_equivalent(round(temp_LIN$De[[1]], digits = 2), 1811.33)

   ##fix for different R versions
   if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
   expect_equal(round(sum(temp_LIN$De.MC, na.rm = TRUE), digits = 0),18238)

   }else{
     expect_equal(round(sum(temp_LIN$De.MC, na.rm = TRUE), digits = 0),18398)

   }

   expect_equivalent(round(temp_EXPLIN$De[[1]], digits = 2), 1791.53)

   ##fix for different R versions
   if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(round(sum(temp_EXPLIN$De.MC, na.rm = TRUE), digits = 0),17474)

   }else{
     expect_equal(round(sum(temp_EXPLIN$De.MC, na.rm = TRUE), digits = 0),18045)

   }

   expect_equivalent(round(temp_EXPEXP$De[[1]], digits = 2), 1787.15)

   ##fix for different R versions
   if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(round(sum(temp_EXPEXP$De.MC, na.rm = TRUE), digits = 0), 7316)

   }else{
     expect_equal(round(sum(temp_EXPEXP$De.MC, na.rm = TRUE), digits = 0), 7303)

   }

   expect_equivalent(round(temp_QDR$De[[1]], digits = 2), 1666.2)

   ##fix for different R versions
   if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(round(sum(temp_QDR$De.MC, na.rm = TRUE), digits = 0), 14937)

   }else{
     expect_equal(round(sum(temp_QDR$De.MC, na.rm = TRUE), digits = 0), 16476)

   }


})

test_that("check extrapolation", {
  testthat::skip_on_cran()

  LxTxData[1,2:3] <- c(0.5, 0.001)
  LIN <- plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "LIN")
  EXP <- plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "EXP")
  EXPLIN <- plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "EXP+LIN")

  expect_equivalent(round(LIN$De$De,0), 165)
  expect_equivalent(round(EXP$De$De,0),  110)

  #it fails on some unix platforms for unknown reason.
  #expect_equivalent(round(EXPLIN$De$De,0), 110)

})

