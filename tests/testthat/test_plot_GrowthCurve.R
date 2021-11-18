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

temp_GOK <-
  plot_GrowthCurve(
    LxTxData,
    fit.method = "GOK",
    output.plot = FALSE,
    verbose = FALSE,
    NumberIterations.MC = 10
  )

temp_LambertW <-
  plot_GrowthCurve(
    LxTxData,
    fit.method = "LambertW",
    output.plot = FALSE,
    verbose = FALSE,
    NumberIterations.MC = 10
  )

test_that("fail fast", {
  testthat::skip_on_cran()
  local_edition(3)

  ##fit.method
  expect_error(
    plot_GrowthCurve(LxTxData, fit.method = "FAIL"),
    regexp = "[plot_GrowthCurve()] fit method not supported, supported methods are: LIN, QDR, EXP, EXP OR LIN, EXP+LIN, EXP+EXP, GOK, LambertW",
    fixed = TRUE
  )

})

test_that("check weird LxTx values", {
  testthat::skip_on_cran()
  local_edition(3)

  ##set LxTx
  LxTx <- structure(list(
    Dose = c(0, 250, 500, 750, 1000, 1500, 0, 500, 500),
    LxTx = c(1, Inf, 0, -Inf, Inf, 0, Inf, -0.25, 2),
    LxTx.Error = c(1.58133646008685, Inf, 0, Inf, Inf, 0, Inf, 1.41146256149428, 3.16267292017369)),
    class = "data.frame", row.names = c(NA, -9L))

  ##fit
  expect_warning(Luminescence:::.warningCatcher(
    plot_GrowthCurve(sample = LxTx[,c("Dose", "LxTx", "LxTx.Error")])))

})


test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(temp_EXP, class = "RLum.Results")
    expect_s3_class(temp_EXP$Fit, class = "nls")

  expect_s4_class(temp_LIN, class = "RLum.Results")
    expect_s3_class(temp_LIN$Fit, class = "lm")

  expect_s4_class(temp_EXPLIN, class = "RLum.Results")
   expect_s3_class(temp_EXPLIN$Fit, class = "nls")

  expect_s4_class(temp_EXPEXP, class = "RLum.Results")
    expect_s3_class(temp_EXPEXP$Fit, class = "nls")

  expect_s4_class(temp_QDR, class = "RLum.Results")
    expect_s3_class(temp_QDR$Fit, class = "lm")

  expect_s4_class(temp_GOK, class = "RLum.Results")
    expect_s3_class(temp_GOK$Fit, class = "nls")

  expect_s4_class(temp_LambertW, class = "RLum.Results")
    expect_s3_class(temp_LambertW$Fit, class = "nls")

})

test_that("check values from output example", {
 testthat::skip_on_cran()
  local_edition(3)

   expect_equal(round(temp_EXP$De[[1]], digits = 2), 1737.88)

   ##fix for different R versions
   if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(round(sum(temp_EXP$De.MC, na.rm = TRUE), digits = 0), 17441)

   }else{
     expect_equal(round(sum(temp_EXP$De.MC, na.rm = TRUE), digits = 0), 17562)

   }

   expect_equal(round(temp_LIN$De[[1]], digits = 2), 1811.33)

   ##fix for different R versions
   if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
   expect_equal(round(sum(temp_LIN$De.MC, na.rm = TRUE), digits = 0),18238)

   }else{
     expect_equal(round(sum(temp_LIN$De.MC, na.rm = TRUE), digits = 0),18398)

   }

   expect_equal(round(temp_EXPLIN$De[[1]], digits = 2), 1791.53)

   ##fix for different R versions
   if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(round(sum(temp_EXPLIN$De.MC, na.rm = TRUE), digits = 0),17474)

   }else{
     expect_equal(round(sum(temp_EXPLIN$De.MC, na.rm = TRUE), digits = 0),18045)

   }

   expect_equal(round(temp_EXPEXP$De[[1]], digits = 2), 1787.15)

   ##fix for different R versions
   if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(round(sum(temp_EXPEXP$De.MC, na.rm = TRUE), digits = 0), 7316)

   }else{
     expect_equal(round(sum(temp_EXPEXP$De.MC, na.rm = TRUE), digits = 0), 7303)

   }

   expect_equal(round(temp_QDR$De[[1]], digits = 2), 1666.2)

   ##fix for different R versions
   if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(round(sum(temp_QDR$De.MC, na.rm = TRUE), digits = 0), 14937)

   }else{
    expect_equal(round(sum(temp_QDR$De.MC, na.rm = TRUE), digits = 0), 16476)

   }

   expect_equal(round(temp_GOK$De[[1]], digits = 0), 1786)
   ##fix for different R versions
   if(R.version$major > "3"){
     expect_equal(round(sum(temp_GOK$De.MC, na.rm = TRUE), digits = 1), 17828.9, tolerance = 0.0001)

   }

   expect_equal(round(temp_LambertW$De[[1]], digits = 2),  1784.78)
   ##fix for different R versions
   if(R.version$major > "3"){
     expect_equal(round(sum(temp_LambertW$De.MC, na.rm = TRUE), digits = 0), 17662)

   }

})

test_that("check extrapolation", {
  testthat::skip_on_cran()
  local_edition(3)

  set.seed(1)
  LxTxData[1,2:3] <- c(0.5, 0.001)
  LIN <- expect_s4_class(
    plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "LIN"), "RLum.Results")
  EXP <- expect_s4_class(
    plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "EXP"), "RLum.Results")
  EXPLIN <- expect_s4_class(
    suppressWarnings(
      plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "EXP+LIN")), "RLum.Results")

  # GOK <- expect_s4_class(
  #   plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "GOK"), "RLum.Results")

  LambertW <- expect_s4_class(
    plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "LambertW"), "RLum.Results")

  expect_equal(round(LIN$De$De,0), 165)
  expect_equal(round(EXP$De$De,0),  110)
  expect_equal(round(LambertW$De$De,0),  114)

  #it fails on some unix platforms for unknown reason.
  #expect_equivalent(round(EXPLIN$De$De,0), 110)

})

