test_that("plot_GrowthCurve", {
  testthat::skip_on_cran()
  local_edition(3)

  ## load data
  data(ExampleData.LxTxData, envir = environment())

  ##fit.method
  expect_error(
    object = plot_GrowthCurve(LxTxData, fit.method = "FAIL"),
    regexp = "\\[plot\\_GrowthCurve\\(\\)\\] Fit method not supported, supported.+")

  ## input object
  expect_error(
    object = plot_GrowthCurve("test"),
    regexp = "\\[plot\\_GrowthCurve\\(\\)\\] Argument 'sample' needs to be of type 'data.frame'\\!")

  ## shorten dataframe
  expect_error(
    object = plot_GrowthCurve(LxTxData[1:2,]),
    regexp = "\\[plot\\_GrowthCurve\\(\\)\\] At least two regeneration points are required!")

  ## wrong argument for mode
  expect_error(
    object = plot_GrowthCurve(LxTxData, mode = "fail"),
    regexp = "\\[plot\\_GrowthCurve\\(\\)\\] Unknown input for argument 'mode'")

# Weird LxTx values --------------------------------------------------------

  ##set LxTx
  LxTx <- structure(list(
    Dose = c(0, 250, 500, 750, 1000, 1500, 0, 500, 500),
    LxTx = c(1, Inf, 0, -Inf, Inf, 0, Inf, -0.25, 2),
    LxTx.Error = c(1.58133646008685, Inf, 0, Inf, Inf, 0, Inf, 1.41146256149428, 3.16267292017369)),
    class = "data.frame", row.names = c(NA, -9L))

  ##fit
  expect_warning(Luminescence:::.warningCatcher(
    plot_GrowthCurve(
      sample = LxTx[,c("Dose", "LxTx", "LxTx.Error")],
      output.plot = FALSE)))

  ##all points have the same dose ... error but NULL
  data(ExampleData.LxTxData, envir = environment())
  tmp_LxTx <- LxTxData
  tmp_LxTx$Dose <- 10

  expect_null(
    object = plot_GrowthCurve(tmp_LxTx))

  ## check input objects ... matrix
  expect_s4_class(
    object = plot_GrowthCurve(as.matrix(LxTxData), output.plot = FALSE),
    class = "RLum.Results")

  ## check input objects ... list
  expect_s4_class(
    object = plot_GrowthCurve(as.list(LxTxData), output.plot = FALSE),
    class = "RLum.Results")

  ## test case for only two columns
  expect_s4_class(
    object = suppressWarnings(plot_GrowthCurve(LxTxData[,1:2], output.plot = FALSE)),
    class = "RLum.Results")

  ## test case with all NA
  tmp_LxTx <- LxTxData
  tmp_LxTx$LxTx <- NA
  expect_null(
    object = suppressWarnings(plot_GrowthCurve(tmp_LxTx, output.plot = FALSE)))

  ## test defunct
  expect_error(
    object = plot_GrowthCurve(LxTxData[,1:2], output.plot = FALSE, na.rm = FALSE))

  ## do not include reg point
  expect_s4_class(
    object = plot_GrowthCurve(
      sample = LxTxData,
      output.plot = FALSE,
      fit.includingRepeatedRegPoints = FALSE),
    class = "RLum.Results")

  ## use odd data set where the calculated De is negative for EXP
  df_odd <- structure(list(dose = c(0, 0, 2.71828182845905, 2.74202785430992,
      2.76598131771852, 2.79014403079188, 2.814517821467, 2.83910453364916,
      2.86390602735137, 2.88892417883514, 2.91416088075237, 2.93961804228855,
      2.96529758930721, 2.99120146449558, 3.01733162751159, 3.04369005513209,
      3.07027874140241, 3.09709969778723, 3.1241549533227, 3.15144655477
      ), LxTx = c(0.439, 0.851865307456887, 0.881530377359027, 0.881788046363334,
      0.882047940677405, 0.882310079298982, 0.882574481384203, 0.882841166248859,
      0.883110153369655, 0.883381462385488, 0.883655113098727, 0.883931125476509,
      0.884209519652028, 0.88449031592586, 0.884773534767266, 0.885059196815536,
      0.885347322881313, 0.885637933947948, 0.885931051172847, 0.886226695888845
      ), LxTx.error = c(0.029, 1e-04, 0.0393190034426231, 0.0393304962836274,
      0.0393420883803856, 0.0393537805802564, 0.0393655737376631, 0.0393774687141504,
      0.03938946637844, 0.0394015676064878, 0.0394137732815415, 0.0394260842941973,
      0.0394385015424588, 0.0394510259317946, 0.0394636583751979, 0.0394763997932453,
      0.0394892511141562, 0.0395022132738537, 0.0395152872160239, 0.0395284738921779
      )), row.names = c(NA, 20L), class = "data.frame")

  ## do not include reg point
  t <- expect_s4_class(
    object = plot_GrowthCurve(
      sample = df_odd,
      output.plot = FALSE),
    class = "RLum.Results")

  expect_true(is.na(t$De[[1]]))

# Check output for regression ---------------------------------------------
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
  temp_LIN <-
    plot_GrowthCurve(
      LxTxData,
      fit.method = "LIN",
      mode = "extrapolation",
      fit.force_through_origin = TRUE,
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
  temp_QDR <-
    plot_GrowthCurve(
      LxTxData,
      fit.method = "QDR",
      output.plot = FALSE,
      mode = "extrapolation",
      fit.force_through_origin = TRUE,
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


# Check more output -------------------------------------------------------
  data(ExampleData.LxTxData, envir = environment())

  set.seed(1)
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
      NumberIterations.MC = 10)
  ## force through the origin
  temp_LxTx <-LxTxData
  temp_LxTx$LxTx[[7]] <- 1
  expect_s4_class(plot_GrowthCurve(
    temp_LxTx,
    fit.method = "GOK",
    output.plot = FALSE,
    verbose = FALSE,
    NumberIterations.MC = 10,
    fit.force_through_origin = TRUE
  ), "RLum.Results")
temp_LambertW <-
  plot_GrowthCurve(
    LxTxData,
    fit.method = "LambertW",
    output.plot = FALSE,
    verbose = FALSE,
    NumberIterations.MC = 10
  )

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
     expect_equal(round(sum(temp_EXPEXP$De.MC, na.rm = TRUE), digits = 0), 7303,
                  tolerance = 10)

   }

   expect_equal(round(temp_QDR$De[[1]], digits = 2), 1666.2)

   ##fix for different R versions
   if (R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(round(sum(temp_QDR$De.MC, na.rm = TRUE), digits = 0), 14937)

   }else{
    expect_equal(round(sum(temp_QDR$De.MC, na.rm = TRUE), digits = 0), 16476)

   }

   expect_equal(round(temp_GOK$De[[1]], digits = 0), 1786)
   ##fix for different R versions
   if (R.version$major > "3"){
     if(any(grepl("aarch64", sessionInfo()$platform))) {
       expect_equal(round(sum(temp_GOK$De.MC, na.rm = TRUE), digits = 1), 17796,
                    tolerance = 0.001)

     } else {
       expect_equal(round(sum(temp_GOK$De.MC, na.rm = TRUE), digits = 1), 17828.9,
                    tolerance = 0.1)

     }
   }

   expect_equal(round(temp_LambertW$De[[1]], digits = 2),  1784.78)
   expect_equal(round(sum(temp_LambertW$De.MC, na.rm = TRUE), digits = 0), 17422)

# Check extrapolation -----------------------------------------------------
  ## load data
  data(ExampleData.LxTxData, envir = environment())

  set.seed(1)
  LxTxData[1,2:3] <- c(0.5, 0.001)
  LIN <- expect_s4_class(
    plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "LIN"),
    "RLum.Results")
  EXP <- expect_s4_class(
    plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "EXP"),
    "RLum.Results")
  EXPLIN <- expect_s4_class(
    suppressWarnings(
      plot_GrowthCurve(
        LxTxData,mode = "extrapolation", fit.method = "EXP+LIN")),
    "RLum.Results")

  GOK <- expect_s4_class(
    plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "GOK"),
    "RLum.Results")

  LambertW <- expect_s4_class(
    plot_GrowthCurve(LxTxData,mode = "extrapolation", fit.method = "LambertW"), "RLum.Results")

  expect_equal(round(LIN$De$De,0), 165)
  expect_equal(round(EXP$De$De,0),  110)
  expect_equal(round(LambertW$De$De,0),  114)

  #it fails on some unix platforms for unknown reason.
  #expect_equivalent(round(EXPLIN$De$De,0), 110)

# Check alternate ---------------------------------------------------------
  ## load data
  data(ExampleData.LxTxData, envir = environment())

  set.seed(1)
  LxTxData[1,2:3] <- c(0.5, 0.001)

  ##LIN
  expect_s4_class(
    object = plot_GrowthCurve(LxTxData,mode = "alternate", fit.method = "LIN", output.plot = FALSE),
    "RLum.Results")

  ## EXP
  EXP <- expect_s4_class(
    object = plot_GrowthCurve(LxTxData,mode = "alternate", fit.method = "EXP", output.plot = FALSE),
    "RLum.Results")

  ## EXP+LIN
  EXPLIN <- expect_s4_class(
    object = suppressWarnings(
      plot_GrowthCurve(LxTxData,mode = "alternate", fit.method = "EXP+LIN", output.plot = FALSE, verbose = FALSE)),
    "RLum.Results")

  ## GOK
  expect_s4_class(
    object = plot_GrowthCurve(
      LxTxData,
      mode = "alternate",
      fit.method = "GOK",
      output.plot = FALSE,
      verbose = FALSE
    ),
    "RLum.Results"
  )

  ## LambertW
  expect_s4_class(
    object = plot_GrowthCurve(
      LxTxData,
      mode = "alternate",
      fit.method = "LambertW",
      output.plot = FALSE,
      verbose = FALSE
    ),
    "RLum.Results"
  )

  ## trigger LambertW related warning for
  ## extrapolation mode
  tmp <- structure(list(
    dose = c(
      0,
      1388.88888888889,
      2777.77777777778,
      4166.66666666667,
      8333.33333333333,
      16666.6666666667,
      33333.3333333333,
      0,
      1388.88888888889,
      2777.77777777778,
      4166.66666666667,
      8333.33333333333,
      16666.6666666667,
      33333.3333333333,
      0,
      1388.88888888889,
      2777.77777777778,
      4166.66666666667,
      8333.33333333333,
      16666.6666666667,
      33333.3333333333
    ),
    LxTx = c(
      1.54252220145258,
      4.43951568403849,
      6.23268064543138,
      7.84372723139206,
      12.1816246695694,
      16.220421545207,
      19.9805214420208,
      1.5693958789807,
      4.01446969642433,
      6.50442121919275,
      8.13912565845306,
      11.2791435536017,
      14.2739718127927,
      17.7646886436743,
      1.55083317135234,
      4.10327222363961,
      6.1705969614814,
      8.30005789933367,
      12.7612004529065,
      14.807776070804,
      17.1563663039162
    ),
    LxTx_X = c(
      0.130074482379272,
      2.59694106608334,
      4.46970034588506,
      3.0630786645803,
      0.744512263874143,
      6.0383153231303,
      0.785060450424326,
      3.16210365279,
      0.0425273193228004,
      2.9667194222907,
      0.187174353876429,
      4.29989597009486,
      4.19802308979151,
      2.77791088935002,
      0.248412040945932,
      0.626745230335262,
      3.80396486752602,
      16.1846310553925,
      4.14921514089229,
      1.40190110413806,
      7.74406545663656
    )
  ),
  class = "data.frame",
  row.names = c(NA, -21L))

  expect_warning(plot_GrowthCurve(
    tmp,
    mode = "extrapolation",
    fit.method = "LambertW",
    output.plot = FALSE,
    verbose = FALSE),
    regexp = "\\[plot\\_GrowthCurve\\(\\)\\] Standard root estimation using stats\\:\\:uniroot\\(\\).+")

})

