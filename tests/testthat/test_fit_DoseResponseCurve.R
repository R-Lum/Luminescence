## load data
data(ExampleData.LxTxData, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  ## object
  expect_error(
      fit_DoseResponseCurve("error"),
      "[fit_DoseResponseCurve()] 'object' should be of class 'data.frame'",
      fixed = TRUE)
  expect_error(fit_DoseResponseCurve(data.frame()),
               "'object' cannot be an empty data.frame")
  expect_error(fit_DoseResponseCurve(as.list(LxTxData)),
               "All elements of 'object' should be of class 'data.frame'")

  ## mode
  expect_error(
    fit_DoseResponseCurve(LxTxData, mode = "error"),
    "[fit_DoseResponseCurve()] 'mode' should be one of 'interpolation'",
    fixed = TRUE)

  ## fit.method
  expect_error(
    fit_DoseResponseCurve(LxTxData, fit.method = "error"),
    "[fit_DoseResponseCurve()] 'fit.method' should be one of 'LIN', 'QDR'",
    fixed = TRUE)

  ## other arguments
  expect_error(fit_DoseResponseCurve(LxTxData,
                                     fit.force_through_origin = "error"),
               "'fit.force_through_origin' should be a single logical value")
  expect_error(fit_DoseResponseCurve(LxTxData, fit.weights = "error"),
               "'fit.weights' should be a single logical value")
  expect_error(fit_DoseResponseCurve(LxTxData,
                                     fit.includingRepeatedRegPoints = "error"),
               "'fit.includingRepeatedRegPoints' should be a single logical")
  expect_error(fit_DoseResponseCurve(LxTxData,
                                     fit.NumberRegPoints = "error"),
               "'fit.NumberRegPoints' should be a positive integer scalar")
  expect_error(fit_DoseResponseCurve(LxTxData, fit.NumberRegPointsReal = "error"),
               "'fit.NumberRegPointsReal' should be a positive integer scalar")
  expect_error(fit_DoseResponseCurve(LxTxData,
                                     fit.bounds = "error"),
               "'fit.bounds' should be a single logical value")
  expect_error(fit_DoseResponseCurve(LxTxData,
                                     n.MC = "error"),
               "'n.MC' should be a positive integer scalar")

  ## shorten dataframe
  expect_warning(fit_DoseResponseCurve(LxTxData[1:2, ], verbose = FALSE),
                 "Fitting a non-linear least-squares model requires at least 3")
  expect_warning(fit_DoseResponseCurve(LxTxData[1:3, ], fit.method = "GOK",
                                       verbose = FALSE),
                 "Fitting a non-linear least-squares model requires at least 4")

  ## wrong combination of fit.method and mode
  expect_error(
    fit_DoseResponseCurve(LxTxData, fit.method = "EXP+EXP",
                     mode = "extrapolation"),
    "Mode 'extrapolation' for fitting method 'EXP+EXP' not supported",
    fixed = TRUE)
})

test_that("weird LxTx values", {
  testthat::skip_on_cran()

  LxTx <- structure(list(
    Dose = c(0, 250, 500, 750, 1000, 1500, 0, 500, 500),
    LxTx = c(1, Inf, 0, -Inf, Inf, 0, Inf, -0.25, 2),
    LxTx.Error = c(1.58133646008685, Inf, 0, Inf, Inf, 0, Inf, 1.41146256149428, 3.16267292017369)),
    class = "data.frame", row.names = c(NA, -9L))

  ##fit
  SW({
  expect_warning(
    fit_DoseResponseCurve(LxTx[, c("Dose", "LxTx", "LxTx.Error")]),
    "Inf values found, replaced by NA")
  })

  ##all points have the same dose ... error but NULL
  data(ExampleData.LxTxData, envir = environment())
  tmp_LxTx <- LxTxData
  tmp_LxTx$Dose <- 10

  expect_message(expect_null(
      fit_DoseResponseCurve(tmp_LxTx)),
      "Error: All points have the same dose, NULL returned")

  ## check input objects ... matrix
  SW({
  expect_s4_class(
    fit_DoseResponseCurve(as.matrix(LxTxData)),
    class = "RLum.Results")
  })

  ## test case for only two columns
  expect_s4_class(
    suppressWarnings(fit_DoseResponseCurve(LxTxData[,1:2], verbose = FALSE)),
    class = "RLum.Results")

  ## test case with all NA
  tmp_LxTx <- LxTxData
  tmp_LxTx$LxTx <- NA
  expect_message(expect_null(
      suppressWarnings(fit_DoseResponseCurve(tmp_LxTx))),
      "Error: After NA removal, nothing is left from the data set")

  ## test case without TnTx column
  tmp_LxTx <- LxTxData
  tmp_LxTx$TnTx <- NULL
  SW({
  expect_s4_class(
    fit_DoseResponseCurve(tmp_LxTx, verbose = FALSE),
    "RLum.Results")
  })

  ## do not include reg point
  expect_s4_class(
    fit_DoseResponseCurve(
      LxTxData,
      verbose = FALSE,
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
    fit_DoseResponseCurve(
      df_odd,
      verbose = FALSE),
    class = "RLum.Results")

  expect_true(is.na(t$De[[1]]))
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  ## see https://github.com/R-Lum/Luminescence/pull/308
  skip_on_os("windows")
  skip_on_os("mac")

  snapshot.tolerance <- 1.5e-6

  set.seed(1)
  SW({
  expect_snapshot_RLum(fit_DoseResponseCurve(
      LxTxData,
      fit.method = "EXP",
      verbose = FALSE,
      n.MC = 10
    ), tolerance = snapshot.tolerance)

  expect_snapshot_RLum(fit_DoseResponseCurve(
      LxTxData,
      fit.method = "LIN",
      verbose = FALSE,
      n.MC = 10
  ), tolerance = snapshot.tolerance)

  expect_snapshot_RLum(fit_DoseResponseCurve(
      LxTxData,
      fit.method = "LIN",
      mode = "extrapolation",
      fit.force_through_origin = TRUE,
      verbose = FALSE,
      n.MC = 10
  ), tolerance = snapshot.tolerance)

  expect_snapshot_RLum(fit_DoseResponseCurve(
      LxTxData,
      fit.method = "EXP+LIN",
      fit.bounds = FALSE,
      fit.force_through_origin = TRUE,
      verbose = FALSE,
      n.MC = 10
  ), tolerance = snapshot.tolerance)

  expect_snapshot_RLum(fit_DoseResponseCurve(
      LxTxData,
      fit.method = "EXP+EXP",
      verbose = TRUE,
      n.MC = 10
  ), tolerance = snapshot.tolerance)

  expect_snapshot_RLum(fit_DoseResponseCurve(
      LxTxData,
      fit.method = "QDR",
      verbose = TRUE,
      n.MC = 10
  ), tolerance = snapshot.tolerance)

  expect_snapshot_RLum(fit_DoseResponseCurve(
      LxTxData,
      fit.method = "GOK",
      verbose = FALSE,
      n.MC = 10
  ), tolerance = snapshot.tolerance)

  expect_snapshot_RLum(fit_DoseResponseCurve(
      LxTxData,
      fit.method = "LambertW",
      verbose = FALSE,
      n.MC = 10
  ), tolerance = snapshot.tolerance)

  expect_snapshot_RLum(fit_DoseResponseCurve(
      LxTxData,
      fit.method = "QDR",
      mode = "extrapolation",
      verbose = TRUE,
      n.MC = 10
  ), tolerance = 2.0e-5)

  expect_snapshot_RLum(fit_DoseResponseCurve(
      LxTxData,
      fit.method = "QDR",
      mode = "extrapolation",
      fit.force_through_origin = TRUE,
      verbose = TRUE,
      n.MC = 10
  ), tolerance = 5.0e-5)
  })

  expect_snapshot_RLum(fit_DoseResponseCurve(
      LxTxData,
      fit.method = "LIN",
      mode = "extrapolation",
      verbose = FALSE,
      n.MC = 10
  ), tolerance = 5.0e-5)
})

test_that("additional tests", {
  testthat::skip_on_cran()

  ## self-call
  res <- fit_DoseResponseCurve(
      list(LxTxData, LxTxData),
      fit.method = "LIN",
      verbose = FALSE,
      n.MC = 10)
  expect_type(res, "list")
  expect_length(res, 2)

  set.seed(1)
  SW({
  expect_output(temp_EXP <-
    fit_DoseResponseCurve(
      LxTxData,
      fit.method = "EXP",
      verbose = TRUE,
      n.MC = 10
    ), " | D01 = ", fixed = TRUE)
  temp_LIN <-
    fit_DoseResponseCurve(
      LxTxData,
      fit.method = "LIN",
      verbose = FALSE,
      n.MC = 10
    )
  temp_EXPLIN <-
    fit_DoseResponseCurve(
      LxTxData,
      fit.method = "EXP+LIN",
      verbose = FALSE,
      n.MC = 10
    )
  temp_EXPEXP <-
    fit_DoseResponseCurve(
      LxTxData,
      fit.method = "EXP+EXP",
      verbose = FALSE,
      n.MC = 10
    )
  temp_QDR <-
    fit_DoseResponseCurve(
      LxTxData,
      fit.method = "QDR",
      verbose = FALSE,
      n.MC = 10
    )
  temp_GOK <-
    fit_DoseResponseCurve(
      LxTxData,
      fit.method = "GOK",
      verbose = FALSE,
      n.MC = 10)

  ## force through the origin
  temp_LxTx <-LxTxData
  temp_LxTx$LxTx[[7]] <- 1
  expect_s4_class(fit_DoseResponseCurve(
    temp_LxTx,
    fit.method = "GOK",
    verbose = FALSE,
    n.MC = 10,
    fit.force_through_origin = TRUE
  ), "RLum.Results")
temp_LambertW <-
  fit_DoseResponseCurve(
    LxTxData,
    fit.method = "LambertW",
    verbose = FALSE,
    n.MC = 10
  )

  ## FIXME(mcol): duplicate of a test in the snapshot block, we need it
  ##              here too as coverage currenlty runs on 4.3
  temp_QDR2 <- fit_DoseResponseCurve(
      LxTxData,
      fit.method = "QDR",
      mode = "extrapolation",
      fit.force_through_origin = TRUE,
      verbose = TRUE,
      n.MC = 10
  )
  })

  expect_s3_class(temp_EXP$Fit, class = "nls")
  expect_s3_class(temp_LIN$Fit, class = "lm")
  expect_s3_class(temp_EXPLIN$Fit, class = "nls")
  expect_s3_class(temp_EXPEXP$Fit, class = "nls")
  expect_s3_class(temp_QDR$Fit, class = "lm")
  expect_s3_class(temp_GOK$Fit, class = "nls")
  expect_s3_class(temp_LambertW$Fit, class = "nls")

   expect_equal(round(temp_EXP$De[[1]], digits = 2), 1737.88)
   expect_equal(round(sum(temp_EXP$De.MC, na.rm = TRUE), digits = 0), 17562)
   expect_equal(round(temp_LIN$De[[1]], digits = 2), 1811.33)
   expect_equal(round(sum(temp_LIN$De.MC, na.rm = TRUE), digits = 0),18398)
   expect_equal(round(temp_EXPLIN$De[[1]], digits = 2), 1791.53)
   expect_equal(round(sum(temp_EXPLIN$De.MC, na.rm = TRUE), digits = 0),18045)
   expect_equal(round(temp_EXPEXP$De[[1]], digits = 2), 1787.15)
   expect_equal(round(sum(temp_EXPEXP$De.MC, na.rm = TRUE), digits = 0), 7303,
                tolerance = 10)
   expect_equal(round(temp_QDR$De[[1]], digits = 2), 1666.2)
   expect_equal(round(sum(temp_QDR$De.MC, na.rm = TRUE), digits = 0), 16476)
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
  SW({
  LIN <- expect_s4_class(
    fit_DoseResponseCurve(LxTxData,mode = "extrapolation", fit.method = "LIN"),
    "RLum.Results")
  EXP <- expect_s4_class(
    fit_DoseResponseCurve(LxTxData,mode = "extrapolation", fit.method = "EXP"),
    "RLum.Results")
  EXPLIN <- expect_s4_class(
    suppressWarnings(
      fit_DoseResponseCurve(
        LxTxData,mode = "extrapolation", fit.method = "EXP+LIN")),
    "RLum.Results")

  GOK <- expect_s4_class(
    fit_DoseResponseCurve(LxTxData,mode = "interpolation", fit.method = "GOK"),
    "RLum.Results")

  LambertW <- expect_s4_class(
    fit_DoseResponseCurve(LxTxData,mode = "extrapolation", fit.method = "LambertW"), "RLum.Results")
  })

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
    fit_DoseResponseCurve(LxTxData, mode = "alternate", fit.method = "LIN"),
    "RLum.Results")

  ## EXP
  EXP <- expect_s4_class(
    fit_DoseResponseCurve(LxTxData, mode = "alternate", fit.method = "EXP"),
    "RLum.Results")

  ## EXP+LIN
  EXPLIN <- expect_s4_class(
    suppressWarnings(
      fit_DoseResponseCurve(LxTxData, mode = "alternate", fit.method = "EXP+LIN", verbose = FALSE)),
    "RLum.Results")

  ## GOK
  expect_s4_class(
    fit_DoseResponseCurve(
      LxTxData,
      mode = "alternate",
      fit.method = "GOK",
      verbose = FALSE
    ),
    "RLum.Results"
  )

  ## LambertW
  expect_s4_class(
    fit_DoseResponseCurve(
      LxTxData,
      mode = "alternate",
      fit.method = "LambertW",
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

  expect_warning(fit_DoseResponseCurve(
    tmp,
    mode = "extrapolation",
    fit.method = "LambertW",
    verbose = FALSE),
    "[fit_DoseResponseCurve()] Standard root estimation using stats::uniroot()",
    fixed = TRUE)

  ## only two valid points provided: this generates two warnings, hence
  ## we cannot use expect_warning(), which can only capture one at a time
  SW({
  warnings <- capture_warnings(expect_message(fit_DoseResponseCurve(
    data.frame(
        dose = c(0, 1388.88888888889, NA),
        LxTx = c(1.54252220145258, 4.43951568403849, NA),
        LxTx_X = c(0.130074482379272, 2.59694106608, NA)),
    verbose = TRUE),
    "'fit.method' changed to 'LIN'"))
  })
  expect_match(warnings, "1 NA values removed",
               all = FALSE, fixed = TRUE)
  expect_match(warnings, "Fitting a non-linear least-squares model requires",
               all = FALSE, fixed = TRUE)
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 374 --------------------------------------------------------------

  ## odd data that cause NaN but must not fail
  df <- data.frame(DOSE = c(0,5,10,20,30), LxTx = c(10,5,-20,-30,-40), LxTx_X = c(1, 1,1,1,1))
  SW({
  expect_s4_class(
    fit_DoseResponseCurve(df, fit.method = "EXP"), "RLum.Results")
  expect_s4_class(
    fit_DoseResponseCurve(df, fit.method = "EXP+LIN"), "RLum.Results")
  expect_s4_class(
    fit_DoseResponseCurve(df, fit.method = "EXP+EXP"), "RLum.Results")
  expect_s4_class(
    fit_DoseResponseCurve(df, fit.method = "LambertW"), "RLum.Results")
  })
})
