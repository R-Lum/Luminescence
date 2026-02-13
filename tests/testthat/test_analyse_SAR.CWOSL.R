## load data
set.seed(1)
data(ExampleData.BINfileData, envir = environment())
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1:2)

##generate different datasets removing TL curves
object_CH_TL <- get_RLum(object, record.id = -seq(1,30,4), drop = FALSE)
object_NO_TL <- get_RLum(object, record.id = -seq(1,30,2), drop = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(analyse_SAR.CWOSL("fail"),
               "'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_SAR.CWOSL(iris),
               "'object' should be of class 'RLum.Analysis'")

  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal_integral = 1:2,
                                 background_integral = 900:1000,
                                 plot_singlePanels = list()),
               "'plot_singlePanels' should be of class 'logical', 'integer' or 'numeric'")

  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal_integral = NULL,
                                 background_integral = 900:1000),
               "'signal_integral' should be of class 'integer', 'numeric' or NA")
  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal_integral = -9:0,
                                 background_integral = 900:1000),
               "'signal_integral' is of length 0 after removing values smaller than 1")
  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal_integral = 1.2:3.5,
                                 background_integral = 900:1000),
               "'signal_integral' should be a vector of integers")

  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal_integral = 1:2,
                                 background_integral = NULL),
               "'background_integral' should be of class 'integer', 'numeric' or NA")
  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal_integral = 1:2,
                                 background_integral = 900:1000,
                                 signal_integral_Tx = NA),
               "'signal_integral_Tx' should be of class 'integer', 'numeric' or NULL")
  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal_integral = 1:2,
                                 background_integral = 900:1000,
                                 signal_integral_Tx = 1:2,
                                 background_integral_Tx = "error"),
               "'background_integral_Tx' should be of class 'integer', 'numeric', NA or NULL")

  expect_warning(expect_error(analyse_SAR.CWOSL(object[[1]],
                                                signal_integral = 1:1500,
                                                background_integral = 1900:2000),
                              "expected to be at least 1001, but the maximum allowed is 1000"),
                 "'signal_integral' out of bounds, reset to be between 1 and 1000")

  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal_integral = 1:2,
                                 background_integral = 900:1000,
                                 rejection.criteria = "error"),
               "'rejection.criteria' should be of class 'list'")
  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal_integral = 1:2,
                                 background_integral = 900:1000,
                                 rejection.criteria = list(recuperation_reference = letters)),
               "'recuperation_reference' in 'rejection.criteria' should be of class 'character'")
  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal_integral = 1:2,
                                 background_integral = 900:1000,
                                 rejection.criteria = list(sn_reference = letters)),
               "'sn_reference' in 'rejection.criteria' should be of class 'character'")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(
    results <- analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 900:1000,
      plot = FALSE,
      verbose = FALSE
    ), tolerance = snapshot.tolerance
  )

  ## fit.method
  expect_snapshot_RLum(
    analyse_SAR.CWOSL(
      object = object[1:2],
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
    ), tolerance = snapshot.tolerance
  )

  ## source dose rate
  expect_snapshot_RLum(
    analyse_SAR.CWOSL(
      object = object[1:2],
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "LIN",
      dose_rate_source = 0.2,
      plot = FALSE,
      verbose = FALSE
    ), tolerance = snapshot.tolerance
  )
  ## with test dose
  expect_snapshot_RLum(
    analyse_SAR.CWOSL(
      object = object[1:2],
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "LIN",
      dose.points.test = 1,
      dose_rate_source = 0.2,
      plot = FALSE,
      verbose = FALSE
    ), tolerance = snapshot.tolerance
  )

  ## check rejection criteria and recuperation point selection
  expect_snapshot_RLum(
    t <- analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "LIN",
      rejection.criteria = list(
        recycling.ratio = NA,
        recuperation.rate = 1,
        palaeodose.error = NA,
        testdose.error = 1,
        recuperation_reference = "R1",
        exceed.max.regpoint = FALSE),
      plot = FALSE,
      verbose = FALSE
    ), tolerance = snapshot.tolerance
  )

  ## check if a different point was selected
  expect_equal(round(t$rejection.criteria$Value[2],2), expected = 0.01)
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## remove position and grain information from the curve data
  object_f <- object[[1]]
  object_f@records <- lapply(object_f@records, function(x){
    x@info$POSITION <- NULL
    x@info$GRAIN <- NULL
    x
  })
  t <- expect_s4_class(
    analyse_SAR.CWOSL(
      object = object_f,
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
    ),
    class = "RLum.Results"
  )

  expect_type(t@data$data$POS, "logical")

  ##signal integral set to NA
  expect_warning(
    analyse_SAR.CWOSL(
      object = object[1],
      signal.integral.min = NA,
      signal.integral.max = NA,
      background.integral.min = NA,
      background.integral.max = NA,
      fit.method = "EXP",
      plot = FALSE,
      verbose = FALSE,
      fit.weights = FALSE
    ),
    "[analyse_SAR.CWOSL()] No signal or background integral applied",
    fixed = TRUE)

  expect_warning(
    analyse_SAR.CWOSL(
      object = object[1],
      signal_integral = NA,
      background_integral = 900:1000,
      fit.method = "EXP",
      plot = FALSE,
      verbose = FALSE,
      fit.weights = FALSE
    ),
    "No signal or background integral applied as 'signal_integral = NA'")

  expect_error(
    analyse_SAR.CWOSL(
      object = object,
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "OTORX",
      plot = FALSE,
      verbose = FALSE,
      fit.weights = FALSE
    ),
    "Column 'Test_Dose' missing but mandatory for 'OTORX' fitting!",
    fixed = TRUE)

  expect_s4_class(
    suppressWarnings(analyse_SAR.CWOSL(
      object = object[1],
      signal.integral.min = NA,
      signal.integral.max = NA,
      background.integral.min = NA,
      background.integral.max = NA,
      fit.method = "EXP",
      plot = FALSE,
      verbose = FALSE
    )),
    class = "RLum.Results"
  )

  SW({
  ##no mix TL and OSL (the only TL will be sorted out automatically)
  only_TL <- set_RLum("RLum.Analysis", records = rep(object_CH_TL[[1]]@records[[2]], length(object_NO_TL[[1]]@records)))
  object_mixed <- c(object_NO_TL, only_TL)

  t <- expect_s4_class(
    analyse_SAR.CWOSL(
      object = object_mixed[2:3],
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "LIN",
      log = "x",
      plot = FALSE,
      verbose = FALSE
    ),
    class = "RLum.Results"
  )
  expect_equal(nrow(t@data$data), 1)

  ## plot_singlePanels
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "EXP",
      plot = TRUE,
      plot_singlePanels = TRUE
    ),
    class = "RLum.Results"
  )
  })

  ## trigger stop of recuperation reference point
  expect_error(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "LIN",
      rejection.criteria= list(
        recycling.ratio = NA,
        recuperation.rate = 1,
        palaeodose.error = 1,
        testdose.error = 1,
        recuperation_reference = "stop",
        test = "new",
        exceed.max.regpoint = FALSE),
      plot = TRUE
    ),
    "Recuperation reference invalid, valid values are: 'Natural', 'R1', 'R2'")

  expect_error(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "LIN",
      rejection.criteria = list(sn_reference = "error"),
      plot = TRUE
    ),
    "Signal-to-noise reference invalid, valid values are: 'Natural', 'R1', 'R2'")

  ## check stop for OSL.components ... failing
  SW({
  expect_message(expect_null(
      analyse_SAR.CWOSL(
       object = object[[1]],
       signal_integral = 1:2,
       background_integral = 900:1000,
       OSL.component = 1,
       verbose = FALSE
      )),
      "Failed to generate the LxTx table, NULL returned")
  })

   expect_error(analyse_SAR.CWOSL(
     object = object[[1]],
     signal_integral = 1:2,
     background_integral = 900:1000,
     dose.points = c(0,1,2),
     fit.method = "LIN",
     plot = FALSE,
     verbose = FALSE),
     "Length of 'dose.points' (3) differs from number of curves (7)",
     fixed = TRUE)

  expect_message(
   expect_null(analyse_SAR.CWOSL(
     object = set_RLum("RLum.Analysis",records = list(set_RLum("RLum.Data.Curve", recordType = "false"))),
     signal_integral = 1:2,
     background_integral = 800:900,
     fit.method = "LIN",
     plot = FALSE,
     verbose = FALSE
   )),
   "No record of type 'OSL', 'IRSL', 'POSL' detected")

  ## check background integral
  expect_warning(analyse_SAR.CWOSL(
      object[[1]],
      signal_integral = 1:500,
      background_integral = 500:1000,
      verbose = FALSE),
      "'background_integral' out of bounds, reset to be between 501 and 1000")

   expect_warning(analyse_SAR.CWOSL(
     object = object[[1]],
     signal_integral = 1:2,
     background_integral = 800:9900,
     fit.method = "LIN",
     plot = FALSE,
     verbose = FALSE
   ), regexp = "'background_integral' out of bounds, reset to be between 800 and 1000")

  expect_warning(analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:1,
      background_integral = 1000:1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
  ), "Background integral should contain at least two values, reset to 975:1000")

  expect_warning(analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      signal_integral_Tx = 1:1,
      background_integral = 900:1000,
      background_integral_Tx = 1000:1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
  ), "Background integral limits for Tx curves cannot be equal, reset to 975:1000")

  expect_warning(analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 600:900,
      background_integral_Tx = 800:1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
  ), "'signal_integral_Tx' set automatically to 1:2")

  expect_warning(expect_warning(analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 600:900,
      background_integral_Tx = 1:1200,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
  ), "'signal_integral_Tx' set automatically to 1:2"),
  "'background_integral_Tx' out of bounds, reset to be between 3 and 1000")

  expect_warning(expect_warning(expect_message(
      analyse_SAR.CWOSL(
          object = object[[1]],
          signal_integral = 1:2,
          signal_integral_Tx = 500:2000,
          background_integral = 800:1000,
          fit.method = "LIN",
          plot = FALSE,
          verbose = FALSE
      ), "Failed to generate the LxTx table, NULL returned"),
      "'signal_integral_Tx' out of bounds, reset to be between 500 and 1000"),
      "'background_integral_Tx' set automatically to 800:1000")

  ## this generates multiple warnings
  warnings <- capture_warnings(analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      signal_integral_Tx = 70:80,
      background_integral = 800:1200,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE))
  expect_match(warnings, all = FALSE,
               "'background_integral' out of bounds, reset to be between 800 and 1000")
  expect_match(warnings, all = FALSE,
               "'background_integral_Tx' set automatically to 800:1000")

  warnings <- capture_warnings(analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 100:400,
      signal_integral_Tx = 500:605,
      background_integral = 800:1200,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE))
  expect_match(warnings, all = FALSE,
               "'background_integral' out of bounds, reset to be between 800 and 1000")
  expect_match(warnings, all = FALSE,
               "'background_integral_Tx' set automatically to 800:1000")

  ## add one OSL curve
  expect_warning(expect_null(
      analyse_SAR.CWOSL(
          object = merge(object[[1]], object[[1]][[2]]),
          signal_integral = 1:2,
          background_integral = 800:1200)),
      "Input OSL/IRSL curves are not a multiple of two")

   ## check different curve numbers by shorten one OSL curve
   object_short <- object
   object_short[[1]]@records[[2]]@data <- object_short[[1]]@records[[2]]@data[-nrow(object_short[[1]]@records[[2]]@data),]

   ## without fix
   expect_warning(expect_null(
     analyse_SAR.CWOSL(
       object = object_short[[1]],
       signal_integral = 1:2,
       background_integral = 800:9900,
       fit.method = "LIN",
       plot = FALSE,
       verbose = FALSE)),
     "Input curves have different lengths (999, 1000), consider setting",
     fixed = TRUE)

   ## with new parameter
   testthat::expect_s4_class(
     object = analyse_SAR.CWOSL(
       object = object_short[[1]],
       signal_integral = 1:2,
       background_integral = 800:999,
       fit.method = "LIN",
       trim_channels = TRUE,
       plot = FALSE,
       verbose = FALSE),
   class = "RLum.Results")

  ## different curve types matching
  object_short[[1]]@records[[4]]@recordType <- "OSL (NA)"
  suppressWarnings( # Input curves have different lengths (999, 1000)
  expect_warning(expect_null(
     analyse_SAR.CWOSL(
       object = object_short[[1]],
       signal_integral = 1:2,
       background_integral = 800:9900,
       fit.method = "LIN",
       trim_channels = TRUE,
       plot = FALSE,
       verbose = FALSE)),
     "Curve type 'OSL' matches multiple record types: 'OSL (PMT)', 'OSL (NA)'",
     fixed = TRUE)
  )

  ## consider.uncertainties
  res <- analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "LIN",
      rejection.criteria = list(
          recuperation.rate = 1,
          recuperation_reference = "R1",
          recycling.ratio = 4,
          consider.uncertainties = FALSE),
      plot = FALSE, verbose = FALSE)
  expect_equal(res$rejection.criteria$Status[1:2],
               c("FAILED", "FAILED"))
  res <- analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 900:1000,
      fit.method = "LIN",
      rejection.criteria = list(
          recuperation.rate = 1,
          recuperation_reference = "R1",
          recycling.ratio = 4,
          consider.uncertainties = TRUE),
      plot = FALSE, verbose = FALSE)
  expect_equal(res$rejection.criteria$Status[1:2],
               c("OK", "OK"))
})

test_that("advance tests run", {
  testthat::skip_on_cran()

  ## irradiation information in a separate curve
  no_irr_object <- object
  replace_metadata(no_irr_object[[1]], info_element = "IRR_TIME") <- NULL
  replace_metadata(no_irr_object[[2]], info_element = "IRR_TIME") <- NULL
  obj_irr <- set_RLum(
      "RLum.Analysis",
      protocol = "testthat",
      records = list(
          no_irr_object[[1]][[1]], no_irr_object[[1]][[2]],
          set_RLum(
          "RLum.Data.Curve",
          recordType = "irradiation",
          data = no_irr_object[[1]]@records[[1]]@data,
          info = no_irr_object[[1]]@records[[1]]@info
          ),
          no_irr_object[[2]][[1]], no_irr_object[[2]][[2]]),
      originator = "read_XSYG2R"
  )
  SW({ # repeated message
  expect_message(
      analyse_SAR.CWOSL(list(obj_irr, obj_irr),
                        signal_integral = 1:2,
                        background_integral = 900:1000,
                        fit.method = "LIN", verbose = FALSE),
      "All points have the same dose, NULL returned"
  )
  })

  ## more coverage
  SW({ # Number of background channels for Lx < 25
  expect_warning(expect_warning(
      analyse_SAR.CWOSL(merge(object[[1]], object[[2]],
                              object[[1]], object[[2]]),
                        signal_integral = 1:2,
                        background_integral = 900:900,
                        background.count.distribution = "poisson",
                        log = "xy", verbose = FALSE),
      "Too many curves, only the first 21 curves are plotted"),
      "Multiple IRSL curves detected")

  expect_warning(
      analyse_SAR.CWOSL(object = object[[1]],
                        signal_integral = 1:2,
                        background_integral = 900:1000,
                        plot.single = TRUE,
                        verbose = FALSE),
      "'plot.single' was deprecated in v1.0.0, use 'plot_singlePanels' instead")
  })

  ##test failed recycling ratio
  test_failed <-
    analyse_SAR.CWOSL(
      object = object[1],
      signal_integral = 1:2,
      background_integral = 200:1000,
      fit.method = "LIN",
      rejection.criteria = list(recycling.ratio = 0),
      plot = FALSE,
      verbose = FALSE)
  expect_equal(object = test_failed$data$RC.Status, "FAILED")

  ##test no irradiation times available
  no_irr_object <- object[1]
  replace_metadata(no_irr_object[[1]], info_element = "IRR_TIME") <- NULL
   expect_error(
     analyse_SAR.CWOSL(
      object = no_irr_object,
      signal_integral = 1:2,
      background_integral = 200:1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE),
     regexp = "[analyse_SAR.CWOSL()] 'dose.points' contains NA values or was not set",
     fixed = TRUE)

   ##get null for single list element
   unsuitable_type <- object[1]
   unsuitable_type[[1]]@records <- lapply(
     unsuitable_type[[1]]@records, function(x) {
      x@recordType = "Error"
      x
     })

  SW({
  expect_null(
    analyse_SAR.CWOSL(
       object = unsuitable_type,
       signal_integral = 1:2,
       background_integral = 200:1000,
       fit.method = "OTOR",
       n.MC = 10,
       plot = FALSE,
       verbose = FALSE))
  })

  ##set all rejection criteria to NA
  test_failed <-
    analyse_SAR.CWOSL(
      object = object[1],
      signal_integral = 1:2,
      background_integral = 200:1000,
      fit.method = "LIN",
      rejection.criteria = list(
        recycling.ratio = NA,
        recuperation.rate = NA,
        palaeodose.error = NA,
        testdose.error = NA,
        exceed.max.regpoint = NA),
      plot = FALSE,
      verbose = FALSE)
  expect_equal(object = test_failed$data$RC.Status, "OK")

  ##the same test but without a named list >>> OK
  test_ok <-
    analyse_SAR.CWOSL(
      object = object[1],
      signal_integral = 1:2,
      background_integral = 200:1000,
      fit.method = "LIN",
      rejection.criteria = list(1),
      plot = FALSE,
      verbose = FALSE)
  expect_equal(object = test_ok$data$RC.Status, "OK")

  ##test multi parameter settings
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[1:2],
      signal_integral = list(1:10, 1:20),
      background_integral = 900:1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
    ),
    class = "RLum.Results"
  )

  ##test rejection criteria list in list + test unknown argument
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[1:2],
      signal_integral = list(1:10, 1:20),
      background_integral = 900:1000,
      rejection.criteria = list(list(recycling.ratio = 0)),
      fit.method = "LIN",
      unknown_argument = "hallo",
      main = "Title",
      plot = TRUE,
      verbose = FALSE
    ),
    class = "RLum.Results"
  )

  ## more coverage
  SW({
  expect_warning(analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 900:900,
      background_integral_Tx = 975:975,
      dose.points = rep(2, 7),
      plot = FALSE,
      verbose = FALSE),
      "The natural signal has a dose of 2 s, which is indicative of")

    ## there should be no warning if we are in the alternate mode
    ## however we have to reset the R1 (this is OK!)
    expect_silent(analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 900:975,
      dose.points = seq_len(7),
      mode = "alternate",
      rejection.criteria = list(
        sn_reference = "R0",
        recuperation_reference = "R1"),
      plot = FALSE,
      verbose = FALSE))

  ## OTORX
  expect_s4_class(suppressWarnings(analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 900:900,
      background_integral_Tx = 975:975,
      fit.method = "OTORX",
      dose.points.test = 5,
      plot = FALSE,
      n.MC = 10,
      verbose = FALSE)), class = "RLum.Results")

  object[[1]]@records[[2]][1, 1] <- 0
  expect_warning(analyse_SAR.CWOSL(
      object = object[[1]],
      signal_integral = 1:2,
      background_integral = 900:975,
      log = "x",
      verbose = FALSE),
      "Curves shifted by one channel for log-plot")
  })

  ## simulate single grain
  sg <- get_RLum(object, recordType = "OSL", drop = FALSE)
  replace_metadata(sg[[1]], info_element = "GRAIN") <- 1
  replace_metadata(sg[[2]], info_element = "GRAIN") <- 2

  expect_s4_class(analyse_SAR.CWOSL(
    object = sg,
    signal_integral = 1:2,
    background_integral = 900:975,
    plot_onePage = TRUE,
    verbose = FALSE), "RLum.Results")
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  set.seed(1)

  SW({
  vdiffr::expect_doppelganger("default",
                              analyse_SAR.CWOSL(
                                  object = object[[1]],
                                  signal_integral = 1:2,
                                  background_integral = 900:1000,
                                  plot_onePage = TRUE))

  vdiffr::expect_doppelganger("source_dose_rate",
                                analyse_SAR.CWOSL(
                                  object = object[[1]],
                                  signal_integral = 1:2,
                                  background_integral = 900:1000,
                                  dose_rate_source = 0.22,
                                  plot_onePage = TRUE))

  vdiffr::expect_doppelganger("list-cex",
                              analyse_SAR.CWOSL(
                                  object = list(object[[1]]),
                                  signal_integral = 1:5,
                                  background_integral = 800:1000,
                                  plot_onePage = TRUE, cex = 1.9))

  vdiffr::expect_doppelganger("CH_TL log x",
                              analyse_SAR.CWOSL(
                                  object = object_CH_TL[[1]],
                                  signal_integral = 4:10,
                                  background_integral = 900:1000,
                                  fit.method = "LIN",
                                  log = "x",
                                  plot_onePage = TRUE))

  vdiffr::expect_doppelganger("NO_TL log xy",
                              analyse_SAR.CWOSL(
                                  object = object_NO_TL[[1]],
                                  signal_integral = 1:5,
                                  background_integral = 600:900,
                                  log = "xy",
                                  plot_onePage = TRUE))

  vdiffr::expect_doppelganger("onlyLxTxTable",
                              analyse_SAR.CWOSL(
                                  object = object,
                                  signal_integral = 1:2,
                                  background_integral = 900:1000,
                                  onlyLxTxTable = TRUE,
                                  plot_onePage = TRUE))

  vdiffr::expect_doppelganger("NA rejection criteria",
                              analyse_SAR.CWOSL(
                                  object = object,
                                  signal_integral = 1:2,
                                  background_integral = 900:1000,
                                  rejection.criteria = list(recycling.ratio = NA,
                                                            sn.ratio = NA,
                                                            consider.uncertainties = TRUE),
                                  plot_onePage = TRUE))

  vdiffr::expect_doppelganger("multiple recuperation rates",
                              analyse_SAR.CWOSL(
                                  object = merge(object[[1]], object[[2]]),
                                  signal_integral = 1:2,
                                  background_integral = 900:1000,
                                  plot_onePage = TRUE))

  vdiffr::expect_doppelganger("background_integral NA",
                              analyse_SAR.CWOSL(
                                  object = object,
                                  signal_integral = 1:5,
                                  background_integral = NA,
                                  plot_onePage = TRUE))
  })
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 868
  data <- object[[1]]
  data@records[[1]]@recordType <- NA_character_
  expect_s4_class(analyse_SAR.CWOSL(data, verbose = FALSE,
                                    signal_integral = 1:2,
                                    background_integral = 900:1000),
                  "RLum.Results")

  ## issue 1370
  SW({
  expect_message(analyse_SAR.CWOSL(object, signal_integral = 1:4,
                                   background_integral = 100:200,
                                   dose.points = 0:6),
                 "[analyse_SAR.CWOSL()] ALQ: #1 | Fit failed for EXP (interpolation)",
                 fixed = TRUE)
  })
})

test_that("deprecated arguments", {
  testthat::skip_on_cran()

  SW({
  expect_warning(analyse_SAR.CWOSL(object[[1]],
                                   signal.integral.min = 1,
                                   signal.integral.max = 2,
                                   background.integral.min = 900,
                                   background.integral.max = 1000),
                 "were deprecated in v1.2.0, use 'signal_integral'")
  expect_warning(analyse_SAR.CWOSL(object[[1]],
                                   signal.integral.min = 1:2,
                                   signal.integral.max = 2:3,
                                   background.integral.min = c(800, 900),
                                   background.integral.max = c(900, 1000)),
                 "were deprecated in v1.2.0, use 'signal_integral'")
  expect_warning(analyse_SAR.CWOSL(object[1:2],
                                   signal.integral.min = list(1, 1),
                                   signal.integral.max = list(10, 20),
                                   background.integral.min = list(900, 950),
                                   background.integral.max = list(1000, 1000)),
                 "were deprecated in v1.2.0, use 'signal_integral'")

  ## mix of deprecated and newly-named arguments is not supported
  expect_error(analyse_SAR.CWOSL(object[1:2],
                                 signal.integral.min = list(1, 1),
                                 signal.integral.max = list(10, 20),
                                 background_integral = 900:1000),
               "Convert all integral arguments to the new names")
  })
})
