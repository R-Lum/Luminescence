## load data
set.seed(1)
data(ExampleData.BINfileData, envir = environment())
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1:2)

##generate different datasets removing TL curves
object_CH_TL <- get_RLum(object, record.id = -seq(1,30,4), drop = FALSE)
object_NO_TL <- get_RLum(object, record.id = -seq(1,30,2), drop = FALSE)

## FIXME(mcol): snapshots were produced on R 4.3.3, and a tolerance of 1.5e-6
## was sufficient; however, both R 4.4 and Rdevel produce slightly different
## values that required increasing the tolerance to 1.5e-3. When a new R
## release happens, so that R 4.3.3 is no longer oldrel, the snapshots should
## be rebuilt and the tolerance restored to 1.5e-6.
snapshot.tolerance <- 1.5e-3

test_that("tests class elements", {
  testthat::skip_on_cran()

  expect_snapshot_RLum(
    results <- analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      plot = FALSE,
      verbose = FALSE
    ), tolerance = snapshot.tolerance
  )

  expect_s4_class(results, "RLum.Results")
  expect_equal(length(results), 4)
  expect_s3_class(results$data, "data.frame")
  expect_s3_class(results$LnLxTnTx.table, "data.frame")
  expect_s3_class(results$rejection.criteria, "data.frame")
  expect_type(results$Formula, "expression")

  expect_equal(object = round(sum(results$data[1:2]), digits = 0), 1716)
  expect_equal(object = round(sum(results$LnLxTnTx.table$LxTx), digits = 5),  20.92051)
  expect_equal(object = round(sum(results$LnLxTnTx.table$LxTx.Error), digits = 2), 0.34)

  expect_type(object = results$data$POS, "integer")
  expect_equal(object = results$data$POS, 1)
  expect_type(object = results$data$ALQ, "double")

  expect_equal(round(sum(results$rejection.criteria$Value), digits = 0),
               1669)

  ## fit.method
  expect_snapshot_RLum(
    analyse_SAR.CWOSL(
      object = object[1:2],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
    ), tolerance = snapshot.tolerance
  )

  ##remove position information from the curve
  ##data
  object_f <- object[[1]]
  object_f@records <- lapply(object_f@records, function(x){
    x@info$POSITION <- NULL
    x
  })
  t <- expect_s4_class(
    analyse_SAR.CWOSL(
      object = object_f,
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
    ),
    class = "RLum.Results"
  )

  expect_type(t@data$data$POS, "logical")
})

test_that("simple run", {
  testthat::skip_on_cran()

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
    "[analyse_SAR.CWOSL()] No signal or background integral applied as they",
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

  ##verbose and plot on
  ##full dataset
  SW({
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      log = "x",
    ),
    class = "RLum.Results"
  )

  ##only CH TL
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object_CH_TL[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      log = "x",
      plot_onePage = TRUE
    ),
    class = "RLum.Results"
  )

  ##no TL
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object_NO_TL[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      log = "x",
      plot_onePage = TRUE
    ),
    class = "RLum.Results"
  )

  ##no mix TL and OSL (the only TL will be sorted out automatically)
  only_TL <- set_RLum("RLum.Analysis", records = rep(object_CH_TL[[1]]@records[[2]], length(object_NO_TL[[1]]@records)))
  object_mixed <- c(object_NO_TL, only_TL)

  t <- expect_s4_class(
    analyse_SAR.CWOSL(
      object = object_mixed[2:3],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
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
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "EXP",
      plot = TRUE,
      plot_singlePanels = TRUE
    ),
    class = "RLum.Results"
  )

  ##check rejection criteria
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      rejection.criteria= list(
        recycling.ratio = NA,
        recuperation.rate = 1,
        palaeodose.error =  NA,
        testdose.error = 1,
        test = "new",
        exceed.max.regpoint = FALSE),
      plot = TRUE,
    ),
    class = "RLum.Results"
  )

  ##check recuperation point selection
  t <- expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      rejection.criteria= list(
        recycling.ratio = NA,
        recuperation.rate = 1,
        palaeodose.error = 1,
        testdose.error = 1,
        recuperation_reference = "R1",
        test = "new",
        exceed.max.regpoint = FALSE),
      plot = TRUE,
    ),
    class = "RLum.Results"
  )
  })

  ## check if a different point was selected
  expect_equal(round(t$rejection.criteria$Value[2],2), expected = 0.01)

  ## trigger stop of recuperation reference point
  expect_error(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      rejection.criteria= list(
        recycling.ratio = NA,
        recuperation.rate = 1,
        palaeodose.error = 1,
        testdose.error = 1,
        recuperation_reference = "stop",
        test = "new",
        exceed.max.regpoint = FALSE),
      plot = TRUE,
    ),
    regexp = "\\[analyse\\_SAR.CWOSL\\(\\)\\] Recuperation reference invalid, valid are")

   # Trigger stops -----------------------------------------------------------
   ##trigger stops for parameters
   ##object
  expect_error(analyse_SAR.CWOSL("fail"),
               "'object' should be of class 'RLum.Analysis'")

  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal.integral.min = 1.2,
                                 signal.integral.max = 3.5,
                                 background.integral.min = 900,
                                 background.integral.max = 1000),
               "'signal.integral' or 'background.integral' is not of type integer")

  ## check stop for OSL.components ... failing
  SW({
  expect_message(expect_null(
      analyse_SAR.CWOSL(
       object = object[[1]],
       signal.integral.min = 1,
       signal.integral.max = 2,
       background.integral.min = 900,
       background.integral.max = 1000,
       OSL.component = 1,
       verbose = FALSE
      )),
      "Something went wrong while generating the LxTx table, NULL returned")
  })

   expect_error(analyse_SAR.CWOSL(
     object = object[[1]],
     signal.integral.min = 1,
     signal.integral.max = 2,
     background.integral.min = 900,
     background.integral.max = 1000,
     dose.points = c(0,1,2),
     fit.method = "LIN",
     plot = FALSE,
     verbose = FALSE
   ), regexp = "Length of 'dose.points' differs from number of curves")

  expect_message(
   expect_null(analyse_SAR.CWOSL(
     object = set_RLum("RLum.Analysis",records = list(set_RLum("RLum.Data.Curve", recordType = "false"))),
     signal.integral.min = 1,
     signal.integral.max = 2,
     background.integral.min = 800,
     background.integral.max = 900,
     fit.method = "LIN",
     plot = FALSE,
     verbose = FALSE
   )),
   "No record of type 'OSL', 'IRSL', 'POSL' detected")

   ##check background integral
   expect_warning(analyse_SAR.CWOSL(
     object = object[[1]],
     signal.integral.min = 1,
     signal.integral.max = 2,
     background.integral.min = 800,
     background.integral.max = 9900,
     fit.method = "LIN",
     plot = FALSE,
     verbose = FALSE
   ), regexp = "Background integral out of bounds")

  expect_warning(analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 1,
      background.integral.min = 800,
      background.integral.max = 1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
  ), "Integral signal limits cannot be equal")

  expect_warning(analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = c(600, 800),
      background.integral.max = c(900, 1000),
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
  ), "Background integral for Tx curves set, but not for the signal integral")


  expect_warning(expect_message(
      analyse_SAR.CWOSL(
          object = object[[1]],
          signal.integral.min = c(1, 1500),
          signal.integral.max = c(2, 2000),
          background.integral.min = 800,
          background.integral.max = 1000,
          fit.method = "LIN",
          plot = FALSE,
          verbose = FALSE
      ), "Something went wrong while generating the LxTx table"),
  "Signal integral for Tx curves set, but not for the background integral")

  ## this generates multiple warnings
  warnings <- capture_warnings(analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = c(1, 70),
      signal.integral.max = c(2, 80),
      background.integral.min = 800,
      background.integral.max = 1200,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE))
  expect_match(warnings, all = FALSE,
               "Background integral for Tx out of bounds")

  expect_message(expect_null(suppressWarnings(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 999,
      signal.integral.max = 1000,
      background.integral.min = c(900, 1975),
      background.integral.max = c(900, 1975),
      plot = FALSE,
      verbose = FALSE)
  )), "Error: Something went wrong while generating the LxTx table")

  ## plot_singlePanels
  expect_error(analyse_SAR.CWOSL(object[[1]],
                                 signal.integral.min = 1,
                                 signal.integral.max = 2,
                                 background.integral.min = 900,
                                 background.integral.max = 1000,
                                 plot_singlePanels = list()),
               "'plot_singlePanels' should be of class 'logical', 'integer'")

  ## add one OSL curve
  expect_warning(expect_null(
      analyse_SAR.CWOSL(
          object = merge(object[[1]], object[[1]][[2]]),
          signal.integral.min = 1, signal.integral.max = 2,
          background.integral.min = 800, background.integral.max = 1200)),
      "Input OSL/IRSL curves are not a multiple of two")

   ## check different curve numbers by shorten one OSL curve
   object_short <- object
   object_short[[1]]@records[[2]]@data <- object_short[[1]]@records[[2]]@data[-nrow(object_short[[1]]@records[[2]]@data),]

   ## without fix
   expect_warning(
     analyse_SAR.CWOSL(
       object = object_short[[1]],
       signal.integral.min = 1,
       signal.integral.max = 2,
       background.integral.min = 800,
       background.integral.max = 9900,
       fit.method = "LIN",
       plot = FALSE,
       verbose = FALSE),
     "[analyse_SAR.CWOSL()] Input curves have different lengths",
     fixed = TRUE)

   ## with new parameter
   testthat::expect_s4_class(
     object = analyse_SAR.CWOSL(
       object = object_short[[1]],
       signal.integral.min = 1,
       signal.integral.max = 2,
       background.integral.min = 800,
       background.integral.max = 999,
       fit.method = "LIN",
       trim_channels = TRUE,
       plot = FALSE,
       verbose = FALSE),
   class = "RLum.Results")
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
          info = no_irr_object[[1]]@records[[1]]@info,
          ),
          no_irr_object[[2]][[1]], no_irr_object[[2]][[2]]),
      originator = "read_XSYG2R"
  )
  SW({ # repeated message
  expect_message(
      analyse_SAR.CWOSL(list(obj_irr, obj_irr),
                        signal.integral.min = 1,
                        signal.integral.max = 2,
                        background.integral.min = 900,
                        background.integral.max = 1000,
                        fit.method = "LIN", verbose = FALSE),
      "All points have the same dose, NULL returned"
  )
  })

  ## more coverage
  SW({ # Number of background channels for Lx < 25
  expect_warning(expect_warning(
      analyse_SAR.CWOSL(merge(object[[1]], object[[2]],
                              object[[1]], object[[2]]),
                        signal.integral.min = 1,
                        signal.integral.max = 2,
                        background.integral.min = 900,
                        background.integral.max = 900,
                        background.count.distribution = "poisson",
                        log = "xy", verbose = FALSE),
      "Too many curves, only the first 21 curves are plotted"),
      "Multiple IRSL curves detected")

  expect_warning(
      analyse_SAR.CWOSL(object = object[[1]],
                        signal.integral.min = 1,
                        signal.integral.max = 2,
                        background.integral.min = 900,
                        background.integral.max = 1000,
                        plot.single = TRUE,
                        verbose = FALSE),
      "'plot.single' is deprecated, use 'plot_singlePanels' instead")
  })

  ##test failed recycling ratio
  test_failed <-
    analyse_SAR.CWOSL(
      object = object[1],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 200,
      background.integral.max = 1000,
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
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 200,
      background.integral.max = 1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE),
     regexp = "\\[analyse\\_SAR\\.CWOSL\\(\\)\\] 'dose.points' contains NA values or have not been set")

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
       signal.integral.min = 1,
       signal.integral.max = 2,
       background.integral.min = 200,
       background.integral.max = 1000,
       fit.method = "LambertW",
       n.MC = 10,
       plot = FALSE,
       verbose = FALSE))
  })

  ##set all rejection criteria to NA
  test_failed <-
    analyse_SAR.CWOSL(
      object = object[1],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 200,
      background.integral.max = 1000,
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
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 200,
      background.integral.max = 1000,
      fit.method = "LIN",
      rejection.criteria = list(1),
      plot = FALSE,
      verbose = FALSE)
  expect_equal(object = test_ok$data$RC.Status, "OK")

  ##test multi parameter settings
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[1:2],
      signal.integral.min = 1,
      signal.integral.max = list(10,20),
      background.integral.min = 900,
      background.integral.max = 1000,
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
      signal.integral.min = 1,
      signal.integral.max = list(10,20),
      background.integral.min = 900,
      background.integral.max = 1000,
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
  analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = c(900, 975),
      background.integral.max = c(900, 975),
      dose.points = rep(2, 7),
      plot = FALSE,
      verbose = FALSE)

  object[[1]]@records[[2]][1, 1] <- 0
  expect_warning(analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 975,
      log = "x",
      verbose = FALSE),
      "Curves shifted by one channel for log-plot")
  })
})
