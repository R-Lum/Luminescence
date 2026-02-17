## load data
data(ExampleData.BINfileData, envir = environment())

## transform the values from the third position in an RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)

test_that("input validation", {
  skip_on_cran()

  expect_error(analyse_SAR.TL(),
               "is missing, with no default")
  expect_error(analyse_SAR.TL("test"),
               "[analyse_SAR.TL()] 'object' should be of class 'RLum.Analysis'",
               fixed = TRUE)
  expect_error(analyse_SAR.TL(object),
               "'signal_integral' should be of class 'integer' or 'numeric'")
  expect_error(analyse_SAR.TL(object, signal_integral = NA),
               "'signal_integral' should be of class 'integer' or 'numeric'")
  expect_error(analyse_SAR.TL(list(object, "test")),
               "All elements of 'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_SAR.TL(object, signal_integral = 1:2),
               "Input TL curves are not a multiple of the sequence structure")
  expect_error(analyse_SAR.TL(object, signal_integral = 1:2,
                              sequence.structure = "EXCLUDE"),
               "'sequence.structure' contains no 'SIGNAL' entry")
  expect_error(analyse_SAR.TL(object, dose.points = c(2, 2),
                              signal_integral = 210:220,
                              sequence.structure = c("SIGNAL", "BACKGROUND")),
               "Length of 'dose.points' not compatible with number of signals")
  expect_error(analyse_SAR.TL(object, signal_integral = 1:2,
                              sequence.structure = c("SIGNAL", "BACKGROUND"),
                              integral_input = "error"),
               "[analyse_SAR.TL()] 'integral_input' should be one of ",
               fixed = TRUE)

  SW({
  expect_warning(analyse_SAR.TL(object, signal_integral = c(0, 1.5),
                                sequence.structure = c("SIGNAL", "BACKGROUND"),
                                integral_input = "measurement"),
                 "Conversion of 'signal_integral' from temperature to channels failed")
  expect_warning(analyse_SAR.TL(object, signal_integral = 2000:2100,
                                sequence.structure = c("SIGNAL", "BACKGROUND"),
                                integral_input = "measurement"),
                 "Conversion of 'signal_integral' from temperature to channels failed")
  })

  obj.rm <- object
  obj.rm@records[[1]]@data <- obj.rm@records[[1]]@data[1:225, ]
  expect_error(analyse_SAR.TL(obj.rm, signal_integral = 210:220,
                              sequence.structure = c("SIGNAL", "BACKGROUND")),
               "Signal ranges differ (225, 250), check sequence structure",
               fixed = TRUE)

  expect_output(expect_null(
      analyse_SAR.TL(object[1:4], fit.method = "ERROR",
                     signal_integral = 2:3,
                     sequence.structure = "SIGNAL"),
      "'fit.method' should be one of 'LIN', 'QDR', 'EXP', 'EXP OR LIN'"))

  ## deprecated argument
  expect_warning(analyse_SAR.TL(object, signal.integral = 2:3,
                                sequence.structure = c("SIGNAL", "BACKGROUND"),
                                verbose = FALSE),
                 "was deprecated in v1.2.0, use 'signal_integral'")
  expect_warning(analyse_SAR.TL(object,
                                signal_integral = 210:220,
                                integral_input = "temperature",
                                sequence.structure = c("SIGNAL", "BACKGROUND"),
                                verbose = FALSE),
                 "was deprecated in v1.2.0, use 'integral_input = \"measurement\"")
})

test_that("snapshot tests", {
  skip_on_cran()

  SW({
  expect_snapshot_RLum(
    analyse_SAR.TL(
        list(object, object),
        signal_integral = 210:220,
        dose.points = 1:7,
        integral_input = "measurement",
        sequence.structure = c("SIGNAL", "BACKGROUND"))
  )

  expect_warning(
  expect_snapshot_RLum(
    analyse_SAR.TL(
        list(object),
        signal_integral = 210:220,
        dose.points = 1:7,
        log = "x",
        sequence.structure = c("SIGNAL", "BACKGROUND"))
    ),
    "Non-positive values detected, log-scale disabled"
  )

  SW({
  expect_message(analyse_SAR.TL(object, signal_integral = 2:3,
                                sequence.structure = "SIGNAL"),
                 "Too many curves, only the first 21 curves are plotted")
  })

  expect_warning(
  expect_snapshot_RLum(
    analyse_SAR.TL(object, signal_integral = 2:3,
                   sequence.structure = c("SIGNAL", "EXCLUDE"))
    ),
  "Error column invalid or 0, 'fit.weights' ignored")
  })
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  vdiffr::expect_doppelganger("default",
                              analyse_SAR.TL(object,
                                             signal_integral = 210:220,
                                             sequence.structure = c("SIGNAL", "BACKGROUND")))
  })
})

test_that("regression tests", {
  skip_on_cran()

  ## issue 147 --------------------------------------------------------------

  SW({
  set.seed(1)
  expect_snapshot_RLum(
    analyse_SAR.TL(object, sequence.structure = c("SIGNAL", "BACKGROUND"),
                   signal_integral = 2:3),
    tolerance = 1.5e-4
  )

  seq.structure <- c("SIGNAL", "EXCLUDE", "BACKGROUND", "EXCLUDE", "PREHEAT",
                     "EXCLUDE", "BACKGROUND", "SIGNAL", "EXCLUDE", "EXCLUDE",
                     "EXCLUDE", "EXCLUDE")
  expect_error(analyse_SAR.TL(object, signal_integral = 2,
                              sequence.structure = seq.structure),
               "[calc_TLLxTxRatio()] 'Tx.data.signal' should be of class 'data.frame'",
               fixed = TRUE)
  })

  expect_message(
  expect_snapshot_RLum(
    analyse_SAR.TL(object, dose.points = 2,
                   signal_integral = 210:220,
                   sequence.structure = c("SIGNAL", "BACKGROUND"))
  ),
  "[fit_DoseResponseCurve()] Error: All points have the same dose, NULL returned",
  fixed = TRUE)

  ## issue 519
  bin.v8 <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  SW({
  expect_error(
      analyse_SAR.TL(read_BIN2R(bin.v8, fastForward = TRUE, verbose = FALSE),
                     signal_integral = 1:20),
      "Input TL curves are not a multiple of the sequence structure")
  })
})
