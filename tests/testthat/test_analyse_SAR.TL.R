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
               "No value set for 'signal.integral.min'")
  expect_error(analyse_SAR.TL(object, signal.integral.min = 1),
               "No value set for 'signal.integral.max'")
  expect_error(analyse_SAR.TL(list(object, "test")),
               "All elements of 'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_SAR.TL(object, signal.integral.min = 1,
                              signal.integral.max = 2),
               "Input TL curves are not a multiple of the sequence structure")
  expect_error(analyse_SAR.TL(object, dose.points = c(2, 2),
                              signal.integral.min = 210,
                              signal.integral.max = 220,
                              sequence.structure = c("SIGNAL", "BACKGROUND")),
               "Length of 'dose.points' not compatible with number of signals")
  expect_error(analyse_SAR.TL(object, signal.integral.min = 1,
                              signal.integral.max = 2,
                              sequence.structure = c("SIGNAL", "BACKGROUND"),
                              integral_input = "error"),
               "[analyse_SAR.TL()] 'integral_input' should be one of ",
               fixed = TRUE)

  obj.rm <- object
  obj.rm@records[[1]]@data <- obj.rm@records[[1]]@data[1:225, ]
  expect_error(analyse_SAR.TL(obj.rm, signal.integral.min = 210,
                              signal.integral.max = 220,
                              sequence.structure = c("SIGNAL", "BACKGROUND")),
               "Signal range differs, check sequence structure")
})

test_that("Test examples", {
  skip_on_cran()

  ##perform analysis
  ## FIXME(mcol): this example doesn't work with snapshotting, presumably
  ## due to setting the `fit.method = "EXP OR LIN"` option
  SW({
  expect_s4_class(
    analyse_SAR.TL(
      object,
      signal.integral.min = 210,
      signal.integral.max = 220,
      fit.method = "EXP OR LIN",
      sequence.structure = c("SIGNAL", "BACKGROUND")
    ),
    "RLum.Results"
  )

  expect_snapshot_RLum(
    analyse_SAR.TL(
        list(object, object),
        signal.integral.min = 210,
        signal.integral.max = 220,
        dose.points = 1:7,
        integral_input = "temperature",
        sequence.structure = c("SIGNAL", "BACKGROUND"))
  )

  expect_warning(
  expect_snapshot_RLum(
    analyse_SAR.TL(
        list(object),
        signal.integral.min = 210,
        signal.integral.max = 220,
        dose.points = 1:7,
        log = "x",
        sequence.structure = c("SIGNAL", "BACKGROUND"))
    ),
    "Non-positive values detected, log-scale disabled"
  )

  SW({
  expect_message(analyse_SAR.TL(object, signal.integral.min = 2,
                                signal.integral.max = 3,
                                sequence.structure = "SIGNAL"),
                 "Too many curves, only the first 21 curves are plotted")
  })

  expect_warning(
  expect_snapshot_RLum(
    analyse_SAR.TL(object, signal.integral.min = 2, signal.integral.max = 3,
                   sequence.structure = c("SIGNAL", "EXCLUDE"))
    ),
  "Error column invalid or 0, 'fit.weights' ignored")
  })
})

test_that("regression tests", {
  skip_on_cran()

  ## issue 147 --------------------------------------------------------------

  SW({
  set.seed(1)
  expect_snapshot_RLum(
    analyse_SAR.TL(object, sequence.structure = c("SIGNAL", "BACKGROUND"),
                   signal.integral.min = 2, signal.integral.max = 3),
    tolerance = 1.5e-4
  )

  seq.structure <- c("SIGNAL", "EXCLUDE", "BACKGROUND", "EXCLUDE", "PREHEAT",
                     "EXCLUDE", "BACKGROUND", "SIGNAL", "EXCLUDE", "EXCLUDE",
                     "EXCLUDE", "EXCLUDE")
  expect_error(analyse_SAR.TL(object, signal.integral.min = 2,
                              signal.integral.max = 2,
                              sequence.structure = seq.structure),
               "[calc_TLLxTxRatio()] Data types of Lx and Tx data differ",
               fixed = TRUE)
  })

  expect_message(
  expect_snapshot_RLum(
    analyse_SAR.TL(object, dose.points = 2,
                   signal.integral.min = 210, signal.integral.max = 220,
                   sequence.structure = c("SIGNAL", "BACKGROUND"))
  ),
  "[fit_DoseResponseCurve()] Error: All points have the same dose, NULL returned",
  fixed = TRUE)

  ## issue 519
  bin.v8 <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  SW({
  expect_error(
      analyse_SAR.TL(read_BIN2R(bin.v8, fastForward = TRUE, verbose = FALSE)),
      "Input TL curves are not a multiple of the sequence structure")
  })
})
