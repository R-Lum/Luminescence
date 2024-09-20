## load data
data(ExampleData.BINfileData, envir = environment())

## transform the values from the third position in an RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)

test_that("input validation", {
  skip_on_cran()

  expect_error(analyse_SAR.TL(),
               "No value set for 'object'")
  expect_error(analyse_SAR.TL("test"),
               "Input object is not of type 'RLum.Analyis'")
  expect_error(analyse_SAR.TL(object),
               "No value set for 'signal.integral.min'")
  expect_error(analyse_SAR.TL(object, signal.integral.min = 1),
               "No value set for 'signal.integral.max'")
  expect_error(analyse_SAR.TL(list(object, "test")),
               "elements in the input list must be of class 'RLum.Analysis'")
  expect_error(analyse_SAR.TL(object, signal.integral.min = 1,
                              signal.integral.max = 2),
               "Input TL curves are not a multiple of the sequence structure")
  expect_error(analyse_SAR.TL(object, dose.points = c(2, 2),
                              signal.integral.min = 210,
                              signal.integral.max = 220,
                              sequence.structure = c("SIGNAL", "BACKGROUND")),
               "Length of 'dose.points' not compatible with number of signals")
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
    "log-scale needs positive values; log-scale disabled"
  )

  expect_warning(
  expect_snapshot_RLum(
    analyse_SAR.TL(object, signal.integral.min = 2, signal.integral.max = 3,
                   sequence.structure = c("SIGNAL", "EXCLUDE"))
    ),
  "'fit.weights' ignored since the error column is invalid or 0")
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
  "[plot_GrowthCurve()] Error: All points have the same dose, NULL returned",
  fixed = TRUE)
})
