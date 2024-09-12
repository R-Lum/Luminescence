## load data
data(ExampleData.BINfileData, envir = environment())

## transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)

test_that("Input validation", {
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
})

test_that("Test examples", {
  skip_on_cran()

  ##perform analysis
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

  expect_s4_class(
    analyse_SAR.TL(
        list(object, object),
        signal.integral.min = 210,
        signal.integral.max = 220,
        dose.points = 1:7,
        integral_input = "temperature",
        sequence.structure = c("SIGNAL", "BACKGROUND")),
    "RLum.Results"
  )

  expect_warning(
    analyse_SAR.TL(
        list(object),
        signal.integral.min = 210,
        signal.integral.max = 220,
        dose.points = 1:7,
        log = "x",
        sequence.structure = c("SIGNAL", "BACKGROUND")),
    "log-scale needs positive values; log-scale disabled"
  )

  expect_warning(
    analyse_SAR.TL(object, signal.integral.min = 2, signal.integral.max = 3,
                   sequence.structure = c("SIGNAL", "EXCLUDE")),
    "'fit.weights' ignored since the error column is invalid or 0")
  })
})
