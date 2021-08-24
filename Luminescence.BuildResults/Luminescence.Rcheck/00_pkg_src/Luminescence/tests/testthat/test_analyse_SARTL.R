##Full check
test_that("Test examples", {
  skip_on_cran()
  local_edition(3)

  ##load data
  data(ExampleData.BINfileData, envir = environment())

  ##transform the values from the first position in a RLum.Analysis object
  object <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos=3)

  ##perform analysis
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

})

