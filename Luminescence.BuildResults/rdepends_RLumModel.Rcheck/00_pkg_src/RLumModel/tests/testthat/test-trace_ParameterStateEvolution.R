test_that("check class and output", {
  skip_on_cran()
  local_edition(3)

  ## create object to trace
  output <- model_LuminescenceSignals(
    model = "Bailey2001",
    sequence = list(
      OSL = c(temp = 20, duration = 1, optical_power = 100),
      OSL = c(temp = 20, duration = 1, optical_power = 100)),
    plot = FALSE,
    verbose = FALSE)

  ## crash function
  ## non RLum.Analysis
  expect_error(trace_ParameterStateEvolution("error"),
      "\\[trace_ParameterStateEvolution\\(\\)\\] object is not of class 'RLum.Analysis!")

  ## wrong originator
  expect_error(trace_ParameterStateEvolution(Luminescence::set_RLum("RLum.Analysis")),
      "\\[trace\\_ParameterStateEvolution\\(\\)] object was not produced by model_LuminescencerSignals\\(\\)!")

  ## select wrong type
  expect_error(suppressWarnings(trace_ParameterStateEvolution(output, step = "tsss")),
         "\\[trace\\_ParameterStateEvolution\\(\\)\\] object has length zero!")

  ## trigger no concentration error
  expect_error(trace_ParameterStateEvolution(get_RLum(output, recordType = "^OSL$", drop = FALSE)),
         "\\[trace\\_ParameterStateEvolution\\(\\)\\] No concentration record found, did you subset your object already?")

  ## simple run no plot
  t <- expect_type(trace_ParameterStateEvolution(output, plot = FALSE), "list")
  expect_length(t, 11)
  expect_type(t[[1]], "double")


  ## run list
  expect_type(trace_ParameterStateEvolution(list(output, output), plot = FALSE), "list")

  ## run with plot
  expect_invisible(trace_ParameterStateEvolution(output, plot = TRUE, grid = TRUE, step_names = TRUE, norm = TRUE))

  ## test more parameters
  expect_invisible(trace_ParameterStateEvolution(output, plot = TRUE, xlim = c(1:10), log = "x"))

})
