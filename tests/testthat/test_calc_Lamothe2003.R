test_that("Force function to break", {
  testthat::skip_on_cran()

  ##argument check

  ##object
  expect_error(calc_Lamothe2003(), regexp = "Input for 'object' missing but required!")

  ##dose_rate.envir
  expect_error(calc_Lamothe2003(object = NULL), regexp = "Input for 'dose_rate.envir' missing but required!")
  expect_error(calc_Lamothe2003(object = NULL, dose_rate.envir = 1, dose_rate.source = 1, g_value = 1),
               regexp = "Input for 'dose_rate.envir' is not of type 'numeric' and/or of length < 2!")

  ##dose_rate.source
  expect_error(calc_Lamothe2003(object = NULL, dose_rate.envir = NULL), regexp = "Input for 'dose_rate.source' missing but required!")
  expect_error(calc_Lamothe2003(object = NULL, dose_rate.envir = c(1,1), dose_rate.source = 1, g_value = 1),
               regexp = "Input for 'dose_rate.source' is not of type 'numeric' and/or of length < 2!")

  ##check warnings
  SW({
  expect_s4_class(
    suppressWarnings(calc_Lamothe2003(
      object = data.frame(
        x = c(0,10,20), y = c(1.4,0.7,2.3), z = c(0.01,0.02, 0.03)),
      dose_rate.envir = c(1,2,3), dose_rate.source = c(1,2,3), g_value = c(1,1,1))),
    "RLum.Results")
  })

  ##g_value
  expect_error(
    calc_Lamothe2003(
      object = NULL,
      dose_rate.envir = NULL,
      dose_rate.source = NULL
    ),
    regexp = "Input for 'g_value' missing but required!"
  )
  expect_error(
    calc_Lamothe2003(object = NULL, dose_rate.envir = c(1,2),
                     dose_rate.source = c(1,2), g_value = 1),
    "Input for 'g_value' is not of type 'numeric' and/or of length < 2"
  )

  ##object
  expect_error(suppressWarnings(
    calc_Lamothe2003(
      object = NULL,
      dose_rate.envir = c(1, 2, 3),
      dose_rate.source = c(1, 2, 3),
      g_value = c(1, 2)
    )),
    regexp = "Unsupported data type for 'object'"
  )

  expect_error(suppressWarnings(
    calc_Lamothe2003(
      object = set_RLum("RLum.Results"),
      dose_rate.envir = c(1, 2, 3),
      dose_rate.source = c(1, 2, 3),
      g_value = c(1, 2)
    )),
    regexp = "Input for 'object' created by"
  )

  ##tc
  expect_error(
    suppressWarnings(calc_Lamothe2003(
      object = set_RLum("RLum.Results"),
      dose_rate.envir = c(1, 2, 3),
      dose_rate.source = c(1, 2, 3),
      g_value = c(1, 1),
      tc.g_value = 1000
    )),
    "If you set 'tc.g_value' you have to provide a value for 'tc' too!"
  )


})

test_that("Test the function itself", {
  testthat::skip_on_cran()

  ##This is based on the package example
  ##load data
  ##ExampleData.BINfileData contains two BINfileData objects
  ##CWOSL.SAR.Data and TL.SAR.Data
  data(ExampleData.BINfileData, envir = environment())

  ##transform the values from the first position in a RLum.Analysis object
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

  ##perform SAR analysis and set rejection criteria
  results <- analyse_SAR.CWOSL(
  object = object,
  signal.integral.min = 1,
  signal.integral.max = 2,
  background.integral.min = 900,
  background.integral.max = 1000,
  verbose = FALSE,
  plot = FALSE,
  onlyLxTxTable = TRUE
  )

  ##run fading correction
  SW({
  expect_s4_class(calc_Lamothe2003(
    object = results,
    dose_rate.envir =  c(1.676 , 0.180),
    dose_rate.source = c(0.184, 0.003),
    g_value =  c(2.36, 0.6),
    plot = TRUE,
    fit.method = "EXP"), class = "RLum.Results")
  })

  ##run fading correction
  SW({
  expect_s4_class(calc_Lamothe2003(
    object = results,
    dose_rate.envir =  c(1.676 , 0.180),
    dose_rate.source = c(0.184, 0.003),
    g_value =  c(2.36, 0.6),
    tc = 1000,
    tc.g_value = 1200,
    plot = TRUE,
    fit.method = "EXP"), class = "RLum.Results")
  })

  ## pretend to have an analyse_pIRIRSequence originator to increase coverage
  results.mod <- results
  results.mod@originator <- "analyse_pIRIRSequence"
  results.mod@data$LnLxTnTx.table$Signal <- 1
  SW({
  expect_s4_class(calc_Lamothe2003(
    object = results.mod,
    dose_rate.envir =  c(1.676 , 0.180),
    dose_rate.source = c(0.184, 0.003),
    g_value =  c(2.36, 0.6),
    plot = FALSE,
    fit.method = "EXP"), class = "RLum.Results")
  })

  ## signal information present
  SW({
  res <- suppressWarnings(
    calc_Lamothe2003(
      object = data.frame(x = c(0,10,20), y = c(1.4,0.7,2.3),
                          z = c(0.01,0.02, 0.03), Signal=1:3),
      dose_rate.envir = c(1,2), dose_rate.source = c(1,2),
      g_value = c(1,1)))
  })
  expect_equal(res$data$SIGNAL, 1:3)
})
