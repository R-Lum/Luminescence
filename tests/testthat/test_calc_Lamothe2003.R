## load data
data(ExampleData.BINfileData, envir = environment())

## transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

## perform SAR analysis
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

test_that("input validation", {
  testthat::skip_on_cran()

  df <- data.frame(x = c(0, 10, 20), y = c(1.4, 0.7, 2.3),
                   z = c(0.01, 0.02, 0.03))

  expect_error(calc_Lamothe2003(),
               "'object' should be of class 'data.frame' or 'RLum.Results'")

  ##dose_rate.envir
  expect_error(calc_Lamothe2003(df),
               "'dose_rate.envir' should be of class 'numeric'")
  expect_error(calc_Lamothe2003(df, dose_rate.envir = 1),
               "'dose_rate.envir' should contain 2 elements")

  ##dose_rate.source
  expect_error(calc_Lamothe2003(df, dose_rate.envir = c(1, 1)),
               "'dose_rate.source' should be of class 'numeric'")
  expect_error(calc_Lamothe2003(df, dose_rate.envir = c(1, 1),
                                dose_rate.source = 1),
               "'dose_rate.source' should contain 2 elements")

  ##g_value
  expect_error(calc_Lamothe2003(object = df, dose_rate.envir = c(1, 1),
                                dose_rate.source = c(1, 2)),
               "'g_value' should be of class 'numeric'")
  expect_error(calc_Lamothe2003(object = df, dose_rate.envir = c(1, 2),
                                dose_rate.source = c(1, 2), g_value = 1),
               "'g_value' should contain 2 elements")

  ##check warnings
  SW({
  expect_warning(
      expect_s4_class(calc_Lamothe2003(object = df,
                                       dose_rate.envir = c(1, 2, 3),
                                       dose_rate.source = c(1, 2, 3),
                                       g_value = c(1, 1, 1)),
                      "RLum.Results"),
      "taking only the first two entries")
  })

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
    "If you set 'tc.g_value' you have to provide a value for 'tc' too"
  )
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  set.seed(1)
  snapshot.tolerance <- 1.5e-6

  SW({
  ##run fading correction
  expect_snapshot_RLum(calc_Lamothe2003(
    object = results,
    dose_rate.envir =  c(1.676 , 0.180),
    dose_rate.source = c(0.184, 0.003),
    g_value =  c(2.36, 0.6),
    plot = TRUE,
    fit.method = "EXP"),
    tolerance = snapshot.tolerance)

  ##run fading correction
  expect_snapshot_RLum(calc_Lamothe2003(
    object = results,
    dose_rate.envir =  c(1.676 , 0.180),
    dose_rate.source = c(0.184, 0.003),
    g_value =  c(2.36, 0.6),
    tc = 1000,
    tc.g_value = 1200,
    plot = TRUE,
    fit.method = "EXP"),
    tolerance = snapshot.tolerance)
  })
})

test_that("more coverage", {
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

  ## signal information present
  res <- suppressWarnings(
    calc_Lamothe2003(
      object = data.frame(x = c(0,10,20), y = c(1.4,0.7,2.3),
                          z = c(0.01,0.02, 0.03), Signal=1:3),
      dose_rate.envir = c(1,2), dose_rate.source = c(1,2),
      g_value = c(1,1)))
  })
  expect_equal(res$data$SIGNAL, 1:3)
})
