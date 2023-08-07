sequence <- list(
  PH = c(125, 5),
  OSL = c(125,10,90),
  PAUSE = c(20,10),
  TL = c(20, 200,5),
  RF = c(20,1,1),
  ILL = c(20,10,10),
  IRR = c(20,1,1),
  LM_OSL = c(20, 100),
  RF_heating = c(20,30,5,1))
model <- "Bailey2001"
parms <- RLumModel:::.set_pars(model)
n <- parms$n


test_translate_sequence <- RLumModel:::.translate_sequence(
  sequence = sequence,
  n = n,
  parms = parms,
  model = model,
  txtProgressBar = FALSE,
  verbose = FALSE)



test_that("check output",{
  skip_on_cran()
  local_edition(3)

  expect_equal(class(test_translate_sequence)[1], "RLum.Analysis")

  expect_equal(class(test_translate_sequence@records), "list")

  expect_equal(class(test_translate_sequence@records[[1]])[1], "RLum.Data.Curve")

  expect_true("OSL" %in% names(test_translate_sequence))
  expect_true("TL" %in% names(test_translate_sequence))
  expect_true("RF" %in% names(test_translate_sequence))

    expect_equal(test_translate_sequence@originator, "model_LuminescenceSignals")

  expect_equal(test_translate_sequence@protocol, "Bailey2001")

  expect_equal(test_translate_sequence@info$sequence$OSL["temp"], c(temp = 125))

  expect_equal(test_translate_sequence@info$sequence$OSL["duration"], c(duration = 10))

  expect_equal(test_translate_sequence@info$sequence$OSL["optical_power"], c(optical_power = 90))

})

