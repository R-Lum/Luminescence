test_that("check output",{
  skip_on_cran()
  local_edition(3)

  own_parameters <- list(
    N = c(1e9, 0),
    E = c(0.4, 0),
    s = c(1e11, 0),
    A = c(1e-9,0),
    B = c(0, 1e-10),
    K = 0,
    model = "customized")

  own_state_parameters <- c(1e8, 1e8)

  own_start_temperature <- -220

  sequence <- list(TL = c(-220, 130, 1))

  TL_Chen2013 <- model_LuminescenceSignals(
    model = "customized",
    sequence = sequence,
    own_parameters = own_parameters,
    own_state_parameters = own_state_parameters,
    own_start_temperature = own_start_temperature,
    verbose = FALSE)


  expect_equal(class(TL_Chen2013)[1], "RLum.Analysis")

})
