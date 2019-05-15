context("model_LuminescenceSignals")

##Full check
test_that("Simple call", {
  skip_on_cran()

  ##simple test from the manual
  sequence <-
    list(
      IRR = c(20, 10, 1),
      TL = c(20, 400, 5)
    )

  ##model sequence
  expect_output(model.output <- model_LuminescenceSignals(
    sequence = sequence,
    model = "Bailey2001"))


})

