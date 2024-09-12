df <- data.frame(LnTn = 2.361, LnTn.error = 0.087,
                 Lr1Tr1 = 2.744, Lr1Tr1.error = 0.091,
                 Dr1 = 34.4)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_gSGC(data = NA),
               "'data' must be a data.frame")
  expect_error(calc_gSGC(data.frame(a = 1, b = 1, c = 1, d = 1, e = 1, f = 1)),
               "'data' is expected to have 5 columns")
  expect_error(calc_gSGC(df, gSGC.type = 3),
               "'gSGC.type' must be of type 'character'")
  expect_error(calc_gSGC(df, gSGC.type = "error"),
               "Unknown 'gSGC.type'")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  SW({
  expect_s4_class(calc_gSGC(data = df,
    gSGC.type = "0-450",
    plot = TRUE,
    verbose = TRUE
  ), "RLum.Results")

  pars <- list(A = 0, A.error = 0, D0 = 0, D0.error = 0,
               c = 0, c.error = 0, Y0 = 0, Y0.error = 0)
  expect_s4_class(calc_gSGC(data = df, gSGC.parameters = pars),
                  "RLum.Results")
  })

  set.seed(seed = 1)
  temp <- calc_gSGC(df, plot = FALSE, verbose = FALSE)

  expect_s4_class(temp, "RLum.Results")
  expect_s3_class(temp$De, "data.frame")
  expect_type(temp$De.MC, "list")
  expect_equal(length(temp), 3)

  expect_equal(round(sum(temp$De), digits = 2), 30.39)
  expect_equal(round(sum(temp$De.MC[[1]]), 0), 10848)

  ## apply some random values for more coverage
  df1 <- data.frame(LnTn = 0.361, LnTn.error = 2.087,
                    Lr1Tr1 = 0.744, Lr1Tr1.error = 10.091,
                    Dr1 = 0.4)
  expect_silent(calc_gSGC(df1, plot = TRUE, verbose = FALSE))

  df2 <- data.frame(LnTn = 10.361, LnTn.error = 0.087,
                    Lr1Tr1 = 0.044, Lr1Tr1.error = 0.091,
                    Dr1 = 0.04)
  expect_silent(calc_gSGC(df2, plot = TRUE, verbose = FALSE))
})
