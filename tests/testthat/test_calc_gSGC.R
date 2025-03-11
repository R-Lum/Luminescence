df <- data.frame(LnTn = 2.361, LnTn.error = 0.087,
                 Lr1Tr1 = 2.744, Lr1Tr1.error = 0.091,
                 Dr1 = 34.4)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_gSGC(data = NA),
               "'data' should be of class 'data.frame'")
  expect_error(calc_gSGC(data.frame(a = 1, b = 1, c = 1, d = 1, e = 1, f = 1)),
               "'data' is expected to have 5 columns")
  expect_error(calc_gSGC(df, gSGC.type = 3),
               "'gSGC.type' should be one of '0-250' or '0-450'")
  expect_error(calc_gSGC(df, gSGC.type = "error"),
               "'gSGC.type' should be one of '0-250' or '0-450'")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## standard test
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

  ## break plot (no solution)
  df_break <- data.frame(LnTn = 0, LnTn.error = 0.087,
                   Lr1Tr1 = 0, Lr1Tr1.error = 0.091,
                   Dr1 = 34.4)
  SW({
    expect_warning(calc_gSGC(data = df_break), regexp = "No solution was found")
  })
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  set.seed(seed = 1)
  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(calc_gSGC(df, plot = FALSE, verbose = FALSE),
                       tolerance = snapshot.tolerance)

  ## apply some random values for more coverage
  df1 <- data.frame(LnTn = 0.361, LnTn.error = 2.087,
                    Lr1Tr1 = 0.744, Lr1Tr1.error = 10.091,
                    Dr1 = 0.4)
  expect_snapshot_RLum(calc_gSGC(df1, plot = TRUE, verbose = FALSE),
                       tolerance = snapshot.tolerance)

  df2 <- data.frame(LnTn = 10.361, LnTn.error = 0.087,
                    Lr1Tr1 = 0.044, Lr1Tr1.error = 0.091,
                    Dr1 = 0.04)
  expect_snapshot_RLum(calc_gSGC(df2, plot = TRUE, verbose = FALSE),
                       tolerance = snapshot.tolerance)

  df3 <- data.frame(LnTn = 521440.0361, LnTn.error = 0.087,
                    Lr1Tr1 = 10.044, Lr1Tr1.error = -2.091,
                    Dr1 = 10.04)
  expect_snapshot_RLum(calc_gSGC(df3, plot = TRUE, verbose = FALSE),
                       tolerance = snapshot.tolerance)

  ## graphical snapshot tests
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("calc_gSGC expected",
                              fig = calc_gSGC(df))
  vdiffr::expect_doppelganger("calc_gSGC expected df1",
                              fig = calc_gSGC(df1))
  vdiffr::expect_doppelganger("calc_gSGC expected df2",
                              fig = calc_gSGC(df2))
  })
})
