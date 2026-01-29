test_that("input validation", {
  testthat::skip_on_cran()

  ##no data.frame
  expect_error(calc_gSGC_feldspar(
    data = "data",
    gSGC.type = "50LxTx",
    plot = FALSE),
    "[calc_gSGC_feldspar()] 'data' should be of class 'data.frame'",
    fixed = TRUE)
  expect_error(calc_gSGC_feldspar(iris[0, ]),
               "'data' cannot be an empty data.frame")

  ## input is somewhat not what we expect for gSGC
  expect_error(
    calc_gSGC_feldspar(
      data = data.frame(a  = 1, b = 1, c = 1, d = 1, e = 1),
      gSGC.type = "wrong",
      plot = FALSE
    ),
    "'gSGC.type' should be one of '50LxTx', '50Lx', '50Tx', '100LxTx', '100Lx'"
  )

  ## incorrect number of columns
  expect_error(
    calc_gSGC_feldspar(
      data = data.frame(a  = 1, b = 1, c = 1, d = 1),
      gSGC.type = "50LxTx",
      plot = FALSE
    ),
    "'data' should have 5 columns"
  )

  expect_error(calc_gSGC_feldspar(iris, n.MC = -1))
  expect_error(calc_gSGC_feldspar(iris, plot = NA))
})

test_that("test functionality", {
  testthat::skip_on_cran()

 ##finally run with plot output
 #test on a generated random sample
 set.seed(1234)
 n_samples <- 2
  data <- data.frame(
    LnTn = rnorm(n=n_samples, mean=1.0, sd=0.02),
    LnTn.error = rnorm(n=n_samples, mean=0.05, sd=0.002),
    Lr1Tr1 = rnorm(n=n_samples, mean=1.0, sd=0.02),
    Lr1Tr1.error = rnorm(n=n_samples, mean=0.05, sd=0.002),
    Dr1 = rep(100,n_samples))

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(calc_gSGC_feldspar(
    data = data,
    gSGC.type = "50LxTx",
    n.MC = 20,
    plot = TRUE),
    tolerance = snapshot.tolerance)

  ## more coverage
  set.seed(733)
  data$LnTn <- rnorm(n = n_samples, mean = 1.0, sd = 0.02)
  expect_snapshot_RLum(calc_gSGC_feldspar(
      data = data, gSGC.type = "50LxTx", n.MC = 10, plot = TRUE),
      tolerance = snapshot.tolerance)

  ## test own curve parameters
  SW({
  expect_message(results <- calc_gSGC_feldspar(
    data = data,
    gSGC.parameters = data.frame(
      y1 = 0.6,
      y1_err = 0.2,
      D1 = 250,
      D1_err = 50,
      y2 = 0.90,
      y2_err = 0.10,
      y0 = 0.001,
      y0_err = 0.0001
    ),
    plot = TRUE),
    "No solution found for dataset")
  })

  ##regression tests
  expect_s4_class(results, "RLum.Results")
  expect_true(all(is.na(unlist(results$m.MC))))
})
