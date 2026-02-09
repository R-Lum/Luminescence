## load data
data(ExampleData.LxTxOSLData, envir = environment())

test_that("test input", {
  testthat::skip_on_cran()

  ##RLum.Curve
  expect_silent(calc_OSLLxTxRatio(
    set_RLum(class = "RLum.Data.Curve", data = as.matrix(Lx.data)),
    set_RLum(class = "RLum.Data.Curve", data = as.matrix(Tx.data)),
    signal_integral = 1:2,
    background_integral = 70:100))

  ##matrix
  expect_silent(calc_OSLLxTxRatio(
    as.matrix(Lx.data),
    as.matrix(Tx.data),
    signal_integral = 1:2,
    background_integral = 70:100))

  ##numeric
  expect_silent(calc_OSLLxTxRatio(
    as.numeric(Lx.data[,2]),
    as.numeric(Tx.data[,2]),
    signal_integral = 1:2,
    background_integral = 70:100))

  ##RLum.Curve
  expect_silent(calc_OSLLxTxRatio(
    set_RLum(class = "RLum.Data.Curve", data = as.matrix(Lx.data)),
    Tx.data = NULL,
    signal_integral = 1:2,
    background_integral = 70:100))

  ##matrix
  expect_silent(calc_OSLLxTxRatio(
    as.matrix(Lx.data),
    Tx.data = NULL,
    signal_integral = 1:2,
    background_integral = 70:100))

  ##numeric
  expect_silent(calc_OSLLxTxRatio(
    as.numeric(Lx.data[,2]),
    Tx.data = NULL,
    signal_integral = 1:2,
    background_integral = 70:100))

  ## list
  expect_type(calc_OSLLxTxRatio(
      list(Lx.data),
      Tx.data = NULL,
      signal_integral = 1:2,
      background_integral = 70:100),
      "list")
})

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_OSLLxTxRatio(numeric()),
               "'Lx.data' cannot be an empty numeric")
  expect_error(calc_OSLLxTxRatio(Lx.data, "error"),
               "'Tx.data' should be of class 'RLum.Data.Curve', 'data.frame', 'matrix', 'numeric', a list")
  expect_error(calc_OSLLxTxRatio("error", "error"),
               "'Lx.data' should be of class 'RLum.Data.Curve', 'data.frame'")
  expect_error(calc_OSLLxTxRatio(Lx.data[, 1, drop = FALSE]),
               "'Lx.data' should have 2 columns")
  expect_error(calc_OSLLxTxRatio(Lx.data, Tx.data[, 1, drop = FALSE]),
               "'Tx.data' should have 2 columns")
  expect_error(calc_OSLLxTxRatio(Lx.data[1:10, ], Tx.data),
               "Different number of channels for Lx (10) and Tx (100)",
               fixed = TRUE)
  expect_error(calc_OSLLxTxRatio(Lx.data[1:10, ], signal_integral = NULL),
               "'signal_integral' should be of class 'integer', 'numeric' or NA")
  expect_error(calc_OSLLxTxRatio(Lx.data[1:10, ], signal_integral = list(1, 2)),
               "'signal_integral' should be of class 'integer', 'numeric' or NA")
  expect_warning(expect_error(
      calc_OSLLxTxRatio(Lx.data[1:10, ], signal_integral = -1:4),
      "'background_integral' should be of class 'integer', 'numeric' or NA"),
      "'signal_integral' reset to be between 1 and 4")
  expect_error(calc_OSLLxTxRatio(Lx.data[1:10, ], signal_integral = 1:2,
                                 background_integral = matrix(1:4, ncol =2)),
               "'background_integral' should be of class 'integer', 'numeric' or NA")
  expect_error(calc_OSLLxTxRatio(list(Lx.data, Lx.data), Tx.data),
               "'Tx.data' should either be a list of the same length or NULL")

  expect_error(calc_OSLLxTxRatio(Lx.data, Tx.data,
                                 signal_integral = 1:20, background_integral = NA,
                                 signal_integral_Tx = 1:20,
                                 background_integral_Tx = NULL),
               "When 'Tx.data' is provided, either both 'signal_integral_Tx' and")

  expect_warning(expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:2000,
    background_integral = 85:100
  ), "'background_integral' is expected to be at least 101, but the maximum allowed is 100"),
  "'signal_integral' reset to be between 1 and 100")

  SW({
  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:90,
    background_integral = 85:100
  ), "'background_integral' reset to be between 91 and 100")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:10,
    signal_integral_Tx = 1:90,
    background_integral = 85:100,
    background_integral_Tx = 85:100
  ), "'background_integral_Tx' reset to be between 91 and 100")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:20,
    background_integral = 85:1000
  ), "'background_integral' reset to be between 85 and 100")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:10,
    signal_integral_Tx = 1:10,
    background_integral = 85:100,
    background_integral_Tx = 85:10000
  ), "'background_integral_Tx' reset to be between 85 and 100")
  })

  expect_warning(expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:10,
    signal_integral_Tx = 1:1000,
    background_integral = 85:100,
    background_integral_Tx = 85:100
  ), "background_integral_Tx' is expected to be at least 101, but the maximum"),
  "'signal_integral_Tx' reset to be between 1 and 100")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:20,
    signal_integral_Tx = 1:20,
    background_integral = 80:100,
    background_integral_Tx = NULL
  ), "When 'Tx.data' is provided, either both 'signal_integral_Tx' and")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:20,
    background_integral = 80:100,
    sigmab = "test"
  ), "'sigmab' should be of class 'numeric'")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:20,
    background_integral = 80:100,
    sigmab = c(1, 2, 3)
  ), "'sigmab' should be of class 'numeric' or NULL and have length 1 or 2")

  expect_error(calc_OSLLxTxRatio(Lx.data, Tx.data, sig0 = -1),
               "'sig0' should be a single non-negative value")
})

test_that("create warnings", {
  testthat::skip_on_cran()

  ## deprecated arguments
  expect_warning(calc_OSLLxTxRatio(
      Lx.data,
      Tx.data,
      signal.integral = 1:2,
      background.integral = 85:100,
      digits = 1),
      "were deprecated in v1.2.0, use 'signal_integral', 'background_integral'")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:20,
    signal_integral_Tx = 1:20,
    background_integral = 80:100,
    background_integral_Tx = 60:100
  ), "Number of background channels for Lx < 25, error estimation might not be reliable")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:20,
    signal_integral_Tx = 1:20,
    background_integral = 60:100,
    background_integral_Tx = 80:100
  ), "Number of background channels for Tx < 25, error estimation might not be reliable")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:20,
    background_integral = 60:100,
    background.count.distribution = "hallo"
  ), "Unknown method for 'background.count.distribution', a non-poisson")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:20,
    signal_integral_Tx = 2:20,
    background_integral = 60:100,
    background_integral_Tx = 40:100,
    use_previousBG = TRUE
  ), "When 'use_previousBG = TRUE', independent Tx integral limits are not")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()
  snapshot.tolerance <- 1.5e-6

  ## digits
  expect_snapshot_RLum(expect_silent(calc_OSLLxTxRatio(
      Lx.data,
      Tx.data,
      signal_integral = 1:2,
      background_integral = 85:100,
      digits = 1)
  ), tolerance = snapshot.tolerance)

  ## sigmab
  expect_snapshot_RLum(expect_silent(calc_OSLLxTxRatio(
      Lx.data,
      Tx.data,
      signal_integral = 1:2,
      background_integral = 85:100,
      sigmab = c(1000,100))
  ), tolerance = snapshot.tolerance)

  ## poisson
  expect_snapshot_RLum(expect_silent(calc_OSLLxTxRatio(
      Lx.data,
      Tx.data,
      signal_integral = 1:2,
      background_integral = 85:100,
      background.count.distribution = "poisson")
  ), tolerance = snapshot.tolerance)

  ## use_previousBG
  expect_snapshot_RLum(calc_OSLLxTxRatio(
      Lx.data = Lx.data,
      Tx.data = Tx.data,
      signal_integral = 1:2,
      background_integral = 85:100,
      use_previousBG = TRUE
  ), tolerance = snapshot.tolerance)

  ## check weird circumstances
  ##(1) - Lx curve 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],0),
    Tx.data,
    signal_integral = 1:2,
    background_integral = 85:100
  ))

  ##(2) - Tx curve 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    Lx.data,
    data.frame(Tx.data[,1],0),
    signal_integral = 1:2,
    background_integral = 85:100
  ))

  ##(3) - Lx and Tx curve 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],0),
    data.frame(Tx.data[,1],0),
    signal_integral = 1:2,
    background_integral = 85:100
  ))

  ##(4) - Lx < 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],-1000),
    data.frame(Tx.data[,1],0),
    signal_integral = 1:2,
    background_integral = 85:100
  ))

  ##(5) - Tx < 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    Lx.data,
    data.frame(Lx.data[,1],-1000),
    signal_integral = 1:2,
    background_integral = 85:100
  ))

  ##(6) - Lx & Tx < 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],-1000),
    data.frame(Tx.data[,1],-1000),
    signal_integral = 1:2,
    background_integral = 85:100
  ))

  ## alternate mode with no signal integrals
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    Lx.data = Lx.data,
    Tx.data = Tx.data,
    signal_integral = NA,
    background_integral = NA
  ))

  ## ------------------------------------------------------------------------
  ## background_integral = NA

  expect_snapshot_RLum(calc_OSLLxTxRatio(
      Lx.data,
      Tx.data = NULL,
      signal_integral = 1:2,
      background_integral = NA))

  expect_snapshot_RLum(calc_OSLLxTxRatio(
      Lx.data,
      Tx.data,
      signal_integral = 1:2,
      background_integral = NA))

  expect_snapshot_RLum(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:20,
    background_integral = NA,
    signal_integral_Tx = 1:10,
    background_integral_Tx = NA))

  expect_snapshot_RLum(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal_integral = 1:20,
    background_integral = NA,
    signal_integral_Tx = 1:10,
    background_integral_Tx = 70:100))

  expect_snapshot_RLum(calc_OSLLxTxRatio(
      Lx.data,
      Tx.data,
      signal_integral = 1:2,
      background_integral = NA,
      use_previousBG = TRUE))

  expect_snapshot_RLum(calc_OSLLxTxRatio(
      Lx.data,
      Tx.data,
      signal_integral = 1:20,
      background_integral = 70:100,
      signal_integral_Tx = 1:10,
      background_integral_Tx = NA))
})
