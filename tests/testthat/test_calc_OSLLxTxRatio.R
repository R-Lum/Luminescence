##preloads
data(ExampleData.LxTxOSLData, envir = environment())

test_that("test arguments", {
  testthat::skip_on_cran()
  snapshot.tolerance <- 1.5e-6

  ##digits
  expect_snapshot_RLum(expect_silent(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:2),
    background.integral = c(85:100),
    digits = 1)
    ), tolerance = snapshot.tolerance)

  ##sigmab
  expect_snapshot_RLum(expect_silent(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:2),
    background.integral = c(85:100),
    sigmab = c(1000,100))
    ), tolerance = snapshot.tolerance)

  ##poisson
  expect_snapshot_RLum(expect_silent(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:2),
    background.integral = c(85:100),
    background.count.distribution = "poisson")
    ), tolerance = snapshot.tolerance)
})

test_that("test input", {
  testthat::skip_on_cran()

  ##RLum.Curve
  expect_silent(calc_OSLLxTxRatio(
    set_RLum(class = "RLum.Data.Curve", data = as.matrix(Lx.data)),
    set_RLum(class = "RLum.Data.Curve", data = as.matrix(Tx.data)),
    signal.integral = c(1:2),
    background.integral = c(70:100)))

  ##matrix
  expect_silent(calc_OSLLxTxRatio(
    as.matrix(Lx.data),
    as.matrix(Tx.data),
    signal.integral = c(1:2),
    background.integral = c(70:100)))

  ##numeric
  expect_silent(calc_OSLLxTxRatio(
    as.numeric(Lx.data[,2]),
    as.numeric(Tx.data[,2]),
    signal.integral = c(1:2),
    background.integral = c(70:100)))


  ##RLum.Curve
  expect_silent(calc_OSLLxTxRatio(
    set_RLum(class = "RLum.Data.Curve", data = as.matrix(Lx.data)),
    Tx.data = NULL,
    signal.integral = c(1:2),
    background.integral = c(70:100)))

  ##matrix
  expect_silent(calc_OSLLxTxRatio(
    as.matrix(Lx.data),
    Tx.data = NULL,
    signal.integral = c(1:2),
    background.integral = c(70:100)))

  ##numeric
  expect_silent(calc_OSLLxTxRatio(
    as.numeric(Lx.data[,2]),
    Tx.data = NULL,
    signal.integral = c(1:2),
    background.integral = c(70:100)))

})

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_OSLLxTxRatio(
    Lx.data[1:10,],
    Tx.data,
    signal.integral = c(1:2),
    background.integral = c(85:100)
  ), "Channel numbers of Lx and Tx data differ")

  expect_error(calc_OSLLxTxRatio(Lx.data, "error"),
               "'Lx.data' and 'Tx.data' have different types")
  expect_error(calc_OSLLxTxRatio("error", "error"),
               "'Lx.data' should be of class 'RLum.Data.Curve', 'data.frame'")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:2000),
    background.integral = c(85:100)
  ), "'signal.integral' is not valid")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:90),
    background.integral = c(85:100)
  ), "'signal.integral' and 'background.integral' overlap")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:10),
    signal.integral.Tx = c(1:90),
    background.integral = c(85:100),
    background.integral.Tx = c(85:100)
  ), "'signal.integral.Tx' and 'background.integral.Tx' overlap")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    background.integral = c(85:1000)
  ), "'background.integral' is not valid, max: 100")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:10),
    signal.integral.Tx = c(1:10),
    background.integral = c(85:100),
    background.integral.Tx = c(85:10000)
  ), "'background.integral.Tx' is not valid, max: 100")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:10),
    signal.integral.Tx = c(1:1000),
    background.integral = c(85:100),
    background.integral.Tx = c(85:100)
  ), "'signal.integral.Tx' is not valid")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    signal.integral.Tx = c(1:20),
    background.integral = 80:100,
    background.integral.Tx = NULL
  ), "You have to provide both 'signal.integral.Tx' and 'background.integral.Tx'")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    background.integral = 80:100,
    sigmab = "test"
  ), "'sigmab' should be of class 'numeric'")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    background.integral = 80:100,
    sigmab = c(1, 2, 3)
  ), "'sigmab' can have at most length 2")
})

test_that("create warnings", {
  testthat::skip_on_cran()

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    signal.integral.Tx = c(1:20),
    background.integral = 80:100,
    background.integral.Tx = 60:100
  ), "Number of background channels for Lx < 25, error estimation might not be reliable")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    signal.integral.Tx = c(1:20),
    background.integral = 60:100,
    background.integral.Tx = 80:100
  ), "Number of background channels for Tx < 25, error estimation might not be reliable")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    background.integral = 60:100,
    background.count.distribution = "hallo"
  ), "Unknown method for 'background.count.distribution', a non-poisson")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    signal.integral.Tx = c(2:20),
    background.integral = 60:100,
    background.integral.Tx = 40:100,
    use_previousBG = TRUE
  ), "With 'use_previousBG = TRUE' independent Lx and Tx integral limits are")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()
  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(calc_OSLLxTxRatio(
      Lx.data = Lx.data,
      Tx.data = Tx.data,
      signal.integral = c(1:2),
      background.integral = c(85:100)
  ), tolerance = snapshot.tolerance)

  ## check weird circumstances
  ##(1) - Lx curve 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],0),
    Tx.data,
    signal.integral = c(1:2),
    background.integral = c(85:100)
  ))

  ##(2) - Tx curve 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    Lx.data,
    data.frame(Tx.data[,1],0),
    signal.integral = c(1:2),
    background.integral = c(85:100)
  ))

  ##(3) - Lx and Tx curve 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],0),
    data.frame(Tx.data[,1],0),
    signal.integral = c(1:2),
    background.integral = c(85:100)
  ))

  ##(4) - Lx < 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],-1000),
    data.frame(Tx.data[,1],0),
    signal.integral = c(1:2),
    background.integral = c(85:100)
  ))

  ##(5) - Tx < 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    Lx.data,
    data.frame(Lx.data[,1],-1000),
    signal.integral = c(1:2),
    background.integral = c(85:100)
  ))

  ##(6) - Lx & Tx < 0
  expect_snapshot_RLum(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],-1000),
    data.frame(Tx.data[,1],-1000),
    signal.integral = c(1:2),
    background.integral = c(85:100)
  ))
})

test_that("test NA mode with no signal integrals", {
  testthat::skip_on_cran()

  data(ExampleData.LxTxOSLData, envir = environment())
  temp <- expect_s4_class(calc_OSLLxTxRatio(
    Lx.data = Lx.data,
    Tx.data = Tx.data,
    signal.integral = NA,
    background.integral = NA), "RLum.Results")

  expect_equal(round(sum(temp$LxTx.table[1,]),0), 391926)
})
