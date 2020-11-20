##preloads
data(ExampleData.LxTxOSLData, envir = environment())
temp <- calc_OSLLxTxRatio(
  Lx.data = Lx.data,
  Tx.data = Tx.data,
  signal.integral = c(1:2),
  background.integral = c(85:100))

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 2)

})

test_that("test arguments", {
  testthat::skip_on_cran()
  local_edition(3)

  ##digits
  expect_silent(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:2),
    background.integral = c(85:100),
    digits = 1))

  ##sigmab
  expect_silent(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:2),
    background.integral = c(85:100),
    sigmab = c(1000,100)
    ))

  ##poisson
  expect_silent(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:2),
    background.integral = c(85:100),
    background.count.distribution = "poisson"
  ))

})


test_that("test input", {
  testthat::skip_on_cran()
  local_edition(3)

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

test_that("force function break", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_error(calc_OSLLxTxRatio(
    Lx.data[1:10,],
    Tx.data,
    signal.integral = c(1:2),
    background.integral = c(85:100)
  ), "Channel numbers of Lx and Tx data differ!")

  expect_error(calc_OSLLxTxRatio(
    "Lx.data",
    Tx.data,
    signal.integral = c(1:2),
    background.integral = c(85:100)
  ), "Data type of Lx and Tx data differs!")

  expect_error(calc_OSLLxTxRatio(
    "Lx.data",
    "Tx.data",
    signal.integral = c(1:2),
    background.integral = c(85:100)
  ), "Data type error! Required types are data.frame or numeric vector.")


  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:2000),
    background.integral = c(85:100)
  ), "signal.integral is not valid!")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:90),
    background.integral = c(85:100)
  ), "Overlapping of 'signal.integral' and 'background.integral' is not permitted!")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:10),
    signal.integral.Tx = c(1:90),
    background.integral = c(85:100),
    background.integral.Tx = c(85:100)
  ), "Overlapping of 'signal.integral.Tx' and 'background.integral.Tx' is not permitted!")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    background.integral = c(85:1000)
  ), "background.integral is not valid! Max: 100")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:10),
    signal.integral.Tx = c(1:10),
    background.integral = c(85:100),
    background.integral.Tx = c(85:10000)
  ), "background.integral.Tx is not valid! Max: 100")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:10),
    signal.integral.Tx = c(1:1000),
    background.integral = c(85:100),
    background.integral.Tx = c(85:100)
  ), "signal.integral.Tx is not valid!")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    signal.integral.Tx = c(1:20),
    background.integral = 80:100,
    background.integral.Tx = NULL
  ), "You have to provide both: signal.integral.Tx and background.integral.Tx!")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    background.integral = 80:100,
    sigmab = "test"
  ), "'sigmab' has to be of type numeric.")

  expect_error(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    background.integral = 80:100,
    sigmab = 1:100
  ), "Maximum allowed vector length for 'sigmab' is 2.")


})

test_that("create warnings", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    signal.integral.Tx = c(1:20),
    background.integral = 80:100,
    background.integral.Tx = 60:100
  ), "Number of background channels for Lx < 25; error estimation might not be reliable!")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    signal.integral.Tx = c(1:20),
    background.integral = 60:100,
    background.integral.Tx = 80:100
  ), "Number of background channels for Tx < 25; error estimation might not be reliable!",)

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    background.integral = 60:100,
    background.count.distribution = "hallo"
  ), "Unknown method for background.count.distribution. A non-poisson distribution is assumed!")

  expect_warning(calc_OSLLxTxRatio(
    Lx.data,
    Tx.data,
    signal.integral = c(1:20),
    signal.integral.Tx = c(2:20),
    background.integral = 60:100,
    background.integral.Tx = 40:100,
    use_previousBG = TRUE
  ), "For option use_previousBG = TRUE independent Lx and Tx integral limits are not allowed. Integral limits of Lx used for Tx.")

})


test_that("check weird circumstances", {
  testthat::skip_on_cran()
  local_edition(3)

  ##(1) - Lx curve 0
  expect_type(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],0),
    Tx.data,
    signal.integral = c(1:2),
    background.integral = c(85:100)
  )$LxTx.table, type = "list")


  ##(2) - Tx curve 0
  expect_type(calc_OSLLxTxRatio(
    Lx.data,
    data.frame(Tx.data[,1],0),
    signal.integral = c(1:2),
    background.integral = c(85:100)
  )$LxTx.table, type = "list")

  ##(3) - Lx and Tx curve 0
  expect_type(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],0),
    data.frame(Tx.data[,1],0),
    signal.integral = c(1:2),
    background.integral = c(85:100)
  )$LxTx.table, type = "list")

  ##(4) - Lx < 0
  expect_type(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],-1000),
    data.frame(Tx.data[,1],0),
    signal.integral = c(1:2),
    background.integral = c(85:100)
  )$LxTx.table, type = "list")

  ##(5) - Tx < 0
  expect_type(calc_OSLLxTxRatio(
    Lx.data,
    data.frame(Lx.data[,1],-1000),
    signal.integral = c(1:2),
    background.integral = c(85:100)
  )$LxTx.table, type = "list")

  ##(6) - Lx & Tx < 0
  expect_type(calc_OSLLxTxRatio(
    data.frame(Lx.data[,1],-1000),
    data.frame(Tx.data[,1],-1000),
    signal.integral = c(1:2),
    background.integral = c(85:100)
  )$LxTx.table, type = "list")


})

test_that("check values from output example", {
  testthat::skip_on_cran()
  local_edition(3)

  results <- get_RLum(temp)

  expect_equal(results$LnLx, 81709)
  expect_equal(results$LnLx.BG, 530)
  expect_equal(results$TnTx, 7403)
  expect_equal(results$TnTx.BG, 513)
  expect_equal(results$Net_LnLx, 81179)
  expect_equal(round(results$Net_LnLx.Error, digits = 4), 286.5461)
  expect_equal(results$Net_TnTx, 6890)
  expect_equal(round(results$Net_TnTx.Error, digits = 5), 88.53581)
  expect_equal(round(results$LxTx, digits = 5), 11.78215)
  expect_equal(round(results$LxTx.Error, digits = 7), 0.1570077)

})

test_that("test NA mode with no signal integrals", {
  testthat::skip_on_cran()
  local_edition(3)

  data(ExampleData.LxTxOSLData, envir = environment())
  temp <- expect_s4_class(calc_OSLLxTxRatio(
    Lx.data = Lx.data,
    Tx.data = Tx.data,
    signal.integral = NA,
    background.integral = NA), "RLum.Results")

  expect_equal(round(sum(temp$LxTx.table[1,]),0), 391926)

})
