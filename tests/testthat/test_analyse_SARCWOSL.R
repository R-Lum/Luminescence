##prepare test file for regression test
set.seed(1)
data(ExampleData.BINfileData, envir = environment())
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1:2)
results <- analyse_SAR.CWOSL(
  object = object[[1]],
  signal.integral.min = 1,
  signal.integral.max = 2,
  background.integral.min = 900,
  background.integral.max = 1000,
  plot = FALSE,
  verbose = FALSE
)

##generate different datasets removing TL curves
object_CH_TL <- get_RLum(object, record.id = -seq(1,30,4), drop = FALSE)
object_NO_TL <- get_RLum(object, record.id = -seq(1,30,2), drop = FALSE)

test_that("tests class elements", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(results, "RLum.Results")
  expect_equal(length(results), 4)
  expect_s3_class(results$data, "data.frame")
  expect_s3_class(results$LnLxTnTx.table, "data.frame")
  expect_s3_class(results$rejection.criteria, "data.frame")
  expect_type(results$Formula, "expression")
})

test_that("regression tests De values", {
  testthat::skip_on_cran()
  local_edition(3)

  ##fix for different R versions
  if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(object = round(sum(results$data[1:2]), digits = 0), 1716)

  }else{
    expect_equal(object = round(sum(results$data[1:2]), digits = 0), 1716)

  }

})

test_that("regression test LxTx table", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(object = round(sum(results$LnLxTnTx.table$LxTx), digits = 5),  20.92051)
  expect_equal(object = round(sum(results$LnLxTnTx.table$LxTx.Error), digits = 2), 0.34)

})

test_that("regression test - check rejection criteria", {
  testthat::skip_on_cran()
  local_edition(3)

  ##fix for different R versions
  if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(object = round(sum(results$rejection.criteria$Value), digits = 0),  1669)

  }else{
    expect_equal(object = round(sum(results$rejection.criteria$Value), digits = 0),  1669)

  }

})

test_that("simple run", {
  testthat::skip_on_cran()
  local_edition(3)

  ##verbose and plot off
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[1:2],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
    ),
    class = "RLum.Results"
  )

  ##signal integral set to NA
  expect_warning(
    analyse_SAR.CWOSL(
      object = object[1],
      signal.integral.min = NA,
      signal.integral.max = NA,
      background.integral.min = NA,
      background.integral.max = NA,
      fit.method = "EXP",
      plot = FALSE,
      verbose = FALSE,
      fit.weights = FALSE
    ), "\\[analyse_SAR.CWOSL\\(\\)\\] No signal or background integral applied, because they were set to NA\\!")


  expect_s4_class(
    suppressWarnings(analyse_SAR.CWOSL(
      object = object[1],
      signal.integral.min = NA,
      signal.integral.max = NA,
      background.integral.min = NA,
      background.integral.max = NA,
      fit.method = "EXP",
      plot = FALSE,
      verbose = FALSE
    )),
    class = "RLum.Results"
  )

  ##verbose and plot on
  ##full dataset
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      log = "x",
    ),
    class = "RLum.Results"
  )

  ##only CH TL
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object_CH_TL[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      log = "x",
      plot_onePage = TRUE
    ),
    class = "RLum.Results"
  )

  ##no TL
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object_NO_TL[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      log = "x",
      plot_onePage = TRUE
    ),
    class = "RLum.Results"
  )

  ##plot single
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "EXP",
      plot = TRUE,
      plot.single = TRUE
    ),
    class = "RLum.Results"
  )

  ##check rejection criteria
  expect_s4_class(
    analyse_SAR.CWOSL(
      object = object[[1]],
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "LIN",
      rejection.criteria= list(
        recycling.ratio = NA,
        recuperation.rate = 1,
        palaeodose.error = 1,
        testdose.error = 1,
        test = "new",
        exceed.max.regpoint = FALSE),
      plot = TRUE,
    ),
    class = "RLum.Results"
  )

   ##trigger stops for parameters
   ##object
   expect_error(analyse_SAR.CWOSL(
      object = "fail",
      background.integral.min = 900,
      fit.method = "LIN",
      plot = FALSE,
      verbose = FALSE
    ), regexp = "Input object is not of type 'RLum.Analysis'!")

    ##check stop for OSL.components ... failing
    expect_null(analyse_SAR.CWOSL(
       object = object[[1]],
       signal.integral.min = 1,
       signal.integral.max = 2,
       background.integral.min = 900,
       background.integral.max = 1000,
       dose.points = c(0,1,2),
       fit.method = "LIN",
       OSL.component = 1,
       plot = FALSE,
       verbose = FALSE
     ))

   expect_error(analyse_SAR.CWOSL(
     object = object[[1]],
     signal.integral.min = 1,
     signal.integral.max = 2,
     background.integral.min = 900,
     background.integral.max = 1000,
     dose.points = c(0,1,2),
     fit.method = "LIN",
     plot = FALSE,
     verbose = FALSE
   ), regexp = "length 'dose.points' differs from number of curves")


   expect_null(analyse_SAR.CWOSL(
     object = set_RLum("RLum.Analysis", records = list(set_RLum("RLum.Data.Curve", recordType = "false"))),
     signal.integral.min = 1,
     signal.integral.max = 2,
     background.integral.min = 800,
     background.integral.max = 900,
     fit.method = "LIN",
     plot = FALSE,
     verbose = FALSE
   ))

   ##check background integral
   expect_warning(analyse_SAR.CWOSL(
     object = object[[1]],
     signal.integral.min = 1,
     signal.integral.max = 2,
     background.integral.min = 800,
     background.integral.max = 9900,
     fit.method = "LIN",
     plot = FALSE,
     verbose = FALSE
   ), regexp = "Background integral out of bounds")

})

