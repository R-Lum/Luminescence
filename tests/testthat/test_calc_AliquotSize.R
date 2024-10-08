set.seed(1)
temp <- calc_AliquotSize(
  grain.size = c(100,150),
  sample.diameter = 1,
  MC.iter = 100,
  plot = FALSE,
  verbose = FALSE)

test_that("consistency checks", {
  testthat::skip_on_cran()

  expect_error(calc_AliquotSize(),
               "Please provide the mean grain size or a range of grain sizes")
  expect_error(calc_AliquotSize(grain.size = 1:3),
               "Please provide the mean grain size or a range of grain sizes")
  expect_error(calc_AliquotSize(grain.size = 100, packing.density = 2),
               "'packing.density' expects values between 0 and 1")
  expect_error(calc_AliquotSize(grain.size = 100, packing.density = 1, sample.diameter = -1),
               "'sample.diameter' should be a positive scalar")
  expect_error(calc_AliquotSize(grain.size = 100, sample.diameter = 9.8,
                                MC = TRUE),
               "'grain.size' must be a vector containing the min and max")
  expect_output(calc_AliquotSize(grain.size = 100, packing.density = 1, sample.diameter = 9.8, grains.counted = 30, MC = TRUE),
                regexp = "Monte Carlo simulation is only available for estimating the amount of grains on the sample disc.")

  SW({
  expect_s4_class(
    calc_AliquotSize(
      grain.size = 100, packing.density = "inf", sample.diameter = 9.8, MC = FALSE), "RLum.Results")
  expect_s4_class(
    calc_AliquotSize(
      grain.size = c(100, 150), grains.counted = 1000, sample.diameter = 9.8, MC = FALSE), "RLum.Results")
  expect_s4_class(
    suppressWarnings(calc_AliquotSize(
      grain.size = c(100, 150), grains.counted = c(1000, 1100, 900), sample.diameter = 10, MC = FALSE)),
    "RLum.Results")
  })
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 2)
  expect_s3_class(temp$summary, "data.frame")
  expect_type(temp$MC, "list")

})

test_that("check summary output", {
  testthat::skip_on_cran()

  result <- get_RLum(temp)

  expect_equal(result$grain.size, 125)
  expect_equal(result$sample.diameter, 1)
  expect_equal(result$packing.density, 0.65)
  expect_equal(result$n.grains, 42)
  expect_equal(result$grains.counted, NA)
})

test_that("check MC run", {
  testthat::skip_on_cran()

  expect_equal(round(temp$MC$statistics$n), 100)
  expect_equal(round(temp$MC$statistics$mean), 43)
  expect_equal(round(temp$MC$statistics$median), 39)
  expect_equal(round(temp$MC$statistics$sd.abs), 19)
  expect_equal(round(temp$MC$statistics$sd.rel), 45)
  expect_equal(round(temp$MC$statistics$se.abs), 2)
  expect_equal(round(temp$MC$statistics$se.rel), 5)
  expect_length(temp$MC$kde$x, 10000)
  expect_length(temp$MC$kde$y, 10000)
})
