##EXAMPLE 1
##calculation for two trap-depths with similar frequency factor for different temperatures
set.seed(1)
temp <- calc_ThermalLifetime(
 E = c(1.66, 1.70),
 s = 1e+13,
 T = 10:20,
 output_unit = "Ma",
 verbose = FALSE
)

##EXAMPLE 2
##profiling of thermal life time for E and s and their standard error
temp2 <- calc_ThermalLifetime(
  E = c(1.600, 0.003),
  s = c(1e+13,1e+011),
  T = 20,
  profiling = TRUE,
  output_unit = "Ma",
  verbose = FALSE,
  plot = FALSE
)

test_that("check class and length of output example 1", {
  testthat::skip_on_cran()

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 2)

  expect_type(temp$lifetimes, "double")
  expect_equal(dim(temp$lifetimes), c(1, 2, 11))

  ##check results for 10 °C
  results <- lapply(1:length(10:20), function(x){
    temp$lifetimes[,,x]
  })

  expect_equal(round(results[[1]], digits = 3),  c("1.66" = 1115.541, "1.7" = 5747.042))
  expect_equal(round(results[[2]], digits = 4),  c("1.66" = 878.0196, "1.7" = 4497.3585))
  expect_equal(round(results[[3]], digits = 4),  c("1.66" = 692.2329, "1.7" = 3525.4738))
  expect_equal(round(results[[4]], digits = 4),  c("1.66" = 546.6658, "1.7" = 2768.3216))
  expect_equal(round(results[[5]], digits = 4),  c("1.66" = 432.4199, "1.7" = 2177.4436))
  expect_equal(round(results[[6]], digits = 4),  c("1.66" = 342.6069, "1.7" = 1715.5406))
  expect_equal(round(results[[7]], digits = 4),  c("1.66" = 271.8854, "1.7" = 1353.8523))
  expect_equal(round(results[[8]], digits = 4),  c("1.66" = 216.1065, "1.7" = 1070.1642))
  expect_equal(round(results[[9]], digits = 4),  c("1.66" = 172.0421, "1.7" = 847.2879))
  expect_equal(round(results[[10]], digits = 4), c("1.66" = 137.1765, "1.7" = 671.9020))
  expect_equal(round(results[[11]], digits = 4), c("1.66" = 109.5458, "1.7" = 533.6641))
})


test_that("check class and length of output example 2", {
  testthat::skip_on_cran()
  expect_s4_class(temp2, c("RLum.Results"))
  testthat::expect_equal(length(temp2), 2)

  testthat::expect_type(temp2$lifetimes, "double")
  testthat::expect_equal(class(temp2$lifetimes), "numeric")
  testthat::expect_equal(length(temp2$lifetimes), 1000)
  testthat::expect_equal(dim(temp2$profiling_matrix), c(1000, 4))
})

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_ThermalLifetime(),
               "'E' should be of class 'numeric'")
  expect_error(calc_ThermalLifetime(E = 1.4),
               "'s' should be of class 'numeric'")

  ##profiling settings
  SW({
  expect_warning(
    calc_ThermalLifetime(E = 1.4, s = 1e05, profiling_config = list(n = 10)),
    "Minimum MC runs are 1000, parameter 'n' in profiling_config reset to 1000")
  })
  expect_error(calc_ThermalLifetime(
    E = 1.4,
    s = 1e05,
    profiling = TRUE,
    profiling_config = list(E.distribution = "test")
  ), "Unknown distribution setting for E profiling")
  expect_error(suppressWarnings(calc_ThermalLifetime(
    E = 1.4,
    s = 1e05,
    profiling = TRUE,
    profiling_config = list(s.distribution = "test"))
  ), "Unknown distribution setting for s profiling")

  ##output
  SW({
  expect_warning(calc_ThermalLifetime(E = 1.4, s = 1e05, output_unit = "test"),
                 "'output_unit' unknown, reset to 's'")
  })
  expect_output(calc_ThermalLifetime(E = 1.4, s = 1e05, verbose = TRUE))
  expect_output(calc_ThermalLifetime(E = c(1.4, 0.001), s = c(1e05,1e03), plot = TRUE, profiling = TRUE))
})
