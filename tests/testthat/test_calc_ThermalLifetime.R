context("calc_ThermalLifetime")


##EXAMPLE 1
##calculation for two trap-depths with similar frequency factor for different temperatures
E <- c(1.66, 1.70)
s <- 1e+13
T <- 10:20

set.seed(1)
temp <- calc_ThermalLifetime(
 E = E,
 s = s,
 T = T,
 output_unit = "Ma",
 verbose = FALSE
)


test_that("check class and length of output example 1", {
  testthat::skip_on_cran()
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 2)

})

test_that("check values from output example 1", {
  testthat::skip_on_cran()
  expect_equal(is(temp$lifetimes), c("array", "structure", "vector"))
  expect_equal(dim(temp$lifetimes), c(1, 2, 11))

  ##check results for 10 °C

  results <- lapply(1:length(T), function(x){
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

##EXAMPLE 2
##profiling of thermal life time for E and s and their standard error
E <- c(1.600, 0.003)
s <- c(1e+13,1e+011)
T <- 20

set.seed(1)
temp <- calc_ThermalLifetime(
  E = E,
  s = s,
  T = T,
  profiling = TRUE,
  output_unit = "Ma",
  verbose = FALSE,
  plot = FALSE
)

test_that("check class and length of output example 2", {
  testthat::skip_on_cran()
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 2)

})

test_that("check values from output example 2", {
  testthat::skip_on_cran()
  expect_equal(is(temp$lifetimes), c("numeric", "vector"))
  expect_equal(length(temp$lifetimes), 1000)
  expect_equal(dim(temp$profiling_matrix), c(1000, 4))
})
