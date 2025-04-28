test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_ThermalLifetime(),
               "'E' should be of class 'numeric'")
  expect_error(calc_ThermalLifetime(E = 1.4),
               "'s' should be of class 'numeric'")
  expect_error(calc_ThermalLifetime(E = 1.4, s = 2, T = "error"),
               "'T' should be of class 'numeric' or 'integer'")

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
  expect_output(calc_ThermalLifetime(E = 1.4, s = 1e05, verbose = TRUE),
                "1 lifetimes calculated in total")
  expect_output(calc_ThermalLifetime(E = c(1.4, 0.001), s = c(1e05,1e03),
                                     profiling = FALSE),
                "4 lifetimes calculated in total")
  expect_output(calc_ThermalLifetime(E = c(1.4, 0.001), s = c(1e05,1e03),
                                     plot = TRUE, profiling = TRUE),
                "1000 lifetimes calculated in total")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  set.seed(1)
  snapshot.tolerance <- 1.5e-6

  ## Example 1: calculation for two trap-depths with similar frequency factor
  ## for different temperatures
  expect_snapshot_RLum(calc_ThermalLifetime(E = c(1.66, 1.70),
                                            s = 1e+13,
                                            T = 10:20,
                                            output_unit = "Ma",
                                            verbose = FALSE),
                       tolerance = snapshot.tolerance)

  ## Example 2: profiling of thermal life time for E and s and their standard error
  expect_snapshot_RLum(calc_ThermalLifetime(E = c(1.600, 0.003),
                                            s = c(1e+13, 1e+11),
                                            T = 20,
                                            profiling = TRUE,
                                            output_unit = "Ma",
                                            verbose = FALSE,
                                            plot = FALSE),
                       tolerance = snapshot.tolerance)
})
