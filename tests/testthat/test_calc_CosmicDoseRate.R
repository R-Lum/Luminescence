test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_CosmicDoseRate(depth = "error"),
               "'depth' should be of class 'numeric'")
  expect_error(calc_CosmicDoseRate(depth = numeric(0), density = 1.7,
                                   latitude = 38.1, longitude = 1.4),
               "'depth' cannot be an empty numeric")
  expect_error(calc_CosmicDoseRate(depth = -2, density = 1.7, altitude = 364,
                                   latitude = 38.1, longitude = 1.4),
               "No negative values allowed for 'depth' and 'density'")
  expect_error(calc_CosmicDoseRate(depth = 10, density = "error"),
               "'density' should be of class 'numeric'")
  expect_error(calc_CosmicDoseRate(depth = 10, density = numeric(0),
                                   latitude = 38.1, longitude = 1.4),
               "'density' cannot be an empty numeric")
  expect_error(calc_CosmicDoseRate(depth = 2.78, density = 1.7, altitude = 364,
                                   latitude = 38.1, longitude = 1.4,
                                   corr.fieldChanges = TRUE),
               "requires an age estimate")
  expect_error(calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                                   latitude = 38.06451),
               "'longitude' should be of class 'numeric'")
  expect_error(calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                                   latitude = 38.1, longitude = numeric(0)),
               "'longitude' cannot be an empty numeric")
  expect_error(calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                                   latitude = 38.06451, longitude = 1.49646),
               "'altitude' should be of class 'numeric'")
  expect_error(calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                                   latitude = 38.06451, longitude = 1.49646,
                                   altitude = numeric(0)),
               "'altitude' cannot be an empty numeric")
  expect_error(calc_CosmicDoseRate(depth = 2.78, density = c(1.7, 2.9),
                                   corr.fieldChanges = TRUE, est.age = 20,
                                   latitude = 38.06451, longitude = 1.49646,
                                   altitude = 364),
               "The number of values for 'density' should either be 1 or")
  expect_error(calc_CosmicDoseRate(depth = rep(2.78, 3), density = c(1.7, 2.9),
                                   latitude = 38.06451, longitude = 1.49646,
                                   altitude = 364),
               "The number of values for 'density' should either be 1 or")

  expect_output(calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                                    corr.fieldChanges = TRUE, est.age = 100,
                                    latitude = 38.0645, longitude = 1.4964,
                                    altitude = 364),
                "No geomagnetic field change correction for samples older than 80")
  expect_output(calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                                    corr.fieldChanges = TRUE, est.age = 20,
                                    latitude = 38.0645, longitude = 1.4964,
                                    altitude = 364, half.depth = TRUE),
                "No geomagnetic field change correction necessary for geomagnetic latitude >35 degrees")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  SW({
  expect_snapshot_RLum(
      calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                          latitude = 38.06451, longitude = 1.49646,
                          altitude = 364, error = 10),
      tolerance = snapshot.tolerance)

  ## length(depth) > length(density), half.depth
  expect_snapshot_RLum(
      calc_CosmicDoseRate(depth = c(2.78, 3.12), density = 1.7,
                          corr.fieldChanges = TRUE, est.age = 20,
                          latitude = 28.06451, longitude = 1.49646,
                          altitude = 364, half.depth = TRUE),
      tolerance = snapshot.tolerance)

  expect_snapshot_RLum(
      calc_CosmicDoseRate(depth = c(1.78, 0.12), density = c(0.7, 0.2),
                          latitude = 120, longitude = 30, altitude = 1200),
      tolerance = snapshot.tolerance)

  expect_snapshot_RLum(
      calc_CosmicDoseRate(depth = c(1.78, 0.12), density = c(0.2, 0.2),
                          latitude = 30, longitude = 120, altitude = 120),
      tolerance = snapshot.tolerance)
  })
})

test_that("check values from output example 2b", {
  testthat::skip_on_cran()

  SW({
  temp <- calc_CosmicDoseRate(depth = c(5.0, 2.78), density = c(2.65, 1.7),
                              latitude = 12.04332, longitude = 4.43243,
                              altitude = 364, corr.fieldChanges = TRUE,
                              est.age = 67, error = 15)
  })
  results <- get_RLum(temp)

  expect_equal(results$depth.1, 5)
  expect_equal(results$depth.2, 2.78)
  expect_equal(results$density.1, 2.65)
  expect_equal(results$density.2, 1.7)
  expect_equal(results$latitude, 12.04332)
  expect_equal(results$longitude, 4.43243)
  expect_equal(results$altitude, 364)
  expect_equal(round(results$total_absorber.gcm2, digits = 0), 1798)
  expect_equal(round(results$d0, digits = 4), 0.0705)
  expect_equal(round(results$geom_lat, digits =  1), 15.1)
  expect_equal(round(results$dc, digits = 3), 0.072)
})
