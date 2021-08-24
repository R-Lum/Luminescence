temp <- calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                            latitude = 38.06451, longitude = 1.49646,
                            altitude = 364, error = 10)


test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 3)

})

test_that("check values from output example 1", {
  testthat::skip_on_cran()
  local_edition(3)

  results <- get_RLum(temp)

  expect_equal(results$depth, 2.78)
  expect_equal(results$density, 1.7)
  expect_equal(results$latitude, 38.06451)
  expect_equal(results$longitude, 1.49646)
  expect_equal(results$altitude, 364)
  expect_equal(round(results$total_absorber.gcm2, digits = 0), 473)
  expect_equal(round(results$d0, digits = 3), 0.152)
  expect_equal(round(results$geom_lat, digits =  1), 41.1)
  expect_equal(round(results$dc, digits = 3), 0.161)



})


test_that("check values from output example 2b", {
  testthat::skip_on_cran()
  local_edition(3)
  temp <- calc_CosmicDoseRate(depth = c(5.0, 2.78), density = c(2.65, 1.7),
                              latitude = 12.04332, longitude = 4.43243,
                              altitude = 364, corr.fieldChanges = TRUE,
                              est.age = 67, error = 15)


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
