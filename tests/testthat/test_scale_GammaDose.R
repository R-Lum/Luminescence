## load data
data(ExampleData.ScaleGammaDose, envir = environment())
d <- ExampleData.ScaleGammaDose

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(
    scale_GammaDose(NA, plot = FALSE, verbose = TRUE),
    "'data' should be of class 'data.frame'"
  )
  expect_error(
    scale_GammaDose(d[ ,1:10], plot = FALSE, verbose = TRUE),
    "'data' should have 12 columns"
  )
  SW({
  expect_warning({
    tmp <- d
    colnames(tmp) <- letters[1:ncol(tmp)]
    scale_GammaDose(tmp, plot = FALSE, verbose = TRUE)
    },
    "Unexpected column names"
    )
  })
  expect_error({
    tmp <- d
    tmp$sample_offset <- NA
    scale_GammaDose(tmp, plot = FALSE, verbose = TRUE)
    },
    "Only one layer must be contain a numeric value in column 'sample_offset'"
  )
  expect_error({
    tmp <- d
    tmp$sample_offset[5] <- "target"
    scale_GammaDose(tmp, plot = FALSE, verbose = TRUE)
    },
    "Non-numeric value in the row of the target layer"
  )
  expect_error({
    tmp <- d
    tmp$sample_offset[5] <- -1
    scale_GammaDose(tmp, plot = FALSE, verbose = TRUE)
  },
  "The numeric value in 'sample_offset' must be positive"
  )
  expect_error({
    tmp <- d
    tmp$sample_offset[5] <- 20
    scale_GammaDose(tmp, plot = FALSE, verbose = TRUE)
  },
  "Sample offset larger than the target-layer's thickness"
  )

  expect_error(scale_GammaDose(d, conversion_factors = c("a", "b")),
               "'conversion_factors' contains multiple values but not all of")
  expect_error(scale_GammaDose(d, conversion_factors = 1),
               "'conversion_factors' should be one of 'Cresswelletal2018'")
  expect_error(scale_GammaDose(d, conversion_factors = "HansGuenter2020"),
               "'conversion_factors' should be one of 'Cresswelletal2018'")

  expect_error(scale_GammaDose(d, fractional_gamma_dose = c("a", "b")),
               "'fractional_gamma_dose' contains multiple values but not all")
  expect_error(scale_GammaDose(d, fractional_gamma_dose = 1),
               "'fractional_gamma_dose' should be one of 'Aitken1985'")
  expect_error(scale_GammaDose(d, fractional_gamma_dose = "error"),
               "'fractional_gamma_dose' should be one of 'Aitken1985'")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  ## Conversion factors: Liritzisetal2013
  SW({
  expect_snapshot_RLum(scale_GammaDose(data = ExampleData.ScaleGammaDose,
                                       conversion_factors = "Liritzisetal2013",
                                       fractional_gamma_dose = "Aitken1985",
                                       plot = TRUE, plot_singlePanels = TRUE,
                                       verbose = TRUE),
                       tolerance = snapshot.tolerance)
  })

  ## Conversion factors: Guerinetal2011
  expect_snapshot_RLum(scale_GammaDose(data = ExampleData.ScaleGammaDose,
                                       conversion_factors = "Guerinetal2011",
                                       fractional_gamma_dose = "Aitken1985",
                                       plot = TRUE, verbose = FALSE),
                       tolerance = snapshot.tolerance)

  ## Conversion factors: AdamiecAitken1998
  expect_snapshot_RLum(scale_GammaDose(data = ExampleData.ScaleGammaDose,
                                       conversion_factors = "AdamiecAitken1998",
                                       fractional_gamma_dose = "Aitken1985",
                                       plot = FALSE, verbose = FALSE),
                       tolerance = snapshot.tolerance)
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("defaults",
                              scale_GammaDose(ExampleData.ScaleGammaDose))
  })
})
