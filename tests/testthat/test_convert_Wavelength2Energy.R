## artifical dataset according to Mooney et al. (2013)
lambda <- seq(400, 800, 50)
data <- cbind(lambda, 1)
rownames(data) <- lambda
colnames(data) <- 1:ncol(data)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(convert_Wavelength2Energy("test"),
               "'object' should be of class 'RLum.Data.Spectrum', 'data.frame'")
  expect_error(convert_Wavelength2Energy(data.frame()),
               "'object' cannot be an empty data.frame")
  expect_error(convert_Wavelength2Energy(matrix()),
               "'object' should have at least two columns")
  expect_error(convert_Wavelength2Energy(list(iris)),
               "'object' should have only numeric fields")
  expect_error(convert_Wavelength2Energy(set_RLum("RLum.Data.Spectrum")),
               "'object' contains no data")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ##set plot function
  p <- function(m) {
    plot(x = m[, 1], y = m[, 2])
    polygon(x = c(m[, 1], rev(m[, 1])), y = c(m[, 2], rep(0, nrow(m))))
    for (i in 1:nrow(m)) {
      lines(x = rep(m[i, 1], 2), y = c(0, m[i, 2]))
    }
  }

  snapshot.tolerance <- 1.5e-6

  ##test all three allowed input objects
  expect_snapshot_plain(convert_Wavelength2Energy(data),
                        tolerance = snapshot.tolerance)
  expect_s3_class(convert_Wavelength2Energy(as.data.frame(data)), class = "data.frame")
  object <- set_RLum(class = "RLum.Data.Spectrum", data = data[,1,drop = FALSE])
  expect_s4_class(convert_Wavelength2Energy(object), class = "RLum.Data.Spectrum")
  expect_type(convert_Wavelength2Energy(matrix(rnorm(5), 1, 5)),
              "double")

  ##test the list option
  expect_type(convert_Wavelength2Energy(list(data, as.data.frame(data), object)), "list")

  ##test order argument
  expect_snapshot_plain(convert_Wavelength2Energy(data, order = TRUE),
                        tolerance = snapshot.tolerance)
  expect_snapshot_RLum(convert_Wavelength2Energy(object, order = TRUE),
                        tolerance = snapshot.tolerance)

  ##test special treatment of RLum.Data.Spectrum objects
  object@info[["curveDescripter"]] <- "energy"
  expect_message(convert_Wavelength2Energy(object),
                 "'object' has already an energy scale, nothing done")
  object@info[["curveDescripter"]] <- "wavelength"
  res <- convert_Wavelength2Energy(object)
  expect_equal(res@info[["curveDescripter"]],
               "energy [eV]")

  ##Code below just a cross check if wanted
  ##matrix
  # m <- cbind(as.numeric(rownames(data)), data)
  # par(mfrow = c(1,2))
  # p(m)
  # p(convert_Wavelength2Energy(m))
  #
  # ##df
  # df <- as.data.frame(cbind(as.numeric(rownames(data)), data))
  # p(df)
  # p(convert_Wavelength2Energy(df))
  #
  # ##RLum.Data.Spectrum
  # object <- set_RLum(class = "RLum.Data.Spectrum", data = data[,1,drop = FALSE])
  # par(mfrow = c(1,2))
  # plot_RLum.Data.Spectrum(object, plot.type = "single", par.local = FALSE)
  # plot_RLum.Data.Spectrum(convert_Wavelength2Energy(object), plot.type = "single", par.local = FALSE)
})
