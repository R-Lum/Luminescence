context("convert_Wavelength2Energy")

test_that("test convert functions", {
  testthat::skip_on_cran()

  # Set up test scenario ------------------------------------------------------------------------
  #create artifical dataset according to Mooney et al. (2013)
  lambda <- seq(400,800,50)
  data <- matrix(data = rep(1, 2 * length(lambda)), ncol = 2)
  rownames(data) <- lambda
  colnames(data) <- 1:ncol(data)

  ##set plot function
  p <- function(m) {
    plot(x = m[, 1], y = m[, 2])
    polygon(x = c(m[, 1], rev(m[, 1])), y = c(m[, 2], rep(0, nrow(m))))
    for (i in 1:nrow(m)) {
      lines(x = rep(m[i, 1], 2), y = c(0, m[i, 2]))

    }
  }

  # Test ----------------------------------------------------------------------------------------
  ##crash function
  expect_error(convert_Wavelength2Energy("test"), regexp = "Class 'character' not supported as input!")

  ##test all three allowed input objects
  expect_is(convert_Wavelength2Energy(data), class = "matrix")
  expect_is(convert_Wavelength2Energy(as.data.frame(data)), class = "data.frame")
  object <- set_RLum(class = "RLum.Data.Spectrum", data = data[,1,drop = FALSE])
  expect_is(convert_Wavelength2Energy(object), class = "RLum.Data.Spectrum")

  ##test the list option
  expect_is(convert_Wavelength2Energy(list(data, as.data.frame(data), object)), class = "list")

  ##test order argument
  expect_is(convert_Wavelength2Energy(data, order = TRUE), class = "matrix")

  ##test special treatment of RLum.Data.Spectrum objects
  object@info[["curveDescripter"]] <- "energy"
  expect_message(convert_Wavelength2Energy(object), regexp = "Your object has already an energy scale, nothing done!")

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
