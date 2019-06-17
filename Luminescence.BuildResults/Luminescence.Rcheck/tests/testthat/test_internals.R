context("internals")

test_that("Test internals", {
  testthat::skip_on_cran()

  # .warningCatcher() ---------------------------------------------------------------------------
  expect_warning(Luminescence:::.warningCatcher(for(i in 1:5){warning("test")}))

  # .smoothing ----------------------------------------------------------------------------------
  expect_silent(Luminescence:::.smoothing(runif(100), k = 5, method = "median"))
  expect_error(Luminescence:::.smoothing(runif(100), method = "test"))


  # fancy_scientific ()--------------------------------------------------------------------------
  plot(seq(1e10, 1e20, length.out = 10),1:10, xaxt = "n")
  expect_silent(axis(1, at = axTicks(1),labels = Luminescence:::fancy_scientific(axTicks(1))))


  # .create_StatisticalSummaryText() ------------------------------------------------------------
  expect_silent(Luminescence:::.create_StatisticalSummaryText())
  expect_is(
    Luminescence:::.create_StatisticalSummaryText(
      calc_Statistics(data.frame(1:10,1:10)), keywords = "mean"), class = "character")


  # .unlist_RLum() ------------------------------------------------------------------------------
  expect_length(Luminescence:::.unlist_RLum(list(a = list(b = list(c = list(d = 1, e = 2))))), 2)


  # .matrix_binning() ---------------------------------------------------------------------------
  m <- matrix(data = c(rep(1:20,each = 20)), ncol = 10, nrow = 20)
  rownames(m) <- 1:nrow(m)
  colnames(m) <- 1:ncol(m)

    ##crash the function
    expect_error(Luminescence:::.matrix_binning("none matrix"),
                 regexp = "Input is not of class 'matrix'!")

    ##test operation modes and arguments
    expect_is(Luminescence:::.matrix_binning(m, bin_size = 4, bin_col = FALSE), class = "matrix")
    expect_is(Luminescence:::.matrix_binning(m, bin_size = 4, bin_col = TRUE), class = "matrix")

    ##test row / column renaming options
    expect_is(Luminescence:::.matrix_binning(m, bin_size = 2, bin_col = FALSE, names = "groups"),
              class = "matrix")
    expect_is(Luminescence:::.matrix_binning(m, bin_size = 2, bin_col = FALSE, names = "mean"),
              class = "matrix")
    expect_is(Luminescence:::.matrix_binning(m, bin_size = 2, bin_col = FALSE, names = "sum"),
              class = "matrix")
    expect_is(Luminescence:::.matrix_binning(m, bin_size = 2, bin_col = FALSE, names = c("test1", "test2")),
              class = "matrix")

    ##clean-up
    rm(m)


  ## C++ code ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  # src_create_RLumDataCurve_matrix -------------------------------------------------------------
  ##RLum.Data.Curve() ... test src_create_RLumDataCurve_matrix()
  expect_output(
    Luminescence:::src_create_RLumDataCurve_matrix(
      DATA = 1:100,
      VERSION = 4,
      NPOINTS = 100,
      LTYPE = "TL",
      LOW = 0,
      HIGH = 500,
      AN_TEMP = 0,
      TOLDELAY = 0,
      TOLON = 0,
      TOLOFF = 0
    )
  )

})
