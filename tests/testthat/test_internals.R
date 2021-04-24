test_that("Test internals", {
  testthat::skip_on_cran()
  local_edition(3)

  # .expand_parameters() ------------------------------------------------------
  ##create empty function ... reminder
  ##this is an internal function, the first object is always discarded, it
  ##might be a list of RLum.Analysis objects is might be super large
  f <- function(object, a, b = 1, c = list(), d = NULL) {
    Luminescence:::.expand_parameters(len = 3)

  }

  ##test some functions
  ##missing arguments must be identified
  expect_error(f(), "Argument <a> missing; with no default!")

  ##check whether the objects are properly recycled
  expect_type(f(object, a = 1), "list")
  expect_length(f(object, a = 1, c = list(a = 1, b = 2, c = 3))$c, 3)

  # .calc_HPDI() ------------------------------------------------------------
  set.seed(1234)
  test <- expect_type(Luminescence:::.calc_HPDI(rnorm(100), plot = TRUE), "double")
  expect_equal(round(sum(test),2), 0.20)

  ##create a case where the value cannot be calculated
  expect_type(.calc_HPDI(rlnorm(n = 100, meanlog = 10, sdlog = 100)), type = "logical")

  # .warningCatcher() ---------------------------------------------------------------------------
  expect_warning(Luminescence:::.warningCatcher(for(i in 1:5) warning("test")),
                 regexp = "\\(1\\) test\\: This warning occurred 5 times\\!")

  # .smoothing ----------------------------------------------------------------------------------
  expect_silent(Luminescence:::.smoothing(runif(100), k = 5, method = "median"))
  expect_error(Luminescence:::.smoothing(runif(100), method = "test"))

  # fancy_scientific ()--------------------------------------------------------------------------
  plot(seq(1e10, 1e20, length.out = 10),1:10, xaxt = "n")
  expect_silent(axis(1, at = axTicks(1),labels = Luminescence:::fancy_scientific(axTicks(1))))

  # .create_StatisticalSummaryText() ------------------------------------------------------------
  expect_silent(Luminescence:::.create_StatisticalSummaryText())
  expect_type(
    Luminescence:::.create_StatisticalSummaryText(
      calc_Statistics(data.frame(1:10,1:10)), keywords = "mean"), "character")

  # .unlist_RLum() ------------------------------------------------------------------------------
  expect_length(Luminescence:::.unlist_RLum(list(a = list(b = list(c = list(d = 1, e = 2))))), 2)

  # .rm_nonRLum() -----------------------------------------------------------
  expect_type(
    Luminescence:::.rm_nonRLum(c(list(set_RLum("RLum.Analysis"), set_RLum("RLum.Analysis")), 2)),
    "list")
  expect_type(
    Luminescence:::.rm_nonRLum(
      c(list(set_RLum("RLum.Analysis"), set_RLum("RLum.Analysis")), 2), class = "RLum.Analysis"),
    "list")

  # .matrix_binning() ---------------------------------------------------------------------------
  m <- matrix(data = c(rep(1:20,each = 20)), ncol = 10, nrow = 20)
  rownames(m) <- 1:nrow(m)
  colnames(m) <- 1:ncol(m)

    ##crash the function
    expect_error(Luminescence:::.matrix_binning("none matrix"),
                 regexp = "Input is not of class 'matrix'!")

    ##test operation modes and arguments
    expect_type(Luminescence:::.matrix_binning(m, bin_size = 4, bin_col = FALSE), "integer")
    expect_type(Luminescence:::.matrix_binning(m, bin_size = 4, bin_col = TRUE), "integer")

    ##test row / column renaming options
    expect_type(Luminescence:::.matrix_binning(m, bin_size = 2, bin_col = FALSE, names = "groups"),
              "integer")
    expect_type(Luminescence:::.matrix_binning(m, bin_size = 2, bin_col = FALSE, names = "mean"),
             "integer")
    expect_type(Luminescence:::.matrix_binning(m, bin_size = 2, bin_col = FALSE, names = "sum"),
             "integer")
    expect_type(Luminescence:::.matrix_binning(m, bin_size = 2, bin_col = FALSE, names = c("test1", "test2")),
              "integer")

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
