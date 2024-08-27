test_that("Test internals", {
  testthat::skip_on_cran()

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
  expect_length(f(object, a = (1), c = list(a = 1, b = 2, c = 3))$c, 3)
  expect_equal(f(object, a = (1 + 10), c = list(a = 1, b = 2, c = 3))$a[[1]], 11)

  # .calc_HPDI() ------------------------------------------------------------
  set.seed(1234)
  test <- expect_type(Luminescence:::.calc_HPDI(rnorm(100), plot = TRUE), "double")
  expect_equal(round(sum(test),2), 0.20, tolerance = 1)

  ##create a case where the value cannot be calculated
  expect_type(.calc_HPDI(rlnorm(n = 100, meanlog = 10, sdlog = 100)), type = "logical")

  # .warningCatcher() ---------------------------------------------------------------------------
  expect_warning(Luminescence:::.warningCatcher(for(i in 1:5) warning("test")),
                 regexp = "\\(1\\) test\\: This warning occurred 5 times\\!")

  # .smoothing ----------------------------------------------------------------------------------
  expect_silent(Luminescence:::.smoothing(runif(100), k = 5, method = "median"))
  suppressWarnings( # suppress second warning: number of items to replace
                    #   is not a multiple of replacement length
  expect_warning(.smoothing(runif(100), k = 4, method = "median"),
                 "'k' must be odd")
  )
  expect_silent(.smoothing(runif(200), method = "median"))
  expect_silent(.smoothing(runif(100), k = 4, method = "mean"))
  expect_error(Luminescence:::.smoothing(runif(100), method = "test"))

  # fancy_scientific ()--------------------------------------------------------------------------
  plot(seq(1e10, 1e20, length.out = 10),1:10, xaxt = "n")
  expect_silent(axis(1, at = axTicks(1),labels = Luminescence:::fancy_scientific(axTicks(1))))

  # .add_fancy_log_axis() -----------------------------------------------------
  y <- c(0.1, 0.001, 0.0001)
  plot(1:length(y), y, yaxt = "n", log = "y")
  expect_silent(Luminescence:::.add_fancy_log_axis(side = 2, las = 1))
  expect_null(.add_fancy_log_axis(side = 1, las = 1))

  # .create_StatisticalSummaryText() ------------------------------------------------------------
  stats <- calc_Statistics(data.frame(1:10,1:10))
  expect_silent(Luminescence:::.create_StatisticalSummaryText())
  expect_equal(.create_StatisticalSummaryText(stats,
                                              keywords = "mean"),
               "mean = 5.5")
  expect_equal(.create_StatisticalSummaryText(stats,
                                              keywords = "unweighted$mean"),
               "mean = 5.5")
  expect_equal(.create_StatisticalSummaryText(stats,
                                              keywords = "weighted$mean"),
               "weighted$mean = 1.89")


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
  m <- matrix(data = c(rep(1:20, each = 20)), ncol = 20, nrow = 20)
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

  # .download_file() --------------------------------------------------------

  ## returns just NULL (no URL detected)
  expect_null(.download_file(url = "_url"))

  ## attempts download but fails
  url.404 <- "https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extg"
  expect_message(
      expect_message(
          expect_message(expect_null(.download_file(url = url.404)),
                         "URL detected:"),
          "Attempting download ..."),
      "FAILED")

  ## attempts download and succeeds
  url.ok <- "https://raw.githubusercontent.com/R-Lum/rxylib/master/codecov.yml"
  suppressMessages( # silence other messages already tested above
      expect_message(expect_type(.download_file(url = url.ok),
                                 "character"),
                     "OK")
  )

  # .get_named_list_element  ------------------------------------------------
  ## create random named list element
  l <- list(
    a = list(x = 1:10),
    b = list(x = 1:10)

  )
  t <- expect_type(.get_named_list_element(l, element = "x"), type = "list")
  expect_equal(sum(unlist(t)), expected = 110)

  ## .throw_error() ---------------------------------------------------------
  fun.int <- function() .throw_error("Error message")
  fun.ext <- function() fun.int()
  expect_error(fun.int(),
               "[fun.int()] Error message", fixed = TRUE)
  expect_error(fun.ext(),
               "[fun.int()] Error message", fixed = TRUE)

  fun.int <- function() .throw_error("Error message", nframe = 2)
  fun.ext <- function() fun.int()
  expect_error(fun.ext(),
               "[fun.ext()] Error message", fixed = TRUE)

  ## .throw_warning() -------------------------------------------------------
  fun.int <- function() .throw_warning("Warning message")
  fun.ext <- function() fun.int()
  expect_warning(fun.int(),
                 "[fun.int()] Warning message", fixed = TRUE)
  expect_warning(fun.ext(),
                 "[fun.int()] Warning message", fixed = TRUE)

  fun.int <- function() .throw_warning("Warning message", nframe = 2)
  fun.ext <- function() fun.int()
  expect_warning(fun.ext(),
                 "[fun.ext()] Warning message", fixed = TRUE)

  ## SW() ------------------------------------------------------------------
  expect_silent(SW(cat("silenced message")))
  expect_silent(SW(message("silenced message")))
  expect_silent(SW(warning("silenced message")))
  expect_silent(SW(.throw_warning("silenced message")))
  expect_error(SW(stop("error message")),
               "error message")
  expect_error(SW(.throw_error("error message")),
               "error message")

  ## .validate_positive_scalar() --------------------------------------------
  expect_silent(.validate_positive_scalar(1.3))
  expect_silent(.validate_positive_scalar(2, int = TRUE))
  expect_silent(.validate_positive_scalar(NULL, int = TRUE, null.ok = TRUE))

  expect_error(.validate_positive_scalar(test <- "a"),
               "'test' must be a positive scalar")
  expect_error(.validate_positive_scalar(test <- NULL),
               "'test' must be a positive scalar")
  expect_error(.validate_positive_scalar(iris),
               "'iris' must be a positive scalar")
  expect_error(.validate_positive_scalar(1:2, name = "var"),
               "'var' must be a positive scalar")
  expect_error(.validate_positive_scalar(0, name = "var"),
               "'var' must be a positive scalar")
  expect_error(.validate_positive_scalar(-1, name = "var"),
               "'var' must be a positive scalar")
  expect_error(.validate_positive_scalar(1.5, int = TRUE, name = "var"),
               "'var' must be a positive integer")

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
