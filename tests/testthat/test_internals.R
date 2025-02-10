test_that("Test internals", {
  testthat::skip_on_cran()

  ## since below we are testing internal functions that use `.throw_error()`
  ## and `.throw_warning()`, we need to treat this block as a function
  ## definition and set the function name
  .set_function_name("test")
  on.exit(.unset_function_name(), add = TRUE)

  # .expand_parameters() ------------------------------------------------------
  ##create empty function ... reminder
  ##this is an internal function, the first object is always discarded, it
  ##might be a list of RLum.Analysis objects is might be super large
  f <- function(object, a, b = 1, c = list(), d = NULL) {
    Luminescence:::.expand_parameters(len = 3)

  }

  ##test some functions
  ##missing arguments must be identified
  expect_error(f(), "Argument 'a' missing, with no default")

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
  expect_silent(.smoothing(runif(200), method = "median"))
  expect_silent(.smoothing(runif(100), k = 4, method = "mean"))
  expect_silent(.smoothing(runif(100), k = 4, method = "median"))
  expect_error(.smoothing(runif(100), method = "error"),
               "'method' should be one of 'mean' or 'median'")
  expect_error(.smoothing(runif(100), align = "error"),
               "'align' should be one of 'right', 'center' or 'left'")

  ## .normalise_curve() -----------------------------------------------------
  data <- runif(100)
  expect_equal(data, .normalise_curve(data, FALSE))
  expect_equal(.normalise_curve(data, TRUE), .normalise_curve(data, "max"))
  expect_silent(.normalise_curve(data, "last"))
  expect_silent(.normalise_curve(data, "huot"))

  data[100] <- 0
  expect_warning(.normalise_curve(data, "last"),
                 "Curve normalisation produced Inf/NaN values, values replaced")

  # fancy_scientific ()--------------------------------------------------------------------------
  plot(seq(1e10, 1e20, length.out = 10),1:10, xaxt = "n")
  expect_silent(axis(1, at = axTicks(1),labels = Luminescence:::fancy_scientific(axTicks(1))))

  # .add_fancy_log_axis() -----------------------------------------------------
  y <- c(0.1, 0.001, 0.0001)
  plot(1:length(y), y, yaxt = "n", log = "y")
  expect_silent(Luminescence:::.add_fancy_log_axis(side = 2, las = 1))
  expect_null(.add_fancy_log_axis(side = 1, las = 1))

  # .get_keyword_coordinates() ----------------------------------------------
  xlim <- c(0, 5)
  ylim <- c(2, 10)
  coords <- .get_keyword_coordinates(xlim = xlim, ylim = ylim)
  expect_type(coords, "list")
  expect_named(coords, c("pos", "adj"))
  expect_length(coords, 2)
  expect_equal(coords$pos,
               c(0, 10))
  expect_equal(.get_keyword_coordinates(pos = c(1, 2), xlim, ylim)$pos,
               c(1, 2))
  expect_equal(.get_keyword_coordinates(pos = "topleft", xlim, ylim)$pos,
               c(0, 10))
  expect_equal(.get_keyword_coordinates(pos = "top", xlim, ylim)$pos,
               c(2.5, 10))
  expect_equal(.get_keyword_coordinates(pos = "topright", xlim, ylim)$pos,
               c(5, 10))
  expect_equal(.get_keyword_coordinates(pos = "left", xlim, ylim)$pos,
               c(0, 6))
  expect_equal(.get_keyword_coordinates(pos = "center", xlim, ylim)$pos,
               c(2.5, 6))
  expect_equal(.get_keyword_coordinates(pos = "right", xlim, ylim)$pos,
               c(5, 6))
  expect_equal(.get_keyword_coordinates(pos = "bottomleft", xlim, ylim)$pos,
               c(0, 2))
  expect_equal(.get_keyword_coordinates(pos = "bottom", xlim, ylim)$pos,
               c(2.5, 2))
  expect_equal(.get_keyword_coordinates(pos = "bottomright", xlim, ylim)$pos,
               c(5, 2))

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

  # .rm_NULL_elements() -----------------------------------------------------------
  expect_type(.rm_NULL_elements(list("a", NULL)),
    "list")
  t <- expect_type(.rm_NULL_elements(list(NULL, NULL)),
              "list")
  expect_length(t, 0)

  # .matrix_binning() ---------------------------------------------------------------------------
  m <- matrix(data = c(rep(1:20, each = 20)), ncol = 20, nrow = 20)
  rownames(m) <- 1:nrow(m)
  colnames(m) <- 1:ncol(m)

    ##crash the function
    expect_error(Luminescence:::.matrix_binning("none matrix"),
                 "'m' should be of class 'matrix'")

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
  fun.int <- function() {
    .set_function_name("fun.int")
    on.exit(.unset_function_name(), add = TRUE)
    .throw_error("Error message")
  }
  fun.ext <- function() fun.int()
  fun.docall <- function() do.call(fun.ext, args = list())
  fun.docall_do <- function() fun.docall()
  expect_error(fun.int(),
               "[fun.int()] Error message", fixed = TRUE)
  expect_error(fun.ext(),
               "[fun.int()] Error message", fixed = TRUE)
  expect_error(fun.docall(),
               "[fun.int()] Error message", fixed = TRUE)
  expect_error(fun.docall_do(),
               "[fun.int()] Error message", fixed = TRUE)

  ## .throw_warning() -------------------------------------------------------
  fun.int <- function() {
    .set_function_name("fun.int")
    on.exit(.unset_function_name(), add = TRUE)
    .throw_warning("Warning message")
  }
  fun.ext <- function() fun.int()
  fun.docall <- function() do.call(fun.ext, args = list())
  fun.docall_do <- function() fun.docall()
  expect_warning(fun.int(),
                 "[fun.int()] Warning message", fixed = TRUE)
  expect_warning(fun.ext(),
                 "[fun.int()] Warning message", fixed = TRUE)
  expect_warning(fun.docall(),
                 "[fun.int()] Warning message", fixed = TRUE)
  expect_warning(fun.docall_do(),
                 "[fun.int()] Warning message", fixed = TRUE)

  ## SW() ------------------------------------------------------------------
  expect_silent(SW(cat("silenced message")))
  expect_silent(SW(message("silenced message")))
  expect_silent(SW(warning("silenced message")))
  expect_silent(SW(.throw_warning("silenced message")))
  expect_error(SW(stop("error message")),
               "error message")
  expect_error(SW(.throw_error("error message")),
               "error message")

  ## .validate_args() -------------------------------------------------------
  fun1 <- function(arg) {
    .validate_args(arg, c("val1", "val2", "val3"), null.ok = TRUE)
  }
  expect_silent(fun1(NULL))
  expect_equal(fun1(arg = "val1"), "val1")
  expect_equal(fun1(arg = c("val1", "val2")), "val1")
  expect_equal(fun1(arg = c("val3", "val2")), "val3")
  expect_error(fun1(arg = c("error", "val1")),
               "[test()] 'arg' contains multiple values but not all of them",
               fixed = TRUE)
  expect_error(fun1(arg = "error"),
               "[test()] 'arg' should be one of 'val1', 'val2', 'val3' or NULL",
               fixed = TRUE)

  fun2 <- function(arg = c("val1", "val2", "val3")) {
    .validate_args(arg, c("val1", "val2", "val3"), name = "other_name")
  }
  expect_equal(fun2(), "val1")
  expect_error(fun2(arg = NULL),
               "[test()] 'other_name' should be one of 'val1', 'val2' or 'val3'",
               fixed = TRUE)
  expect_error(fun2(arg = "error"),
               "[test()] 'other_name' should be one of 'val1', 'val2' or 'val3'",
               fixed = TRUE)

  fun3 <- function(arg) {
    .validate_args(arg, c("val1", "val2"),
                   extra = "'other.val'", null.ok = FALSE)
  }
  expect_error(fun3(arg = "error"),
               "[test()] 'arg' should be one of 'val1', 'val2' or 'other.val'",
               fixed = TRUE)

  fun4 <- function(arg) {
    .validate_args(arg, c("val1", "val2"),
                   extra = "'other.val'", null.ok = TRUE)
  }
  expect_error(fun4(arg = "error"),
               "[test()] 'arg' should be one of 'val1', 'val2', 'other.val' or NULL",
               fixed = TRUE)

  fun.err <- function(arg) {
    .validate_args(arg)
  }
  expect_error(fun.err("val1"),
               "is missing, with no default")

  ## .validate_class() ------------------------------------------------------
  fun1 <- function(arg) {
    .validate_class(arg, "data.frame")
  }
  fun2 <- function(arg) {
    .validate_class(arg, "data.frame", throw.error = FALSE)
  }
  expect_true(fun1(iris))
  expect_true(.validate_class(iris, c("data.frame", "integer")))
  expect_true(.validate_class(iris, c("data.frame", "integer"),
                              throw.error = FALSE))
  expect_warning(expect_false(.validate_class(arg <- NULL, "data.frame",
                                   throw.error = FALSE)),
      "'arg' should be of class 'data.frame'")
  expect_error(fun1(),
               "'arg' should be of class 'data.frame'")
  expect_error(fun1(NULL),
               "'arg' should be of class 'data.frame'")
  expect_error(.validate_class(test <- 1:5),
               "is missing, with no default")
  expect_error(.validate_class(test <- 1:5, "data.frame"),
               "'test' should be of class 'data.frame'")
  expect_error(.validate_class(test <- 1:5, c("list", "data.frame", "numeric")),
               "'test' should be of class 'list', 'data.frame' or 'numeric'")
  expect_error(.validate_class(test <- 1:5, c("list", "data.frame")),
               "'test' should be of class 'list' or 'data.frame'")
  expect_error(.validate_class(test <- 1:5, c("list", "data.frame"),
                               extra = "another type"),
               "'test' should be of class 'list', 'data.frame' or another")
  expect_error(.validate_class(test <- 1:5, c("list", "data.frame"),
                               name = "'other_name'"),
               "'other_name' should be of class 'list' or 'data.frame'")
  expect_warning(fun2(),
                 "'arg' should be of class 'data.frame'")

  ## .validate_not_empty() --------------------------------------------------
  expect_true(.validate_not_empty(letters, "vector"))

  expect_error(.validate_not_empty(test <- c(), "vector"),
               "'test' cannot be an empty vector")
  expect_error(.validate_not_empty(test <- list()),
               "'test' cannot be an empty list")
  expect_error(.validate_not_empty(test <- numeric(0)),
               "'test' cannot be an empty numeric")
  expect_error(.validate_not_empty(test <- data.frame()),
               "'test' cannot be an empty data.frame")
  expect_error(.validate_not_empty(iris[0, ]),
               "'iris' cannot be an empty data.frame")
  expect_error(.validate_not_empty(iris[, 0]),
               "'iris' cannot be an empty data.frame")
  expect_error(.validate_not_empty(test <- matrix(NA, 0, 5)),
               "'test' cannot be an empty matrix")
  expect_error(.validate_not_empty(test <- matrix(NA, 5, 0)),
               "'test' cannot be an empty matrix")
  expect_error(.validate_not_empty(test <- set_RLum("RLum.Analysis")),
               "'test' cannot be an empty RLum.Analysis")
  expect_error(.validate_not_empty(list(), "list", name = "'other_name'"),
               "'other_name' cannot be an empty list")
  expect_warning(expect_false(.validate_not_empty(test <- list(), "list",
                                                  throw.error = FALSE)),
                 "'test' cannot be an empty list")

  ## .validate_length() -----------------------------------------------------
  expect_true(.validate_length(letters, 26))
  expect_error(.validate_length(letters),
               "is missing, with no default")
  expect_error(.validate_length(letters, 25),
               "'letters' should have length 25")
  expect_error(.validate_length(letters, 25, name = "'other_name'"),
               "'other_name' should have length 25")
  expect_warning(expect_false(.validate_length(letters, 25, throw.error = FALSE)),
                 "'letters' should have length 25")

  ## .validate_positive_scalar() --------------------------------------------
  expect_silent(.validate_positive_scalar(int = TRUE))
  expect_silent(.validate_positive_scalar(1.3))
  expect_silent(.validate_positive_scalar(2, int = TRUE))
  expect_silent(.validate_positive_scalar(NULL, int = TRUE, null.ok = TRUE))

  expect_error(.validate_positive_scalar(test <- "a"),
               "'test' should be a positive scalar")
  expect_error(.validate_positive_scalar(test <- NULL),
               "'test' should be a positive scalar")
  expect_error(.validate_positive_scalar(iris),
               "'iris' should be a positive scalar")
  expect_error(.validate_positive_scalar(1:2, name = "'var'"),
               "'var' should be a positive scalar")
  expect_error(.validate_positive_scalar(0, name = "'var'"),
               "'var' should be a positive scalar")
  expect_error(.validate_positive_scalar(-1, name = "'var'"),
               "'var' should be a positive scalar")
  expect_error(.validate_positive_scalar(1.5, int = TRUE, name = "'var'"),
               "'var' should be a positive integer")
  expect_error(.validate_positive_scalar(NA, int = TRUE, name = "The variable"),
               "The variable should be a positive integer")

  ## .validate_logical_scalar() ---------------------------------------------
  expect_silent(.validate_logical_scalar())
  expect_silent(.validate_logical_scalar(TRUE))
  expect_silent(.validate_logical_scalar(FALSE))
  expect_silent(.validate_logical_scalar(NULL, null.ok = TRUE))

  expect_error(.validate_logical_scalar(test <- "a"),
               "'test' should be a single logical value")
  expect_error(.validate_logical_scalar(test <- NULL),
               "'test' should be a single logical value")
  expect_error(.validate_logical_scalar(iris),
               "'iris' should be a single logical value")
  expect_error(.validate_logical_scalar(c(TRUE, FALSE), name = "'var'"),
               "'var' should be a single logical value")
  expect_error(.validate_logical_scalar(0, name = "'var'"),
               "'var' should be a single logical value")
  expect_error(.validate_logical_scalar(NA, name = "The variable"),
               "The variable should be a single logical value")

  ## .require_suggested_package() -------------------------------------------
  expect_true(.require_suggested_package("utils"))
  expect_error(.require_suggested_package("error"),
               "This function requires the 'error' package: to install it")
  expect_error(.require_suggested_package("error",
                                          reason = "Reporting a good error"),
               "Reporting a good error requires the 'error' package")
  expect_warning(
      expect_false(.require_suggested_package("error", throw.error = FALSE),
                   "This function requires the 'error' package: to install it"))

  ## .listify() -------------------------------------------------------------
  expect_equal(.listify(1, length = 3),
               list(1, 1, 1))
  expect_equal(.listify(letters, length = 5),
               .listify(list(letters), length = 5))


  ## .collapse() ------------------------------------------------------------
  expect_equal(.collapse(1:3),
               "'1', '2', '3'")
  expect_equal(.collapse(1:3, quote = FALSE),
               "1, 2, 3")
  expect_equal(.collapse(NULL), "")

  ## .shorten_filename() ----------------------------------------------------
  expect_equal(.shorten_filename("/path/to/filename"),
               "/path/to/filename")
  expect_equal(.shorten_filename("/path/to/a_somewhat_longer_filename",
                                 max.width = 27),
               "/path/…what_longer_filename")


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
  ## cover NA case
  expect_type(
    Luminescence:::src_create_RLumDataCurve_matrix(
      DATA = 1:100,
      VERSION = 4,
      NPOINTS = 0,
      LTYPE = "OSL",
      LOW = 0,
      HIGH = 500,
      AN_TEMP = 0,
      TOLDELAY = 0,
      TOLON = 0,
      TOLOFF = 0), type = "double")

  ## case for a delayed hit ramp start
  expect_type(
    Luminescence:::src_create_RLumDataCurve_matrix(
      DATA = 1:100,
      VERSION = 4,
      NPOINTS = 0,
      LTYPE = "OSL",
      LOW = 0,
      HIGH = 500,
      AN_TEMP = 0,
      TOLDELAY = 10,
      TOLON = 0,
      TOLOFF = 0), type = "double")
  ## case for a delayed hit ramp start
  expect_type(
    Luminescence:::src_create_RLumDataCurve_matrix(
      DATA = 1:100,
      VERSION = 4,
      NPOINTS = 0,
      LTYPE = "OSL",
      LOW = 0,
      HIGH = 500,
      AN_TEMP = 100,
      TOLDELAY = 0,
      TOLON = 0,
      TOLOFF = 0), type = "double")
  ## generate strange curve and more tests
  expect_type(Luminescence:::src_create_RLumDataCurve_matrix(
    DATA = 1:100,
    VERSION = 4,
    NPOINTS = 100,
    LTYPE = "TL",
    LOW = 0,
    HIGH = 500,
    AN_TEMP = 200,
    TOLDELAY = 10,
    TOLON = 10,
    TOLOFF = 400),  type = "double")
})
