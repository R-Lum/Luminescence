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

  ## .warningCatcher() ------------------------------------------------------
  expect_warning(Luminescence:::.warningCatcher(for(i in 1:5) {
                                                  warning("message 1")
                                                  if (i %% 2 == 0)
                                                    warning("message 2")
                                                  if (i %% 3 == 0)
                                                    warning("message 3")
                                                }),
                 paste(c("(1) message 1 [5 times]",
                         "(2) message 2 [2 times]",
                         "(3) message 3"), collapse = "\n"),
                 fixed = TRUE)
  expect_warning(Luminescence:::.warningCatcher(warning("single message")),
                 "single message")

  # .smoothing ----------------------------------------------------------------------------------
  expect_silent(Luminescence:::.smoothing(runif(100), k = 5, method = "median"))
  expect_silent(.smoothing(runif(200), method = "median"))
  expect_silent(.smoothing(runif(100), k = 4, method = "mean"))
  expect_silent(.smoothing(runif(100), k = 4, method = "median"))
  expect_equal(.smoothing(c(1, 1, 2, 50, 0, 2, 1, 2, 0, 1, 50),
                          method = "Carter"),
               c(1, 1, 2, 1, 0, 2, 1, 2, 0, 1, NA))
  expect_equal(.smoothing(c(1, 1, 2, 50, 0, 2, 1, 2, 0, 1, 50),
                          method = "Carter", fill = 0),
               c(1, 1, 2, 1, 0, 2, 1, 2, 0, 1, 0))
  expect_equal(.smoothing(c(1, 1, 2, 50, 0, 2, 1, 2, 0, 1, 50) * 10,
                          method = "Carter", p_acceptance = 1e-30),
               c(10, 10, 20, 17, 17, 20, 10, 20, 13, 10, NA))
  expect_error(.smoothing(c(1, 1, 2, 50, 0, 2, 1, 2, 0, 1, 50),
                          method = "Carter", p_acceptance = 0.5),
               "'p_acceptance' rejects all counts, set it to a smaller value")
  expect_error(.smoothing(runif(100), k = integer()),
               "'k' should be a single positive integer value or NULL")
  expect_error(.smoothing(runif(100), fill = numeric()),
               "'fill' should be of class 'numeric' or NA and have length 1")
  expect_error(.smoothing(runif(100), fill = matrix()),
               "'fill' should be of class 'numeric' or NA and have length 1")
  expect_error(.smoothing(runif(100), method = "error"),
               "'method' should be one of 'mean', 'median' or")
  expect_error(.smoothing(runif(100), align = "error"),
               "'align' should be one of 'right', 'center' or 'left'")
  expect_error(.smoothing(runif(100), p_acceptance = "error"),
               "'p_acceptance' should be a single positive value")

  ## .weighted.median() -----------------------------------------------------
  expect_equal(.weighted.median(1:10, w = rep(1, 10)),
               median(1:10))
  expect_equal(.weighted.median(1:5, w = c(0.15, 0.1, 0.2, 0.3, 0.25)),
               4)
  expect_equal(.weighted.median(c(1:5, NA), w = c(0.15, 0.1, 0.2, 0.3, 0.25, 1)),
               NA_real_)
  expect_equal(.weighted.median(c(1:5, NA), w = c(0.15, 0.1, 0.2, 0.3, 0.25, 1),
                                na.rm = TRUE),
               4)

  ## .normalise_curve() -----------------------------------------------------
  ## vector
  data <- runif(100)
  expect_equal(data, .normalise_curve(data, FALSE))
  expect_equal(.normalise_curve(data, TRUE), .normalise_curve(data, "max"))
  expect_silent(.normalise_curve(data, "min"))
  expect_silent(.normalise_curve(data, "first"))
  expect_silent(.normalise_curve(data, "last"))
  expect_silent(.normalise_curve(data, "huot"))
  expect_silent(.normalise_curve(data, "intensity"))
  expect_silent(.normalise_curve(data, 2.2))

  ## check for matrix
  m <- matrix(runif(10), ncol = 2)
  expect_equal(m, .normalise_curve(m, FALSE))
  expect_equal(.normalise_curve(m, TRUE), .normalise_curve(m, "max"))
  expect_silent(.normalise_curve(m, "min"))
  expect_silent(.normalise_curve(m, "first"))
  expect_true(is.matrix(.normalise_curve(m, "last")))
  expect_silent(.normalise_curve(m, "huot"))
  expect_silent(.normalise_curve(m, "intensity"))
  expect_silent(.normalise_curve(m, 2.2))

  data[100] <- 0
  expect_warning(.normalise_curve(data, "last"),
                 "Curve normalisation produced Inf/NaN values, values replaced")
  expect_warning(.normalise_curve(data, "min"),
                 "Curve normalisation produced Inf/NaN values, values replaced")

  data[99] <- NA_real_
  ## this will produce warnings, but still normalise, because
  ## we use na.rm internally for max(), min() etc.
  expect_equal(
    suppressWarnings(.normalise_curve(data, TRUE)),
    suppressWarnings(.normalise_curve(data, "max")))
  expect_warning(.normalise_curve(data, "last"),
                 "Curve normalisation produced Inf/NaN values, values replaced")
  expect_warning(.normalise_curve(data, "huot"),
                 "Curve normalisation produced Inf/NaN values, values replaced")
  expect_warning(.normalise_curve(data, "min"),
                 "Curve normalisation produced Inf/NaN values, values replaced")
  expect_warning(.normalise_curve(data, 2),
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
  expect_equal(Luminescence:::.create_StatisticalSummaryText(),
               "")
  expect_equal(.create_StatisticalSummaryText(stats, keywords = ""),
               "")
  expect_equal(.create_StatisticalSummaryText(stats, keywords = "nonexisting"),
               "")
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

  ## regression test for issue 907
  expect_length(Luminescence:::.rm_nonRLum(list(matrix(iris)), "RLum.Analysis"),
                0)

  # .rm_NULL_elements() -----------------------------------------------------------
  expect_type(.rm_NULL_elements(list("a", NULL)),
    "list")
  t <- expect_type(.rm_NULL_elements(list(NULL, NULL)),
              "list")
  expect_length(t, 0)

  # .rm_unnamed_elements() --------------------------------------------------
  expect_equal(.rm_unnamed_elements(list(a = "a", b = "b")),
               list(a = "a", b = "b"))
  expect_equal(.rm_unnamed_elements(list(a = "a", b = NULL, 3, 4, c = 5)),
               list(a = "a", b = NULL, c = 5))
  expect_null(.rm_unnamed_elements(list("a", NULL)))
  expect_null(.rm_unnamed_elements(list()))

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
  expect_null(.download_file(url = "https://"))
  expect_null(.download_file(url = "_https://github.com"))

  ## attempts download but fails
  url.404 <- "https://raw.githubusercontent.com/R-Lum/rxylib/master/inst/extg"
  expect_message(
          expect_message(expect_equal(.download_file(url = url.404), NA),
                         "Downloading"),
      "FAILED")

  ## attempts download and succeeds
  url.ok <- "https://raw.githubusercontent.com/R-Lum/rxylib/master/codecov.yml"
  destfile <- tempfile()
  SW({
  expect_message(expect_equal(.download_file(url = url.ok, destfile),
                              destfile),
                 "OK")
  expect_message(.download_file(url = paste("  ", url.ok, "\n")),
                 "OK")
  })

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

  ## .throw_message() -------------------------------------------------------
  fun.int <- function(error = TRUE) {
    .set_function_name("fun.int")
    on.exit(.unset_function_name(), add = TRUE)
    .throw_message("Simple message", error = error)
  }
  fun.ext <- function(error = TRUE) fun.int(error)
  fun.docall <- function(error = TRUE) do.call(fun.ext, args = list(error = error))
  fun.docall_do <- function(error = TRUE) fun.docall(error)
  expect_message(fun.int(),
                 "[fun.int()] Error: Simple message", fixed = TRUE)
  expect_message(fun.ext(),
                 "[fun.int()] Error: Simple message", fixed = TRUE)
  expect_message(fun.docall(),
                 "[fun.int()] Error: Simple message", fixed = TRUE)
  expect_message(fun.docall_do(),
                 "[fun.int()] Error: Simple message", fixed = TRUE)
  expect_message(fun.int(error = FALSE),
                 "[fun.int()] Simple message", fixed = TRUE)
  expect_message(fun.ext(error = FALSE),
                 "[fun.int()] Simple message", fixed = TRUE)
  expect_message(fun.docall(error = FALSE),
                 "[fun.int()] Simple message", fixed = TRUE)
  expect_message(fun.docall_do(error = FALSE),
                 "[fun.int()] Simple message", fixed = TRUE)

  ## .deprecated() ----------------------------------------------------------
  expect_warning(.deprecated("old", "new", "1.0"),
                 "'old' was deprecated in v1.0, use 'new' instead")
  expect_warning(.deprecated(c("old1", "old2"), c("new1", "new2"), "1.0"),
                 "'old1' and 'old2' were deprecated in v1.0, use 'new1' and 'new2' instead")

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
    .validate_args(arg, c("val1", "val2", "val3"), name = "'other_name'")
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
    .validate_class(arg, "data.frame", null.ok = TRUE)
  }
  fun3 <- function(arg) {
    .validate_class(arg, "data.frame", throw.error = FALSE)
  }
  expect_true(fun1(iris))
  expect_true(fun2(NULL))
  expect_true(.validate_class(iris, c("data.frame", "integer")))
  expect_true(.validate_class(iris, c("data.frame", "integer"),
                              throw.error = FALSE))
  expect_true(.validate_class(1:3, "integer", length = c(2, 3)))
  expect_warning(expect_false(.validate_class(arg <- NULL, "data.frame",
                                   throw.error = FALSE)),
      "'arg' should be of class 'data.frame'")
  expect_error(fun1(),
               "'arg' should be of class 'data.frame'")
  expect_error(fun2(),
               "'arg' should be of class 'data.frame' or NULL")
  expect_error(fun1(NULL),
               "'arg' should be of class 'data.frame'")
  expect_error(.validate_class(test <- 1:5),
               "is missing, with no default")
  expect_error(.validate_class(test <- 1:5, "data.frame"),
               "'test' should be of class 'data.frame'")
  expect_error(.validate_class(test <- 1:5, "data.frame", length = 2),
               "'test' should be of class 'data.frame' and have length 2")
  expect_error(.validate_class(test <- 1:5, "data.frame", length = 1:2),
               "'test' should be of class 'data.frame' and have length 1 or 2")
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
  expect_warning(fun3(),
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

  ## .validate_scalar() -----------------------------------------------------
  expect_equal(.validate_scalar(1.3),
               1.3)
  expect_equal(.validate_scalar(-2, int = TRUE),
               -2)
  expect_null(.validate_scalar(NULL, int = TRUE, null.ok = TRUE))

  expect_error(.validate_scalar(int = TRUE),
               "'NA' should be a single integer value")
  expect_error(.validate_scalar(test <- "a"),
               "'test' should be a single value")
  expect_error(.validate_scalar(test <- NULL),
               "'test' should be a single value")
  expect_error(.validate_scalar(iris),
               "'iris' should be a single value")
  expect_error(.validate_scalar(iris[, 1, drop = FALSE]),
               "'iris' should be a single value")
  expect_error(.validate_scalar(iris[1, 0, drop = FALSE]),
               "'iris' should be a single value")
  expect_error(.validate_scalar(iris, null.ok = TRUE),
               "'iris' should be a single value or NULL")
  expect_error(.validate_scalar(iris, extra = "something else"),
               "'iris' should be a single value or something else")
  expect_error(.validate_scalar(iris, extra = "something else", null.ok = TRUE),
               "'iris' should be a single value or something else or NULL")
  expect_error(.validate_scalar(array(1, c(1, 1, 0))),
               "'NA' should be a single value")
  expect_error(.validate_scalar(-1:2, name = "'var'"),
               "'var' should be a single value")
  expect_error(.validate_scalar(Inf, int = TRUE, name = "'var'"),
               "'var' should be a single integer value")
  expect_error(.validate_scalar(1.5, int = TRUE, name = "'var'"),
               "'var' should be a single integer value")
  expect_error(.validate_scalar(NA, int = TRUE, name = "The variable"),
               "The variable should be a single integer value")
  expect_error(.validate_scalar(-1:2, pos = TRUE, name = "'var'"),
               "'var' should be a single positive value")
  expect_error(.validate_scalar(-1:2, int = TRUE, pos = TRUE, name = "'var'"),
               "'var' should be a single positive integer value")

  ## .validate_positive_scalar() --------------------------------------------
  expect_equal(.validate_positive_scalar(1.3),
               1.3)
  expect_equal(.validate_positive_scalar(2, int = TRUE),
               2)
  expect_null(.validate_positive_scalar(NULL, int = TRUE, null.ok = TRUE))

  expect_error(.validate_positive_scalar(int = TRUE),
               "'NA' should be a single positive integer value")
  expect_error(.validate_positive_scalar(test <- "a"),
               "'test' should be a single positive value")
  expect_error(.validate_positive_scalar(test <- NULL),
               "'test' should be a single positive value")
  expect_error(.validate_positive_scalar(iris),
               "'iris' should be a single positive value")
  expect_error(.validate_positive_scalar(iris, null.ok = TRUE),
               "'iris' should be a single positive value or NULL")
  expect_error(.validate_positive_scalar(iris, extra = "something else"),
               "'iris' should be a single positive value or something else")
  expect_error(.validate_positive_scalar(iris, extra = "something else", null.ok = TRUE),
               "'iris' should be a single positive value or something else or NULL")
  expect_error(.validate_positive_scalar(1:2, name = "'var'"),
               "'var' should be a single positive value")
  expect_error(.validate_positive_scalar(0, name = "'var'"),
               "'var' should be a single positive value")
  expect_error(.validate_positive_scalar(-1, name = "'var'"),
               "'var' should be a single positive value")
  expect_error(.validate_positive_scalar(Inf, int = TRUE, name = "'var'"),
               "'var' should be a single positive integer value")
  expect_error(.validate_positive_scalar(1.5, int = TRUE, name = "'var'"),
               "'var' should be a single positive integer value")
  expect_error(.validate_positive_scalar(NA, int = TRUE, name = "The variable"),
               "The variable should be a single positive integer value")

  ## .validate_nonnegative_scalar() -----------------------------------------
  expect_equal(.validate_nonnegative_scalar(0),
               0)
  expect_equal(.validate_nonnegative_scalar(2, int = TRUE),
               2)
  expect_null(.validate_nonnegative_scalar(NULL, int = TRUE, null.ok = TRUE))

  expect_error(.validate_nonnegative_scalar(int = TRUE),
               "'NA' should be a single non-negative integer value")
  expect_error(.validate_nonnegative_scalar(test <- "a"),
               "'test' should be a single non-negative value")
  expect_error(.validate_nonnegative_scalar(test <- NULL),
               "'test' should be a single non-negative value")
  expect_error(.validate_nonnegative_scalar(iris),
               "'iris' should be a single non-negative value")
  expect_error(.validate_nonnegative_scalar(iris, null.ok = TRUE),
               "'iris' should be a single non-negative value or NULL")
  expect_error(.validate_nonnegative_scalar(iris, extra = "something else"),
               "'iris' should be a single non-negative value or something else")
  expect_error(.validate_nonnegative_scalar(iris, extra = "something else", null.ok = TRUE),
               "'iris' should be a single non-negative value or something else or NULL")
  expect_error(.validate_nonnegative_scalar(1:2, name = "'var'"),
               "'var' should be a single non-negative value")
  expect_error(.validate_nonnegative_scalar(-1, name = "'var'"),
               "'var' should be a single non-negative value")
  expect_error(.validate_nonnegative_scalar(Inf, int = TRUE, name = "'var'"),
               "'var' should be a single non-negative integer value")
  expect_error(.validate_nonnegative_scalar(1.5, int = TRUE, name = "'var'"),
               "'var' should be a single non-negative integer value")
  expect_error(.validate_nonnegative_scalar(NA, int = TRUE, name = "The variable"),
               "The variable should be a single non-negative integer value")

  ## .validate_logical_scalar() ---------------------------------------------
  expect_equal(.validate_logical_scalar(TRUE),
               TRUE)
  expect_equal(.validate_logical_scalar(FALSE),
               FALSE)
  expect_null(.validate_logical_scalar(NULL, null.ok = TRUE))

  expect_error(.validate_logical_scalar(),
               "'NA' should be a single logical value")
  expect_error(.validate_logical_scalar(test <- "a"),
               "'test' should be a single logical value")
  expect_error(.validate_logical_scalar(test <- NULL),
               "'test' should be a single logical value")
  expect_error(.validate_logical_scalar(iris),
               "'iris' should be a single logical value")
  expect_error(.validate_logical_scalar(iris, null.ok = TRUE),
               "'iris' should be a single logical value or NULL")
  expect_error(.validate_logical_scalar(iris, extra = "something else"),
               "'iris' should be a single logical value or something else")
  expect_error(.validate_logical_scalar(iris, extra = "something else", null.ok = TRUE),
               "'iris' should be a single logical value or something else or NULL")
  expect_error(.validate_logical_scalar(c(TRUE, FALSE), name = "'var'"),
               "'var' should be a single logical value")
  expect_error(.validate_logical_scalar(0, name = "'var'"),
               "'var' should be a single logical value")
  expect_error(.validate_logical_scalar(NA, name = "The variable"),
               "The variable should be a single logical value")

  ## .validate_originator() ----------------------------------------------------
  expect_error(.validate_originator(set_RLum("RLum.Analysis"), "orig"),
               "'NA' has an unsupported originator (expected 'orig', but found",
               fixed = TRUE)
  expect_equal(.validate_originator(set_RLum("RLum.Analysis", originator = "orig"),
                                    "orig"),
               "orig")

  ## .check_originator() ----------------------------------------------------
  expect_true(.check_originator(set_RLum("RLum.Analysis", originator = "orig"),
                                "orig"))
  expect_false(.check_originator(set_RLum("RLum.Analysis", originator = "orig"),
                                 c("orig1", "orig2")))
  expect_false(.check_originator(set_RLum("RLum.Analysis"), "orig"))
  expect_false(.check_originator(NULL, "orig"))
  expect_false(.check_originator(iris, "orig"))

  ## .validate_integral() ----------------------------------------------------
  expect_null(.validate_integral(NULL, null.ok = TRUE))
  expect_equal(.validate_integral(integral <- 5:1),
               1:5)
  expect_equal(.validate_integral(integral <- NA, na.ok = TRUE),
               NA)
  expect_error(.validate_integral(integral <- 1:5 + 0.1, int = FALSE),
               integral)
  expect_warning(expect_equal(.validate_integral(integral <- c(5:1, -3:3)),
                              1:5),
                 "'integral' out of bounds, reset to be between 1 and 5")
  expect_warning(expect_equal(.validate_integral(integral <- 1:100,
                                                 min = 5, max = 50),
                              5:50),
                 "'integral' out of bounds, reset to be between 5 and 50")
  expect_error(.validate_integral(integral <- "error"),
               "'integral' should be of class 'integer' or 'numeric'")
  expect_error(.validate_integral(integral <- list(NA), na.ok = TRUE),
               "'integral' should be of class 'integer', 'numeric' or NA")
  expect_error(.validate_integral(integral <- NA, na.ok = FALSE),
               "'integral' should be of class 'integer' or 'numeric'")
  expect_error(.validate_integral(integral <- -9:0),
               "'integral' is of length 0 after removing values smaller than 1$")
  expect_error(.validate_integral(integral <- 1:10, min = 50, max = 100),
               "after removing values smaller than 50 and greater than 100")
  expect_error(.validate_integral(integral <- 1:10, min = 150, max = 100),
               "is expected to be at least 150, but the maximum allowed is 100")
  expect_error(.validate_integral(integral <- 1:5 + 0.1),
               "'integral' should be a vector of integers")
  expect_error(.validate_integral(list.integral <- list(1:4)),
               "'list.integral' should be of class 'integer' or 'numeric'")
  expect_error(.validate_integral(list.integral <- list("error"), list.ok = TRUE),
               "All elements of 'list.integral' should be of class 'integer' or")
  expect_equal(.validate_integral(list.integral <- list(5:1), list.ok = TRUE),
               list(1:5))
  expect_warning(.validate_integral(integral <- c(1, 3)),
                 "'integral' was defined as c(1, 3) but in general we would",
                 fixed = TRUE)
  expect_no_warning(.validate_integral(integral <- c(1, 2)),
                    message = "'integral' was defined as c(1, 2) but in general")

  ## .convert_to_channels() -------------------------------------------------
  data(ExampleData.BINfileData, envir = environment())
  tl <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, run = 1)@records[[1]]
  expect_equal(.convert_to_channels(tl, 0:20),
               1:11)
  expect_equal(.convert_to_channels(tl, 200:220),
               111:122)
  signal_integral <- c(0, 1.5)
  expect_warning(expect_equal(.convert_to_channels(tl, signal_integral,
                                                   unit = "temperature"),
                              1),
                 "Conversion of 'signal_integral' from temperature to channels failed")
  signal_integral <- c(1000, 2000)
  expect_warning(expect_equal(.convert_to_channels(tl, signal_integral,
                                                   unit = "temperature"),
                              250),
                 "Conversion of 'signal_integral' from temperature to channels failed")

  ## .validate_file() -------------------------------------------------------

  dir.path <- system.file("extdata", package = "Luminescence")
  expect_equal(.validate_file(file <- c("file1", "file2")),
               list("file1", "file2"))
  expect_equal(.validate_file(file <- list("file1")),
               list("file1"))
  expect_message(expect_equal(.validate_file(dir.path),
                              as.list(dir(dir.path, full.names = TRUE))),
                 "Directory detected, looking for any files")
  expect_silent(.validate_file(dir.path, verbose = FALSE))
  expect_message(expect_equal(basename(.validate_file(dir.path, pattern = "xsyg")),
                              "XSYG_file.xsyg"),
                 "Directory detected, looking for 'xsyg' files")
  expect_message(expect_message(
      expect_length(.validate_file(dir.path, pattern = "_none_"), 0),
      "Directory detected, looking for '_none_' files"),
      "No files matching the given pattern found")

  url <- "https://raw.githubusercontent.com/R-Lum/rxylib/master"
  expect_message(expect_message(.validate_file(file.path(url, "_pkgdown.yml")),
                                "Downloading"), "OK")
  expect_message(expect_message(.validate_file(file.path(url, "_error_")),
                                "Downloading"), "FAILED")

  expect_error(.validate_file(file <- TRUE),
               "'file' should be of class 'character' or 'list'")
  expect_error(.validate_file(file <- list(TRUE)),
               "All elements of 'file' should be of class 'character' and have length 1")
  expect_error(.validate_file("_error_"),
               "File '.*_error_' does not exist") # windows CI needs the regexp
  expect_message(expect_null(.validate_file("_error_", throw.error = FALSE)),
                 "File '.*_error_' does not exist") # windows CI needs the regexp
  expect_error(.validate_file(dir.path, scan.dir = FALSE),
               "File '.*' does not exist")
  file.create(zero <- tempfile(pattern = "zero", fileext = ".binx"))
  expect_error(.validate_file(zero),
               "is a zero-byte file")
  expect_message(expect_null(.validate_file(zero, throw.error = FALSE)),
                             "is a zero-byte file")
  expect_error(.validate_file(test_path("test_read_BIN2R.R"), ext = "e1"),
               "File extension 'R' is not supported, only 'e1' is valid")
  expect_error(.validate_file(test_path("test_read_BIN2R.R"), ext = c("e1", "e2")),
               "File extension 'R' is not supported, only 'e1' and 'e2' are valid")
  expect_message(expect_null(.validate_file(test_path("test_read_BIN2R.R"),
                                            ext = c("e1", "e2", "e3"), throw.error = FALSE)),
                 "File extension 'R' is not supported, only 'e1', 'e2' and 'e3'")

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


  ## .strict_na() -----------------------------------------------------------
  expect_true(.strict_na(NA))
  expect_true(.strict_na(NA_real_))
  expect_false(.strict_na(NULL))
  expect_false(.strict_na(c(1, NA)))
  expect_false(.strict_na(c(NA, NA)))
  expect_false(.strict_na(matrix()))
  expect_false(.strict_na(set_RLum("RLum.Data.Curve")))

  ## .collapse() ------------------------------------------------------------
  expect_equal(.collapse(1:3),
               "'1', '2', '3'")
  expect_equal(.collapse(1:3, quote = FALSE),
               "1, 2, 3")
  expect_equal(.collapse(1:2, quote = FALSE, last_sep = " or "),
               "1 or 2")
  expect_equal(.collapse(1:3, quote = FALSE, last_sep = " or "),
               "1, 2 or 3")
  expect_equal(.collapse(as.factor(c("060920", "070920", "080920", "090920")),
                         quote = FALSE),
               "060920, 070920, 080920, 090920")
  expect_equal(.collapse(NULL), "")

  ## .format_range() ________________________________________________________
  expect_equal(.format_range(1:10),
               "1:10")
  expect_equal(.format_range(c(1, 10, NA, 8)),
               "1:10")
  expect_equal(.format_range(c(-1, -10, NA, -8)),
               "-10:-1")
  expect_equal(.format_range(NULL),
               "NA:NA")
  expect_equal(.format_range(c(NA, NA, NA)),
               "NA:NA")
  expect_equal(.format_range(c(0, 1, -1), sep = ", "),
               "-1, 1")
  expect_equal(.format_range(c(0.53, 1.39, 1.14)),
               "0.53:1.39")
  expect_equal(.format_range(c(0.53, 1.39, 1.14), nsmall = 3),
               "0.530:1.390")

  ## .shorten_filename() ----------------------------------------------------
  expect_equal(.shorten_filename("/path/to/filename"),
               "/path/to/filename")
  max.width <- 27
  shortened <- .shorten_filename("/path/to/a_somewhat_longer_filename",
                                 max.width = max.width)
  expect_equal(shortened,
               "/path/to/a_s...ger_filename")
  expect_equal(nchar(shortened), max.width)

  ## check vector
  max.width <- 10
  shortened <- .shorten_filename(c("short", NA, "muchmuchlonger"), 10)
  expect_equal(shortened,
               c("short", NA, "muc...nger"))
  expect_equal(nchar(shortened), c(5, NA, max.width))

  ## .rescale() ----------------------------------------------------
  x <- stats::rnorm(100)
  y_dens <- stats::density(x)
  expect_equal(max(.rescale(
   x = y_dens$y,
   range_old = c(min(y_dens$y), max(y_dens$y)),
   range_new = c(0, 20))), 20)

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
