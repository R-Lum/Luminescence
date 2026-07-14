#' @title Function to calculate statistic measures
#'
#' @description This function calculates a number of descriptive statistics for estimates
#' with a given standard error (SE), most fundamentally using error-weighted approaches.
#'
#' @details The option to use Monte Carlo Methods (`n.MCM`) allows calculating
#' all descriptive statistics based on random values. The distribution of these
#' random values is based on the Normal distribution with `De` values as
#' means and `De_error` values as one standard deviation. Increasing the
#' number of MCM-samples linearly increases computation time. On a Lenovo X230
#' machine evaluation of 25 Aliquots with n.MCM = 1000 takes 0.01 s, with
#' n = 100000, ca. 1.65 s. It might be useful to work with logarithms of these
#' values. See Dietze et al. (2016, Quaternary Geochronology) and the function
#' [Luminescence::plot_AbanicoPlot] for details.
#'
#' @param data [data.frame] or [Luminescence::RLum.Results-class] object (**required**):
#' for [data.frame] two columns: De (`data[, 1]`) and De error (`data[, 2]`).
#'
#' @param weight.calc [character] (*with default*):
#' type of weight calculation. One out of `"reciprocal"` (weight is 1/error),
#' `"square"` (weight is 1/error^2). Default is `"square"`.
#'
#' @param digits [integer] (*with default*):
#' number of decimal places to be used when rounding numbers. If set to `NULL`
#' (default), no rounding occurs.
#'
#' @param n.MCM [numeric] (*with default*):
#' number of samples drawn for Monte Carlo-based statistics.
#' `NULL` (the default) disables MC runs.
#'
#' @param na.rm [logical] (*with default*):
#' indicating whether `NA` values should be stripped before the computation proceeds.
#'
#' @return Returns a list with weighted and unweighted statistic measures.
#'
#' @section Function version: 0.1.7
#'
#' @keywords datagen
#'
#' @author Michael Dietze, GFZ Potsdam (Germany)
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ## show a rough plot of the data to illustrate the non-normal distribution
#' plot_KDE(ExampleData.DeValues$BT998)
#'
#' ## calculate statistics and show output
#' str(calc_Statistics(ExampleData.DeValues$BT998))
#'
#' \dontrun{
#' ## now the same for 10000 normal distributed random numbers with equal errors
#' x <- as.data.frame(cbind(rnorm(n = 10^5, mean = 0, sd = 1),
#'                          rep(0.001, 10^5)))
#'
#' ## note the congruent results for weighted and unweighted measures
#' str(calc_Statistics(x))
#' }
#'
#' @export
calc_Statistics <- function(
  data,
  weight.calc = c("square", "reciprocal"),
  digits = NULL,
  n.MCM = NULL,
  na.rm = TRUE
) {
  .set_function_name("calc_Statistics")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(data, c("RLum.Results", "data.frame"))
  .validate_not_empty(data)
  if (inherits(data, "RLum.Results")) {
    data <- get_RLum(data, "data")[, 1:2]
  }

  ##strip na values
  .validate_logical_scalar(na.rm)
  if(na.rm){
    data <- na.exclude(data)
    if (nrow(data) == 0) {
      .throw_error("'data' contains only NA values")
    }
  }

  ## handle error-free data sets
  if(ncol(data) == 1) {
    data <- cbind(data, rep(NA, length(data)))
  }

  ## replace Na values in error by 0
  data[is.na(data[,2]),2] <- 0

  ## replace zeros by a small value
  if (any(data[, 2] == 0)) {
    if (sum(data[, 2]) == 0) {
      .throw_warning("All errors are NA or zero, automatically set to 10^-9")
    }
    data[,2] <- rep(x = 10^-9, length(data[,2]))
  }

  weight.calc <- .validate_args(weight.calc, c("square", "reciprocal"))
  if(weight.calc == "reciprocal") {
    S.weights <- 1 / data[,2]
  } else if(weight.calc == "square") {
    S.weights <- 1 / data[,2]^2
  }

  S.weights <- S.weights / sum(S.weights)

  .validate_positive_scalar(digits, int = TRUE, null.ok = TRUE)
  .validate_positive_scalar(n.MCM, int = TRUE, null.ok = TRUE)

  ## create MCM data
  if (is.null(n.MCM)) {
    data.MCM <- cbind(data[, 1])
  } else {
    data.MCM <-
      matrix(data = rnorm(
        n = n.MCM * nrow(data),
        mean = data[, 1],
        sd = data[, 2]
      ),
      ncol = n.MCM)
  }

  S.n <- nrow(data)

  ## unweighted statistics
  u.mean <- mean(data[, 1], na.rm = na.rm)
  u.median <- median(data[, 1], na.rm = na.rm)
  u.sd <- sd(data[, 1], na.rm = na.rm)
  u.skewness <- 1 / S.n * sum(((data[, 1] - u.mean) / u.sd)^3)
  u.kurtosis <- 1 / S.n * sum(((data[, 1] - u.mean) / u.sd)^4)

  ## weighted statistics
  w.mean <- stats::weighted.mean(data[, 1], w = S.weights, n.rm = na.rm)
  w.median <- .weighted.median(data[, 1], w = S.weights, na.rm = na.rm)
  w.sd <- sqrt(sum(S.weights * (data[,1] - w.mean)^2) /
               (((S.n - 1) * sum(S.weights)) / S.n))

  ## MCM statistics
  m.mean <- mean(data.MCM, na.rm = na.rm)
  m.median <- median(data.MCM, na.rm = na.rm)
  m.sd <- sd(data.MCM, na.rm = na.rm)
  m.n <- S.n * ncol(data.MCM)
  m.skewness <- 1 / m.n * sum(((data.MCM - m.mean) / m.sd)^3)
  m.kurtosis <- 1 / m.n * sum(((data.MCM - m.mean) / m.sd)^4)

  ## build result lists -----------------------------------------------------
  .build_list <- function(n, mean, median, sd, skewness, kurtosis) {
    list(n = n, mean = mean, median = median,
         sd.abs = sd,
         sd.rel = sd / mean * 100,
         se.abs = sd / sqrt(n),
         se.rel = sd / sqrt(n) / mean * 100,
         skewness = skewness, kurtosis = kurtosis)
  }

  results <- list(
    weighted   = .build_list(S.n, w.mean, w.median, w.sd, u.skewness, u.kurtosis),
    unweighted = .build_list(S.n, u.mean, u.median, u.sd, u.skewness, u.kurtosis),
    MCM        = .build_list(S.n, m.mean, m.median, m.sd, m.skewness, m.kurtosis)
  )

  if (!is.null(digits))
    results <- lapply(results, function(x) lapply(x, round, digits = digits))

  results
}
