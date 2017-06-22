#' Analyse portable CW-OSL measurements
#'
#' The function analyses CW-OSL curve data produced by a SUERC portable OSL reader and
#' produces a combined plot of OSL/IRSL signal intensities, OSL/IRSL depletion ratios
#' and the IRSL/OSL ratio.
#'
#' This function only works with `RLum.Analysis` objects produced by [read_PSL2R].
#' It further assumes (or rather requires) an equal amount of OSL and IRSL curves that
#' are pairwise combined for calculating the IRSL/OSL ratio. For calculating the depletion ratios
#' the cumulative signal of the last n channels (same number of channels as specified
#' by `signal.integral`) is divided by cumulative signal of the first n channels (`signal.integral`).
#'
#' @param object [RLum.Analysis-class] (**required**):
#' `RLum.Analysis` object produced by [read_PSL2R].
#'
#' @param signal.integral [vector] (**required**): 
#' A vector of two values specifying the lower and upper channel used to 
#' calculate the OSL/IRSL signal. Can be provided in form of `c(1, 5)` or `1:5`.
#'
#'
#' @param invert [logical] (*with default*): 
#' `TRUE` to calculate and plot the data in reverse order.
#'
#' @param normalise [logical] (*with default*):
#' `TRUE` to normalise the OSL/IRSL signals by the mean of all corresponding
#' data curves.
#'
#' @param plot [logical] (*with default*): 
#' enable/disable plot output
#'
#' @param ... currently not used.
#'
#' @return 
#' Returns an S4 [RLum.Results-class] object.
#'
#' @seealso [RLum.Analysis-class], [RLum.Data.Curve-class]
#'
#' @author Christoph Burow, University of Cologne (Germany)
#'
#' @section Function version: 0.0.3
#'
#' @keywords datagen plot
#'
#' @examples
#'
#' # (1) load example data set
#' data("ExampleData.portableOSL", envir = environment())
#'
#' # (2) merge and plot all RLum.Analysis objects
#' merged <- merge_RLum(ExampleData.portableOSL)
#' plot_RLum(merged, combine = TRUE)
#' merged
#'
#' # (3) analyse and plot
#' results <- analyse_portableOSL(merged, signal.integral = 1:5, invert = FALSE, normalise = TRUE)
#' get_RLum(results)
#'
#'
#'
#' @md
#' @export
analyse_portableOSL <-
  function(object,
           signal.integral,
           invert = FALSE,
           normalise = FALSE,
           plot = TRUE,
           ...)
  {

  ## INPUT VERIFICATION ----
  if (!inherits(object, "RLum.Analysis"))
    stop("Only objects of class 'RLum.Analysis' are allowed.", call. = FALSE)
  if (!all(sapply(object, class) == "RLum.Data.Curve"))
    stop("The 'RLum.Analysis' object must only contain objects of class 'RLum.Data.Curve'.", call. = FALSE)
  if (!all(sapply(object, function(x) x@originator) == "read_PSL2R"))
    stop("Only objects originating from 'read_PSL2R()' are allowed.", call. = FALSE)

  if (missing(signal.integral)) {
    signal.integral <- c(1, 1)
    warning("No value for 'signal.integral' provided. Only the first data point of each curve was used!",
            call. = FALSE)
  }

  ## CALCULATIONS ----

  # OSL
  OSL <- get_RLum(object, recordType = "OSL")
  OSL <- do.call(rbind, lapply(OSL, function(x) {
    posl_get_signal(x, signal.integral)
  }))

  # IRSL
  IRSL <- get_RLum(object, recordType = "IRSL")
  IRSL <- do.call(rbind, lapply(IRSL, function(x) {
    posl_get_signal(x, signal.integral)
  }))


  ## NORMALISE ----
  if (normalise) {
    OSL <- posl_normalise(OSL)
    IRSL <- posl_normalise(IRSL)
  }

  ## INVERT ----
  if (invert) {
    OSL <- posl_invert(OSL)
    IRSL <- posl_invert(IRSL)
  }

  # OSL/IRSL Ratio
  RATIO <- IRSL$sum_signal / OSL$sum_signal

  ## PLOTTING ----
  if (plot) {
    par.old.full <- par(no.readonly = TRUE)
    on.exit(par(par.old.full))

    # default: par(mar = c(5, 4, 4, 2) + 0.1) // bottom, left, top, right
    par(mfrow = c(1, 6))

    par(mar = c(5, 4, 4, 1) + 0.1)

    frame()

    par(mar = c(5, 0, 4, 1) + 0.1)

    plot(
      OSL$sum_signal,
      1:nrow(OSL),
      type = "b",
      pch = 16,
      col = "blue",
      xlim = range(pretty(OSL$sum_signal)),
      xlab = "BSL",
      ylab = "Index",
      bty = "n",
      yaxt = "n"
    )
    axis(2, line = 3, at = 1:nrow(OSL))
    axis(3)
    mtext("Index", side = 2, line = 6)

    plot(
      IRSL$sum_signal,
      1:nrow(IRSL),
      type = "b",
      pch = 16,
      col = "red",
      xlim = range(pretty(IRSL$sum_signal)),
      xlab = "IRSL",
      ylab = "",
      bty = "n",
      yaxt = "n"
    )
    axis(3)

    plot(
      OSL$sum_signal_depletion,
      1:nrow(OSL),
      type = "b",
      pch = 1,
      col = "blue",
      xlim = range(pretty(OSL$sum_signal_depletion)),
      xlab = "BSL depl.",
      ylab = "",
      bty = "n",
      yaxt = "n",
      lty = 2
    )
    axis(3)

    plot(
      IRSL$sum_signal_depletion,
      1:nrow(IRSL),
      type = "b",
      pch = 1,
      col = "red",
      xlim = range(pretty(IRSL$sum_signal_depletion)),
      xlab = "IRSL depl.",
      ylab = "",
      bty = "n",
      yaxt = "n",
      lty = 2
    )
    axis(3)

    plot(
      RATIO,
      1:length(RATIO),
      type = "b",
      pch = 16,
      col = "black",
      xlim = range(pretty(RATIO)),
      xlab = "IRSL/BSL",
      ylab = "",
      bty = "n",
      yaxt = "n"
    )
    axis(3)
  }

  ## RETURN VALUE ----
  call<- sys.call()
  args <- as.list(call)[2:length(call)]
  summary <- data.frame(BSL = OSL$sum_signal,
                        BSL_error = OSL$sum_signal_err,
                        IRSL = IRSL$sum_signal,
                        IRSL_error = IRSL$sum_signal_err,
                        BSL_depletion = OSL$sum_signal_depletion,
                        IRSL_depletion = IRSL$sum_signal_depletion,
                        IRSL_BSL_RATIO = RATIO)


  newRLumResults <- set_RLum(
    class = "RLum.Results",
    data = list(
      summary=summary,
      data = object,
      args=args
    ),
    info = list(call = call))

  return(newRLumResults)

}

################################################################################
##                              HELPER FUNCTIONS                              ##
################################################################################

## This extracts the relevant curve data information of the RLum.Data.Curve
## objects
posl_get_signal <- function(x, signal.integral) {
    raw_signal <- get_RLum(x)[,2]
    sigint <- range(signal.integral)
    if (sigint[2] > length(raw_signal)) {
      sigint[2] <- length(raw_signal)
      warning("'signal.integral' (", paste(range(signal.integral), collapse = ", "),") ",
              "exceeded the number of available data points (n = ", length(raw_signal),") and ",
              "has been automatically reduced to the maximum number.", call. = FALSE)
    }
    sum_signal <- sum(raw_signal[sigint[1]:sigint[2]])
    sum_signal_err <- sqrt(sum(x@info$raw_data$counts_per_cycle_error[sigint[1]:sigint[2]]^2))
    sum_signal_depletion <- sum(raw_signal[(length(raw_signal)-length(sigint[1]:sigint[2])):length(raw_signal)]) / sum_signal
    return(data.frame(sum_signal, sum_signal_err, sum_signal_depletion))
}

## This function normalises the data curve by the mean signal
posl_normalise <- function(x) {
  rel.error <- x$sum_signal_err / x$sum_signal
  x$sum_signal <- x$sum_signal / mean(x$sum_signal)
  x$sum_signal_err <- x$sum_signal * rel.error
  x$sum_signal_depletion <- x$sum_signal_depletion / mean(x$sum_signal_depletion)
  return(x)
}

## This function invertes the data.frame (useful when the sample are in inverse
## stratigraphic order)
posl_invert <- function(x) {
  x <- x[nrow(x):1, ]
}
