#' @title Linearity Correction for Photomultiplier tubes (PMT)
#'
#' @description
#' Correct linearity of PMT counts to avoid saturation effects, depending
#' on pulse-pair-resolution of individual PMTs.
#'
#' @details
#' We correct for count linearity using a well-known formula that can be
#' found for example in the Hamamatsu Photomultiplier handbook
#' (Hamamatsu Photonics K.K., 2017):
#'
#' \deqn{N = \frac{M}{1 - M*t}}
#'
#' where \eqn{N} (in s\eqn{^{-1}}) is the true count rate, \eqn{M} (in s\eqn{^{-1}}) the measured count rate,
#' and \eqn{t} (in s) the pulse pair resolution.
#'
#' @param object [RLum.Analysis-class] [RLum.Data.Curve-class] (**required**):
#' object with records to correct.
#'
#' @param PMT_pulse_pair_resolution [numeric] (*with default*): pulse-pair resolution
#' in ns. Values can be found on the PMT datasheets. If `NULL` nothing is done.
#'
#' @returns
#' Returns the same type of object type as `object`.
#'
#' @note
#' This function is an adaptation of core from the R package
#' 'OSLdecomposition'.
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' Dirk Mittelstrass, Institute of Geography, Heidelberg University (Germany)
#'
#' @references
#'
#' Hamamatsu Photonics K.K., 2017. Photomultiplier Tubes: Basics and Applications, 4th edition. ed. Hamamatsu.
#'
#' Mittelstraß, D., Kreutzer, S., Schmidt, C., 2022. OSLdecomposition: Signal
#' component analysis for optically stimulated luminescence. \doi{10.32614/CRAN.package.OSLdecomposition}
#'
#' @examples
#' o <- set_RLum("RLum.Data.Curve")
#' correct_PMTLinearity(o, PMT_pulse_pair_resolution = 10)
#'
#' @keywords internal
#'
#' @md
#' @export
correct_PMTLinearity <- function(
    object,
    PMT_pulse_pair_resolution = NULL
) {
  ## set function name
  .set_function_name("correct_PMTLinearity")
  on.exit(.unset_function_name(), add = TRUE)

  ## input validation
  .validate_class(object, c("RLum.Analysis", "RLum.Data.Curve"))
  .validate_positive_scalar(PMT_pulse_pair_resolution, null.ok = TRUE)

  ## run only of parameter was set
  if (is.null(PMT_pulse_pair_resolution))
    return(object)

  ## extract records
  if (inherits(object, "RLum.Analysis"))
    records <- object@records
  else
    records <- object

  ## convert to seconds
  res <- PMT_pulse_pair_resolution[1] * 1e-9

  ## set threshold for correction (taken from OSLdecomposition)
  ## function OSLdecomposition::RLum.OSL_correction() (v1.0.0)
  thresh <- -0.5 * 0.5 + 0.5 * sqrt(0.5^2 + 2 / res)

  ## loop over records
  records <- lapply(records, \(x) {
    ## skip non-supported records
    ## no further test to keep the speed up
    if (!inherits(x, "RLum.Data.Curve"))
      return(x)

    ## access the slot
    rec <- x@data

    ## get channel width (we assume constant channel width)
    ## if only one channel set width to 1
    if (nrow(rec) >= 2)
      w <- rec[2, 1] - rec[1, 1]
    else
      w <- 1

    ## get cps
    cps <- rec[, 2, drop = FALSE] / w

    ## define threshold and tag
    idx <- cps > thresh

    ## correct if required
    idx <- cps > thresh
    if (any(idx)) {
      new_vals <- round(cps[idx] / (1 - cps[idx] * res) * w)
      rec[idx, 2] <- new_vals
      x@data <- rec
    }
    return(x)
  })

  ## extract records
  if (inherits(object, "RLum.Analysis"))
    object@records <- records

  ## return
  return(object)
}
