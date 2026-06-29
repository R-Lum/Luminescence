#' @title Apply spectral efficiency correction to RLum.Data.Spectrum objects
#'
#' @description
#' The function applies spectral efficiency correction to
#' [Luminescence::RLum.Data.Spectrum-class] objects.
#'
#' @details
#' The efficiency correction is based on a spectral response dataset provided
#' by the user. Usually the data set for the quantum efficiency is of lower
#' resolution and values are interpolated for the required spectral resolution using
#' function [stats::approx].
#'
#' If the energy calibration differs for both data set `NA` values are produces that
#' will be removed from the matrix.
#'
#' @param object [Luminescence::RLum.Data.Spectrum-class] or [Luminescence::RLum.Analysis-class] (**required**):
#' object of class `RLum.Data.Spectrum`, `RLum.Analysis`or a [list] of such
#' objects. Other objects in the list are skipped.
#'
#' @param spectral.efficiency [data.frame] or [character] (**required**):
#' either a data frame with 2 columns containing wavelengths and relative
#' spectral response values (values between 0 and 1); or a character string
#' specifying the path to a CSV file (with a header row), which the function
#' will attempt to import via [utils::read.csv]. When `object` is a [list],
#' the provided data will be used to correct all spectra.
#'
#' @return Returns same object as provided as input
#'
#' @note
#' Please note that the spectral efficiency data from the camera alone may not
#' sufficiently correct for spectral efficiency of the entire optical system
#' (e.g., spectrometer, camera ...).
#'
#' @section Function version: 0.3.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @seealso [Luminescence::RLum.Data.Spectrum-class], [Luminescence::RLum.Analysis-class]
#'
#' @keywords manip
#'
#' @examples
#'
#' ##(1) - use with your own data (uncomment for usage)
#' ## spectral.efficiency <- read.csv("your data")
#' ##
#' ## your.spectrum <- apply_EfficiencyCorrection(your.spectrum, )
#'
#' @export
apply_EfficiencyCorrection <- function(
  object,
  spectral.efficiency
) {
  .set_function_name("apply_EfficiencyCorrection")
  on.exit(.unset_function_name(), add = TRUE)

  # self-call -----------------------------------------------------------------------------------

  .apply_to_spectra <- function(objects, spectral.efficiency, valid_classes) {
    lapply(objects, function(o) {
      if (!inherits(o, valid_classes)) {
        .throw_warning("Skipping '", class(o), "' object in input list")
        return(o)
      }
      apply_EfficiencyCorrection(o, spectral.efficiency)
    })
  }

  if (inherits(object, "list")) {
    return(.apply_to_spectra(object, spectral.efficiency,
                             c("RLum.Data.Spectrum", "RLum.Analysis")))
  }
  if (inherits(object, "RLum.Analysis")) {
    object@records <- .apply_to_spectra(object@records, spectral.efficiency,
                                        "RLum.Data.Spectrum")
    return(object)
  }

  ## Integrity checks -------------------------------------------------------
  .validate_class(object, "RLum.Data.Spectrum")
  .validate_class(spectral.efficiency, c("data.frame", "character"))
  .validate_not_empty(spectral.efficiency)
  if (inherits(spectral.efficiency, "data.frame")) {
    if (ncol(spectral.efficiency) < 2)
      .throw_error("'spectral.efficiency' should have 2 columns")
  } else {
    ## case of the file path
    .validate_length(spectral.efficiency, 1)
    if (!file.exists(spectral.efficiency))
      .throw_error("'spectral.efficiency' does not provide a valid file path")

    ## import
    spectral.efficiency <- read.csv(spectral.efficiency, header = TRUE)
  }

  ## remove missing values
  spectral.efficiency <- na.exclude(spectral.efficiency)
  if (nrow(spectral.efficiency) == 0)
    .throw_error("No valid data remains in 'spectral.efficiency' after ",
                 "removing NA values")

  ## convert to matrix
  temp.efficiency <- as.matrix(spectral.efficiency[,1:2])

  ##test max
  if(max(temp.efficiency[,2]) > 1)
    .throw_error("Relative quantum efficiency values > 1 are not allowed")

  # Apply method ------------------------------------------------------------
  ##the interpolation is needed to align the resolution
  #set data for interpolation
  temp.matrix <- as(object, "matrix")
  temp.efficiency.x <- as.numeric(row.names(temp.matrix))

  interpolated  <- approx(
    x = temp.efficiency[,1],
    y = temp.efficiency[,2],
    xout = temp.efficiency.x,
    ties = mean)

  if (all(is.na(interpolated$y))) {
    .throw_error("Interpolation failed: this happens when the x-values in ",
                 "'spectral.efficiency' are outside the range of the x-values ",
                 "in 'object'")
  }

  ##correct for quantum efficiency
  temp.matrix <- vapply(1:ncol(temp.matrix), FUN = function(x) {
    temp.matrix[, x] / interpolated$y * max(interpolated$y, na.rm = TRUE)
  }, FUN.VALUE = numeric(nrow(temp.matrix)))

  ##remove NA values
  temp.matrix <- na.exclude(temp.matrix)

  ##correct colnames
  colnames(temp.matrix) <- colnames(get_RLum(object))

  # Return Output------------------------------------------------------------
  set_RLum(
    class = "RLum.Data.Spectrum",
    recordType = object@recordType,
    curveType = object@curveType,
    data = temp.matrix,
    info = object@info)
}
