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
#' @param spectral.efficiency [data.frame] (**required**):
#' data frame with 2 columns containing wavelengths and relative spectral
#' response values (values between 0 and 1). The provided data will be used to
#' correct all spectra if `object` is a [list].
#'
#' @return Returns same object as provided as input
#'
#' @note
#' Please note that the spectral efficiency data from the camera alone may not
#' sufficiently correct for spectral efficiency of the entire optical system
#' (e.g., spectrometer, camera ...).
#'
#' @section Function version: 0.2.1
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

  ##case we have a list
  if(inherits(object, "list")){
    output_list <- lapply(object, function(o){
      if (!inherits(o, c("RLum.Data.Spectrum", "RLum.Analysis"))) {
        .throw_warning("Skipping '", class(o), "' object in input list")
        return(o)
      }
      apply_EfficiencyCorrection(o, spectral.efficiency)
    })

    return(output_list)
  }

  ##the case of an RLum.Analysis object
  if(inherits(object, "RLum.Analysis")){
    object@records <- lapply(object@records, function(o){
      if (!inherits(o, "RLum.Data.Spectrum")) {
        .throw_warning("Skipping '", class(o), "' object in input list")
        return(o)
      }
      apply_EfficiencyCorrection(o, spectral.efficiency)
    })

    return(object)
  }

  ## Integrity checks -------------------------------------------------------
  .validate_class(object, "RLum.Data.Spectrum")
  .validate_class(spectral.efficiency, "data.frame")
  .validate_not_empty(spectral.efficiency)
  if (ncol(spectral.efficiency) < 2)
    .throw_error("'spectral.efficiency' should have 2 columns")

  ## grep data matrix from the input object
  temp.matrix <- as(object, "matrix")

  ## remove missing values
  spectral.efficiency <- na.exclude(spectral.efficiency)
  if (nrow(spectral.efficiency) == 0)
    .throw_error("No valid data remains in 'spectral.efficiency' after ",
                 "removing NA values")

  ## grep efficency values
  temp.efficiency <- as.matrix(spectral.efficiency[,1:2])

  ##test max
  if(max(temp.efficiency[,2]) > 1)
    .throw_error("Relative quantum efficiency values > 1 are not allowed")

  # Apply method ------------------------------------------------------------

  ##the interpolation is needed to align the resolution
  #set data for interpolation
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
