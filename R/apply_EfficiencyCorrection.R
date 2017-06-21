#' Function to apply spectral efficiency correction to RLum.Data.Spectrum S4
#' class objects
#'
#' The function allows spectral efficiency corrections for RLum.Data.Spectrum
#' S4 class objects
#'
#' The efficiency correction is based on a spectral response dataset provided
#' by the user. Usually the data set for the quantum efficiency is of lower
#' resolution and values are interpolated for the required spectral resolution using
#' the function \code{\link[stats]{approx}}
#'
#' If the energy calibration differes for both data set \code{NA} values are produces that
#' will be removed from the matrix.
#'
#' @param object \code{\linkS4class{RLum.Data.Spectrum}} (\bold{required}): S4
#' object of class \code{RLum.Data.Spectrum}
#'
#' @param spectral.efficiency \code{\link{data.frame}} (\bold{required}): Data
#' set containing wavelengths (x-column) and relative spectral response values
#' (y-column) in percentage
#'
#' @return Returns same object as input
#' (\code{\linkS4class{RLum.Data.Spectrum}})
#'
#' @note Please note that the spectral efficiency data from the camera alone may not
#' sufficiently correct for spectral efficiency of the entire optical system
#' (e.g., spectrometer, camera ...).
#'
#' @section Function version: 0.1.1
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France),\cr Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @seealso \code{\linkS4class{RLum.Data.Spectrum}}
#'
#' @references -
#'
#' @keywords manip
#'
#' @examples
#'
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
){

  # Integrity check -----------------------------------------------------------

  ##check if object is of class RLum.Data.Spectrum
  if(class(object) != "RLum.Data.Spectrum"){

    stop("[apply_EfficiencyCorrection()] Input object is not of type RLum.Data.Spectrum")

  }

  if(class(spectral.efficiency) != "data.frame"){

    stop("[apply_EfficiencyCorrection()] Input object is not of type data.frame")

  }

  ## grep data matrix
  temp.matrix <- as(object, "matrix")

  ## grep efficency values
  temp.efficiency <- as.matrix(spectral.efficiency)

  # Apply method ------------------------------------------------------------

  #set data for interpolation
  temp.efficiency.x <- as.numeric(row.names(temp.matrix))

  temp.efficiency.interpolated  <- approx(
    x = temp.efficiency[,1],
    y = temp.efficiency[,2],
    xout = temp.efficiency.x)


  ##correct for quantum efficiency
  temp.matrix <- vapply(X = 1:ncol(temp.matrix), FUN = function(x){
    temp.matrix[,x]/temp.efficiency.interpolated$y*max(temp.efficiency.interpolated$y, na.rm = TRUE)

  }, FUN.VALUE =  numeric(length = nrow(temp.matrix)))

  ##remove NA values
  temp.matrix <- na.exclude(temp.matrix)

  ##correct colnames
  colnames(temp.matrix) <- colnames(get_RLum(object))


  # Return Output------------------------------------------------------------

  temp.output <- set_RLum(
    class = "RLum.Data.Spectrum",
    recordType = object@recordType,
    curveType = object@curveType,
    data = temp.matrix,
    info = object@info)

  invisible(temp.output)

}
