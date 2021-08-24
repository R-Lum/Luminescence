#' Function to apply spectral efficiency correction to RLum.Data.Spectrum S4
#' class objects
#'
#' The function allows spectral efficiency corrections for RLum.Data.Spectrum
#' S4 class objects
#'
#' The efficiency correction is based on a spectral response dataset provided
#' by the user. Usually the data set for the quantum efficiency is of lower
#' resolution and values are interpolated for the required spectral resolution using
#' the function [stats::approx][stats::approxfun]
#'
#' If the energy calibration differs for both data set `NA` values are produces that
#' will be removed from the matrix.
#'
#' @param object [RLum.Data.Spectrum-class] or [RLum.Analysis-class] (**required**):
#' S4 object of class `RLum.Data.Spectrum`,  `RLum.Analysis`or a [list] of such objects. Other objects in
#' the list are skipped.
#'
#' @param spectral.efficiency [data.frame] (**required**):
#' Data set containing wavelengths (x-column) and relative spectral response values
#' (y-column) (values between 0 and 1). The provided data will be used to correct all spectra if `object` is
#' a [list]
#'
#' @return Returns same object as provided as input
#'
#' @note
#' Please note that the spectral efficiency data from the camera alone may not
#' sufficiently correct for spectral efficiency of the entire optical system
#' (e.g., spectrometer, camera ...).
#'
#' @section Function version: 0.2.0
#'
#' @author
#' Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France)\cr
#' Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @seealso [RLum.Data.Spectrum-class], [RLum.Analysis-class]
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
#' @md
#' @export
apply_EfficiencyCorrection <- function(
  object,
  spectral.efficiency
){


  # self-call -----------------------------------------------------------------------------------

  ##case we have a list
  if(class(object) == "list"){
    output_list <- lapply(object, function(o){
      if(class(o) == "RLum.Data.Spectrum" || class(o) == "RLum.Analysis"){
        apply_EfficiencyCorrection(object = o, spectral.efficiency = spectral.efficiency)

      }else{
        warning(paste0("[apply_EfficiencyCorrection()] Skipping ",class(o)," object in input list."), call. = FALSE)
        return(o)
      }

    })

    return(output_list)

  }

  ##the case of an RLum.Analysis object
  if(class(object) == "RLum.Analysis"){
    object@records <- lapply(object@records, function(o){
      if(class(o) == "RLum.Data.Spectrum"){
        apply_EfficiencyCorrection(object = o, spectral.efficiency = spectral.efficiency)

      }else{
        warning(paste0("[apply_EfficiencyCorrection()] Skipping ",class(o)," object in input list."), call. = FALSE)
        return(o)
      }

    })

    return(object)

  }



  # Integrity check -----------------------------------------------------------

  ##check if object is of class RLum.Data.Spectrum
  if(class(object) != "RLum.Data.Spectrum")
    stop("[apply_EfficiencyCorrection()] Input object is not of type RLum.Data.Spectrum",call. = FALSE)


  if(class(spectral.efficiency) != "data.frame")
    stop("[apply_EfficiencyCorrection()] 'spectral.efficiency' is not of type data.frame", call. = FALSE)


  ## grep data matrix from the input object
  temp.matrix <- as(object, "matrix")

  ## grep efficency values
  temp.efficiency <- as.matrix(spectral.efficiency[,1:2])

  ##test max
  if(max(temp.efficiency[,2]) > 1)
    stop("[apply_EfficiencyCorrection()] Relative quantum efficiency values > 1 are not allowed.", call. = FALSE)

  # Apply method ------------------------------------------------------------

  ##the interpolation is needed to align the resolution
  #set data for interpolation
  temp.efficiency.x <- as.numeric(row.names(temp.matrix))

  temp.efficiency.interpolated  <- approx(
    x = temp.efficiency[,1],
    y = temp.efficiency[,2],
    xout = temp.efficiency.x,
    ties = mean)


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
