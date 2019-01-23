#'@title Emission Spectra Conversion from Wavelength to Energy Scales
#'
#'@description The function provides a convienent and fast way to convert a wavelength
#'to an energy scale. The function works on [RLum.Data.Spectrum-class], [data.frame] and [matrix] and
#'a [list] of such objects. The function was written to smooth the workflow while using emission
#'spectra data. This is in particular useful if you want to further treat your data and apply, e.g.,
#'a signal deconvolution.
#'
#'@details
#'
#' The intensity of the spectrum is re-calcualted using the
#' following approach (cf. Appendix 4 in Blasse and Grabmeier, 1994):
#'
#' \deqn{\phi_{E} = \phi_{\lambda} * \lambda^2 / (hc)}
#'
#' with
#' \eqn{\phi_{E}} the intensity per interval of energy \eqn{E} (1/eV),
#' \eqn{\phi_{\lambda}} the intensity per interval of wavelength \eqn{\lambda}
#' (1/nm) and
#' \eqn{h} (eV * s) the Planck constant and \eqn{c} (nm/s) the velocity of light.
#'
#' For transforming the wavelength axis (x-values) the equation as follow is used
#'
#' \deqn{E = hc/\lambda}
#'
#' @param object [RLum.Data.Spectrum-class], [data.frame], [matrix] (**required**): input object to be converted.
#' If the input is not an [RLum.Data.Spectrum-class], the first column is always treated as the wavelength
#' column. The function supports a list of allowed input objects.
#'
#' @return The same object class as provided as input is returned.
#'
#' @note This conversion works solely for emission spectra.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @seealso [RLum.Data.Spectrum-class], [plot_RLum]
#'
#' @keywords IO
#'
#' @references
#'
#' Blasse, G., Grabmaier, B.C., 1994. Luminescent Materials. Springer.
#'
#' Mooney, J., Kambhampati, P., 2013. Get the Basics Right: Jacobian Conversion of Wavelength and
#' Energy Scales for Quantitative Analysis of Emission Spectra. J. Phys. Chem. Lett. 4, 3316–3318.
#' \doi{10.1021/jz401508t}
#'
#' @examples
#'
#' ##create artifical dataset according to Mooney et al. (2013)
#' m <- matrix(
#'   data = c(seq(400, 800, 50), rep(1, 9)), ncol = 2)
#'
#'##set plot function
#'p <- function(m) {
#'  plot(x = m[, 1], y = m[, 2])
#'  polygon(x = c(m[, 1], rev(m[, 1])), y = c(m[, 2], rep(0, nrow(m))))
#'  for (i in 1:nrow(m)) {
#'   lines(x = rep(m[i, 1], 2), y = c(0, m[i, 2]))
#'  }
#'}
#' ##plot curves
#' par(mfrow = c(1,2))
#' p(m)
#' p(convert_Wavelength2Energy(m))
#'
#'@md
#'@export
convert_Wavelength2Energy <- function(
  object){


  # Self-call -----------------------------------------------------------------------------------
  if(class(object) == "list"){
    return(lapply(object, convert_Wavelength2Energy))

  }

  # Conversion function -------------------------------------------------------------------------

  ##this treats the matrix; in either caes and we play safe, means, we create in either case
  ##colnames and rownames, but remove them later depending on the input
  .conv_intensity <- function(m){
      h <- 4.135667662e-15 #eV * s
      c <- 299792458e+09 #nm/s

      ##convert count values
      m <- m * as.numeric(rownames(m))^2 / (h * c)

      ##modify rownames
      rownames(m) <- round((h * c) / as.numeric(rownames(m)),2)

      ##return results
      return(m)

  }


  # Treat input data ----------------------------------------------------------------------------
  if(class(object) == "RLum.Data.Spectrum"){
    object@data <- .conv_intensity(object@data)
    return(object)

  }else if(class(object) == "matrix" || class(object) == "data.frame"){
    temp <- as.matrix(object[,2:ncol(object)])

    ##set rownames
    rownames(temp) <- object[,1]

    ##convert values
    temp <- .conv_intensity(temp)

    ##construct new full matrix

    temp <- cbind(as.numeric(rownames(temp)), temp)
    rownames(temp) <- rownames(object)
    colnames(temp) <- colnames(object)

    ##return
    if(class(object) == "data.frame")
      return(as.data.frame(temp))

    return(temp)
  }else{
    stop(
      paste0(
        "[convert_Wavelength2Energy()] Class '",
        class(object)[1],
        "' not supported as input!"
      ),
      call. = FALSE
    )

  }

}
