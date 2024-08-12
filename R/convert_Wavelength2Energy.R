#'@title Emission Spectra Conversion from Wavelength to Energy Scales (Jacobian Conversion)
#'
#'@description The function provides a convenient and fast way to convert emission spectra wavelength
#'to energy scales. The function works on [RLum.Data.Spectrum-class], [data.frame] and [matrix] and
#'a [list] of such objects. The function was written to smooth the workflow while analysing
#'emission spectra data. This is in particular useful if you want to further treat your data
#'and apply, e.g., a signal deconvolution.
#'
#'@details
#'
#' The intensity of the spectrum is re-calculated using the following approach to recalculate
#' wavelength and corresponding intensity values
#' (e.g., Appendix 4 in Blasse and Grabmeier, 1994; Mooney and Kambhampati, 2013):
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
#' @param digits [integer] (*with default*): set the number of digits on the returned energy axis
#'
#' @param order [logical] (*with default*): enables/disables sorting of the values in ascending energy
#' order. After the conversion the longest wavelength has the lowest energy value and the shortest
#' wavelength the highest. While this is correct, some R functions expect increasing x-values.
#'
#' @return The same object class as provided as input is returned.
#'
#' @note This conversion works solely for emission spectra. In case of absorption spectra only
#' the x-axis has to be converted.
#'
#' @section Function version: 0.1.1
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
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
#' Mooney, J., Kambhampati, P., 2013. Correction to “Get the Basics Right: Jacobian Conversion of
#' Wavelength and Energy Scales for Quantitative Analysis of Emission Spectra.” J. Phys. Chem. Lett. 4,
#' 3316–3318. \doi{10.1021/jz401508t}
#'
#' **Further reading**
#'
#' Angulo, G., Grampp, G., Rosspeintner, A., 2006. Recalling the appropriate representation of
#' electronic spectra. Spectrochimica Acta Part A: Molecular and Biomolecular Spectroscopy 65,
#' 727–731. \doi{10.1016/j.saa.2006.01.007}
#'
#' Wang, Y., Townsend, P.D., 2013. Potential problems in collection and data processing of
#' luminescence signals. Journal of Luminescence 142, 202–211. \doi{10.1016/j.jlumin.2013.03.052}
#'
#' @examples
#'
#' ##=====================##
#' ##(1) Literature example after Mooney et al. (2013)
#' ##(1.1) create matrix
#' m <- matrix(
#'   data = c(seq(400, 800, 50), rep(1, 9)), ncol = 2)
#'
#'##(1.2) set plot function to reproduce the
#'##literature figure
#'p <- function(m) {
#'  plot(x = m[, 1], y = m[, 2])
#'  polygon(
#'  x = c(m[, 1], rev(m[, 1])),
#'  y = c(m[, 2], rep(0, nrow(m))))
#'  for (i in 1:nrow(m)) {
#'   lines(x = rep(m[i, 1], 2), y = c(0, m[i, 2]))
#'  }
#'}
#'
#'##(1.3) plot curves
#' par(mfrow = c(1,2))
#' p(m)
#' p(convert_Wavelength2Energy(m))
#'
#'##=====================##
#'##(2) Another example using density curves
#' ##create dataset
#' xy <- density(
#'  c(rnorm(n = 100, mean = 500, sd = 20),
#'  rnorm(n = 100, mean = 800, sd = 20)))
#' xy <- data.frame(xy$x, xy$y)
#'
#' ##plot
#' par(mfrow = c(1,2))
#' plot(
#'  xy,
#'  type = "l",
#'  xlim = c(150, 1000),
#'  xlab = "Wavelength [nm]",
#'  ylab = "Luminescence [a.u.]"
#' )
#'plot(
#'  convert_Wavelength2Energy(xy),
#'  xy$y,
#'  type = "l",
#'  xlim = c(1.23, 8.3),
#'  xlab = "Energy [eV]",
#'  ylab = "Luminescence [a.u.]"
#' )
#'
#'@md
#'@export
convert_Wavelength2Energy <- function(
  object,
  digits = 3L,
  order = FALSE
  ){


  # Self-call -----------------------------------------------------------------------------------
  if(inherits(object, "list")){
    return(lapply(object, convert_Wavelength2Energy))

  }

  # Conversion function -------------------------------------------------------------------------

  ##this treats the matrix; in either caes and we play safe, means, we create in either case
  ##colnames and rownames, but remove them later depending on the input
  .conv_intensity <- function(m){
      h <- 4.135667662e-15 #eV * s
      c <- 299792458e+09 #nm/s

      ##convert count values
      m[] <- m * as.numeric(rownames(m))^2 / (h * c)

      ##modify rownames
      rownames(m) <- round((h * c) / as.numeric(rownames(m)),digits)

      ##return results
      return(m)

  }


  # Treat input data ----------------------------------------------------------------------------
  if(inherits(object, "RLum.Data.Spectrum")){
     ##check whether the object might have this scale already
    ##this only works on RLum.Data.Spectrum objects and is sugar for using RLum-objects
    if(any("curveDescripter" %in% names(object@info))){
     if(any(grepl(pattern = "energy", x = tolower(object@info$curveDescripter), fixed = TRUE))){
         message("[convert_Wavelength2Energy()] Your object has already an energy scale, nothing done!")
         return(object)
     }

    }


    ##convert data
    object@data <- .conv_intensity(object@data)

    #sort values if needed
    if(order){
      object@data <- object@data[order(as.numeric(rownames(object@data))), ,
                                 drop = FALSE]
      rownames(object@data) <- sort(as.numeric(rownames(object@data)))

    }

    ##correct $curveDescripter (we do not attach the table, otherwise the object gets too big)
    if(any("curveDescripter" %in% names(object@info))){
     temp_descripter <- strsplit(object@info$curveDescripter, ";", TRUE)[[1]]
     temp_descripter[grepl(x = temp_descripter,pattern = "wavelength", fixed = TRUE)] <- "energy [eV]"
      object@info$curveDescripter <- paste(temp_descripter, collapse = ";")
    }

    ##return new object
    return(object)

  }else if(inherits(object, "matrix") || inherits(object, "data.frame")){
    temp <- as.matrix(object[,2:ncol(object)])

    ##set rownames
    rownames(temp) <- object[,1]

    ##convert values
    temp <- .conv_intensity(temp)

    ##construct new full matrix
    temp <- cbind(as.numeric(rownames(temp)), temp)
    rownames(temp) <- rownames(object)
    colnames(temp) <- colnames(object)

    ##order on request (remember, here it is the first column)
    if(order) temp <- temp[order(temp[,1]),]

    ##return
    if(inherits(object, "data.frame"))
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
