#' @title Plot function for an RLum.Data.Curve S4 class object
#'
#' @description The function provides a standardised plot output for curve data of an
#' `RLum.Data.Curve` S4-class object.
#'
#' @details Only single curve data can be plotted with this function.Arguments
#' according to [plot].
#'
#' **Curve normalisation**
#'
#' The argument `norm` normalises all count values, to date the following
#' options are supported:
#'
#' `norm = TRUE` or `norm = "max"`: Curve values are normalised to the highest
#' count value in the curve
#'
#' `norm = "last"`: Curves values are normalised to the last count value
#' (this can be useful in particular for radiofluorescence curves)
#'
#' `norm = "huot"`: Curve values are normalised as suggested by Sébastien Huot
#'  via GitHub:
#' \deqn{
#' y = (observed - median(background)) / (max(observed) - median(background))
#' }
#'
#' The background of the curve is defined as the last 20 % of the count values
#' of a curve.
#'
#' @param object [RLum.Data.Curve-class] (**required**):
#' S4 object of class `RLum.Data.Curve`
#'
#' @param par.local [logical] (*with default*):
#' use local graphical parameters for plotting, e.g. the plot is shown in one
#' column and one row. If `par.local = FALSE`, global parameters are inherited.
#'
#' @param norm [logical] [character] (*with default*): allows curve normalisation to the
#' highest count value ('default'). Alternatively, the function offers the
#' modes `"max"`, `"min"` and `"huot"` for a background corrected normalisation, see details.
#'
#' @param smooth [logical] (*with default*):
#' provides an automatic curve smoothing based on [zoo::rollmean]
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to the `plot` function
#'
#' @return Returns a plot.
#'
#' @note Not all arguments of [plot] will be passed!
#'
#' @section Function version: 0.2.5
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @seealso [plot], [plot_RLum]
#'
#' @keywords aplot
#'
#' @examples
#'
#' ##plot curve data
#'
#' #load Example data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' #transform data.frame to RLum.Data.Curve object
#' temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")
#'
#' #plot RLum.Data.Curve object
#' plot_RLum.Data.Curve(temp)
#'
#'
#' @md
#' @export
plot_RLum.Data.Curve<- function(
  object,
  par.local = TRUE,
  norm = FALSE,
  smooth = FALSE,
  ...
){

# Integrity check -------------------------------------------------------------

  ##check if object is of class RLum.Data.Curve
  if(!inherits(object, "RLum.Data.Curve"))
    stop("[plot_RLum.Data.Curve()] Input object is not of type RLum.Data.Curve", call. = FALSE)

  ## check for NA values
  if(all(is.na(object@data))){
    warning("[plot_RLum.Data.Curve()] Curve contains only NA-values, nothing plotted.", call. = FALSE)
    return(NULL)

  }


# Preset plot -------------------------------------------------------------

    ## preset
    lab.unit <- "Unknown"
    lab.xlab <- "Independent"
    xlab.xsyg <- ylab.xsyg <- NA

    ##set labelling unit
    if(!is.na(object@recordType)){
      if(object@recordType[1] %in% c("OSL", "IRSL", "RL", "RF", "LM-OSL", "RBR")){
        lab.unit <- "s"
        lab.xlab <- "Stimulation time"

      } else if(object@recordType[1] == "TL") {
        lab.unit <- "\u00B0C"
        lab.xlab <- "Temperature"

      }
    }

    ##XSYG
    ##check for curveDescripter
    if ("curveDescripter" %in% names(object@info)) {
      temp.lab <-
        strsplit(object@info$curveDescripter,
                 split = ";",
                 fixed = TRUE)[[1]]

      xlab.xsyg <- temp.lab[1]
      ylab.xsyg <- temp.lab[2]

    }

    ##normalise curves if argument has been set
    if(norm[1] %in% c('max', 'last', 'huot') || norm[1] == TRUE){
      if (norm[1] == "max" || norm[1] == TRUE) {
        object@data[,2] <- object@data[,2] / max(object@data[,2])

      } else if (norm[1] == "last") {
        object@data[,2] <- object@data[,2] / object@data[nrow(object@data),2]

      } else if (norm[1] == "huot") {
        bg <- median(object@data[floor(nrow(object@data)*0.8):nrow(object@data),2])
        object@data[,2] <-  (object@data[,2] - bg) / max(object@data[,2] - bg)

      }

      ##check for Inf and NA
      if(any(is.infinite(object@data[,2])) || anyNA(object@data[,2])){
        object@data[,2][is.infinite(object@data[,2]) | is.na(object@data[,2])] <- 0
        warning("[plot_RLum.Data.Curve()] Normalisation led to Inf or NaN values. Values replaced by 0.", call. = FALSE)

      }

    }


    ylab <- if (!is.na(ylab.xsyg)) {
      ylab.xsyg
    } else if (lab.xlab == "Independent") {
      "Dependent [unknown]"

    } else {
      paste(
        object@recordType,
        " [cts/", round(max(object@data[,1]) / length(object@data[,1]),digits =
                          2)
        , " ", lab.unit,"]", sep = ""
      )
    }

    sub <-  if ((grepl("TL", object@recordType) == TRUE) &
          "RATE" %in% names(object@info)) {
        paste("(",object@info$RATE," K/s)", sep = "")
      } else if ((grepl("OSL", object@recordType) |
           grepl("IRSL", object@recordType)) &
          "interval" %in% names(object@info)) {
        paste("(resolution: ",object@info$interval," s)", sep = "")
      }


    ##deal with additional arguments
    plot_settings <- modifyList(x = list(
      main = object@recordType[1],
      xlab = if (!is.na(xlab.xsyg)) xlab.xsyg else  paste0(lab.xlab, " [", lab.unit, "]"),
      ylab = ylab,
      sub = sub,
      cex = 1,
      type = "l",
      lwd = 1,
      lty = 1,
      pch = 1,
      col = 1,
      xlim = range(object@data[,1], na.rm = TRUE),
      ylim = range(object@data[,2], na.rm = TRUE),
      log = "",
      mtext = ""

    ), val = list(...))


    ##par setting for possible combination with plot method for RLum.Analysis objects
    if (par.local)
      par(mfrow = c(1,1), cex = plot_settings$cex)


    ##smooth
    if(smooth){
      k <- ceiling(length(object@data[, 2])/100)
      object@data[, 2] <- zoo::rollmean(object@data[, 2],
                                        k = k, fill = NA)
    }

    ##plot curve
    plot(
      object@data[,1], object@data[,2],
      main = plot_settings$main,
      xlim = plot_settings$xlim,
      ylim = plot_settings$ylim,
      xlab = plot_settings$xlab,
      ylab = plot_settings$ylab,
      sub = plot_settings$sub,
      type = plot_settings$type,
      log = plot_settings$log,
      col = plot_settings$col,
      lwd = plot_settings$lwd,
      pch = plot_settings$pch,
      lty = plot_settings$lty
    )

    ##plot additional mtext
    mtext(plot_settings$mtext, side = 3, cex = plot_settings$cex * 0.8)
}

