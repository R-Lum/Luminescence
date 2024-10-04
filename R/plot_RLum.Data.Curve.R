#' @title Plot function for an RLum.Data.Curve S4 class object
#'
#' @description The function provides a standardised plot output for curve data of an
#' `RLum.Data.Curve` S4-class object.
#'
#' @details Only single curve data can be plotted with this function. Arguments
#' according to [plot].
#'
#' **Curve normalisation**
#'
#' The argument `norm` normalises all count values. To date the following
#' options are supported:
#'
#' `norm = TRUE` or `norm = "max"`: Curve values are normalised to the highest
#' count value in the curve
#'
#' `norm = "last"`: Curve values are normalised to the last count value
#' (this can be useful in particular for radiofluorescence curves)
#'
#' `norm = "huot"`: Curve values are normalised as suggested by Sébastien Huot
#'  via GitHub:
#' \deqn{
#' y = (observed - median(background)) / (max(observed) - median(background))
#' }
#'
#' The background of the curve is defined as the last 20% of the count values
#' of a curve.
#'
#' @param object [RLum.Data.Curve-class] (**required**):
#' S4 object of class `RLum.Data.Curve`
#'
#' @param par.local [logical] (*with default*):
#' use local graphical parameters for plotting, e.g. the plot is shown in one
#' column and one row. If `par.local = FALSE`, global parameters are inherited.
#'
#' @param norm [logical] [character] (*with default*): whether curve
#' normalisation should occur (`FALSE` by default). Alternatively, the function
#' offers modes `"max"` (used with `TRUE`), `"last"` and `"huot"`, see details.
#'
#' @param smooth [logical] (*with default*):
#' provides automatic curve smoothing based on the internal function `.smoothing`
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to the `plot` function
#'
#' @return Returns a plot.
#'
#' @note Not all arguments of [plot] will be passed!
#'
#' @section Function version: 0.2.6
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
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
) {
  .set_function_name("plot_RLum.Data.Curve")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity tests  -------------------------------------------------------

  .validate_class(object, "RLum.Data.Curve")

  ## check for NA values
  if(all(is.na(object@data))){
    .throw_warning("Curve contains only NA-values, nothing plotted")
    return(NULL)
  }

  if (is.logical(norm))
    norm <- norm[1]
  else
    norm <- .validate_args(norm, c("max", "last", "huot"),
                           extra = "a logical value")

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

    ## curve normalisation
    if (norm == TRUE || norm %in% c("max", "last", "huot")) {
      object@data[, 2] <- .normalise_curve(object@data[, 2], norm)
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
      las = NULL,
      lwd = 1,
      lty = 1,
      pch = 1,
      col = 1,
      axes = TRUE,
      xlim = range(object@data[,1], na.rm = TRUE),
      ylim = range(object@data[,2], na.rm = TRUE),
      log = "",
      mtext = ""

    ), val = list(...), keep.null = TRUE)


    ##par setting for possible combination with plot method for RLum.Analysis objects
    if (par.local)
      par(mfrow = c(1,1), cex = plot_settings$cex)

    ##smooth
    if(smooth){
      k <- ceiling(length(object@data[, 2])/100)
      object@data[, 2] <- .smoothing(object@data[, 2], k = k, align = "center")
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
      lty = plot_settings$lty,
      axes = plot_settings$axes,
      las = plot_settings$las)

    ##plot additional mtext
    mtext(plot_settings$mtext, side = 3, cex = plot_settings$cex * 0.8)
}
