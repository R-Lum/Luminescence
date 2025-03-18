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
#' @param auto_scale [logical] (*with default*): if activated, auto scales `xlim` or `ylim`
#' to the extent of the other. If both are set, the auto-scaling is skipped.
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to [graphics::plot.default] and [graphics::par]
#'
#' @return Returns a plot.
#'
#' @note Not all arguments of [plot] will be passed!
#'
#' @section Function version: 0.3.0
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
#' @md
#' @export
plot_RLum.Data.Curve<- function(
  object,
  par.local = TRUE,
  norm = FALSE,
  smooth = FALSE,
  auto_scale = FALSE,
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
    if (norm[1] == TRUE || norm %in% c("max", "last", "huot")) {
      object@data[, 2] <- .normalise_curve(object@data[, 2], norm)
    }

    ylab <- if (!is.na(ylab.xsyg)) {
      ylab.xsyg
    } else if (lab.xlab == "Independent") {
      "Dependent [unknown]"

    } else {
      paste0(
        object@recordType,
        " [cts/", round(
          max(object@data[,1]) / length(object@data[,1]),digits = 2)
        , " ", lab.unit,"]")
    }

    sub <-  if ((grepl("TL", object@recordType)) &
          "RATE" %in% names(object@info)) {
        paste0("(",object@info$RATE," K/s)")
      } else if ((grepl("OSL", object@recordType) |
           grepl("IRSL", object@recordType)) &
          "interval" %in% names(object@info)) {
        paste0("(resolution: ",object@info$interval," s)")
      }

    ## set auto scale if required
    if(auto_scale[1] && !all(is.null(c(list(...)$xlim, list(...)$ylim)))) {
      ## because of the if condition, we can only have xlim or ylim set
      ## if both is set, auto_scale is not required

      if(!is.null(list(...)$xlim))  {
        x_range <- range(object@data[,1])
        x_min <- max(c(x_range[1], min(c(list(...)$xlim[1], x_range[2]))), na.rm = TRUE)
        x_max <- min(c(x_range[2], max(c(list(...)$xlim[2], x_range[1]))), na.rm = TRUE)

        object@data <- object[object@data[,1] >= x_min & object@data[,1] <= x_max, ,drop = FALSE]

      }

      if(!is.null(list(...)$ylim)) {
        y_range <- range(object@data[,2])
        y_min <- max(c(y_range[1], min(c(list(...)$ylim[1], y_range[2]))), na.rm = TRUE)
        y_max <- min(c(y_range[2], max(c(list(...)$ylim[2], y_range[1]))), na.rm = TRUE)

        object@data <- object[object@data[,2] >= y_min & object@data[,2] <= y_max, ,drop = FALSE]
      }

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
      ## get list of allowed parameters
      args <- list(...)

      ## remove parameters already set in plot_settings
      args[names(args) %in% names(plot_settings)] <- NULL

      ## combine
      args <- c(args, plot_settings)

      ## remove parameters not supported by plot.default and par
      args[!names(args) %in% c(names(formals(graphics::plot.default)), names(par()))] <- NULL

      ## call the plot
      do.call(graphics::plot.default, args = c(list(x = object@data[,1], y = object@data[,2]), args))

    ##plot additional mtext
    mtext(plot_settings$mtext, side = 3, cex = plot_settings$cex * 0.8)
}

