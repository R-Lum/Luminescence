#' @title Plot function for an `RLum.Data.Curve` S4 class object
#'
#' @description
#' The function provides a standardised plot output for curve data of an
#' [Luminescence::RLum.Data.Curve-class] S4-class object. Only single curve
#' data can be plotted with this function.
#'
#' @param object [Luminescence::RLum.Data.Curve-class] (**required**):
#' S4 object of class [Luminescence::RLum.Data.Curve-class].
#'
#' @param norm [logical] [character] (*with default*):
#' whether curve normalisation should occur (`FALSE` by default), or any of
#' the normalisation methods provided by [Luminescence::normalise_RLum].
#'
#' @param smooth [logical] (*with default*):
#' provides automatic curve smoothing based on the internal function `.smoothing`.
#'
#' @param auto_scale [logical] (*with default*):
#' if activated, auto scales `xlim` or `ylim` to the extent of the other. If
#' both are set, no auto-scaling occurs.
#'
#' @param interactive [logical] (*with default*):
#' enables/disables interactive plotting mode using [plotly::plot_ly].
#'
#' @param par.local [logical] (*with default*):
#' whether local graphical parameters should be used for plotting. If `TRUE`
#' (default), the plot is shown in one column and one row; otherwise, global
#' parameters set via `par()` are inherited.
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to [graphics::plot.default] and [graphics::par].
#'
#' @return Returns a plot.
#'
#' @note Not all arguments of [plot] will be passed!
#'
#' @section Function version: 0.4.1
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [plot], [Luminescence::plot_RLum]
#'
#' @keywords aplot
#'
#' @examples
#' ## load example data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ## transform data.frame to RLum.Data.Curve object and plot it
#' temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")
#' plot_RLum.Data.Curve(temp)
#'
#' @export
plot_RLum.Data.Curve<- function(
  object,
  norm = FALSE,
  smooth = FALSE,
  auto_scale = FALSE,
  interactive = FALSE,
  par.local = TRUE,
  ...
) {
  .set_function_name("plot_RLum.Data.Curve")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks  ------------------------------------------------------
  .validate_class(object, "RLum.Data.Curve")

  ## check for NA values
  if(all(is.na(object@data))){
    .throw_warning("Curve contains only NA-values, nothing plotted")
    return(NULL)
  }

  ## `norm` is not validated here but will be validated by normalise_RLum()
  .validate_logical_scalar(par.local)
  .validate_logical_scalar(smooth)
  .validate_logical_scalar(auto_scale)
  .validate_logical_scalar(interactive)

# Preset plot -------------------------------------------------------------

  lab.unit <- "unknown"
  lab.xlab <- "Independent"
  xlab.xsyg <- ylab.xsyg <- NULL

  ## set labelling unit
  if (!is.na(object@recordType)) {
    recordType.stripped <- gsub(" \\(.*)", "", object@recordType[1])
    if (recordType.stripped %in% c("OSL", "IRSL", "RL", "RF", "LM-OSL", "RBR")) {
      ## read the irradiation unit for BIN/BINX files version 03-04
      lab.unit <- switch(as.character(object@info$IRR_UNIT %||% ""),
                         "1" = "Gy",
                         "2" = "rad",
                         "4" = "min",
                         "5" = "h",
                         "s")
        lab.xlab <- "Stimulation time"
    } else if (recordType.stripped == "TL") {
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

  xlab <- xlab.xsyg %||% paste0(lab.xlab, " [", lab.unit, "]")
  ylab <- ylab.xsyg %||% (
    if (lab.xlab == "Independent") {
      "Dependent [unknown]"
    } else {
      vals <- object@data[, 1]
      sprintf("%s [cts/%.2f %s]", object@recordType,
              round(max(vals) / length(vals), digits = 2), lab.unit)
    }
  )

  sub <- if (grepl("TL", object@recordType) && "RATE" %in% names(object@info)) {
           paste0("(", object@info$RATE, " K/s)")
         } else if (grepl("OSL|IRSL", object@recordType) &&
                    "interval" %in% names(object@info)) {
           paste("(resolution:", object@info$interval, "s)")
         }

  ## curve normalisation
  if (!isFALSE(norm)) {
    object <- normalise_RLum(object, norm)
  }

  extraArgs <- list(...)

  ## set auto scale if required and only one of xlim or ylim is specified
  if (auto_scale && xor(is.null(extraArgs$xlim), is.null(extraArgs$ylim))) {
    if (!is.null(extraArgs$xlim))  {
        x_range <- range(object@data[,1])
        x_min <- max(c(x_range[1], min(c(extraArgs$xlim[1], x_range[2]))), na.rm = TRUE)
        x_max <- min(c(x_range[2], max(c(extraArgs$xlim[2], x_range[1]))), na.rm = TRUE)

        object@data <- object[object@data[,1] >= x_min & object@data[,1] <= x_max, ,drop = FALSE]
      }
    if (!is.null(extraArgs$ylim)) {
        y_range <- range(object@data[,2])
        y_min <- max(c(y_range[1], min(c(extraArgs$ylim[1], y_range[2]))), na.rm = TRUE)
        y_max <- min(c(y_range[2], max(c(extraArgs$ylim[2], y_range[1]))), na.rm = TRUE)

        object@data <- object[object@data[,2] >= y_min & object@data[,2] <= y_max, ,drop = FALSE]
      }
  }

  ## deal with additional arguments
  plot_settings <- modifyList(x = list(
      main = object@recordType[1],
      xlab = xlab,
      ylab = ylab,
      sub = sub,
      cex = 1,
      type = if(interactive) "scatter" else "l",
      las = NULL,
      lwd = 1,
      lty = 1,
      pch = NULL,
      col = 1,
      axes = TRUE,
      log = "",
      mtext = ""
    ), val = extraArgs, keep.null = TRUE)

    ##par setting for possible combination with plot method for RLum.Analysis objects
    if (par.local)
      par(mfrow = c(1,1), cex = plot_settings$cex)

    ##smooth
    if(smooth){
      k <- ceiling(length(object@data[, 2])/100)
      object@data[, 2] <- .smoothing(object@data[, 2], k = k, align = "center")
    }

  ## remove parameters already set in plot_settings
  extraArgs[names(extraArgs) %in% names(plot_settings)] <- NULL
  extraArgs <- c(extraArgs, plot_settings)

  ## remove parameters not supported by plot.default and par
  extraArgs[!names(extraArgs) %in% c(names(formals(graphics::plot.default)), names(par()))] <- NULL

  ## call the plot
  if (interactive) {
        .require_suggested_package("plotly", "Displaying interactive plots")
        p <- do.call(
          plotly::plot_ly,
          args = c(
            list(
              x = object@data[,1],
              y = object@data[,2],
              type = "scatter",
              line = list(
                width = plot_settings$lwd * 1.3,
                dash = switch(as.character(plot_settings$lty), `2` = "dash", `3` = "dot", "solid"),
                color = if(is.numeric(plot_settings$col))
                  grDevices::palette()[plot_settings$col]
                else
                  plot_settings$col)),
              symbol = plot_settings$pch,
              mode = "lines"))

        ## add more scene information
        p <-  plotly::layout(
          p = p,
          xaxis = list(
            title = plot_settings$xlab,
            type = if (grepl(pattern = "x", plot_settings$log[1])) "log" else "linear"
            ),
          yaxis = list(
            title = plot_settings$ylab,
            type = if (grepl(pattern = "y", plot_settings$log[1])) "log" else "linear"
            ),
          title = plot_settings$main,
          annotations = list(plot_settings$mtext)
        )

        ## print and return only if hidden shiny
        ## flag is not set
        if(is.null(list(...)$.shiny))
          suppressMessages(print(p))

        on.exit(return(p), add = TRUE)

  } else {
    do.call(graphics::plot.default,
            c(list(x = object@data[, 1], y = object@data[, 2]), extraArgs))

    ## plot additional mtext
    mtext(plot_settings$mtext, side = 3, cex = 0.8 * ifelse(par.local, extraArgs$cex, 1))
  }
}
