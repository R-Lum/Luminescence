#' Plot function for an RLum.Data.Curve S4 class object
#'
#' The function provides a standardised plot output for curve data of an
#' RLum.Data.Curve S4 class object
#'
#' Only single curve data can be plotted with this function.  Arguments
#' according to \code{\link{plot}}.
#'
#' @param object \code{\linkS4class{RLum.Data.Curve}} (\bold{required}): S4
#' object of class \code{RLum.Data.Curve}
#'
#' @param par.local \code{\link{logical}} (with default): use local graphical
#' parameters for plotting, e.g. the plot is shown in one column and one row.
#' If \code{par.local = FALSE}, global parameters are inherited.
#'
#' @param norm \code{\link{logical}} (with default): allows curve normalisation
#' to the highest count value
#'
#' @param smooth \code{\link{logical}} (with default): provides an automatic curve smoothing
#' based on \code{\link[zoo]{rollmean}}
#'
#' @param \dots further arguments and graphical parameters that will be passed
#' to the \code{plot} function
#'
#' @return Returns a plot.
#'
#' @note Not all arguments of \code{\link{plot}} will be passed!
#'
#' @section Function version: 0.2.3
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#'
#' @seealso \code{\link{plot}}, \code{\link{plot_RLum}}
#'
#' @references #
#'
#' @keywords aplot
#'
#' @examples
#'
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
  if(class(object) != "RLum.Data.Curve"){
    stop("[plot_RLum.Data.Curve()] Input object is not of type RLum.Data.Curve")

  }

  ##stop for NA values
  if (!all(is.na(object@data))) {

    ##set labeling unit
    if(!is.na(object@recordType)){
      lab.unit <- if (object@recordType == "OSL" |
                      object@recordType == "IRSL" |
                      object@recordType == "RL" |
                      object@recordType == "RF" |
                      object@recordType == "LM-OSL" |
                      object@recordType == "RBR") {
        "s"
      } else if (object@recordType == "TL") {
        "\u00B0C"
      }
      else {
        "Unknown"
      }
    }else{
      lab.unit <- "Unknown"

    }

    if(!is.na(object@recordType)){
      lab.xlab <- if (object@recordType == "OSL" |
                      object@recordType == "IRSL" |
                      object@recordType == "RL" |
                      object@recordType == "RF" |
                      object@recordType == "RBR" |
                      object@recordType == "LM-OSL"){

        "Stimulation time"
      }
      else if (object@recordType == "TL") {
        "Temperature"
      }
      else {
        "Independent"
      }
    }else{
      lab.xlab <- "Independent"

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

    } else{
      xlab.xsyg <- NA
      ylab.xsyg <- NA

    }

    ##normalise curves if argument has been set
    if (norm) {
      object@data[,2] <- object@data[,2] / max(object@data[,2])

    }

    ##deal with additional arguments
    extraArgs <- list(...)

    main <- if ("main" %in% names(extraArgs)) {
      extraArgs$main
    } else
    {
      object@recordType
    }

    xlab <- if ("xlab" %in% names(extraArgs)) {
      extraArgs$xlab
    } else
    {
      if (!is.na(xlab.xsyg)) {
        xlab.xsyg
      } else
      {
        paste0(lab.xlab, " [", lab.unit, "]")
      }
    }

    ylab <- if ("ylab" %in% names(extraArgs)) {
      extraArgs$ylab
    }else if (!is.na(ylab.xsyg)) {
      ylab.xsyg
    }
    else if (lab.xlab == "Independent") {
      "Dependent [unknown]"
    }
    else {
      paste(
        object@recordType,
        " [cts/", round(max(object@data[,1]) / length(object@data[,1]),digits =
                          2)
        , " ", lab.unit,"]", sep = ""
      )
    }

    sub <-  if ("sub" %in% names(extraArgs)) {
      extraArgs$sub
    } else
    {
      if ((grepl("TL", object@recordType) == TRUE) &
          "RATE" %in% names(object@info)) {
        paste("(",object@info$RATE," K/s)", sep = "")
      }

      if ((grepl("OSL", object@recordType) |
           grepl("IRSL", object@recordType)) &
          "interval" %in% names(object@info)) {
        paste("(resolution: ",object@info$interval," s)", sep = "")
      }

    }
    cex <- if ("cex" %in% names(extraArgs)) {
      extraArgs$cex
    } else
    {
      1
    }

    type <- if ("type" %in% names(extraArgs)) {
      extraArgs$type
    } else
    {
      "l"
    }

    lwd <- if ("lwd" %in% names(extraArgs)) {
      extraArgs$lwd
    } else
    {
      1
    }

    lty <- if ("lty" %in% names(extraArgs)) {
      extraArgs$lty
    } else
    {
      1
    }

    pch <- if ("pch" %in% names(extraArgs)) {
      extraArgs$pch
    } else
    {
      1
    }

    col <- if ("col" %in% names(extraArgs)) {
      extraArgs$col
    } else
    {
      1
    }

    ylim <- if ("ylim" %in% names(extraArgs)) {
      extraArgs$ylim
    } else
    {
      c(min(object@data[,2], na.rm = TRUE),max(object@data[,2], na.rm = TRUE))
    }

    xlim <- if ("xlim" %in% names(extraArgs)) {
      extraArgs$xlim
    } else
    {
      c(min(object@data[,1]),max(object@data[,1]))
    }

    log <- if ("log" %in% names(extraArgs)) {
      extraArgs$log
    } else
    {
      ""
    }

    mtext <- if ("mtext" %in% names(extraArgs)) {
      extraArgs$mtext
    } else
    {
      ""
    }

    fun  <-
      if ("fun" %in% names(extraArgs)) {
        extraArgs$fun
      } else {
        FALSE
      }

    ##to avoid problems with plot method of RLum.Analysis
    plot.trigger <-
      if ("plot.trigger" %in% names(extraArgs)) {
        extraArgs$plot.trigger
      } else
      {
        FALSE
      }

    ##par setting for possible combination with plot method for RLum.Analysis objects
    if (par.local == TRUE) {
      par(mfrow = c(1,1), cex = cex)
    }

    ##smooth
    if(smooth){

      k <- ceiling(length(object@data[, 2])/100)
      object@data[, 2] <- zoo::rollmean(object@data[, 2],
                                        k = k, fill = NA)
    }


    ##plot curve
    plot(
      object@data[,1], object@data[,2],
      main = main,
      xlim = xlim,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      sub = sub,
      type = type,
      log = log,
      col = col,
      lwd = lwd,
      pch = pch,
      lty = lty
    )

    ##plot additional mtext
    mtext(mtext, side = 3, cex = cex * 0.8)

    if (fun == TRUE) {
      sTeve()
    }

  }else{
    warning("[plot_RLum.Data.Curve()] Curve contains only NA-values, nothing plotted.", call. = FALSE)

  }

}
