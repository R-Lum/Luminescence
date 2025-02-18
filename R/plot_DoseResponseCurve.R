#' @title Plot a dose-response curve for luminescence data (Lx/Tx against dose)
#'
#' @description
#'
#' A dose-response curve is produced for luminescence measurements using a
#' regenerative or additive protocol as implemented in [fit_DoseResponseCurve].
#'
#' @param object [RLum.Results-class] (**required**):
#' An object produced by [fit_DoseResponseCurve].
#'
#' @param plot_extended [logical] (*with default*):
#' If `TRUE`, 3 plots on one plot area are provided:
#' 1. growth curve,
#' 2. histogram from Monte Carlo error simulation and
#' 3. a test dose response plot.
#'
#' If `FALSE`, just the growth curve will be plotted.
#'
#' @param plot_singlePanels [logical] (*with default*):
#' single plot output (`TRUE/FALSE`) to allow for plotting the results in
#' single plot windows. Ignored if `plot_extended = FALSE`.
#'
#' @param cex.global [numeric] (*with default*):
#' global scaling factor.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param ... Further graphical parameters to be passed (supported:
#' `main`, `mtext`, `xlim`, `ylim`, `xlab`, `ylab`, `legend`, `reg_points_pch`,
#' `density_polygon` (`TRUE/FALSE`), `density_polygon_col`, `density_rug` (`TRUE`/`FALSE`)),
#'  `box` (`TRUE`/`FALSE`)
#'
#' @return
#' A plot (or a series of plots) is produced.
#'
#' @section Function version: 1.0.2
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
#' Michael Dietze, GFZ Potsdam (Germany) \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @references
#'
#' Berger, G.W., Huntley, D.J., 1989. Test data for exponential fits. Ancient TL 7, 43-46.
#'
#' Guralnik, B., Li, B., Jain, M., Chen, R., Paris, R.B., Murray, A.S., Li, S.-H., Pagonis, P.,
#' Herman, F., 2015. Radiation-induced growth and isothermal decay of infrared-stimulated luminescence
#' from feldspar. Radiation Measurements 81, 224-231.
#'
#' Pagonis, V., Kitis, G., Chen, R., 2020. A new analytical equation for the dose response of dosimetric materials,
#' based on the Lambert W function. Journal of Luminescence 225, 117333. \doi{10.1016/j.jlumin.2020.117333}
#'
#' @seealso [fit_DoseResponseCurve]
#'
#' @examples
#'
#' ##(1) plot dose-response curve for a dummy dataset
#' data(ExampleData.LxTxData, envir = environment())
#' fit <- fit_DoseResponseCurve(LxTxData)
#' plot_DoseResponseCurve(fit)
#'
#' ##(1b) horizontal plot arrangement
#' layout(mat = matrix(c(1,1,2,3), ncol = 2))
#' plot_DoseResponseCurve(fit, plot_singlePanels = TRUE)
#'
#' ##(2) plot the dose-response curve with pdf output - uncomment to use
#' ##pdf(file = "~/Dose_Response_Curve_Dummy.pdf", paper = "special")
#' plot_DoseResponseCurve(fit)
#' ##dev.off()
#'
#' ##(3) plot the growth curve with pdf output - uncomment to use, single output
#' ##pdf(file = "~/Dose_Response_Curve_Dummy.pdf", paper = "special")
#' plot_DoseResponseCurve(fit, plot_singlePanels = TRUE)
#' ##dev.off()
#'
#' ##(4) plot resulting function for given interval x
#' x <- seq(1,10000, by = 100)
#' plot(
#'  x = x,
#'  y = eval(fit$Formula),
#'  type = "l"
#' )
#'
#' @md
#' @export
plot_DoseResponseCurve <- function(
  object,
  plot_extended = TRUE,
  plot_singlePanels = FALSE,
  cex.global = 1,
  verbose = TRUE,
  ...
) {
  .set_function_name("plot_DoseResponseCurve")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(object, "RLum.Results")
  .validate_logical_scalar(plot_extended)
  .validate_logical_scalar(plot_singlePanels)
  .validate_logical_scalar(verbose)
  .validate_positive_scalar(cex.global)

  ## Fitting arguments ------------------------------------------------------
  fit.args <- object$Fit.Args
  mode <- fit.args$mode
  sample <- fit.args$object

  ## for interpolation the first point is considered as natural dose
  first.idx <- ifelse(mode == "interpolation", 2, 1)
  last.idx <- fit.args$fit.NumberRegPoints + 1
  xy <- sample[first.idx:last.idx, 1:2]
  colnames(xy) <- c("x", "y")
  y.Error <- sample[first.idx:last.idx, 3]

  De <- object@data$De$De.plot
  x.natural <- na.exclude(object@data$De.MC)
  De.MonteCarlo <- mean(na.exclude(x.natural))
  De.Error <- sd(na.exclude(x.natural))

  ## Graphical arguments ----------------------------------------------------

  ymax <- max(xy$y) + if (max(xy$y) * 0.1 > 1.5) 1.5 else max(xy$y) * 0.2
  ylim <- if (mode == "extrapolation" || fit.args$fit.force_through_origin) {
            c(0 - max(y.Error), ymax)
          } else {
            c(0, ymax)
          }

  xmin <- if (!is.na(De)) min(De * 2, 0) else -min(xy$x) * 2
  xmax <- max(xy$x) + if (max(xy$x) * 0.4 > 50) 50 else max(xy$x) * 0.4
  xlim <- if (mode == "extrapolation") {
            c(xmin, xmax)
          } else {
            c(0, xmax)
          }

  ## set plot settings
  plot_settings <- modifyList(
    x = list(
      main = "Dose-response curve",
      xlab = "Dose [s]",
      ylab = if (mode == "interpolation") expression(L[x]/T[x]) else "Luminescence [a.u.]",
      ylim = ylim,
      xlim = xlim,
      cex = 1,
      mtext = if (mode != "alternate") {
          substitute(
            D[e] == De,
            list(De = paste(
              round(
                abs(De), digits = 2), "\u00B1",
              format(De.Error, scientific = TRUE, digits = 2),
              " | fit: ", fit.args$fit.method)))
        } else {
          ""
        },
      legend = TRUE,
      reg_points_pch = c(19,2, 1),
      density_polygon = TRUE,
      density_polygon_col = rgb(1,0,0,0.2),
      density_rug = TRUE,
      box = TRUE),
    val = list(...),
    keep.null = TRUE
  )

  ## Main plots -------------------------------------------------------------

  ## set plot check
  plot_check <- NULL

  ## open plot area
  par(cex = cex.global)

  if (plot_extended && !plot_singlePanels) {
    ## get graphic values
    par_default <- par(no.readonly = TRUE)
    on.exit(par(par_default), add = TRUE)

    ## set new parameter
    layout(matrix(c(1, 1, 1, 1, 2, 3), 3, 2, byrow = TRUE), respect = TRUE)
    par(cex = 0.8 * plot_settings$cex)
  }

  #PLOT		#Plot input values
  ##Make selection to support manual number of reg points input
  plot_check <- try(plot(
      xy[1:fit.args$fit.NumberRegPointsReal, ],
      ylim = plot_settings$ylim,
      xlim = plot_settings$xlim,
      pch = plot_settings$reg_points_pch,
      xlab = plot_settings$xlab,
      ylab = plot_settings$ylab,
      frame.plot = plot_settings$box[1]
  ),
  silent = TRUE)

  if (!is(plot_check, "try-error")) {
    if (mode == "extrapolation") {
      abline(v = 0, lty = 1, col = "grey")
      abline(h = 0, lty = 1, col = "grey")
    }

    ### add header
    title(main = plot_settings$main, line = NA)

    ## add curve
    if (inherits(object$Formula, "expression")) {
      x <- seq(par()$usr[1], par()$usr[2], length.out = 100)
      lines(x, eval(object$Formula))
    }

    ## natural value
    if (mode == "interpolation") {
      points(sample[1, 1:2], col = "red", pch = plot_settings$reg_points_pch[1])
      segments(sample[1, 1], sample[1, 2] - sample[1, 3],
               sample[1, 1], sample[1, 2] + sample[1, 3], col = "red")

    } else if (mode == "extrapolation"){
      points(
        x = De,
        y = 0,
        bg = "red",
        pch = 21,
        col = "black",
        cex = 1.1 * cex.global)
    }

    ## repeated Point
    points(
        x = xy[which(duplicated(xy[, 1])), 1],
        y = xy[which(duplicated(xy[, 1])), 2],
        pch = if(is.na(plot_settings$reg_points_pch[2]))
                plot_settings$reg_points_pch[1]
              else
                plot_settings$reg_points_pch[2])

    ## reg Point 0
    points(
        x = xy[which(xy == 0), 1],
        y = xy[which(xy == 0), 2],
        pch = if(is.na(plot_settings$reg_points_pch[3]))
               plot_settings$reg_points_pch[1]
              else
               plot_settings$reg_points_pch[3],
        cex = 1.5 * cex.global)

    ## ARROWS	#y-error Bar
    segments(xy$x, xy$y - y.Error, xy$x, xy$y + y.Error)

    ## LINES	#Insert Ln/Tn
    if (mode == "interpolation") {
      xmax <- if (is.na(De)) max(sample[, 1]) * 2 else De
      try(lines(
          c(par()$usr[1], xmax),
          c(sample[1, 2], sample[1, 2]),
          col = "red",
          lty = 2,
          lwd = 1.25
      ), silent = TRUE)
      try(lines(
          c(De, De),
          c(par()$usr[3], sample[1, 2]),
          col = "red",
          lty = 2,
          lwd = 1.25), silent = TRUE)
      try({
        points(
          x = De,
          y = sample[1, 2],
          col = "black",
          pch = 21,
          bg = "red",
          cex = 1.1 * cex.global)
        },
        silent = TRUE)


      if (plot_settings$density_polygon[1] & length(x.natural) > 1 &&
          !all(is.na(x.natural))) {
          ##calculate density De.MC
          density_De <- density(x.natural, na.rm = TRUE)

          ##calculate transformation function
          x.1 <- max(density_De$y)
          x.2 <- min(density_De$y)
          y.1 <- c(sample[1, 2])/2
          y.2 <- par("usr")[3]

          m <- (y.1 - y.2) / (x.1 + x.2)
          n <- y.1 - m * x.1
          density_De$y <- m * density_De$y + n

          polygon(
            x = density_De$x,
            y = density_De$y,
            col = plot_settings$density_polygon_col)

          rm(x.1,x.2,y.1,y.2,m,n, density_De)
      }

      if(plot_settings$density_rug[1])
        suppressWarnings(graphics::rug(x = x.natural, side = 3))

    } else if (mode == "extrapolation"){
      if (!is.na(De)) {
        abline(v = De, lty = 2, col = "red")
        lines(x = c(0,De), y = c(0,0), lty = 2, col = "red")
      }
    }

    ## insert fit and result
    try(mtext(side = 3,
              plot_settings$mtext,
              line = 0,
              cex = 0.8 * cex.global),
        silent = TRUE)

    ## write error message in plot if De is NaN
    try(if (is.nan(De)) {
          text(sample[2, 1],
              0,
              "Error: De could not be calculated!",
              adj = c(0, 0),
              cex = 0.8,
              col = "red"
          )
        }, silent = TRUE)

    ## plot legend
    if(plot_settings$legend) {
      if (mode == "interpolation") {
        legend(
            "topleft",
            c("REG point", "REG point repeated", "REG point 0"),
            pch = plot_settings$reg_points_pch,
            cex = 0.8 * cex.global,
            bty = "n")
      } else {
        legend(
            "bottomright",
            c("Dose point", "Dose point rep.", "Dose point 0"),
            pch = plot_settings$reg_points_pch,
            cex = 0.8 * cex.global,
            bty = "n")
      }
    }

    if (plot_extended) {
      ## Histogram ----------------------------------------------------------
      if (!plot_singlePanels)
        par(cex = 0.7 * cex.global)

      ## calculate histogram data
      try(histogram <- hist(x.natural, plot = FALSE), silent = TRUE)

      ## to avoid errors plot only if histogram exists
      if (exists("histogram") && length(histogram$counts) > 2) {

        ## calculate normal distribution curves for overlay
        norm.curve.x <- seq(min(x.natural, na.rm = TRUE),
                            max(x.natural, na.rm = TRUE),
                            length = 101)
        norm.curve.y <- dnorm(norm.curve.x,
                              mean = mean(x.natural, na.rm = TRUE),
                              sd = sd(x.natural, na.rm = TRUE))

        ## plot histogram
        histogram <- try(hist(
            x.natural,
            xlab = plot_settings$xlab,
            ylab = "Frequency",
            main = "MC runs",
            freq = FALSE,
            border = "white",
            axes = FALSE,
            ylim = c(0, max(norm.curve.y)),
            sub = paste0("valid fits = ", length(na.exclude(x.natural)),
                         "/", fit.args$n.MC),
            col = "grey"
        ), silent = TRUE)

        ## add axes
        if (!is(histogram, "try-error")) {
          axis(side = 1)
          axis(side = 2,
               at = seq(min(histogram$density),
                        max(histogram$density), length = 5),
               labels = round(seq(min(histogram$counts),
                                  max(histogram$counts), length = 5),
                              digits = 0))

          ## add norm curve
          lines(norm.curve.x, norm.curve.y, col = "red")

          ## add rug
          suppressWarnings(graphics::rug(x.natural))

          ## De + Error from MC simulation + quality of error estimation
          try(mtext(side = 3,
                    substitute(D[e[MC]] == De,
                               list(De = paste(
                                        abs(round(De.MonteCarlo, 2)),
                                        "\u00B1",
                                        format(De.Error, scientific = TRUE, digits = 2),
                                        " | diff. = ",
                                        abs(round((abs(abs(De) - De.MonteCarlo) / abs(De)) * 100,1)),
                                        "%"
                                    )
                                    )),
                    cex = 0.6 * cex.global), silent = TRUE)

        } else {
          plot_check <- histogram
        }

      } else {
        plot_check <- try(plot(NA, NA, xlim = c(0, 10), ylim = c(0, 10),
                               main = expression(paste(D[e], " from MC runs"))),
                          silent = TRUE)
        if (!is(plot_check,"try-error"))
          text(5, 5, "not available")
      }

      ## Test dose response curve if available ------------------------------
      ## plot Tx/Tn value for sensitivity change
      if (!is(plot_check, "try-error")) {
        if ("TnTx" %in% colnames(sample)) {
          plot(
              1:length(sample[, "TnTx"]),
              sample[, "TnTx"] / sample[1, "TnTx"],
              xlab = "SAR cycle",
              ylab = expression(paste(T[x] / T[n])),
              main = "Test-dose response",
              type = "o",
              pch = 20,
              )
          lines(c(1, length(sample[, "TnTx"])), c(1, 1), lty = 2, col = "gray")
        } else {
          plot(NA, NA, xlim = c(0, 10), ylim = c(0, 10),
               main = "Test-dose response")
          text(5, 5, "not available\n no TnTx column")
        }
      }
    }
  }

  ##reset graphic device if the plotting failed!
  if (inherits(plot_check, "try-error")) {
    .throw_message("Figure margins too large, nothing plotted")
    grDevices::dev.off()
  }

  ## return
  invisible(object)
}
