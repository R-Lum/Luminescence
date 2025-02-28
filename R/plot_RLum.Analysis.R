#' @title Plot function for an RLum.Analysis S4 class object
#'
#' @description The function provides a standardised plot output for curve data of an
#' RLum.Analysis S4 class object
#'
#' The function produces a multiple plot output. A file output is recommended
#' (e.g., [pdf]).
#'
#' **curve.transformation**
#'
#' This argument allows transforming continuous wave (CW) curves to pseudo
#' (linear) modulated curves. For the transformation, the functions of the
#' package are used. Currently, it is not possible to pass further arguments to
#' the transformation functions. The argument works only for `ltype`
#' `OSL` and `IRSL`.
#'
#' Please note: The curve transformation within this functions works roughly,
#' i.e. every IRSL or OSL curve is transformed, without considering whether it
#' is measured with the PMT or not! However, for a fast look it might be
#' helpful.
#'
#' @param object [RLum.Analysis-class] (**required**):
#' S4 object of class `RLum.Analysis`
#'
#' @param subset named [list] (*optional*):
#' subsets elements for plotting. The arguments in the named [list] will be
#' directly passed to the function [get_RLum]
#' (e.g., `subset = list(curveType = "measured")`)
#'
#' @param nrows [integer] (*optional*):
#' sets number of rows for plot output, if nothing is set the function
#' tries to find a value.
#'
#' @param ncols [integer] (*optional*):
#' sets number of columns for plot output, if nothing is set the function
#' tries to find a value.
#'
#' @param abline [list] (*optional*):
#' allows to add ab-lines to the plot. Argument are provided
#' in a list and will be forward to the function [abline],
#' e.g., `list(v = c(10, 100))` adds two vertical lines add 10 and 100 to all
#' plots. In contrast `list(v = c(10), v = c(100)` adds a vertical at 10 to
#' the first and a vertical line at 100 to the 2nd plot.
#'
#' @param combine [logical] (*with default*):
#' allows to combine all [RLum.Data.Curve-class] objects in one single plot.
#'
#' @param records_max [numeric] (*optional*): limits number of records
#' shown if `combine = TRUE`. Shown are always the first and the last curve,
#' the other number of curves to be shown a distributed evenly, this may result
#' in fewer curves plotted as specified. This parameter has only
#' an effect for  n > 2.
#'
#' @param curve.transformation [character] (*with default*):
#' allows transforming CW-OSL and CW-IRSL curves to pseudo-LM curves via
#' transformation functions. Allowed values are: `CW2pLM`, `CW2pLMi`,
#' `CW2pHMi` and `CW2pPMi`, see details. If set to `None` (default), no
#' transformation is applied.
#'
#' @param plot_singlePanels [logical] (*with default*):
#' global par settings are considered, normally this should end in one plot per page
#'
#' @param ... further arguments and graphical parameters will be passed to
#' the `plot` function.
#'
#' Supported arguments: `main`, `mtext`, `log`, `lwd`, `lty` `type`, `pch`, `col`,
#' `norm` (see [plot_RLum.Data.Curve]), `xlim`,`ylim`, `xlab`, `ylab`, ...
#'
#' and for `combine = TRUE` also: `sub_title`, `legend`, `legend.text`, `legend.pos`
#' (typical plus 'outside'), `legend.col`, `smooth`.
#'
#' All arguments can be provided as `vector` or `list` to gain in full control
#' of all plot settings.
#'
#' @return Returns multiple plots.
#'
#' @note
#' Not all arguments available for [plot] will be passed and they partly do not behave in the
#' way you might expect them to work. This function was designed to serve as an overview
#' plot, if you want to have more control, extract the objects and plot them individually.
#'
#' @section Function version: 0.3.16
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [plot], [plot_RLum], [plot_RLum.Data.Curve]
#'
#' @keywords aplot
#'
#' @examples
#'
#'##load data
#'data(ExampleData.BINfileData, envir = environment())
#'
#'##convert values for position 1
#'temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
#'
#'##(1) plot (combine) TL curves in one plot
#'plot_RLum.Analysis(
#' temp,
#' subset = list(recordType = "TL"),
#' combine = TRUE,
#' norm = TRUE,
#' abline = list(v = c(110))
#' )
#'
#'##(2) same as example (1) but using
#'## the argument smooth = TRUE
#'plot_RLum.Analysis(
#' temp,
#' subset = list(recordType = "TL"),
#' combine = TRUE,
#' norm = TRUE,
#' smooth = TRUE,
#' abline = list(v = c(110))
#' )
#'
#' @md
#' @export
plot_RLum.Analysis <- function(
  object,
  subset = NULL,
  nrows,
  ncols,
  abline = NULL,
  combine = FALSE,
  records_max = NULL,
  curve.transformation = "None",
  plot_singlePanels = FALSE,
  ...
) {
  .set_function_name("plot_RLum.Analysis")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(object, "RLum.Analysis")

  if(!is.null(subset)){
    ##check whether the user set the drop option and remove it, as we cannot work with it
    subset <- subset[!sapply(names(subset), function(x){"drop" %in% x})]
    object <- do.call(get_RLum, c(object = object, subset, drop = FALSE))
  }

  # Deal with additional arguments.  ------------------------------------------------------------

  ##create plot settings list
  plot.settings <- list(
    main = NULL,
    mtext = NULL,
    log = "",
    lwd = 1,
    lty = 1,
    type = "l",
    xlab = NULL,
    ylab = NULL,
    xlim = NULL,
    ylim = NULL,
    pch = 1,
    col = "auto",
    norm = FALSE,
    sub_title = NULL,
    cex = 1,
    legend = TRUE,
    legend.text = NULL,
    legend.pos = NULL,
    legend.col = NULL,
    smooth = FALSE
  )

  plot.settings <- modifyList(x = plot.settings, val = list(...), keep.null = TRUE)

  ## deprecated argument
  if ("plot.single" %in% names(list(...))) {
    plot_singlePanels <- list(...)$plot.single
    .throw_warning("'plot.single' is deprecated, use 'plot_singlePanels' ",
                   "instead")
  }

  ##try to find optimal parameters, this is however, a little bit stupid, but
  ##better than without any presetting
  .validate_logical_scalar(combine)
  if (combine && length(object@records) <= 1) {
    combine <- FALSE
    .throw_warning("'combine' can't be used with fewer than two curves, ",
                   "reset to FALSE")
  }
  if (combine) {
    sapply(object@records, function(x) {
      if (!inherits(x, "RLum.Data.Curve")) {
        .throw_error("'combine' is valid only for 'RLum.Data.Curve' objects")
      }
    })
    n.plots <- length(unique(as.character(structure_RLum(object)$recordType)))
  }
  else
    n.plots <- max(length_RLum(object), 1)

  .validate_positive_scalar(nrows)
  .validate_positive_scalar(ncols)

  ## set appropriate values for nrows and ncols if not both specified
  if (missing(nrows) || missing(ncols)) {
    if (n.plots == 1) {
      if (missing(nrows))
        nrows <- 1
      if (missing(ncols))
        ncols <- 1

    } else { # n.plots > 1
      if (missing(ncols)) {
        ncols <- 2
      }
      if (missing(nrows)) {
        if (n.plots <= 4) {
          nrows <- ceiling(n.plots / 2)
        } else {
          nrows <- 3
        }
      }
    }
  }

  curve.transformation <- .validate_args(curve.transformation,
                                         c("CW2pLM", "CW2pLMi",
                                           "CW2pHMi", "CW2pPMi", "None"))
  ## complete the function name
  if (curve.transformation != "None") {
    curve.transformation <- paste0("convert_", curve.transformation)
  }

  # Plotting ------------------------------------------------------------------
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##(1) NORMAL (combine == FALSE)
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  if (!combine) {

    ##grep RLum.Data.Curve or RLum.Data.Spectrum objects
    temp <- lapply(object@records, function(x) {
      if (inherits(x, "RLum.Data.Curve") ||
          inherits(x, "RLum.Data.Spectrum")) {
        x
      }})

    ##calculate number of pages for mtext
    if (length(temp) == 0 || length(temp) %% (nrows * ncols) > 0) {
      n.pages <- round(length(temp) / (nrows * ncols), digits = 0) + 1

    } else{
      n.pages <- length(temp) / (nrows * ncols)
    }

    ##set par
    par.default <- par("mfrow")
    if (!plot_singlePanels) {
      par(mfrow = c(nrows, ncols))
      on.exit(par(mfrow = par.default), add = TRUE)
    }

    ##expand plot settings list
    plot.settings <- lapply(setNames(1:length(plot.settings), names(plot.settings)),
                            function(x) {
                              if (!is.null(plot.settings[[x]])) {
                                if(length(plot.settings[[x]]) > 1){

                                  if(is(plot.settings[[x]], "list")){
                                    rep_len(plot.settings[[x]], length.out = length(temp))
                                  }else{
                                   rep_len(list(plot.settings[[x]]), length.out = length(temp))
                                  }

                                }else{
                                  rep_len(plot.settings[[x]], length.out = length(temp))
                                }

                              } else{
                                plot.settings[[x]]
                              }
                            })

    ##expand abline
    if(!is.null(abline)){
      abline.names <- rep_len(names(abline), length.out = length(temp))
      abline <- rep_len(abline, length.out = length(temp))
      names(abline) <- abline.names
    }

    ##apply curve transformation
    for (i in seq_along(temp)) {

      if (inherits(temp[[i]], "RLum.Data.Curve")) {

        ##set curve transformation if wanted
        if (grepl("IRSL|OSL", temp[[i]]@recordType) &&
            curve.transformation != "None") {

          ## get the actual function from the parameter value and apply it
          temp[[i]] <- get(curve.transformation)(temp[[i]])
        }

        ##check plot settings and adjust
        ##xlim
        if (!is.null(plot.settings$xlim)) {
          xlim.set <- plot.settings$xlim[[i]]
          if (plot.settings$xlim[[i]][1] < min(temp[[i]]@data[,1])) {
            .throw_warning("min('xlim') < x-value range for curve #", i,
                           ", reset to minimum")
            xlim.set[1] <- min(temp[[i]]@data[,1])
          }
          if (plot.settings$xlim[[i]][2] > max(temp[[i]]@data[,1])) {
            .throw_warning("max('xlim') > x-value range for curve #", i,
                           ", reset to maximum")
            xlim.set[2] <- max(temp[[i]]@data[,1])
          }

        }else{
          xlim.set <- plot.settings$xlim[[i]]
        }

        ##ylim
        if (!is.null(plot.settings$ylim)) {
          ylim.set <- plot.settings$ylim[[i]]
          if (plot.settings$ylim[[i]][1] < min(temp[[i]]@data[,2])) {
            .throw_warning("min('ylim') < y-value range for curve #", i,
                           ", reset to minimum")
            ylim.set[1] <- min(temp[[i]]@data[,2])
          }
          if (plot.settings$ylim[[i]][2] > max(temp[[i]]@data[,2])) {
            .throw_warning("max('ylim') > y-value range for curve #", i,
                           ", reset to maximum")
            ylim.set[2] <- max(temp[[i]]@data[,2])
          }

        }else{
          ylim.set <- plot.settings$ylim[[i]]
        }

        ##col
        if (unique(plot.settings$col) != "auto") {
          col <- plot.settings$col[i]

        } else{
          col <- "black"
          if (grepl("IRSL", temp[[i]]@recordType)) {
            col <- "red"
          } else if (grepl("OSL", temp[[i]]@recordType)) {
            col <- "blue"
          }
        }

        ##main
        main <- if (is.null(plot.settings$main[[i]])) {
          temp[[i]]@recordType
        } else{
          plot.settings$main[[i]]
        }

        ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ##PLOT
        ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ##plot RLum.Data.Curve curve
          ##we have to do this via this way, otherwise we run into a duplicated arguments
          ##problem
          ##check and remove duplicated arguments
          arguments <- c(
            list(
              object = temp[[i]],
              col = col,
              mtext = if (!is.null(plot.settings$mtext[[i]])) {
                plot.settings$mtext[[i]]
              } else{
                paste("#", i, sep = "")
              },
              par.local = FALSE,
              main = main,
              log = plot.settings$log[[i]],
              lwd = plot.settings$lwd[[i]],
              type = plot.settings$type[[i]],
              lty = plot.settings$lty[[i]],
              xlim = xlim.set,
              ylim = ylim.set,
              norm = plot.settings$norm,
              pch = plot.settings$pch[[i]],
              cex = plot.settings$cex[[i]],
              legend.col = plot.settings$legend.col[[i]],
              smooth = plot.settings$smooth[[i]]
            ),
            list(...)
          )

          arguments[duplicated(names(arguments))] <- NULL

        ##call the function plot_RLum.Data.Curve
        do.call(what = "plot_RLum.Data.Curve", args = arguments)
        rm(arguments)

        ##add abline
        if(!is.null(abline[[i]])){
          do.call(what = "abline", args = abline[i])
        }

      } else if(inherits(temp[[i]], "RLum.Data.Spectrum")) {
        ## remove already provided arguments
        args <- list(...)[!names(list(...)) %in% c("object", "mtext", "par.local", "main")]

        do.call(what = "plot_RLum.Data.Spectrum", args = c(list(
            object = temp[[i]],
            mtext =  if(!is.null(plot.settings$mtext[[i]])) plot.settings$mtext[[i]] else paste("#", i, sep = ""),
            par.local = FALSE,
            main = if(!is.null(plot.settings$main)) plot.settings$main else temp[[i]]@recordType
        ), args))
      }

    }#end for loop

  }else{

    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##(2) NORMAL (combine == TRUE)
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##(1) check RLum objects in the set

    ##account for different curve types, combine similar
    temp.object.structure  <- structure_RLum(object)
    temp.recordType <- as.character(unique(temp.object.structure$recordType))

    ##change graphic settings
    if (!plot_singlePanels) {
      par.default <- par()[c("cex", "mfrow")]
      if(!missing(ncols) & !missing(nrows)){
        par(mfrow = c(nrows, ncols))
      }
    } else {
      par.default <- par()["cex"]
    }
    ## this 2nd par request is needed as setting mfrow resets the par
    ## settings ... this might not be wanted
    par(cex = plot.settings$cex)

    ##expand plot settings list
    ##expand list
    plot.settings <- lapply(setNames(1:length(plot.settings), names(plot.settings)), function(x) {
      if (!is.null(plot.settings[[x]])) {
        if(is.list(plot.settings[[x]])){
          rep_len(plot.settings[[x]], length.out = length(temp.recordType))

        }else{
          rep_len(list(plot.settings[[x]]), length.out = length(temp.recordType))
        }

      } else{
        plot.settings[[x]]
      }
    })

    ##expand abline
    if(!is.null(abline)){
      abline.names <- rep_len(names(abline), length.out = length(temp.recordType))
      abline <- rep_len(abline, length.out = length(temp.recordType))
      names(abline) <- abline.names
    }


    ##(2) PLOT values
    for(k in 1:length(temp.recordType)) {

      ###get type of curves
      temp.object <-
        get_RLum(object, recordType = temp.recordType[k], drop = FALSE)

      ##get structure
      object.structure  <- structure_RLum(temp.object)

      ##now get the real list object (note the argument recursive = FALSE)
      object.list <-
        get_RLum(object, recordType = temp.recordType[k], recursive = FALSE)

      ## limit number of records shown ... show always first and last;
      ## distribute the rest
      if(!is.null(records_max) && records_max[1] > 2){
        records_show <- ceiling(seq(1,length(object.list), length.out = records_max))
        object.list[(1:length(object.list))[-records_show]] <- NULL
      }

      ##transform values to data.frame and norm values
      temp.data.list <- lapply(1:length(object.list), function(x) {

        ## set curve transformation if wanted
        if (grepl("IRSL|OSL", object.list[[x]]@recordType) &&
            curve.transformation != "None") {

          ## get the actual function from the parameter value and apply it
          object.list[[x]] <- get(curve.transformation)(object.list[[x]])
        }

        temp.data <- as(object.list[[x]], "data.frame")

        ## curve normalisation
        if (plot.settings$norm[[k]][1] == TRUE ||
            plot.settings$norm[[k]][1] %in% c("max", "last", "huot")) {
          temp.data[[2]] <- .normalise_curve(temp.data[[2]],
                                             plot.settings$norm[[k]])
        }

        return(temp.data)

      })

      ##set plot parameters
      ##main
      main <- if (!is.null(plot.settings$main[[k]])) {
        plot.settings$main[[k]]
      } else{
        paste0(temp.recordType[[k]], " combined")
      }

      ##xlab
      xlab <- if(!is.null(plot.settings$xlab[[k]])){
        plot.settings$xlab[[k]]
      }else{
        if (temp.recordType[[k]] == "TL")
          "Temperature [\u00B0C]"
        else
          "Time [s]"
      }

      ##ylab
      ylab <- if(!is.null(plot.settings$ylab[[k]])){
        plot.settings$ylab[[k]]
      }else{
        paste0(temp.recordType[[k]], " [a.u.]")
      }

      ##xlim
      xlim <- if (!is.null(plot.settings$xlim[[k]]) & length(plot.settings$xlim[[k]]) >1) {
        plot.settings$xlim[[k]]
      } else {
        c(min(object.structure$x.min), max(object.structure$x.max))
      }
      if (grepl("x", plot.settings$log[[k]], ignore.case = TRUE))
        xlim[which(xlim == 0)] <- 1

      ##ylim
      ylim <- if (!is.null(plot.settings$ylim[[k]]) & length(plot.settings$ylim[[k]]) > 1) {
        plot.settings$ylim[[k]]
      } else {
        range(unlist(lapply(X = temp.data.list, FUN = function(x){
          range(x[,2])
        })))
      }

      if (grepl("y", plot.settings$log[[k]], ignore.case = TRUE))
        ylim[which(ylim == 0)] <- 1

      ##col (again)
      col <- if(length(plot.settings$col[[k]]) > 1 || plot.settings$col[[k]][1] != "auto"){
        plot.settings$col[[k]]

      }else{
        col <- get("col", pos = .LuminescenceEnv)
      }

      ##if length of provided colours is < the number of objects, just one colour is supported
      if (length(col) < length(object.list)) {
        col <- rep_len(col, length(object.list))
      }

      ##lty
      lty <- plot.settings$lty[[k]]
      if (length(plot.settings$lty[[k]]) < length(object.list)) {
        lty <- rep(plot.settings$lty[[k]], times = length(object.list))
      }

      ##pch
      pch <- plot.settings$pch[[k]]
      if (length(plot.settings$pch[[k]]) < length(object.list)) {
        pch <- rep(plot.settings$pch[[k]], times = length(object.list))
      }

      ##legend.text
      legend.text <- if(!is.null(plot.settings$legend.text[[k]])){
        plot.settings$legend.text[[k]]

      }else{
        if(!is.null(records_max) && records_max[1] > 2) {
          paste("Curve", records_show)

        } else {
          paste("Curve", 1:length(object.list))
        }
      }

      ##legend.col
      legend.col <- plot.settings$legend.col[[k]]

      ##legend.pos
      legend.pos <- if(!is.null(plot.settings$legend.pos[[k]])){
        plot.settings$legend.pos[[k]]

      }else{
        "topright"
      }

      if (legend.pos == "outside") {
        par.default.outside <- par()[c("mar", "xpd")]
        par(mar = c(5.1, 4.1, 4.1, 8.1))
      }

      ##open plot area
      plot(
        NA,NA,
        xlim = xlim,
        ylim = ylim,
        main = main,
        xlab = xlab,
        ylab = ylab,
        log = plot.settings$log[[k]],
        sub = plot.settings$sub_title[[k]]
      )

      ##plot single curve values
      ## ...?Why using matplot is a bad idea: The channel resolution might be different
      for (n in 1:length(temp.data.list)) {

        ##smooth
        ##Why here again ... because the call differs from the one before, where the argument
        ##is passed to plot_RLum.Data.Curve()
        if(plot.settings$smooth[[k]]){

          k_factor <- ceiling(length(temp.data.list[[n]][, 2])/100)
          temp.data.list[[n]][, 2] <- .smoothing(temp.data.list[[n]][, 2],
                                                 k = k_factor, fill = NA,
                                                 align = "center")
        }

        ##remove 0 values if plotted on a log-scale
        # y-Axis
        if (grepl("y", plot.settings$log[[k]], ignore.case = TRUE))
          temp.data.list[[n]] <- temp.data.list[[n]][which(temp.data.list[[n]]$y > 0), ]
        # x-Axis
        if (grepl("x", plot.settings$log[[k]], ignore.case = TRUE))
          temp.data.list[[n]] <- temp.data.list[[n]][which(temp.data.list[[n]]$x > 0), ]

        ##print lines
        if (plot.settings$type[[k]] == "l" | plot.settings$type[[k]] == "b" ) {
          lines(
            temp.data.list[[n]],
            col = col[n],
            lty = lty[n],
            lwd = plot.settings$lwd[[k]]
          )
        }

        ##add points if requested
        if (plot.settings$type[[k]] == "p" | plot.settings$type[[k]] == "b" ) {
          points(
            temp.data.list[[n]],
            col = col[n],
            pch = pch[n],
          )
        }
      }

      ##add abline
      if(!is.null(abline[[k]])){
        do.call(what = "abline", args = abline[k])
      }

      ##mtext
      mtext(plot.settings$mtext[[k]], side = 3, cex = .8 * plot.settings$cex[[k]])

      ##if legend is outside of the plotting area we need to allow overplotting
      ##AFTER all lines have been drawn
      if (legend.pos == "outside") {
        par(xpd = TRUE)

        # determine legend position on log(y) scale
        ypos <- par()$usr[4]
        if (grepl("y", plot.settings$log[[k]], ignore.case = TRUE))
          ypos <- 10^par()$usr[4]

        # determine position on log(x) scale
        xpos <- par()$usr[2]
        if (grepl("x", plot.settings$log[[k]], ignore.case = TRUE))
          xpos <- 10^par()$usr[2]
      }

      ##legend
      if (plot.settings$legend[[k]]) {
        legend(
          x = ifelse(legend.pos == "outside", xpos, legend.pos),
          y = ifelse(legend.pos == "outside", ypos, NULL),
          legend = legend.text,
          lwd = plot.settings$lwd[[k]],
          lty = plot.settings$lty[[k]],
          col = if (is.null(legend.col)) {
            col[1:length(object.list)]
          } else{
            legend.col
          },
          bty = "n",
          cex = 0.8 * plot.settings$cex[[k]]
        )

        # revert the over plotting
        if (legend.pos == "outside")
          par(xpd = FALSE)
      }
    }

    ##reset graphic settings
    if (exists("par.default.outside")) {
      par(par.default.outside)
    }
    par(par.default)
  }
}
