#' Plot function for an RLum.Analysis S4 class object
#'
#' The function provides a standardised plot output for curve data of an
#' RLum.Analysis S4 class object
#'
#' The function produces a multiple plot output. A file output is recommended
#' (e.g., \code{\link{pdf}}).
#'
#' \bold{curve.transformation}\cr
#'
#' This argument allows transforming continuous wave (CW) curves to pseudo
#' (linear) modulated curves. For the transformation, the functions of the
#' package are used. Currently, it is not possible to pass further arguments to
#' the transformation functions. The argument works only for \code{ltype}
#' \code{OSL} and \code{IRSL}.\cr
#'
#' Please note: The curve transformation within this functions works roughly,
#' i.e. every IRSL or OSL curve is transformed, without considerung whether it
#' is measured with the PMT or not! However, for a fast look it might be
#' helpful.\cr
#'
#'
#' @param object \code{\linkS4class{RLum.Analysis}} (\bold{required}): S4
#' object of class \code{RLum.Analysis}
#'
#' @param subset named \code{\link{list}} (optional): subsets elements for plotting. The
#' arguments in the named \code{\link{list}} will be directly passed to the function \code{\link{get_RLum}}
#' (e.g., \code{subset = list(curveType = "measured")})
#'
#' @param nrows \code{\link{integer}} (optional): sets number of rows for
#' plot output, if nothing is set the function tries to find a value.
#'
#' @param ncols \code{\link{integer}} (optional): sets number of columns
#' for plot output, if nothing is set the function tries to find a value.
#'
#' @param abline \code{\link{list}} (optional): allows to add ablines to the plot. Argument are provided
#' in a list and will be forwared to the function \code{\link{abline}}, e.g., \code{list(v = c(10, 100))}
#' adds two vertical lines add 10 and 100 to all plots. In contrast \code{list(v = c(10), v = c(100)}
#' adds a vertical at 10 to the first and a vertical line at 100 to the 2nd plot.
#'
#' @param combine \code{\link{logical}} (with default): allows to combine all
#' \code{\linkS4class{RLum.Data.Curve}} objects in one single plot.
#'
#' @param curve.transformation \code{\link{character}} (optional): allows
#' transforming CW-OSL and CW-IRSL curves to pseudo-LM curves via
#' transformation functions. Allowed values are: \code{CW2pLM}, \code{CW2pLMi},
#' \code{CW2pHMi} and \code{CW2pPMi}. See details.
#'
#' @param plot.single \code{\link{logical}} (with default): global par settings are
#' considered, normally this should end in one plot per page
#'
#' @param \dots further arguments and graphical parameters will be passed to
#' the \code{plot} function. Supported arguments: \code{main}, \code{mtext},
#' \code{log}, \code{lwd}, \code{lty} \code{type}, \code{pch}, \code{col},
#' \code{norm}, \code{xlim},\code{ylim}, \code{xlab}, \code{ylab}... and for \code{combine = TRUE}
#' also: \code{sub}, \code{legend}, \code{legend.text}, \code{legend.pos} (typical plus 'outside'), \code{legend.col}, \code{smooth}.
#' All arguments can be provided as \code{vector} or \code{list} to gain in full control
#' of all plot settings.
#'
#' @return Returns multiple plots.
#'
#' @note Not all arguments available for \code{\link{plot}} will be passed!
#' Only plotting of \code{RLum.Data.Curve} and \code{RLum.Data.Spectrum}
#' objects are currently supported.\cr
#'
#' @section Function version: 0.3.6
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#'
#' @seealso \code{\link{plot}}, \code{\link{plot_RLum}},
#' \code{\link{plot_RLum.Data.Curve}}
#'
#' @references #
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
#'##plot (combine) TL curves in one plot
#'plot_RLum.Analysis(
#' temp,
#' subset = list(recordType = "TL"),
#' combine = TRUE,
#' norm = TRUE,
#' abline = list(v = c(110))
#' )
#'
#' @export
plot_RLum.Analysis <- function(
  object,
  subset,
  nrows,
  ncols,
  abline = NULL,
  combine = FALSE,
  curve.transformation,
  plot.single = FALSE,
  ...
){

  # Integrity check ----------------------------------------------------------------------------

  ##check if object is of class RLum.Analysis (lists are handled via plot_RLum())
  if (!is(object, "RLum.Analysis")) {
    stop("[plot_RLum.Analysis()] Input object is not of type 'RLum.Analysis'")

  }

  # Make selection if wanted  -------------------------------------------------------------------

  if(!missing(subset)){

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
    col = "black",
    norm = FALSE,
    sub = NULL,
    cex = 1,
    legend = TRUE,
    legend.text = NULL,
    legend.pos = NULL,
    legend.col = NULL,
    smooth = FALSE
  )

  plot.settings <- modifyList(x = plot.settings, val = list(...), keep.null = TRUE)

  ##try to find optimal parameters, this is however, a little bit stupid, but
  ##better than without any presetting

  if(combine){
    n.plots <- length(unique(as.character(structure_RLum(object)$recordType)))

  }else{
    n.plots <- length_RLum(object)

  }


  if (missing(ncols) | missing(nrows)) {
    if (missing(ncols) & !missing(nrows)) {
      if (n.plots  == 1) {
        ncols <- 1

      } else{
        ncols <- 2

      }

    }
    else if (!missing(ncols) & missing(nrows)) {
      if (n.plots  == 1) {
        nrows <- 1

      }
      else if (n.plots  > 1 & n.plots <= 4) {
        nrows <- 2

      } else{
        nrows <- 3

      }


    } else{
      if (n.plots  == 1) {
        nrows <- 1
        ncols <- 1

      }
      else if (n.plots  > 1 & n.plots  <= 2) {
        nrows <- 1
        ncols <- 2

      } else if (n.plots  > 2 & n.plots <= 4) {
        nrows <- 2
        ncols <- 2

      }
      else{
        nrows <- 3
        ncols <- 2

      }

    }

  }


  # Plotting ------------------------------------------------------------------

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##(1) NORMAL (combine == FALSE)
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(!combine || length(object@records) == 1){

    ##show warning message
    if(combine & length(object@records) == 1){
      warning("Nothing to combine, object contains a single curve.")

    }

    ##grep RLum.Data.Curve or RLum.Data.Spectrum objects
    temp <- lapply(1:length(object@records), function(x){

      if(is(object@records[[x]], "RLum.Data.Curve") ||
         is(object@records[[x]], "RLum.Data.Spectrum")){

        object@records[[x]]

      }})

    ##calculate number of pages for mtext
    if (length(temp) %% (nrows * ncols) > 0) {
      n.pages <- round(length(temp) / (nrows * ncols), digits = 0) + 1

    } else{
      n.pages <- length(temp) / (nrows * ncols)

    }

    ##set par
    par.default <- par("mfrow")
    if(!plot.single){on.exit(par(mfrow = par.default))}
    if(!plot.single) {
      par(mfrow = c(nrows, ncols))
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
    for(i in 1:length(temp)){

      if(is(temp[[i]], "RLum.Data.Curve") == TRUE){

        ##set curve transformation if wanted
        if((grepl("IRSL", temp[[i]]@recordType) | grepl("OSL", temp[[i]]@recordType)) &
           !missing(curve.transformation)){

          if(curve.transformation=="CW2pLM"){
            temp[[i]] <- CW2pLM(temp[[i]])

          }else if(curve.transformation=="CW2pLMi"){
            temp[[i]] <- CW2pLMi(temp[[i]])

          }else if(curve.transformation=="CW2pHMi"){
            temp[[i]]<- CW2pHMi(temp[[i]])

          }else if(curve.transformation=="CW2pPMi"){
            temp[[i]] <- CW2pPMi(temp[[i]])

          }else{
            warning("Function for 'curve.transformation' is unknown. No transformation is performed.")

          }

        }


        ##check plot settings and adjust
        ##xlim
        if (!is.null(plot.settings$xlim)) {
          xlim.set <- plot.settings$xlim[[i]]
          if (plot.settings$xlim[[i]][1] < min(temp[[i]]@data[,1])) {
            xlim.set[1] <- min(temp[[i]]@data[,1])
          }
          if (plot.settings$xlim[[i]][2] > max(temp[[i]]@data[,1])) {
            xlim.set[2] <- max(temp[[i]]@data[,1])
          }

        }else{
          xlim.set <- plot.settings$xlim[[i]]

        }

        ##ylim
        if (!is.null(plot.settings$ylim)) {
          ylim.set <- plot.settings$ylim
          if (plot.settings$ylim[[i]][1] < min(temp[[i]]@data[,2])) {
            ylim.set[1] <- min(temp[[i]]@data[,2])
          }
          if (plot.settings$ylim[[i]][2] > max(temp[[i]]@data[,2])) {
            ylim.set[2] <- max(temp[[i]]@data[,2])
          }

        }else{
          ylim.set <- plot.settings$ylim[[i]]

        }

        ##col
        if (unique(plot.settings$col) != "black") {
          col <- plot.settings$col[i]
        } else{
          if (grepl("IRSL", temp[[i]]@recordType)) {
            col <- "red"
          } else
            if (grepl("OSL", temp[[i]]@recordType)) {
              col <- "blue"
            } else
            {
              col <- plot.settings$col[[i]]
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
        plot_RLum.Data.Curve(
          temp[[i]],
          col = col,
          mtext = if(!is.null(plot.settings$mtext[[i]])){
            plot.settings$mtext[[i]]
          }else{
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
          pch = plot.settings$pch[[i]],
          cex = plot.settings$cex[[i]],
          smooth = plot.settings$smooth[[i]],
          ...
        )

        ##add abline
        if(!is.null(abline[[i]])){
          do.call(what = "abline", args = abline[i])

        }


      } else if(is(temp[[i]], "RLum.Data.Spectrum")) {

        plot_RLum.Data.Spectrum(temp[[i]],
                                mtext =  if(!is.null(plot.settings$mtext[[i]])){
                                  plot.settings$mtext[[i]]
                                }else{
                                  paste("#", i, sep = "")
                                },
                                par.local = FALSE,
                                main = if(!is.null(plot.settings$main)){
                                  plot.settings$main
                                }else{
                                  temp[[i]]@recordType
                                })

      }

    }#end for loop

  }else{

    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##(2) NORMAL (combine == TRUE)
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##(1) check RLum objects in the set
    object.list <- get_RLum(object)

    sapply(1:length(object.list), function(x){
      if(is(object.list[[x]])[1] != "RLum.Data.Curve"){
        stop("[plot_RLum.Analysis()] Using 'combine' is limited to 'RLum.Data.Curve' objects.")

      }

    })


    ##account for different curve types, combine similar
    temp.object.structure  <- structure_RLum(object)
    temp.recordType <- as.character(unique(temp.object.structure$recordType))


    ##change graphic settings
    if(!plot.single){
      par.default <- par()[c("cex", "mfrow")]

      if(!missing(ncols) & !missing(nrows)){
        par(mfrow = c(nrows, ncols))

      }


      ##this 2nd par request is needed as seeting mfrow resets the par settings ... this might
      ##not be wanted
      par(cex = plot.settings$cex[1])

    }else{
      par.default <- par()[c("cex")]
      par(cex = plot.settings$cex)

    }


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

      ##prevent problems for non set argument
      if (missing(curve.transformation)) {
        curve.transformation <- "None"
      }

      ##transform values to data.frame and norm values
      temp.data.list <- lapply(1:length(object.list), function(x) {
        ##set curve transformation if wanted

        if (grepl("IRSL", object.list[[x]]@recordType) |
            grepl("OSL", object.list[[x]]@recordType)) {
          if (curve.transformation == "CW2pLM") {
            object.list[[x]] <- CW2pLM(object.list[[x]])

          }else if (curve.transformation == "CW2pLMi") {
            object.list[[x]] <- CW2pLMi(object.list[[x]])

          }else if (curve.transformation == "CW2pHMi") {
            object.list[[x]] <- CW2pHMi(object.list[[x]])

          }else if (curve.transformation == "CW2pPMi") {
            object.list[[x]] <- CW2pPMi(object.list[[x]])

          }

        }


        temp.data <- as(object.list[[x]], "data.frame")

        ##normalise curves if argument has been set
        if (plot.settings$norm[[k]]) {
          temp.data[,2] <- temp.data[,2] / max(temp.data[,2])

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
        switch(temp.recordType[[k]],
               "TL" = "Temperature [\u00B0C]",
               "IRSL" = "Time [s]",
               "OSL" = "Time [s]",
               "RF" = "Time [s]",
               "RBR" = "Time [s]",
               "LM-OSL" = "Time [s]"
        )

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

      ##ylim
      ylim <- if (!is.null(plot.settings$ylim[[k]]) & length(plot.settings$ylim[[k]]) > 1) {
        plot.settings$ylim[[k]]
      } else {
        range(unlist(lapply(X = temp.data.list, FUN = function(x){
          range(x[,2])
        })))

      }

      ##col (again)
      col <- if(length(plot.settings$col[[k]]) > 1 || plot.settings$col[[k]][1] != "black"){
        plot.settings$col[[k]]

      }else{
        col <- get("col", pos = .LuminescenceEnv)
      }

      ##if length of provided colours is < the number of objects, just one colour is supported
      if (length(col) < length(object.list)) {
        col <- rep_len(col, length(object.list))

      }

      ##lty
      if (length(plot.settings$lty[[k]]) < length(object.list)) {
        lty <- rep(plot.settings$lty[[k]], times = length(object.list))

      }else{
        lty <- plot.settings$lty[[k]]

      }

      ##legend.text
      legend.text <- if(!is.null(plot.settings$legend.text[[k]])){
        plot.settings$legend.text[[k]]

      }else{
        paste("Curve", 1:length(object.list))

      }

      ##legend.col
      legend.col <- if(!is.null(plot.settings$legend.col[[k]])){
        plot.settings$legend.col[[k]]

      }else{
        NULL

      }

      ##legend.pos
      legend.pos <- if(!is.null(plot.settings$legend.pos[[k]])){
        plot.settings$legend.pos[[k]]

      }else{
        "topright"

      }

      if (legend.pos == "outside") {
        par.default.outside <- par()[c("mar", "xpd")]
        par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
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
        sub = plot.settings$sub[[k]]
      )

      ##plot single curve values
      ## ...?Why using matplot is a bad idea: The channel resolution might be different
      for (n in 1:length(temp.data.list)) {


        ##smooth
        ##Why here again ... because the call differs from the one before, where the argument
        ##is passed to plot_RLum.Data.Curve()
        if(plot.settings$smooth[[k]]){

          k_factor <- ceiling(length(temp.data.list[[n]][, 2])/100)
          temp.data.list[[n]][, 2] <- zoo::rollmean(temp.data.list[[n]][, 2],
                                            k = k_factor, fill = NA)
        }

        ##print lines
        lines(temp.data.list[[n]],
              col = col[n],
              lty = lty[n],
              lwd = plot.settings$lwd[[k]])

      }

      ##add abline
      if(!is.null(abline[[k]])){
        do.call(what = "abline", args = abline[k])

      }

      ##mtext
      mtext(plot.settings$mtext[[k]], side = 3, cex = .8 * plot.settings$cex[[k]])

      ##legend
      if (plot.settings$legend[[k]]) {
        legend(
          x = ifelse(legend.pos == "outside", par()$usr[2], legend.pos),
          y = ifelse(legend.pos == "outside", par()$usr[4], NULL),
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

      }

    }

    ##reset graphic settings
    if (exists("par.default.outside")) {
      par(par.default.outside)
      rm(par.default.outside)
    }
    par(par.default)
    rm(par.default)

  }

}
