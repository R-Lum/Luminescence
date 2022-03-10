#' @title Visualise dose recovery test results
#'
#' @description The function provides a standardised plot output for dose recovery test
#' measurements.
#'
#' @details Procedure to test the accuracy of a measurement protocol to reliably
#' determine the dose of a specific sample. Here, the natural signal is erased
#' and a known laboratory dose administered which is treated as unknown. Then
#' the De measurement is carried out and the degree of congruence between
#' administered and recovered dose is a measure of the protocol's accuracy for
#' this sample.\cr
#' In the plot the normalised De is shown on the y-axis, i.e. obtained De/Given Dose.
#'
#' @param values [RLum.Results-class] or [data.frame] (**required**):
#' input values containing at least De and De error. To plot
#' more than one data set in one figure, a `list` of the individual data
#' sets must be provided (e.g. `list(dataset.1, dataset.2)`).
#'
#' @param given.dose [numeric] (*optional*):
#' given dose used for the dose recovery test to normalise data.
#' If only one given dose is provided this given dose is valid for all input
#' data sets (i.e., `values` is a list).  Otherwise a given dose for each input
#' data set has to be provided (e.g., `given.dose = c(100,200)`).
#' If `given.dose` in `NULL` the values are plotted without normalisation
#' (might be useful for preheat plateau tests).
#' **Note:** Unit has to be the same as from the input values (e.g., Seconds or
#' Gray).
#'
#' @param error.range [numeric]:
#' symmetric error range in percent will be shown as dashed lines in the plot.
#' Set `error.range` to 0 to void plotting of error ranges.
#'
#' @param preheat [numeric]:
#' optional vector of preheat temperatures to be used for grouping the De values.
#' If specified, the temperatures are assigned to the x-axis.
#'
#' @param boxplot [logical]:
#' optionally plot values, that are grouped by preheat temperature as boxplots.
#'  Only possible when `preheat` vector is specified.
#'
#' @param mtext [character]:
#' additional text below the plot title.
#'
#' @param summary [character] (*optional*):
#' adds numerical output to the plot.  Can be one or more out of:
#' - `"n"` (number of samples),
#' - `"mean"` (mean De value),
#' - `"weighted$mean"` (error-weighted mean),
#' - `"median"` (median of the De values),
#' - `"sd.rel"` (relative standard deviation in percent),
#' - `"sd.abs"` (absolute standard deviation),
#' - `"se.rel"` (relative standard error) and
#' - `"se.abs"` (absolute standard error)
#'
#' and all other measures returned by the function [calc_Statistics].
#'
#' @param summary.pos [numeric] or [character] (*with default*):
#' optional position coordinates or keyword (e.g. `"topright"`)
#' for the statistical summary. Alternatively, the keyword `"sub"` may be
#' specified to place the summary below the plot header. However, this latter
#' option in only possible if `mtext` is not used.
#'
#' @param legend [character] vector (*optional*):
#' legend content to be added to the plot.
#'
#' @param legend.pos [numeric] or [character] (*with default*):
#' optional position coordinates or keyword (e.g. `"topright"`) for the
#' legend to be plotted.
#'
#' @param par.local [logical] (*with default*):
#' use local graphical parameters for plotting, e.g. the plot is shown in one
#' column and one row. If `par.local = FALSE`, global parameters are inherited,
#' i.e. parameters provided via `par()` work
#'
#' @param na.rm [logical]: indicating whether `NA` values are
#' removed before plotting from the input data set
#'
#' @param ... further arguments and graphical parameters passed to [plot], supported are:
#' `xlab`, `ylab`, `xlim`, `ylim`, `main`, `cex`, `las` and `pch``
#'
#' @return A plot is returned.
#'
#' @note
#' Further data and plot arguments can be added by using the appropriate R
#' commands.
#'
#' @section Function version: 0.1.14
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)\cr
#' Michael Dietze, GFZ Potsdam (Germany)
#'
#' @seealso [plot]
#'
#' @references
#' Wintle, A.G., Murray, A.S., 2006. A review of quartz optically
#' stimulated luminescence characteristics and their relevance in
#' single-aliquot regeneration dating protocols. Radiation Measurements, 41,
#' 369-391.
#'
#' @keywords dplot
#'
#' @examples
#'
#' ## read example data set and misapply them for this plot type
#' data(ExampleData.DeValues, envir = environment())
#'
#' ## plot values
#' plot_DRTResults(
#'   values = ExampleData.DeValues$BT998[7:11,],
#'   given.dose = 2800,
#'   mtext = "Example data")
#'
#' ## plot values with legend
#' plot_DRTResults(
#'   values = ExampleData.DeValues$BT998[7:11,],
#'   given.dose = 2800,
#'   legend = "Test data set")
#'
#' ## create and plot two subsets with randomised values
#' x.1 <- ExampleData.DeValues$BT998[7:11,]
#' x.2 <- ExampleData.DeValues$BT998[7:11,] * c(runif(5, 0.9, 1.1), 1)
#'
#' plot_DRTResults(
#'   values = list(x.1, x.2),
#'   given.dose = 2800)
#'
#' ## some more user-defined plot parameters
#' plot_DRTResults(
#'   values = list(x.1, x.2),
#'   given.dose = 2800,
#'   pch = c(2, 5),
#'   col = c("orange", "blue"),
#'   xlim = c(0, 8),
#'   ylim = c(0.85, 1.15),
#'   xlab = "Sample aliquot")
#'
#' ## plot the data with user-defined statistical measures as legend
#' plot_DRTResults(
#'   values = list(x.1, x.2),
#'   given.dose = 2800,
#'   summary = c("n", "weighted$mean", "sd.abs"))
#'
#' ## plot the data with user-defined statistical measures as sub-header
#' plot_DRTResults(
#'   values = list(x.1, x.2),
#'   given.dose = 2800,
#'   summary = c("n", "weighted$mean", "sd.abs"),
#'   summary.pos = "sub")
#'
#' ## plot the data grouped by preheat temperatures
#' plot_DRTResults(
#'   values = ExampleData.DeValues$BT998[7:11,],
#'   given.dose = 2800,
#'   preheat = c(200, 200, 200, 240, 240))
#'
#' ## read example data set and misapply them for this plot type
#' data(ExampleData.DeValues, envir = environment())
#'
#' ## plot values
#' plot_DRTResults(
#'   values = ExampleData.DeValues$BT998[7:11,],
#'   given.dose = 2800,
#'   mtext = "Example data")
#'
#' ## plot two data sets grouped by preheat temperatures
#' plot_DRTResults(
#'   values = list(x.1, x.2),
#'   given.dose = 2800,
#'   preheat = c(200, 200, 200, 240, 240))
#'
#' ## plot the data grouped by preheat temperatures as boxplots
#' plot_DRTResults(
#'   values = ExampleData.DeValues$BT998[7:11,],
#'   given.dose = 2800,
#'   preheat = c(200, 200, 200, 240, 240),
#'   boxplot = TRUE)
#'
#' @md
#' @export
plot_DRTResults <- function(
  values,
  given.dose = NULL,
  error.range = 10,
  preheat,
  boxplot = FALSE,
  mtext,
  summary,
  summary.pos,
  legend,
  legend.pos,
  par.local = TRUE,
  na.rm  = FALSE,
  ...
){

  ## Validity checks ----------------------------------------------------------

  ##avoid crash for wrongly set boxlot argument
  if(missing(preheat) & boxplot == TRUE){

    warning("[plot_DRTResults()] Option 'boxplot' not valid without any value in 'preheat'. Reset to FALSE.")
    boxplot  <- FALSE

  }

  if(missing(summary) == TRUE) {summary <- NULL}
  if(missing(summary.pos) == TRUE) {summary.pos <- "topleft"}
  if(missing(legend.pos) == TRUE) {legend.pos <- "topright"}
  if(missing(mtext) == TRUE) {mtext <- ""}

  ## Homogenise and check input data
  if(is(values, "list") == FALSE) {values <- list(values)}

  for(i in 1:length(values)) {
    if(is(values[[i]], "RLum.Results")==FALSE &
         is(values[[i]], "data.frame")==FALSE){
      stop(paste("[plot_DRTResults()] Wrong input data format",
                 "(!= 'data.frame' or 'RLum.Results')"))
    } else {
      if(is(values[[i]], "RLum.Results")==TRUE){
        values[[i]] <- get_RLum(values[[i]])[,1:2]
      }
    }
  }

  ## Check input arguments ----------------------------------------------------
  for(i in 1:length(values)) {

    ##check for preheat temperature values
    if(missing(preheat) == FALSE) {
      if(length(preheat) != nrow(values[[i]])){
        stop("[plot_DRTResults()] number of preheat temperatures != De values!")
      }
    }

    ##remove NA values; yes Micha, it is not that simple
    if(na.rm  == TRUE){

      ##currently we assume that all input data sets comprise a similar of data
      if(!missing(preheat) & i == length(values)){

        ##find and mark NA value indicies
        temp.NA.values <- unique(c(which(is.na(values[[i]][,1])), which(is.na(values[[i]][,2]))))

        ##remove preheat entries
        preheat <- preheat[-temp.NA.values]

      }

      values[[i]] <- na.exclude(values[[i]])

    }
  }

  ## create global data set
  values.global <- NULL
  n.values <- NULL
  for(i in 1:length(values)) {
    values.global <- rbind(values.global, values[[i]])
    n.values <- c(n.values, nrow(values[[i]]))
  }

  ## Set plot format parameters -----------------------------------------------
  extraArgs <- list(...) # read out additional arguments list

  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else
  {"Dose recovery test"}

  xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else {
    ifelse(missing(preheat) == TRUE, "# Aliquot", "Preheat temperature [\u00B0C]")
  }

  ylab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} else
  {if(!is.null(given.dose)){
    expression(paste("Normalised ", D[e], sep=""))
  }else{expression(paste(D[e], " [s]"), sep = "")}}

  xlim <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} else
  {c(1, max(n.values) + 1)}

  ylim <- if("ylim" %in% names(extraArgs)) {extraArgs$ylim} else
  {c(0.75, 1.25)} #check below for further corrections if boundaries exceed set range

  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else {1}

  pch <- if("pch" %in% names(extraArgs)) {extraArgs$pch} else {
    abs(seq(from = 20, to = -100))
  }

  ##axis labels
  las <- if("las" %in% names(extraArgs)) extraArgs$las else 0

  fun <- if("fun" %in% names(extraArgs)) {extraArgs$fun} else {FALSE}

  ## calculations and settings-------------------------------------------------

  ## normalise data if given.dose is given
  if(!is.null(given.dose)){

    if(length(given.dose) > 1){

      if(length(values) < length(given.dose)){
        stop("[plot_DRTResults()] 'given.dose' > number of input data sets!", call. = FALSE)

      }

      for(i in 1:length(values)) {
        values[[i]] <- values[[i]]/given.dose[i]
      }

    }else{

      for(i in 1:length(values)) {
        values[[i]] <- values[[i]]/given.dose
      }

    }
  }

  ##correct ylim for data set which exceed boundaries
  if((max(sapply(1:length(values), function(x){max(values[[x]][,1], na.rm = TRUE)}))>1.25 |
        min(sapply(1:length(values), function(x){min(values[[x]][,1], na.rm = TRUE)}))<0.75) &
       ("ylim" %in% names(extraArgs)) == FALSE){

    ylim <- c(
      min(sapply(1:length(values), function(x){
        min(values[[x]][,1], na.rm = TRUE) - max(values[[x]][,2], na.rm = TRUE)})),
      max(sapply(1:length(values), function(x){
        max(values[[x]][,1], na.rm = TRUE) + max(values[[x]][,2], na.rm = TRUE)})))

  }


  ## optionally group data by preheat temperature
  if(missing(preheat) == FALSE) {
    modes <- as.numeric(rownames(as.matrix(table(preheat))))
    values.preheat <- list(NA)
    values.boxplot <- list(NA)
    for(i in 1:length(modes)) {
      for(j in 1:length(values)) {
        values.preheat[[length(values.preheat) + 1]] <-
          cbind(values[[j]][preheat == modes[i],],
                preheat[preheat == modes[i]])
        values.boxplot[[length(values.boxplot) + 1]] <-
          values[[j]][preheat == modes[i],1]
      }
      j <- 1
    }
    values.preheat[[1]] <- NULL
    values.boxplot[[1]] <- NULL
    modes.plot <- rep(modes, each = length(values))

  } else {
    modes <- 1

  }

  ## assign colour indices
  col <- if("col" %in% names(extraArgs)) {extraArgs$col} else {
    if(missing(preheat) == TRUE) {
      rep(seq(from = 1, to = length(values)), each = length(modes))
    } else {
      rep(seq(from = 1, to = length(values)), length(modes))
    }
  }

  ## calculate and paste statistical summary
  if(summary.pos[1] != "sub") {
    label.text <- lapply(1:length(values), function(i) {
      .create_StatisticalSummaryText(
        x = calc_Statistics(values[[i]]),
        keywords = summary,
        digits = 2,
        sep = " \n",
        prefix = paste(rep("\n", (i - 1) * length(summary)), collapse = "")
      )
    })

  }else{
    label.text <- lapply(1:length(values), function(i) {
      .create_StatisticalSummaryText(
        x = calc_Statistics(values[[i]]),
        keywords = summary,
        digits = 2,
        sep = " | "
      )
    })

  }

  ## convert keywords into summary placement coordinates
  if(missing(summary.pos) == TRUE) {
    summary.pos <- c(xlim[1], ylim[2])
    summary.adj <- c(0, 1)
  } else if(length(summary.pos) == 2) {
    summary.pos <- summary.pos
    summary.adj <- c(0, 1)
  } else if(summary.pos[1] == "topleft") {
    summary.pos <- c(xlim[1], ylim[2])
    summary.adj <- c(0, 1)
  } else if(summary.pos[1] == "top") {
    summary.pos <- c(mean(xlim), ylim[2])
    summary.adj <- c(0.5, 1)
  } else if(summary.pos[1] == "topright") {
    summary.pos <- c(xlim[2], ylim[2])
    summary.adj <- c(1, 1)
  }  else if(summary.pos[1] == "left") {
    summary.pos <- c(xlim[1], mean(ylim))
    summary.adj <- c(0, 0.5)
  } else if(summary.pos[1] == "center") {
    summary.pos <- c(mean(xlim), mean(ylim))
    summary.adj <- c(0.5, 0.5)
  } else if(summary.pos[1] == "right") {
    summary.pos <- c(xlim[2], mean(ylim))
    summary.adj <- c(1, 0.5)
  }else if(summary.pos[1] == "bottomleft") {
    summary.pos <- c(xlim[1], ylim[1])
    summary.adj <- c(0, 0)
  } else if(summary.pos[1] == "bottom") {
    summary.pos <- c(mean(xlim), ylim[1])
    summary.adj <- c(0.5, 0)
  } else if(summary.pos[1] == "bottomright") {
    summary.pos <- c(xlim[2], ylim[1])
    summary.adj <- c(1, 0)
  }

  ## convert keywords into legend placement coordinates
  if(missing(legend.pos) == TRUE) {
    legend.pos <- c(xlim[2], ylim[2])
    legend.adj <- c(1, 1)
  } else if(length(legend.pos) == 2) {
    legend.pos <- legend.pos
    legend.adj <- c(0, 1)
  } else if(legend.pos[1] == "topleft") {
    legend.pos <- c(xlim[1], ylim[2])
    legend.adj <- c(0, 1)
  } else if(legend.pos[1] == "top") {
    legend.pos <- c(mean(xlim), ylim[2])
    legend.adj <- c(0.5, 1)
  } else if(legend.pos[1] == "topright") {
    legend.pos <- c(xlim[2], ylim[2])
    legend.adj <- c(1, 1)
  } else if(legend.pos[1] == "left") {
    legend.pos <- c(xlim[1], mean(ylim))
    legend.adj <- c(0, 0.5)
  } else if(legend.pos[1] == "center") {
    legend.pos <- c(mean(xlim), mean(ylim))
    legend.adj <- c(0.5, 0.5)
  } else if(legend.pos[1] == "right") {
    legend.pos <- c(xlim[2], mean(ylim))
    legend.adj <- c(1, 0.5)
  } else if(legend.pos[1] == "bottomleft") {
    legend.pos <- c(xlim[1], ylim[1])
    legend.adj <- c(0, 0)
  } else if(legend.pos[1] == "bottom") {
    legend.pos <- c(mean(xlim), ylim[1])
    legend.adj <- c(0.5, 0)
  } else if(legend.pos[1] == "bottomright") {
    legend.pos <- c(xlim[2], ylim[1])
    legend.adj <- c(1, 0)
  }

  ## Plot output --------------------------------------------------------------

  ## determine number of subheader lines to shif the plot
  shift.lines <- if(summary.pos[1] == "sub" & mtext == "") {
    length(label.text) - 1
  } else {1}

  ## setup plot area
  if(par.local){
    if (shift.lines <= 0)
    shift.lines <- 1
    par.default <- par(mfrow = c(1, 1), cex = cex, oma = c(0, 1, shift.lines - 1, 1))
    on.exit(par(par.default))
  }

  ## optionally plot values and error bars
  if(boxplot == FALSE) {
    ## plot data and error
    if(missing(preheat) == TRUE) {
      ## create empty plot
      plot(NA,NA,
           xlim = xlim,
           ylim = ylim,
           xlab = xlab,
           ylab = ylab,
           xaxt = "n",
           las = las,
           main = "")

      ##add x-axis ... this avoids digits in the axis labelling
      axis(side = 1, at = 1:(nrow(values[[1]])+1), labels = 1:(nrow(values[[1]])+1), las = las)

      ## add title
      title(main = main,
            line = shift.lines + 2)

      ## add additional lines
      if (!is.null(given.dose)) {
        abline(h = 1)

        if (error.range > 0) {
          ## error range lines
          abline(h = 1 * (1 + error.range / 100), lty = 2)
          abline(h = 1 * (1 - error.range / 100), lty = 2)

          ## error range labels
          text(
            par()$usr[2],
            (1 + error.range / 100) + 0.02,
            paste("+", error.range , " %", sep = ""),
            pos = 2,
            cex = 0.8
          )
          text(
            par()$usr[2],
            (1 - error.range / 100) - 0.02,
            paste("-", error.range , "%", sep = ""),
            pos = 2,
            cex = 0.8
          )
        }

      }

      ## add data and error bars
      for(i in 1:length(values)) {

        points(x = c(1:nrow(values[[i]])),
               y = values[[i]][,1],
               pch = if(nrow(values[[i]]) == length(pch)){ pch } else { pch[i] },
               col = if(nrow(values[[i]]) == length(col)){ col } else { col[i] },
               cex = 1.2 * cex)

        arrows(c(1:nrow(values[[i]])),
               values[[i]][,1] + values[[i]][,2],
               c(1:nrow(values[[i]])),
               values[[i]][,1] - values[[i]][,2],
               angle = 90,
               length = 0.075,
               code = 3,
               col = if(nrow(values[[i]]) == length(col)){ col } else { col[i] })

        ## add summary content
        if(summary.pos[1] != "sub") {
          text(x = summary.pos[1],
               y = summary.pos[2],
               adj = summary.adj,
               labels = label.text[[i]],
               cex = 0.8 * cex,
               col = if(nrow(values[[i]]) == length(col)){ "black" } else { col[i] })
        } else {
          if(mtext == "") {
            mtext(side = 3,
                  line = - i + 2.5,
                  text = label.text[[i]],
                  col = if(nrow(values[[i]]) == length(col)){ "black" } else { col[i] },
                  cex = cex * 0.8)
          }
        }
      }
    } else {

      ## option for provided preheat data
      ## create empty plot
      plot(NA,NA,
           xlim = c(min(modes.plot) * 0.9, max(modes.plot) * 1.1),
           ylim = ylim,
           xlab = xlab,
           ylab = ylab,
           las = las,
           main = "",
           axes = FALSE,
           frame.plot = TRUE)

      ## add axes
      axis(1,
           at = modes.plot,
           labels = modes.plot)
      axis(2)

      ## add title
      title(main = main,
            line = shift.lines + 2)

      ## add additional lines
      if (!is.null(given.dose)) {
        abline(h = 1)

        if (error.range > 0) {
          ## error range lines
          abline(h = 1 * (1 + error.range / 100), lty = 2)
          abline(h = 1 * (1 - error.range / 100), lty = 2)

          ## error range labels
          text(
            par()$usr[2],
            (1 + error.range / 100) + 0.02,
            paste("+", error.range , " %", sep = ""),
            pos = 2,
            cex = 0.8
          )
          text(
            par()$usr[2],
            (1 - error.range / 100) - 0.02,
            paste("-", error.range , "%", sep = ""),
            pos = 2,
            cex = 0.8
          )
        }
      }

      ## plot values
      for(i in 1:length(values.preheat)) {
        points(x = values.preheat[[i]][,3],
               y = values.preheat[[i]][,1],
               pch = pch[i],
               col = col[i],
               cex = 1.2 * cex)

        arrows(values.preheat[[i]][,3],
               values.preheat[[i]][,1] + values.preheat[[i]][,2],
               values.preheat[[i]][,3],
               values.preheat[[i]][,1] - values.preheat[[i]][,2],
               angle = 90,
               length = 0.075,
               code = 3,
               col = col[i])
      }
    }
  }

  ## optionally, plot boxplot
  if(boxplot) {
    ## create empty plot
    boxplot(values.boxplot,
            names = modes.plot,
            ylim = ylim,
            xlab = xlab,
            ylab = ylab,
            las = las,
            xaxt = "n",
            main = "",
            border = col)

    ## add axis label, if necessary
    if (length(modes.plot) == 1) {
      axis(side = 1, at = 1, labels = modes.plot, las = las)

    } else if (length(modes.plot) > length(unique(modes.plot))){
      ticks <- seq(from = 1 + ((length(values.boxplot)/length(unique(modes.plot)) - 1)/2),
                   to = length(values.boxplot),
                   by = length(values.boxplot)/length(unique(modes.plot)))

      axis(
        side = 1,
        at = ticks,
        las = las,
        labels = unique(modes.plot))

      ##polygon for a better graphical representation of the groups
      polygon.x <- seq(
        1,length(values.boxplot),
        by = length(values.boxplot) / length(unique(modes.plot))
      )

      polygon.step <- unique(diff(polygon.x) - 1)

      for (x.plyg in polygon.x) {
        polygon(
          x = c(x.plyg,x.plyg,x.plyg + polygon.step, x.plyg + polygon.step),
          y = c(
            par()$usr[3],
            ylim[1] - (ylim[1] - par()$usr[3]) / 2,
            ylim[1] - (ylim[1] - par()$usr[3]) / 2,
            par()$usr[3]
          ),
          col = "grey",
          border = "grey")

      }

    }else{
      axis(side = 1, at = 1:length(unique(modes.plot)), labels = unique(modes.plot), las = las)

    }

    ## add title
    title(main = main,
          line = shift.lines + 2)

    ## add additional lines
    if(!is.null(given.dose)){
      abline(h = 1)

      if(error.range > 0){
        ## error range lines
        abline(h = 1 * (1 + error.range / 100), lty = 2)
        abline(h = 1 * (1 - error.range / 100), lty = 2)

        ## error range labels
        text(par()$usr[2], (1 + error.range / 100) + 0.02,
             paste("+", error.range ," %", sep = ""), pos = 2, cex = 0.8)
        text(par()$usr[2], (1 - error.range / 100) - 0.02,
             paste("-", error.range ,"%", sep = ""), pos = 2, cex = 0.8)
      }
    }

    ## plot data and error
    for(i in 1:length(values)) {
      ## add summary content
      if(summary.pos[1] != "sub") {
        text(x = summary.pos[1],
             y = summary.pos[2],
             adj = summary.adj,
             labels = label.text[[i]],
             cex = 0.8 * cex,
             col = if(nrow(values[[i]]) == length(col)){ "black" } else { col[i] })
      } else {
        if(mtext == "") {
          mtext(side = 3,
                line = - i + 2.5,
                text = label.text[[i]],
                col = if(nrow(values[[i]]) == length(col)){ "black" } else { col[i] },
                cex = cex * 0.8)
        }
      }
    }
  }

  ## optionally add legend content
  if(missing(legend) == FALSE) {
    legend(x = legend.pos[1],
           y = legend.pos[2],
           xjust = legend.adj[1],
           yjust = legend.adj[2],
           legend = legend,
           col = unique(col),
           pch = unique(pch),
           lty = 1,
           cex = cex * 0.8)
  }

  ## optionally add subheader text
  mtext(side = 3,
        line = shift.lines,
        text = mtext,
        cex = 0.8 * cex)

  ##FUN by R Luminescence Team
  if(fun == TRUE) {sTeve()}

}

