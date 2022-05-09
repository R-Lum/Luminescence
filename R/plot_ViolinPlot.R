#' @title  Create a violin plot
#'
#' @description
#' Draws a kernel density plot in combination with a boxplot in its middle. The shape of the violin
#' is constructed using a mirrored density curve. This plot is especially designed for cases
#' where the individual errors are zero or to small to be visualised. The idea for this plot is
#' based on the the 'volcano plot' in the ggplot2 package by Hadley Wickham and Winston Chang.
#' The general idea for the violin plot seems to be introduced by Hintze and Nelson (1998).
#'
#' The function is passing several arguments to the function [plot],
#' [stats::density], [graphics::boxplot]:
#'
#' Supported arguments are:
#' `xlim`, `main`, `xlab`, `ylab`, `col.violin`, `col.boxplot`, `mtext`, `cex`, `mtext`
#'
#' **`Valid summary keywords`**
#'
#' `'n'`, `'mean'`, `'median'`, `'sd.abs'`, `'sd.rel'`, `'se.abs'`, `'se.rel'`.
#' `'skewness'`, `'kurtosis'`
#'
#' @param data [numeric] or [RLum.Results-class] (**required**):
#' input data for plotting. Alternatively a [data.frame] or a [matrix] can
#' be provided, but only the first column will be considered by the
#' function
#'
#' @param boxplot [logical] (*with default*):
#' enable or disable boxplot
#'
#' @param rug [logical] (*with default*):
#' enable or disable rug
#'
#' @param summary [character] (*optional*):
#' add statistic measures of centrality and dispersion to the plot.
#' Can be one or more of several keywords. See details for available keywords.
#'
#' @param summary.pos [numeric] or [character] (*with default*):
#' optional position keywords (cf. [legend]) for the statistical summary.
#' Alternatively, the keyword `"sub"` may be specified to place the summary
#' below the plot header. However, this latter option in only possible if
#' `mtext` is not used.
#'
#' @param na.rm [logical] (*with default*):
#' exclude NA values from the data set prior to any further operations.
#'
#' @param ... further arguments and graphical parameters passed to
#' [plot.default], [stats::density] and [boxplot]. See details for further information
#'
#' @note
#' Although the code for this function was developed independently and just the idea for the plot
#' was based on the 'ggplot2' package plot type 'volcano', it should be mentioned that, beyond this,
#' two other R packages exist providing a possibility to produces this kind of plot, namely:
#' `'vioplot'` and `'violinmplot'` (see references for details).
#'
#' @section Function version: 0.1.4
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @references
#' Daniel Adler (2005). vioplot: A violin plot is a combination of a box plot and a kernel density plot.
#' R package version 0.2 http://CRAN.R-project.org/package=violplot
#'
#' Hintze, J.L., Nelson, R.D., 1998. A Box Plot-Density Trace Synergism. The American Statistician 52, 181-184.
#'
#' Raphael W. Majeed (2012). violinmplot: Combination of violin plot with mean and standard deviation.
#' R package version 0.2.1. http://CRAN.R-project.org/package=violinmplot
#'
#' Wickham. H (2009). ggplot2: elegant graphics for data analysis. Springer New York.
#'
#' @seealso [stats::density], [plot], [boxplot], [rug], [calc_Statistics]
#'
#' @examples
#'
#' ## read example data set
#' data(ExampleData.DeValues, envir = environment())
#' ExampleData.DeValues <- Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))
#'
#' ## create plot straightforward
#' plot_ViolinPlot(data = ExampleData.DeValues)
#'
#' @md
#' @export
plot_ViolinPlot <- function(
  data,
  boxplot = TRUE,
  rug = TRUE,
  summary = NULL,
  summary.pos = "sub",
  na.rm = TRUE,
  ...
) {


  # Integrity tests and conversion --------------------------------------------------------------

    ##Prechecks

    if(missing(data)){
      stop("[plot_ViolinPlot()] I don't know what to do, data input needed." )

    }else{

      ##check for RLum.Results object
      if(is(data, "RLum.Results")){
        data <- get_RLum(data, "data")

      }

      ##if data.frame or matrix
      if(is(data, "data.frame") | is(data, "matrix")){
        data <- data[,1]

      }

    }

    ##Remove NA values
    if(na.rm){
      data <- na.exclude(data)

      if(length(attr(data, "na.action")) > 0){
        warning(paste("[plot_ViolinPlot()]", length(attr(data, "na.action")), "NA values removed!"), call. = FALSE)
      }

    }

    #Further checks
    if(!is(summary.pos, "character")){
      stop("[plot_ViolinPlot()] argument 'summary.pos' needs to be of type character!")

    }

  ##stop if only one or 0 values are left in data
  if(length(data) == 0){
    warning("[plot_ViolinePlot()] Actually it is rather hard to plot 0 values. NULL returned", call. = FALSE)
    return()
  }

  # Pre-calculations ----------------------------------------------------------------------------


  ##density for the violin
  if(length(data)>1){
    density <-
      density(x = data,
              bw = ifelse("bw" %in% names(list(...)),list(...)$bw,"nrd0"))

  }else{
    density <- NULL
    warning("[plot_ViolinePlot()] single data point found, no density calculated.", call. = FALSE)

  }


  ##some statistical parameter, get rid of the weighted statistics
  stat.summary <- list(suppressWarnings(calc_Statistics(as.data.frame(data), digits = 2)[["unweighted"]]))
  names(stat.summary) <- "unweighted"

    ##make valid summary string
    if(is.null(summary)){
      summary <- c("n","median")

    }

    ##at least show a warning for invalid keywords
    if(!all(summary %in% names(stat.summary[[1]]))){
      warning(paste0("[plot_ViolinePlot()] Only keywords for weighted statistical measures are supported. Valid keywords are: ",
                     paste(names(stat.summary[[1]]), collapse = ", ")), call. = FALSE)
    }

    ##make sure that only valid keywords make it
    summary <- summary[(summary %in% names(stat.summary[[1]]))]

    stat.text <- .create_StatisticalSummaryText(stat.summary, keywords = summary, sep = " \n ")
    stat.mtext <- .create_StatisticalSummaryText(stat.summary, keywords = summary, sep = " | ")


  # Plot settings -------------------------------------------------------------------------------

  ##set default values
  plot.settings <- list(
    xlim = if(!is.null(density)){range(density$x)}else{c(data[1]*0.9, data[1]*1.1)},
    main = "Violin Plot",
    xlab = expression(paste(D[e], " [a.u.]")),
    ylab = if(!is.null(density)){"Density"}else{" "},
    col.violin = rgb(0,0,0,0.2),
    col.boxplot = NULL,
    mtext = ifelse(summary.pos != 'sub', "", stat.mtext),
    cex = 1
  )

  ##modify list accordingly
  plot.settings <- modifyList(plot.settings, val = list(...))


  # Plot ----------------------------------------------------------------------------------------

  ##open empty plot area
  plot(
    NA,NA,
    xlim = plot.settings$xlim,
    ylim = c(0.2,1.8),
    xlab = plot.settings$xlab,
    ylab = plot.settings$ylab,
    yaxt = "n",
    main = plot.settings$main,
    cex = plot.settings$cex
  )

  ##add polygon ... the violin
  if(!is.null(density)){
    polygon(
      x = c(density$x, rev(density$x)),
      y = c(1 + density$y / max(density$y) * 0.5,
            rev(1 - density$y / max(density$y) * 0.5)),
      col = plot.settings$col.violin,
      border = plot.settings$col.violin
    )


  }

  ##add the boxplot
  if(boxplot){
    boxplot(
      data,
      outline = TRUE,
      boxwex = 0.4,
      horizontal = TRUE,
      axes = FALSE,
      add = TRUE,
      col = plot.settings$col.boxplot
    )

  }

  ##add rug
  if(rug){
    rug(x = data)

  }

  ##add mtext
  if(!is.null(plot.settings$mtext)){
    mtext(side = 3, text = plot.settings$mtext)

  }

  ##add stat.text
  if (summary.pos != "sub") {

    valid_keywords <-
      c(
        "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"
      )

    if (any(
      summary.pos %in% valid_keywords
    )) {
      legend(summary.pos, legend = stat.text, bty = "n")

    }else{
      warning_text <- paste0("Value provided for 'summary.pos' is not a valid keyword, valid keywords are:",
                             paste(valid_keywords, collapse = ", "))
      warning(warning_text)

    }

  }

}
