#' Plot kernel density estimate with statistics
#'
#' Plot a kernel density estimate of measurement values in combination with the
#' actual values and associated error bars in ascending order. If enabled, the
#' boxplot will show the usual distribution parameters (median as
#' bold line, box delimited by the first and third quartile, whiskers defined
#' by the extremes and outliers shown as points) and also the mean and
#' standard deviation as pale bold line and pale polygon, respectively.
#'
#' The function allows passing several plot arguments, such as \code{main},
#' \code{xlab}, \code{cex}. However, as the figure is an overlay of two
#' separate plots, \code{ylim} must be specified in the order: c(ymin_axis1,
#' ymax_axis1, ymin_axis2, ymax_axis2) when using the cumulative values plot
#' option. See examples for some further explanations. For details on the
#' calculation of the bin-width (parameter \code{bw}) see
#' \code{\link{density}}.\cr\cr
#' A statistic summary, i.e. a collection of statistic measures of
#' centrality and dispersion (and further measures) can be added by specifying
#' one or more of the following keywords:
#' \itemize{
#' \item \code{"n"} (number of samples)
#' \item \code{"mean"} (mean De value)
#' \item \code{"median"} (median of the De values)
#' \item \code{"sd.rel"} (relative standard deviation in percent)
#' \item \code{"sd.abs"} (absolute standard deviation)
#' \item \code{"se.rel"} (relative standard error)
#' \item \code{"se.abs"} (absolute standard error)
#' \item \code{"in.2s"} (percent of samples in 2-sigma range)
#' \item \code{"kurtosis"} (kurtosis)
#' \item \code{"skewness"} (skewness)
#' }
#' Note that the input data for the statistic summary is sent to the function
#' \code{calc_Statistics()} depending on the log-option for the z-scale. If
#' \code{"log.z = TRUE"}, the summary is based on the logarithms of the input
#' data. If \code{"log.z = FALSE"} the linearly scaled data is used. \cr
#' Note as well, that \code{"calc_Statistics()"} calculates these statistic
#' measures in three different ways: \code{unweighted}, \code{weighted} and
#' \code{MCM-based} (i.e., based on Monte Carlo Methods). By default, the
#' MCM-based version is used. If you wish to use another method, indicate this
#' with the appropriate keyword using the argument \code{summary.method}.\cr\cr
#' @param data \code{\link{data.frame}} or \code{\linkS4class{RLum.Results}}
#' object (required): for \code{data.frame}: two columns: De
#' (\code{values[,1]}) and De error (\code{values[,2]}). For plotting multiple
#' data sets, these must be provided as \code{list} (e.g. \code{list(dataset1,
#' dataset2)}).
#'
#' @param na.rm \code{\link{logical}} (with default): exclude NA values
#' from the data set prior to any further operations.
#'
#' @param values.cumulative \code{\link{logical}} (with default): show
#' cumulative individual data.
#'
#' @param order \code{\link{logical}}: Order data in ascending order.
#'
#' @param boxplot \code{\link{logical}} (with default): optionally show a
#' boxplot (depicting median as thick central line, first and third quartile
#' as box limits, whiskers denoting +/- 1.5 interquartile ranges and dots
#' further outliers).
#'
#' @param rug \code{\link{logical}} (with default): optionally add rug.
#'
#' @param summary \code{\link{character}} (optional): add statistic measures of
#' centrality and dispersion to the plot. Can be one or more of several
#' keywords. See details for available keywords.
#'
#' @param summary.pos \code{\link{numeric}} or \code{\link{character}} (with
#' default): optional position coordinates or keyword (e.g. \code{"topright"})
#' for the statistical summary. Alternatively, the keyword \code{"sub"} may be
#' specified to place the summary below the plot header. However, this latter
#' option in only possible if \code{mtext} is not used. In case of coordinate
#' specification, y-coordinate refers to the right y-axis.
#'
#' @param summary.method \code{\link{character}} (with default): keyword
#' indicating the method used to calculate the statistic summary. One out of
#' \code{"unweighted"}, \code{"weighted"} and \code{"MCM"}. See
#' \code{\link{calc_Statistics}} for details.
#'
#' @param bw \code{\link{character}} (with default): bin-width, chose a numeric
#' value for manual setting.
#'
#' @param output \code{\link{logical}}: Optional output of numerical plot
#' parameters. These can be useful to reproduce similar plots. Default is
#' \code{FALSE}.
#'
#' @param \dots further arguments and graphical parameters passed to
#' \code{\link{plot}}.
#'
#' @note The plot output is no 'probability density' plot (cf. the discussion
#' of Berger and Galbraith in Ancient TL; see references)!
#'
#' @section Function version: 3.5.2
#'
#' @author Michael Dietze, GFZ Potsdam (Germany),\cr Sebastian Kreutzer,
#' IRAMAT-CRP2A, Universite Bordeaux Montaigne
#'
#' @seealso \code{\link{density}}, \code{\link{plot}}
#'
#' @examples
#'
#' ## read example data set
#' data(ExampleData.DeValues, envir = environment())
#' ExampleData.DeValues <-
#'   Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))
#'
#' ## create plot straightforward
#' plot_KDE(data = ExampleData.DeValues)
#'
#' ## create plot with logarithmic x-axis
#' plot_KDE(data = ExampleData.DeValues,
#'          log = "x")
#'
#' ## create plot with user-defined labels and axes limits
#' plot_KDE(data = ExampleData.DeValues,
#'          main = "Dose distribution",
#'          xlab = "Dose (s)",
#'          ylab = c("KDE estimate", "Cumulative dose value"),
#'          xlim = c(100, 250),
#'          ylim = c(0, 0.08, 0, 30))
#'
#' ## create plot with boxplot option
#' plot_KDE(data = ExampleData.DeValues,
#'          boxplot = TRUE)
#'
#' ## create plot with statistical summary below header
#' plot_KDE(data = ExampleData.DeValues,
#'          summary = c("n", "median", "skewness", "in.2s"))
#'
#' ## create plot with statistical summary as legend
#' plot_KDE(data = ExampleData.DeValues,
#'          summary = c("n", "mean", "sd.rel", "se.abs"),
#'          summary.pos = "topleft")
#'
#' ## split data set into sub-groups, one is manipulated, and merge again
#' data.1 <- ExampleData.DeValues[1:15,]
#' data.2 <- ExampleData.DeValues[16:25,] * 1.3
#' data.3 <- list(data.1, data.2)
#'
#' ## create plot with two subsets straightforward
#' plot_KDE(data = data.3)
#'
#' ## create plot with two subsets and summary legend at user coordinates
#' plot_KDE(data = data.3,
#'          summary = c("n", "median", "skewness"),
#'          summary.pos = c(110, 0.07),
#'          col = c("blue", "orange"))
#'
#' ## example of how to use the numerical output of the function
#' ## return plot output to draw a thicker KDE line
#' KDE_out <- plot_KDE(data = ExampleData.DeValues,
#' output = TRUE)
#'
#' @export
plot_KDE <- function(
  data,
  na.rm = TRUE,
  values.cumulative = TRUE,
  order = TRUE,
  boxplot = TRUE,
  rug = TRUE,
  summary,
  summary.pos,
  summary.method = "MCM",
  bw = "nrd0",
  output = FALSE,
  ...
) {

  ## check data and parameter consistency -------------------------------------

  ## account for depreciated arguments
  if("centrality" %in% names(list(...))) {

    boxplot <- TRUE

    warning(paste("[plot_KDE()] Argument 'centrality' no longer supported. ",
                  "Replaced by 'boxplot = TRUE'."))
  }

  if("dispersion" %in% names(list(...))) {

    boxplot <- TRUE

    warning(paste("[plot_KDE()] Argument 'dispersion' no longer supported. ",
                  "Replaced by 'boxplot = TRUE'."))
  }

  if("polygon.col" %in% names(list(...))) {

    boxplot <- TRUE

    warning(paste("[plot_KDE()] Argument 'polygon.col' no longer supported. ",
                  "Replaced by 'boxplot = TRUE'."))
  }

  if("weights" %in% names(list(...))) {

    warning(paste("[plot_KDE()] Argument 'weights' no longer supported. ",
                  "Weights are omitted."))
  }

  ## Homogenise input data format
  if(is(data, "list") == FALSE) {

    data <- list(data)
  }

  ## check/adjust input data structure
  for(i in 1:length(data)) {

    if(is(data[[i]], "RLum.Results") == FALSE &
         is(data[[i]], "data.frame") == FALSE &
         is.numeric(data[[i]]) == FALSE) {
      stop(paste("[plot_KDE()] Input data format is neither",
                 "'data.frame', 'RLum.Results' nor 'numeric'"))
    } else {

      if(is(data[[i]], "RLum.Results") == TRUE) {
        data[[i]] <- get_RLum(data[[i]], "data")
      }

      if(length(data[[i]]) < 2) {
        data[[i]] <- cbind(data[[i]], rep(NA, length(data[[i]])))
      }
    }
  }

  ## check/set function parameters
  if(missing(summary) == TRUE) {
    summary <- ""
  }

  if(missing(summary.pos) == TRUE) {
    summary.pos <- "sub"
  }

  ## set mtext output
  if("mtext" %in% names(list(...))) {
    mtext <- list(...)$mtext
  } else {
    mtext <- ""
  }

  ## check/set layout definitions
  if("layout" %in% names(list(...))) {
    layout <- get_Layout(layout = list(...)$layout)
  } else {
    layout <- get_Layout(layout = "default")
  }

  ## data preparation steps ---------------------------------------------------

  ## optionally, count and exclude NA values and print result
  if(na.rm == TRUE) {
    for(i in 1:length(data)) {
      n.NA <- sum(is.na(data[[i]][,1]))
      if(n.NA == 1) {
        message(paste("1 NA value excluded from data set", i, "."))
      } else if(n.NA > 1) {
        message(paste(n.NA, "NA values excluded from data set", i, "."))
      }
      data[[i]] <- na.exclude(data[[i]])
    }
  }

  ## optionally, order data set
  if(order == TRUE) {
    for(i in 1:length(data)) {
      data[[i]] <- data[[i]][order(data[[i]][,1]),]
    }
  }

  ## calculate and paste statistical summary
  De.stats <- matrix(nrow = length(data), ncol = 12)
  colnames(De.stats) <- c("n",
                          "mean",
                          "median",
                          "kde.max",
                          "sd.abs",
                          "sd.rel",
                          "se.abs",
                          "se.rel",
                          "q.25",
                          "q.75",
                          "skewness",
                          "kurtosis")
  De.density <- list(NA)

  ## loop through all data sets
  for(i in 1:length(data)) {
    statistics <- calc_Statistics(data[[i]])[[summary.method]]

    De.stats[i,1] <- statistics$n
    De.stats[i,2] <- statistics$mean
    De.stats[i,3] <- statistics$median
    De.stats[i,5] <- statistics$sd.abs
    De.stats[i,6] <- statistics$sd.rel
    De.stats[i,7] <- statistics$se.abs
    De.stats[i,8] <- statistics$se.rel
    De.stats[i,9] <- quantile(data[[i]][,1], 0.25)
    De.stats[i,10] <- quantile(data[[i]][,1], 0.75)
    De.stats[i,11] <- statistics$skewness
    De.stats[i,12] <- statistics$kurtosis

    De.density[[length(De.density) + 1]] <- density(data[[i]][,1],
                                                    kernel = "gaussian",
                                                    bw = bw)
  }

  ## remove dummy list element
  De.density[[1]] <- NULL

  ## create global data set
  De.global <- data[[1]][,1]
  De.error.global <- data[[1]][,2]
  De.density.range <- matrix(nrow = length(data),
                             ncol = 4)

  for(i in 1:length(data)) {
    ##global De and De.error vector
    De.global <- c(De.global, data[[i]][,1])
    De.error.global <- c(De.error.global, data[[i]][,2])

    ## density ranges
    De.density.range[i,1] <- min(De.density[[i]]$x)
    De.density.range[i,2] <- max(De.density[[i]]$x)
    De.density.range[i,3] <- min(De.density[[i]]$y)
    De.density.range[i,4] <- max(De.density[[i]]$y)

    ## position of maximum KDE value
    De.stats[i,4] <- De.density[[i]]$x[which.max(De.density[[i]]$y)]
  }

  ## Get global range of densities
  De.density.range <- c(min(De.density.range[,1]),
                        max(De.density.range[,2]),
                        min(De.density.range[,3]),
                        max(De.density.range[,4]))

  label.text = list(NA)

  if(summary.pos[1] != "sub") {
    n.rows <- length(summary)

    for(i in 1:length(data)) {
      stops <- paste(rep("\n", (i - 1) * n.rows), collapse = "")

      summary.text <- character(0)

      for(j in 1:length(summary)) {
        summary.text <- c(summary.text,
                          paste(
                            "",
                            ifelse("n" %in% summary[j] == TRUE,
                                   paste("n = ",
                                         De.stats[i,1],
                                         "\n",
                                         sep = ""),
                                   ""),
                            ifelse("mean" %in% summary[j] == TRUE,
                                   paste("mean = ",
                                         round(De.stats[i,2], 2),
                                         "\n",
                                         sep = ""),
                                   ""),
                            ifelse("median" %in% summary[j] == TRUE,
                                   paste("median = ",
                                         round(De.stats[i,3], 2),
                                         "\n",
                                         sep = ""),
                                   ""),
                            ifelse("kde.max" %in% summary[j] == TRUE,
                                   paste("kdemax = ",
                                         round(De.stats[i,4], 2),
                                         " \n ",
                                         sep = ""),
                                   ""),
                            ifelse("sd.abs" %in% summary[j] == TRUE,
                                   paste("sd = ",
                                         round(De.stats[i,5], 2),
                                         "\n",
                                         sep = ""),
                                   ""),
                            ifelse("sd.rel" %in% summary[j] == TRUE,
                                   paste("rel. sd = ",
                                         round(De.stats[i,6], 2), " %",
                                         "\n",
                                         sep = ""),
                                   ""),
                            ifelse("se.abs" %in% summary[j] == TRUE,
                                   paste("se = ",
                                         round(De.stats[i,7], 2),
                                         "\n",
                                         sep = ""),
                                   ""),
                            ifelse("se.rel" %in% summary[j] == TRUE,
                                   paste("rel. se = ",
                                         round(De.stats[i,8], 2), " %",
                                         "\n",
                                         sep = ""),
                                   ""),
                            ifelse("skewness" %in% summary[j] == TRUE,
                                   paste("skewness = ",
                                         round(De.stats[i,11], 2),
                                         "\n",
                                         sep = ""),
                                   ""),
                            ifelse("kurtosis" %in% summary[j] == TRUE,
                                   paste("kurtosis = ",
                                         round(De.stats[i,12], 2),
                                         "\n",
                                         sep = ""),
                                   ""),
                            ifelse("in.2s" %in% summary[j] == TRUE,
                                   paste("in 2 sigma = ",
                                         round(sum(data[[i]][,1] >
                                                     (De.stats[i,2] - 2 *
                                                        De.stats[i,5]) &
                                                     data[[i]][,1] <
                                                     (De.stats[i,2] + 2 *
                                                        De.stats[i,5])) /
                                                 nrow(data[[i]]) * 100 , 1),
                                         " %",
                                         sep = ""),
                                   ""),
                            sep = ""))
      }

      summary.text <- paste(summary.text, collapse = "")

      label.text[[length(label.text) + 1]] <- paste(stops,
                                                    summary.text,
                                                    stops,
                                                    sep = "")
    }
  } else {
    for(i in 1:length(data)) {

      summary.text <- character(0)

      for(j in 1:length(summary)) {
        summary.text <- c(summary.text,
                          ifelse("n" %in% summary[j] == TRUE,
                                 paste("n = ",
                                       De.stats[i,1],
                                       " | ",
                                       sep = ""),
                                 ""),
                          ifelse("mean" %in% summary[j] == TRUE,
                                 paste("mean = ",
                                       round(De.stats[i,2], 2),
                                       " | ",
                                       sep = ""),
                                 ""),
                          ifelse("median" %in% summary[j] == TRUE,
                                 paste("median = ",
                                       round(De.stats[i,3], 2),
                                       " | ",
                                       sep = ""),
                                 ""),
                          ifelse("kde.max" %in% summary[j] == TRUE,
                                 paste("kdemax = ",
                                       round(De.stats[i,4], 2),
                                       " | ",
                                       sep = ""),
                                 ""),
                          ifelse("sd.rel" %in% summary[j] == TRUE,
                                 paste("rel. sd = ",
                                       round(De.stats[i,6], 2), " %",
                                       " | ",
                                       sep = ""),
                                 ""),
                          ifelse("sd.abs" %in% summary[j] == TRUE,
                                 paste("abs. sd = ",
                                       round(De.stats[i,5], 2),
                                       " | ",
                                       sep = ""),
                                 ""),
                          ifelse("se.rel" %in% summary[j] == TRUE,
                                 paste("rel. se = ",
                                       round(De.stats[i,8], 2), " %",
                                       " | ",
                                       sep = ""),
                                 ""),
                          ifelse("se.abs" %in% summary[j] == TRUE,
                                 paste("abs. se = ",
                                       round(De.stats[i,7], 2),
                                       " | ",
                                       sep = ""),
                                 ""),
                          ifelse("skewness" %in% summary[j] == TRUE,
                                 paste("skewness = ",
                                       round(De.stats[i,11], 2),
                                       " | ",
                                       sep = ""),
                                 ""),
                          ifelse("kurtosis" %in% summary[j] == TRUE,
                                 paste("kurtosis = ",
                                       round(De.stats[i,12], 2),
                                       " | ",
                                       sep = ""),
                                 ""),
                          ifelse("in.2s" %in% summary[j] == TRUE,
                                 paste("in 2 sigma = ",
                                       round(sum(data[[i]][,1] >
                                                   (De.stats[i,2] - 2 *
                                                      De.stats[i,5]) &
                                                   data[[i]][,1] <
                                                   (De.stats[i,2] + 2 *
                                                      De.stats[i,5])) /
                                               nrow(data[[i]]) * 100 , 1),
                                       " %   ",
                                       sep = ""),
                                 "")
        )
      }

      summary.text <- paste(summary.text, collapse = "")

      label.text[[length(label.text) + 1]]  <- paste(
        "  ",
        summary.text,
        sep = "")
    }

    ## remove outer vertical lines from string
    for(i in 2:length(label.text)) {
      label.text[[i]] <- substr(x = label.text[[i]],
                                start = 3,
                                stop = nchar(label.text[[i]]) - 3)
    }
  }

  ## remove dummy list element
  label.text[[1]] <- NULL

  ## read out additional parameters -------------------------------------------
  if("main" %in% names(list(...))) {
    main <- list(...)$main
  } else {
    main <- expression(bold(paste(D[e], " distribution")))
  }

  if("sub" %in% names(list(...))) {
    sub <- list(...)$sub
  } else {
    sub <- NULL
  }

  if("xlab" %in% names(list(...))) {
    xlab <- list(...)$xlab
  } else {
    xlab <- expression(paste(D[e], " [Gy]"))
  }

  if("ylab" %in% names(list(...))) {
    ylab <- list(...)$ylab
  } else {
    ylab <- c("Density", "Cumulative frequency")
  }

  if("xlim" %in% names(list(...))) {
    xlim.plot <- list(...)$xlim
  } else {
    xlim.plot <- c(min(c(De.global - De.error.global),
                       De.density.range[1],
                       na.rm = TRUE),
                   max(c(De.global + De.error.global),
                       De.density.range[2],
                       na.rm = TRUE))
  }

  if("ylim" %in% names(list(...))) {
    ylim.plot <- list(...)$ylim
  } else {
    ylim.plot <- c(De.density.range[3],
                   De.density.range[4],
                   0,
                   max(De.stats[,1]))
  }

  if("log" %in% names(list(...))) {
    log.option <- list(...)$log
  } else {
    log.option <- ""
  }

  if("col" %in% names(list(...))) {

    col.main <- list(...)$col
    col.xlab <- 1
    col.ylab1 <- 1
    col.ylab2 <- 1
    col.xtck <- 1
    col.ytck1 <- 1
    col.ytck2 <- 1
    col.box <- 1
    col.mtext <- 1
    col.stats <- list(...)$col
    col.kde.line <- list(...)$col
    col.kde.fill <- NA
    col.value.dot <- list(...)$col
    col.value.bar <- list(...)$col
    col.value.rug <- list(...)$col
    col.boxplot <- list(...)$col
    col.mean.line <- adjustcolor(col = list(...)$col,
                                 alpha.f = 0.4)
    col.sd.bar <- adjustcolor(col = list(...)$col,
                              alpha.f = 0.4)
    col.background <- NA
  } else {

    if(length(layout$kde$colour$main) == 1) {
      col.main <- 1:length(data)
    } else {
      col.main <- layout$kde$colour$main
    }

    if(length(layout$kde$colour$xlab) == 1) {
      col.xlab <- 1:length(data)
    } else {
      col.xlab <- layout$kde$colour$xlab
    }

    if(length(layout$kde$colour$ylab1) == 1) {
      col.ylab1 <- 1:length(data)
    } else {
      col.ylab1 <- layout$kde$colour$ylab1
    }

    if(length(layout$kde$colour$ylab2) == 1) {
      col.ylab2 <- 1:length(data)
    } else {
      col.ylab2 <- layout$kde$colour$ylab2
    }

    if(length(layout$kde$colour$xtck) == 1) {
      col.xtck <- 1:length(data)
    } else {
      col.xtck <- layout$kde$colour$xtck
    }

    if(length(layout$kde$colour$ytck1) == 1) {
      col.ytck1 <- 1:length(data)
    } else {
      col.ytck1 <- layout$kde$colour$ytck1
    }

    if(length(layout$kde$colour$ytck2) == 1) {
      col.ytck2 <- 1:length(data)
    } else {
      col.ytck2 <- layout$kde$colour$ytck2
    }

    if(length(layout$kde$colour$box) == 1) {
      col.box <- 1:length(data)
    } else {
      col.box <- layout$kde$colour$box
    }

    if(length(layout$kde$colour$mtext) == 1) {
      col.mtext <- 1:length(data)
    } else {
      col.mtext <- layout$kde$colour$mtext
    }

    if(length(layout$kde$colour$stats) == 1) {
      col.stats <- 1:length(data)
    } else {
      col.stats <- layout$kde$colour$stats
    }

    if(length(layout$kde$colour$kde.line) == 1) {
      col.kde.line <- 1:length(data)
    } else {
      col.kde.line <- layout$kde$colour$kde.line
    }

    if(length(layout$kde$colour$kde.fill) == 1) {
      col.kde.fill <- 1:length(data)
    } else {
      col.kde.fill <- layout$kde$colour$kde.fill
    }

    if(length(layout$kde$colour$value.dot) == 1) {
      col.value.dot <- 1:length(data)
    } else {
      col.value.dot <- layout$kde$colour$value.dot
    }

    if(length(layout$kde$colour$value.bar) == 1) {
      col.value.bar <- 1:length(data)
    } else {
      col.value.bar <- layout$kde$colour$value.bar
    }

    if(length(layout$kde$colour$value.rug) == 1) {
      col.value.rug <- 1:length(data)
    } else {
      col.value.rug <- layout$kde$colour$value.rug
    }

    if(length(layout$kde$colour$boxplot) == 1) {
      col.boxplot <- 1:length(data)
    } else {
      col.boxplot <- layout$kde$colour$boxplot
    }

    if(length(layout$kde$colour$mean.line) == 1) {
      col.mean.line <- adjustcolor(col = 1:length(data),
                                   alpha.f = 0.4)
    } else {
      col.mean.line <- layout$kde$colour$mean.point
    }

    if(length(layout$kde$colour$sd.bar) == 1) {
      col.sd.bar <- 1:length(data)
    } else {
      col.sd.bar <- layout$kde$colour$sd.line
    }

    if(length(layout$kde$colour$background) == 1) {
      col.background <- 1:length(data)
    } else {
      col.background <- layout$kde$colour$background
    }

  }

  if("lty" %in% names(list(...))) {
    lty <- list(...)$lty
  } else {
    lty <- rep(1, length(data))
  }

  if("lwd" %in% names(list(...))) {
    lwd <- list(...)$lwd
  } else {
    lwd <- rep(1, length(data))
  }

  if("cex" %in% names(list(...))) {
    cex <- list(...)$cex
  } else {
    cex <- 1
  }

  if("fun" %in% names(list(...))) {
    fun <- list(...)$fun
  } else {
    fun <- FALSE
  }

  ## convert keywords into summary placement coordinates
  if(missing(summary.pos) == TRUE) {
    summary.pos <- c(xlim.plot[1], ylim.plot[2])
    summary.adj <- c(0, 1)
  } else if(length(summary.pos) == 2) {
    summary.pos <- summary.pos
    summary.adj <- c(0, 1)
  } else if(summary.pos[1] == "topleft") {
    summary.pos <- c(xlim.plot[1], ylim.plot[2])
    summary.adj <- c(0, 1)
  } else if(summary.pos[1] == "top") {
    summary.pos <- c(mean(xlim.plot), ylim.plot[2])
    summary.adj <- c(0.5, 1)
  } else if(summary.pos[1] == "topright") {
    summary.pos <- c(xlim.plot[2], ylim.plot[2])
    summary.adj <- c(1, 1)
  }  else if(summary.pos[1] == "left") {
    summary.pos <- c(xlim.plot[1], mean(ylim.plot[1:2]))
    summary.adj <- c(0, 0.5)
  } else if(summary.pos[1] == "center") {
    summary.pos <- c(mean(xlim.plot), mean(ylim.plot[1:2]))
    summary.adj <- c(0.5, 0.5)
  } else if(summary.pos[1] == "right") {
    summary.pos <- c(xlim.plot[2], mean(ylim.plot[1:2]))
    summary.adj <- c(1, 0.5)
  }else if(summary.pos[1] == "bottomleft") {
    summary.pos <- c(xlim.plot[1], ylim.plot[1])
    summary.adj <- c(0, 0)
  } else if(summary.pos[1] == "bottom") {
    summary.pos <- c(mean(xlim.plot), ylim.plot[1])
    summary.adj <- c(0.5, 0)
  } else if(summary.pos[1] == "bottomright") {
    summary.pos <- c(xlim.plot[2], ylim.plot[1])
    summary.adj <- c(1, 0)
  }

  ## plot data sets -----------------------------------------------------------

  ## setup plot area
  if(length(summary) >= 1 & summary.pos[1] == "sub") {

    toplines <- length(data)
  } else {

    toplines <- 1
  }

  ## extract original plot parameters
  par(bg = layout$kde$colour$background)
  bg.original <- par()$bg

  par(mar = c(5, 5.5, 2.5 + toplines, 4.5),
      xpd = FALSE,
      cex = cex)

  if(layout$kde$dimension$figure.width != "auto" |
     layout$kde$dimension$figure.height != "auto") {
    par(mai = layout$kde$dimension$margin / 25.4,
        pin = c(layout$kde$dimension$figure.width / 25.4 -
                  layout$kde$dimension$margin[2] / 25.4 -
                  layout$kde$dimension$margin[4] / 25.4,
                layout$kde$dimension$figure.height / 25.4 -
                  layout$kde$dimension$margin[1] / 25.4 -
                  layout$kde$dimension$margin[3]/25.4))
  }

  ## create empty plot to get plot dimensions
  plot(NA,
       xlim = xlim.plot,
       ylim = ylim.plot[1:2],
       sub = sub,
       log = log.option,
       axes = FALSE,
       ann = FALSE)

  ## get line height in xy coordinates
  l_height <- par()$cxy[2]

  ## optionally update ylim
  if(boxplot == TRUE) {

    ylim.plot[1] <- ylim.plot[1] - 1.4 * l_height
  }

  ## create empty plot to set adjusted plot dimensions
  par(new = TRUE)
  plot(NA,
       xlim     = xlim.plot,
       ylim     = ylim.plot[1:2],
       log      = log.option,
       cex      = cex,
       axes = FALSE,
       ann = FALSE)

  ## add box
  box(which = "plot",
      col = layout$kde$colour$box)

  ## add x-axis
  axis(side = 1,
       col = layout$kde$colour$xtck,
       col.axis = layout$kde$colour$xtck,
       labels = NA,
       tcl = -layout$kde$dimension$xtcl / 200,
       cex = cex)

  axis(side = 1,
       line = 2 * layout$kde$dimension$xtck.line / 100 - 2,
       lwd = 0,
       col = layout$kde$colour$xtck,
       family = layout$kde$font.type$xtck,
       font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                      layout$kde$font.deco$xtck],
       col.axis = layout$kde$colour$xtck,
       cex.axis = layout$kde$font.size$xlab/12)

  mtext(text = xlab,
        side = 1,
        line = 3 * layout$kde$dimension$xlab.line / 100,
        col = layout$kde$colour$xlab,
        family = layout$kde$font.type$xlab,
        font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                       layout$kde$font.deco$xlab],
        cex = cex * layout$kde$font.size$xlab/12)

  ## add left y-axis
  axis(side = 2,
       at = pretty(x = range(De.density.range[3:4])),
       col = layout$kde$colour$ytck1,
       col.axis = layout$kde$colour$ytck1,
       labels = NA,
       tcl = -layout$kde$dimension$ytck1 / 200,
       cex = cex)

  axis(side = 2,
       at = pretty(x = range(De.density.range[3:4])),
       line = 2 * layout$kde$dimension$ytck1.line / 100 - 2,
       lwd = 0,
       col = layout$kde$colour$ytck1,
       family = layout$kde$font.type$ytck1,
       font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                      layout$kde$font.deco$ytck1],
       col.axis = layout$kde$colour$ytck1,
       cex.axis = layout$kde$font.size$ylab1/12)

  mtext(text = ylab[1],
        side = 2,
        line = 3 * layout$kde$dimension$ylab1.line / 100,
        col = layout$kde$colour$ylab1,
        family = layout$kde$font.type$ylab1,
        font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                       layout$kde$font.deco$ylab1],
        cex = cex * layout$kde$font.size$ylab1/12)

  for(i in 1:length(data)) {
    polygon(x = c(par()$usr[1], De.density[[i]]$x, par()$usr[2]),
            y = c(min(De.density[[i]]$y),De.density[[i]]$y, min(De.density[[i]]$y)),
            border = col.kde.line[i],
            col = col.kde.fill,
            lty = lty[i],
            lwd = lwd[i])
  }

  ## add plot title
  cex.old <- par()$cex
  par(cex = layout$kde$font.size$main / 12)
  title(main = main,
        family = layout$kde$font.type$main,
        font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                       layout$kde$font.deco$main],
        col.main = layout$kde$colour$main,
        line = (toplines + 1.2) * layout$kde$dimension$main / 100)
  par(cex = cex.old)

  ## optionally add mtext line
  if(mtext != "") {

    mtext(text = mtext,
          side = 3,
          line = 0.5,
          family = layout$kde$font.type$mtext,
          font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                         layout$kde$font.deco$mtext],
          col.main = layout$kde$colour$mtext,
          cex = layout$kde$font.size$mtext / 12)
  }

  ## add summary content
  for(i in 1:length(data)) {

    if(summary.pos[1] != "sub") {

      text(x = summary.pos[1],
           y = summary.pos[2],
           adj = summary.adj,
           labels = label.text[[i]],
           col = col.stats[i],
           cex = layout$kde$font.size$stats / 12)
    } else {

      if(mtext == "") {

        mtext(side = 3,
              line = (toplines + 0.3 - i) * layout$kde$dimension$stats.line / 100,
              text = label.text[[i]],
              col = col.stats[i],
              cex = layout$kde$font.size$stats / 12)
      }
    }
  }

  if(values.cumulative == TRUE) {

    ## create empty overlay plot
    par(new = TRUE) # adjust plot options

    ## add empty plot, scaled to preliminary secondary plot content
    plot(x = NA,
         xlim = xlim.plot,
         ylim = ylim.plot[3:4],
         log  = log.option,
         ann = FALSE,
         axes = FALSE
         )

    ## get line height in xy coordinates
    l_height <- par()$cxy[2]

    ## optionally update ylim
    if(boxplot == TRUE) {

      ylim.plot[3] <- ylim.plot[3] - 1.4 * l_height
    }

    ## create correctly scaled empty overlay plot
    par(new = TRUE) # adjust plot options

    ## add empty plot, scaled to secondary plot content
    plot(NA,
         xlim = xlim.plot,
         ylim = ylim.plot[3:4],
         log  = log.option,
         ann = FALSE,
         axes = FALSE)

    ## optionally add boxplot
    if(boxplot == TRUE) {

      ## add zero line
      abline(h = 0)

      ## get extended boxplot data
      boxplot.data <- list(NA)

      for(i in 1:length(data)) {
        boxplot.i <- boxplot(x = data[[i]][,1],
                             plot = FALSE)
        boxplot.i$group <- mean(x = data[[i]][,1],
                                                   na.rm = TRUE)
        boxplot.i$names <- sd(x = data[[i]][,1],
                                                   na.rm = TRUE)
        boxplot.data[[length(boxplot.data) + 1]] <- boxplot.i
      }

      ## remove dummy list object
      boxplot.data[[1]] <- NULL

      ## get new line hights
      l_height <- par()$cxy[2]

      for(i in 1:length(data)) {

        # ## draw sd line
        # lines(x = c(boxplot.data[[i]]$group[1] - boxplot.data[[i]]$names[1],
        #             boxplot.data[[i]]$group[1] + boxplot.data[[i]]$names[1]),
        #       y = c(-5/8 * l_height,
        #             -5/8 * l_height),
        #       col = col.mean.line[i])
        #
        # ## draw mean line
        # points(x = boxplot.data[[i]]$group[1],
        #       y = -5/8 * l_height,
        #       pch = 18,
        #       col = col.mean.line[i])

        ## draw median line
        lines(x = c(boxplot.data[[i]]$stats[3,1],
                    boxplot.data[[i]]$stats[3,1]),
              y = c(-11/8 * l_height,
                    -7/8 * l_height),
              lwd = 2,
              col = col.boxplot[i])

        ## draw q25-q75-polygon
        polygon(x = c(boxplot.data[[i]]$stats[2,1],
                      boxplot.data[[i]]$stats[2,1],
                      boxplot.data[[i]]$stats[4,1],
                      boxplot.data[[i]]$stats[4,1]),
                y = c(-11/8 * l_height,
                      -7/8 * l_height,
                      -7/8 * l_height,
                      -11/8 * l_height),
                border = col.boxplot[i])

        ## draw whiskers
        lines(x = c(boxplot.data[[i]]$stats[2,1],
                    boxplot.data[[i]]$stats[1,1]),
              y = c(-9/8 * l_height,
                    -9/8 * l_height),
              col = col.boxplot[i])

        lines(x = c(boxplot.data[[i]]$stats[1,1],
                    boxplot.data[[i]]$stats[1,1]),
              y = c(-10/8 * l_height,
                    -8/8 * l_height),
              col = col.boxplot[i])

        lines(x = c(boxplot.data[[i]]$stats[4,1],
                    boxplot.data[[i]]$stats[5,1]),
              y = c(-9/8 * l_height,
                    -9/8 * l_height),
              col = col.boxplot[i])

        lines(x = c(boxplot.data[[i]]$stats[5,1],
                    boxplot.data[[i]]$stats[5,1]),
              y = c(-10/8 * l_height,
                    -8/8 * l_height),
              col = col.boxplot[i])

        ## draw outliers
        points(x = boxplot.data[[i]]$out,
               y = rep(-9/8 * l_height,
                       length(boxplot.data[[i]]$out)),
               col = col.boxplot[i],
               cex = cex * 0.8)
      }

    }

    ## optionally add rug
    if(rug == TRUE) {

      for(i in 1:length(data)) {

        for(j in 1:nrow(data[[i]])) {

          lines(x = c(data[[i]][j,1],
                      data[[i]][j,1]),
                y = c(0,
                      -2/8 * l_height),
                col = col.value.rug[i])
        }
      }
    }

    ## add secondary y-axis
    ticks_axis <- pretty(x = c(1, ylim.plot[4]))
    ticks_axis <- ifelse(test = ticks_axis == 0,
                         yes = NA,
                         no = ticks_axis)

    ## add right y-axis
    axis(side = 4,
         at = ticks_axis,
         col = layout$kde$colour$ytck2,
         col.axis = layout$kde$colour$ytck2,
         labels = NA,
         tcl = -layout$kde$dimension$ytck2 / 200,
         cex = cex)

    axis(side = 4,
         at = ticks_axis,
         line = 2 * layout$kde$dimension$ytck2.line / 100 - 2,
         lwd = 0,
         col = layout$kde$colour$ytck2,
         family = layout$kde$font.type$ytck2,
         font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                        layout$kde$font.deco$ytck2],
         col.axis = layout$kde$colour$ytck2,
         cex.axis = layout$kde$font.size$ylab2/12)

    mtext(text = ylab[2],
          side = 4,
          line = 3 * layout$kde$dimension$ylab2.line / 100,
          col = layout$kde$colour$ylab2,
          family = layout$kde$font.type$ylab2,
          font = (1:4)[c("plain", "bold", "italic", "bold italic") ==
                         layout$kde$font.deco$ylab2],
          cex = cex * layout$kde$font.size$ylab2/12)

    ## add De error bars
    for(i in 1:length(data)) {
      arrows(data[[i]][,1] - data[[i]][,2]/2,
             1:length(data[[i]][,1]),
             data[[i]][,1] + data[[i]][,2]/2,
             1:length(data[[i]][,1]),
             code = 3,
             angle = 90,
             length = 0.05,
             col = col.value.bar[i])

      ## add De measurements
      points(data[[i]][,1], 1:De.stats[i,1],
             col = col.value.dot[i],
             pch = 20)
    }
  }

  ## add empty plot
  par(new = TRUE)
  plot(NA,
       ann = FALSE,
       axes = FALSE,
       xlim     = xlim.plot,
       ylim     = ylim.plot[1:2],
       log      = log.option,
       cex      = cex,
       cex.lab  = cex,
       cex.main = cex,
       cex.axis = cex)

  ## FUN by R Luminescence Team
  if(fun==TRUE){sTeve()}

  if(output == TRUE) {
    return(list(De.stats = De.stats,
                summary.pos = summary.pos,
                De.density = De.density))
  }

}
